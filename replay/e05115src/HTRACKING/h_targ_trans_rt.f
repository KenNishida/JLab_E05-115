      SUBROUTINE H_TARG_TRANS_RT(ABORT,err,istat)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods :  Transforms tracks from hks focal plane to 
*     -                          target.
*     -
*     -      Required Input BANKS     HKS_FOCAL_PLANE
*     -
*     -      Output BANKS             HKS_TARGET
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     -   istat   (integer) Status flag. 
*     -   Value returned indicates the following:
*     -           = 1      Normal return.
*     -           = 2      Matrix elements not initted correctly.
*     - 
*     Version:  0.1 (In development)  18-Nov-1993 (DHP)
*     -   
*     -Modified 21-JAN-94  D.F.Geesaman
*     -            Add ABORT and err
*     $Log: h_targ_trans_rt.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.2  2005/08/23 17:26:38  cdaq
*     Use Raytrace for momentum term and ERIKA for angles.
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.1  2005/04/19 15:26:56  miyoshi
*     initial version
*
*     Revision 1.4  2005/04/08 19:01:53  miyoshi
*     RAYTRACE x,y is not matched with analysis x,y so it is converted here
*
*     Revision 1.3  2005/03/08 20:22:10  yuan
*     Remove HNSS specified part
*
*     Revision 1.2  2005/03/02 16:28:14  miyoshi
*     move hist cmn file
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.1.1.1  1999/11/01 13:54:56  ysato
*     Upgrade for HNSS
*     
*     Revision 1.13  1996/09/05 20:15:53  saw
*     (JRA) Apply offsets to reconstruction
*
*     Revision 1.12  1996/01/17 18:10:27  cdaq
*     (JRA)
*     
*     Revision 1.11  1995/10/10 17:52:40  cdaq
*     (JRA) Cleanup
*     
*     Revision 1.10  1995/08/08 16:01:57  cdaq
*     (DD) Add detector and angular offsets
*     
*     Revision 1.9  1995/05/22  19:45:57  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.8  1995/03/23  16:51:57  cdaq
*     (SAW) Previous change wrong.  COSY wants slopes.
*     Target track data is now slopes.
*     
*     Revision 1.7  1995/02/23  16:03:05  cdaq
*     (SAW) Convert focal plane slopes to angles before COSY transport.
*     Target track data is now angles.
*     
*     Revision 1.6  1994/11/23  14:03:27  cdaq
*     (SPB) Recopied from hms file and modified names for SOS
*     
*     Revision 1.5  1994/08/18  04:35:28  cdaq
*     (SAW) ???
*     
*     Revision 1.4  1994/06/14  04:33:22  cdaq
*     (DFG) Add fill SLINK_TAR_FP 1 to 1
*     
*     Revision 1.3  1994/06/07  01:58:56  cdaq
*     (DFG) Protect against asin argument > 1.0
*     
*     Revision 1.2  1994/05/13  03:45:52  cdaq
*     (DFG) Add call to s_fill_dc_target_hist
*     Add calculation of SP_TAR
*     (SAW) Cosmetic changes to source
*     
*     Revision 1.1  1994/02/21  16:41:11  cdaq
*     Initial revision
*     
*     
*     Coordinates for HKS E arm (Splitter+Ene):
*     X=right, Y=down, Z=downstream          L.Y. 03/08/2005

*     Abstract: Reconstruct target scattering variables from track variables in
*     the detectors, using a polynomial (Taylor series) map. The track,
*     target, and map data are all maintained in common blocks.
*     
*     NOTE:     This version assumes that the beam is not rastered.
*     Also, there is no treatment of error matrices, yet.
*     -
*     Right-handed coordinates are assumed: X=down, Z=downstream, Y = (Z cross X)
*     
*     Author:   David H. Potterveld, Argonne National Lab, Nov. 1993
*______________________________________________________________________________
      IMPLICIT NONE
      SAVE
*     
      character*12 here
      parameter (here= 'h_targ_trans')
*     
      logical ABORT
      character*(*) err
      integer*4   istat,k
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hks_tracking.cmn'
      include 'hks_recon_elements.cmn'
      include 'hks_id_histid.cmn'
      include 'hks_physics_sing.cmn'
*     
*     Misc. variables.
      
      integer*4        i,j,itrk
      real*8           sum(4),hut(5),term,hut_rot(5)
      
*=============================Executable Code ================================
      ABORT= .FALSE.
      err= ' '
      
*     Check for correct initialization.
      
      if (h_recon_initted.ne.1) then
         istat = 2
         return
      endif
      istat = 1
      
*     Loop over tracks.
      
      hntracks_tar = hntracks_fp
      do itrk = 1,hntracks_fp
*     
*     set link between target and focal plane track. Currenty 1 to 1
         hlink_tar_fp(itrk) = itrk
*     
*     Reset COSY sums.
         do i = 1,4
            sum(i) = 0.
         enddo
         
*     Load track data into local array, Converting to RAYTRACE units.
*     Note:  At this point, the focal plane variables sxp_fp and syp_fp are
*     still slopes.  We convert them to angles before running them through the
*     COSY transport matrices.
*     It is assumed that the track coordinates are reported at
*     the same focal plane as the COSY matrix elements were calculated.
         
c      write(*,*) 'hx_fp, hxp_fp, hy_fp, hyp_fp'
c      write(*,*)  hx_fp(itrk), hxp_fp(itrk), 
c     >               hy_fp(itrk), hyp_fp(itrk)
         
c     --- x(cm),y(cm),xp,yp at fp


         hut(1) = ((hx_fp(itrk) + h_z_true_focus*hxp_fp(itrk) 
     $        + h_det_offset_x)-Center_of_hfp(1))/Nfactor_of_hfp(1)
         
         hut(2) = (hxp_fp(itrk) + h_ang_offset_x
     &          - Center_of_hfp(2))/Nfactor_of_hfp(2)
         ! xp_fp (tan.)
         
         hut(3) = ((hy_fp(itrk) + h_z_true_focus * hyp_fp(itrk)  
     $        + h_det_offset_y)-Center_of_hfp(3))/Nfactor_of_hfp(3)
         
         hut(4) = (hyp_fp(itrk) + h_ang_offset_y 
     &          - Center_of_hfp(4))/Nfactor_of_hfp(4)
         ! yp_fp (tan.)
         
!     now transform
         hut_rot(1) = hut(1)
         hut_rot(2) = hut(2) + hut(1) * h_ang_slope_x
         hut_rot(3) = hut(3)
         hut_rot(4) = hut(4) + hut(3) * h_ang_slope_y
c         hut_rot(5) = hut(5)
c      write(*,*) "hut",hut
*     Compute COSY sums.
         do i = 1,h_num_recon_terms
            term = 1.
            do j = 1,4
               if (h_recon_expon(j,i).ne.0.) 
     $              term = term*hut(j)**h_recon_expon(j,i)
            enddo
c    Write(*,*) 'term=',i,term
c      Write(*,*) 'hreconexpon=',(h_recon_expon(j,i),j=1,4)
c      Write(*,*) 'hreconcoeff=',(h_recon_coeff(j,i),j=1,4)
c    Write(*,*) 'term*coeff=',i,
c   &           (term*h_recon_coeff(j,i),j=1,4)

            sum(1) = sum(1) + term * h_recon_coeff(1,i)!mom
            sum(2) = sum(2) + term * h_recon_coeff(2,i)!xpt
            sum(3) = sum(3) + term * h_recon_coeff(3,i)!ypt
            sum(4) = sum(4) + term * h_recon_coeff(4,i)!path length
         enddo
c     Write(*,*) 'sum=',(Real(sum(i)),i=1,4)
         
*     Protect against asin argument > 1.
c     if(sum(1).gt. 1.0)  sum(1)= 0.99
c     if(sum(1).lt. -1.0) sum(1)= -.99
c     if(sum(3).gt. 1.0)  sum(3)= 0.99
c     if(sum(3).lt. -1.0) sum(3)= -.99
         
*     Load output values.
         hx_tar(itrk)  = 0.0    ! ** No beam raster **
         hy_tar(itrk)  = 0.0    ! ** No beam raster **
         hz_tar(itrk)  = 0.0    ! Track is at origin
         hp_tar(itrk) = sum(1)  ! mom (GeV/c)
         hxp_tar(itrk) = tan(sum(2)) ! Slope xp (rad)
         hyp_tar(itrk) = tan(sum(3)) ! Slope yp (rad)
         htrk_pathlength(itrk) = sum(4) ! path length (cm)


*     Apply offsets to reconstruction.
c         hdelta_tar(itrk) = hdelta_tar(itrk) + hdelta_offset
cc         hxp_tar(itrk) = hxp_tar(itrk) + hphi_offset
cc         hyp_tar(itrk) = hyp_tar(itrk) + htheta_offset
c         hp_tar(itrk)  = hpcentral * (1.0 + hdelta_tar(itrk)/100.) 
*     Momentum in GeV
c         write(*,*) 'hx_tar, sxp_tar, hy_tar, hyp_tar, hdelta_tar='
c         write(*,*)  hxp_tar(itrk), 
c     >        hyp_tar(itrk), hp_tar(itrk)
         
*     The above coordinates are in the spectrometer reference frame in which the
*     Z axis is along the central ray. Do we need to rotate to the lab frame?
*     For now, I assume not.

      enddo               
      call h_coll_trans(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
      endif
      
*       histogram target quantities
*       

      call h_fill_target_hist(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
      endif

      
      return
      end
