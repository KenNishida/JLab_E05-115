      SUBROUTINE E_TARG_TRANS(ABORT,err,istat)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods :  Transforms tracks from enge focal plane to 
*     -                          target.
*     -
*     -      Required Input BANKS     Enge_FOCAL_PLANE
*     -
*     -      Output BANKS             Enge_TARGET
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
*     $Log: e_targ_trans.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.2  2005/06/23 17:17:36  cdaq
*     Bug fix ENGE optics
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.6  2005/04/21 23:22:32  miyoshi
*     add target variables readout for debuging only
*
*     Revision 1.5  2005/04/19 17:48:05  miyoshi
*     correct coordinate
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
      parameter (here= 'e_targ_trans')
*     
      logical ABORT
      character*(*) err
      integer*4   istat,k
*
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'hes_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hes_tracking.cmn'
      include 'hes_recon_elements.cmn'
      include 'hes_id_histid.cmn'
      include 'hes_physics_sing.cmn'
      include 'gen_event_info.cmn'
*     
*     Misc. variables.
      
      integer*4        i,j,itrk
      real*8           sum(4),hut(4),term
      
*=============================Executable Code ================================
      ABORT= .FALSE.
      err= ' '

*************************************************************
*     begin of debugging
*     !!! debug only !!!
*************************************************************
      if(e_tar_output_on .eq. 1) then ! tracking output exists.
         if(e_kept_tar_index .eq. 0) then
            Read(82,*,ERR=71) e_kept_tar_ID_number,e_kept_ntar
c     Write(*,*) e_kept_track_ID_number,e_kept_ntrack
            e_kept_tar_index = 1
         EndIF
         if(e_kept_tar_ID_number .eq. gen_event_ID_number) then
            if(e_kept_ntar .gt. 0) then
               Do i=1,entracks_fp
                  Read(82,*) exp_tar(i),eyp_tar(i),ep_tar(i)
                  ex_tar(i) = 0
                  ey_tar(i) = 0
                  edelta_tar(i) = (ep_tar(i)/epcentral-1)*100
               EndDo		! track loop
            EndIF		! ntrack>0
            e_kept_tar_index = 0
         Else			! not match
c---  first ,kept=0 < genID=1
            Do while(e_kept_tar_ID_number .lt. gen_event_ID_number)
c---  Here trackID < eventID.
c---  e_kept_track_index = 0 ; the next readout is track ID and ntrack.
               if(e_kept_tar_index .eq. 0) then
                  Read(82,*) e_kept_tar_ID_number,e_kept_ntar
c     Write(*,*) e_kept_track_ID_number,e_kept_ntrack
                  e_kept_tar_index = 1
               EndIF
               if(e_kept_tar_ID_number .eq. gen_event_ID_number) then
                  if(e_kept_ntar .gt. 0) then
                     Do i=1,entracks_fp
                        Read(82,*) exp_tar(i),eyp_tar(i),ep_tar(i)
                        ex_tar(i) = 0
                        ey_tar(i) = 0
                        edelta_tar(i) = (ep_tar(i)/epcentral-1)*100
                     EndDo	! track loop
                  EndIF         ! ntrack>0
                  e_kept_tar_index = 0
               EndIF		! keptID=genID
            EndDo		! keptID<genID
         EndIf                  ! keptID=genID
      EndIF			! hdc_tot_hits=0
      
***************************************************************
*     end of debugging
***************************************************************
      
*     Check for correct initialization.
      
      if(e_tar_output_on .ne. 1) then
         
         if (e_recon_initted.ne.1) then
            istat = 2
            return
         endif
         istat = 1
         
*     Loop over tracks.
         
         entracks_tar = entracks_fp
         do itrk = 1,entracks_fp
*     
*     set link between target and focal plane track. Currenty 1 to 1
            elink_tar_fp(itrk) = itrk
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
            
c     --- x(cm),y(cm),xp,yp at fp
            
            hut(1) = ((ex_fp(itrk) + e_z_true_focus * exp_fp(itrk)
     $           + e_det_offset_x)-Center_of_efp(1))/Nfactor_of_efp(1)
            
            hut(2) = (exp_fp(itrk)+e_ang_offset_x
     &           - Center_of_efp(2))/Nfactor_of_efp(2) 
            ! xp_fp (tan*1000.)
            
            hut(3) = ((ey_fp(itrk) + e_z_true_focus * eyp_fp(itrk)
     $           + e_det_offset_y)-Center_of_efp(3))/Nfactor_of_efp(3)
            
            hut(4) = ((eyp_fp(itrk)+ e_ang_offset_y)
     &           - Center_of_efp(4))/Nfactor_of_efp(4) 
            ! yp_fp (tan*1000.)
        
*     Compute COSY sums.
            !hut(4)=0
            do i = 1,e_num_recon_terms
               term = 1.
               do j = 1,4
                  if (e_recon_expon(j,i).ne.0.)
     $                 term = term*hut(j)**e_recon_expon(j,i)
               enddo
               sum(1) = sum(1) + term * e_recon_coeff(1,i)!mom
               sum(2) = sum(2) + term * e_recon_coeff(2,i)!xpt
               sum(3) = sum(3) + term * e_recon_coeff(3,i)!ypt
               sum(4) = sum(4) + term * e_recon_coeff(4,i)!len
            enddo
            
*     Protect against asin argument > 1.
c     if(sum(1).gt. 1.0)  sum(1)= 0.99
c     if(sum(1).lt. -1.0) sum(1)= -.99
c     if(sum(3).gt. 1.0)  sum(3)= 0.99
c     if(sum(3).lt. -1.0) sum(3)= -.99
            
*     Load output values.
            ex_tar(itrk)  = 0.0 ! ** No beam raster **
            ey_tar(itrk)  = 0.0 ! ** No beam raster **
            ez_tar(itrk)  = 0.0 ! Track is at origin
            ep_tar(itrk) = sum(1) !(GeV/c)
            exp_tar(itrk) = tan(sum(2)) ! xp (rad->tan)
            eyp_tar(itrk) = tan(sum(3)) ! yp (rad->tan)
            etrk_pathlength(itrk) = sum(4) ! path length (cm)
            
*     Apply offsets to reconstruction.
            exp_tar(itrk) = exp_tar(itrk) + ephi_offset
            eyp_tar(itrk) = eyp_tar(itrk) + etheta_offset
c            ep_tar(itrk)  = epcentral * (1.0 + edelta_tar(itrk)/100.) 
            
         enddo                  !End of loop over tracks. entracks_fp
         
c--   Transfer to track quantities on HES collimator plane
         call e_coll_trans(ABORT,err)
         if(ABORT) then
            call g_add_path(here,err)
         endif
         
      EndIf                     ! e_tar_output_on .ne. 1

      goto 72
 71   Write(*,*) 'Error in readout of lun=82'
 72   continue
      
*     All done...
*     check print flag to print results
      if(edebugtartrackprint.gt.0) then
         call e_print_tar_tracks
      endif
*     Fill hardwired histograms if sturnon_target_hist is non zero
*     

      call e_fill_target_hist(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
      endif

      
      return
      end
