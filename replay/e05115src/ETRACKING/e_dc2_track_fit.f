      subroutine E_DC2_TRACK_FIT(ABORT,err,ierr)
*     primary track fitting routine for the HKS spectrometer
*     
*     Called by S_TRACK
*     
*     d.f. geesaman         8 Sept 1993
*     $Log: e_dc2_track_fit.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.6  2005/04/08 20:56:38  miyoshi
*     change subroutine location
*
*     Revision 1.5  2005/03/14 19:52:17  miyoshi
*     change tracking variables and histogram names
*
*     Revision 1.4  2005/03/07 16:36:45  miyoshi
*     add track hist
*
*     Revision 1.3  2005/03/02 22:32:43  miyoshi
*     add best chi2 variables
*
*     Revision 1.2  2004/12/24 21:35:57  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.8  1996/01/17 18:56:08  cdaq
*     (JRA) Fill sdc_layer_wirecenter and sdc_layer_wirecoord arrays
*     
*     Revision 1.7  1995/10/11 18:15:12  cdaq
*     (JRA) Comment out MINUIT track fitting for now.
*     
*     Revision 1.6  1995/08/31 20:44:56  cdaq
*     (JRA) Don't fill single_residual arrray
*     
*     Revision 1.5  1995/07/20  19:06:05  cdaq
*     (SAW) Move data statements for f2c compatibility
*     
*     Revision 1.4  1995/05/22  19:46:01  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, hks, and coin parts"
*     
*     Revision 1.3  1995/04/06  19:45:16  cdaq
*     (JRA) Rename residuals variables
*     
*     Revision 1.2  1994/11/23  14:24:18  cdaq
*     (SPB) Recopied from hms file and modified names for HKS
*     
*     Revision 1.1  1994/02/21  16:42:27  cdaq
*     Initial revision
*     
      implicit none
      include "hes_data_structures.cmn"
      include "hes_tracking.cmn"
      include "hes_geometry.cmn"
      include 'hes_id_histid.cmn'
*     
c     external S_FCNCHISQ
c     external S_DPSIFUN
c     real*8 S_DPSIFUN
c     real*8 S_FCNCHISQ
*
*     local variables
*     
      logical ABORT
      character*11 here
      parameter (here='E_DC2_TRACK_FIT')
      character*(*) err
      integer*4 itrk            ! track loop index
      integer*4 i,j             ! loop index
      integer*4 ierr            ! error return flag
c     integer*4 ivarbl                  ! dummy MINUIT variable
      integer*4 ihit, pln, hit
c     real*8 pos
      real*8 dray(enum_fpray_param)
      real*8 TT(enum_fpray_param)
      real*8 AA(enum_fpray_param,enum_fpray_param)
c     real*8 error(enum_fpray_param)
c     real*8 initialray(enum_fpray_param)
c     real*8 initialsteps(enum_fpray_param)
c     real*8 zero
c     real*8 arglis(10)
c     real*8 bnd1,bnd2                     ! unused MUNUIT output variables
      real*8 chi2
c     real*8 fedm,errder                   ! unused MUNUIT output variables
c     integer*4 npari,nparz,istat          ! unused MUNUIT output variables
c     character*10 fitnames(4)
      
c     data initialray/0.D0,0.D0,.5D-2,.5D-2/     ! starting ray values
c     data initialsteps/1.D0,1.D0,.5E-2,.5D-2/
c     data zero/0.0D0/
c     data fitnames/' x_t ',' y_t ','tan(xp)','tan(yp)'/
      
c     save initialray,initialsteps,fitnames   ! starting ray, steps, names
      integer*4 remap(enum_fpray_param)
      data remap/5,6,3,4/
      save remap
      real*8 fpx_coeff(enum_fpray_param),fpx_sigma !DK
      real*8 fpy_coeff(enum_fpray_param),fpy_sigma !DK
*     
      ABORT= .FALSE.
      ierr=0
*     initailize residuals
      
cc      print *, 'in edc2_track'
      do pln=1,edc2_num_layers
         do itrk=1,entracks_fp
            edc2_single_residual(itrk,pln)=100000
         enddo
      enddo
      
cDK      edc2_bestchi2_index = -1
cDK      edc2_bestchi2 = 10000000

      fpx_sigma=0.008 !DK
      fpy_sigma=0.021 !DK
               
      fpx_coeff(1)=1
      fpx_coeff(2)=0
      fpx_coeff(3)=0
      fpx_coeff(4)=0
      fpy_coeff(1)=0
      fpy_coeff(2)=1
      fpy_coeff(3)=0
      fpy_coeff(4)=0
      
*     test for no tracks
      if(entracks_pre.ge.1) then
         do itrk=1,entracks_pre
cDK            edc2track_fit_num=itrk
            
*     are there enough degrees of freedom
            enfree_fp(itrk)=entrack_hits_pre(itrk,1)-enum_fpray_param

cc            print *, 'start', itrk, enfree_fp(itrk)

            if(enfree_fp(itrk).gt.0) then
               
c     *     initialize parameters
c     do i=1,enum_fpray_param
c     call MNPARM(i,fitnames(i),initialray(i),initialsteps(i),
c     &                  zero,zero,ierr)  
c     if(ierr.ne.0) then
c     write(sluno,'(a,i,a)') ' Unable to define parameter no.',i,
c     &                           fitnames(i)
c     ierr=1
c     go to 1000                     ! error return
c     endif
c     enddo                              ! end loop over track param
c     do i=1,10
c     arglis(i)=0.
c     enddo
c     *     Do track fit on track number strack_fit_num (passes in hks_tracking)
c     Call MNSETI(' Track Fitting in HKS Spectrometer')
c     Call MNEXCM(S_FCNCHISQ,'MIGRAD',arglis,0,ierr)
c     Call MNEXCM(S_FCNCHISQ,'MINOS',arglis,0,ierr)
c     Call MNSTAT(chi2,fedm,errder,npari,nparz,istat)
c     do i=1,enum_fpray_param
c     call MNPOUT(i,fitnames(i),ray(i),error(i),bnd1,bnd2,ivarbl)
c     enddo     
c     sx_fp(itrk)=real(ray(1))
c     sy_fp(itrk)=real(ray(2))
c     sz_fp(itrk)=0.                   ! z=0 of tracking.
c     sxp_fp(itrk)=real(ray(3))
c     syp_fp(itrk)=real(ray(4))
c     schi2_fp(itrk)=real(chi2)
c     
c     * calculate residuals
c     ray1(1)=dble(sx_fp(itrk))
c     ray1(2)=dble(sy_fp(itrk))
c     ray1(3)=dble(sxp_fp(itrk))
c     ray1(4)=dble(syp_fp(itrk))
c     do ihit=2,entrack_hits_pre(itrk,1)+1
c     hit=entrack_hits_pre(itrk,ihit)
c     pln=edc2_layer_num(hit)
c     pos=s_dpsifun(ray1,pln)
c     edc2_single_residual(itrk,pln)=edc2_wire_coord(hit)-pos
c     enddo
c     endif                          ! end test on degrees of freedom
c     enddo                            ! end loop over tracks
c     endif
              
*     initialize parameters
               do i=1,enum_fpray_param
                  TT(i)=0.
                  do ihit=2,entrack_hits_pre(itrk,1)+1
                     hit=entrack_hits_pre(itrk,ihit)
                     pln=edc2_layer_num(hit)
                     TT(i)=TT(i)+((edc2_wire_coord(hit)*
     &                    edc2layer_coeff(remap(i),pln))
     &                    /(edc2_sigma(pln)*edc2_sigma(pln)))
                  enddo
c$$$                  TT(i)=TT(i)+
c$$$     &                  ((ex_fp_pre2(itrk)*fpx_coeff(i))/
c$$$     &                  (fpx_sigma*fpx_sigma))+
c$$$     &                  ((ey_fp_pre2(itrk)*fpy_coeff(i))/
c$$$     &                  (fpy_sigma*fpy_sigma))
               enddo
               do i=1,enum_fpray_param
                  do j=1,enum_fpray_param
                     AA(i,j)=0.
                     if(j.lt.i)then
                        AA(i,j)=AA(j,i)
                     else
                        do ihit=2,entrack_hits_pre(itrk,1)+1
                           hit=entrack_hits_pre(itrk,ihit)
                           pln=edc2_layer_num(hit)
                           AA(i,j)=AA(i,j) + (
     &                          edc2layer_coeff(remap(i),pln)
     &                          *edc2layer_coeff(remap(j),pln)
     &                          /(edc2_sigma(pln)*edc2_sigma(pln)))
                        enddo   ! end loop on ihit
c$$$                        AA(i,j)=AA(i,j) + 
c$$$     &                       (fpx_coeff(i)*fpx_coeff(j)
c$$$     &                        /(fpx_sigma*fpx_sigma))+
c$$$     &                       (fpy_coeff(i)*fpy_coeff(j)
c$$$     &                        /(fpy_sigma*fpy_sigma))
                     endif      ! end test on j .lt. i
                  enddo         ! end loop on j
               enddo            ! end loop on i
*     

*     solve four by four equations
               call solve_four_by_four(TT,AA,dray,ierr)
*     
               if(ierr.ne.0) then
                  dray(1)=10000.
                  dray(2)=10000.
                  dray(3)=2.
                  dray(4)=2.
               else
*     calculate chi2
                  chi2=0.
                  
*     ray(1)=dray(1)
*     ray(2)=dray(2)
*     ray(3)=dray(3)
*     ray(4)=dray(4)
                  
*     calculate hit coord at each layer for chisquared and efficiency calculations.
                  do pln=1,edc2_num_layers
                     edc2_track_coord_pre(itrk,pln)=
     &                    edc2layer_coeff(remap(1),pln)*dray(1)
     &                    +edc2layer_coeff(remap(2),pln)*dray(2)
     &                    +edc2layer_coeff(remap(3),pln)*dray(3)
     &                    +edc2layer_coeff(remap(4),pln)*dray(4)
                  enddo
                  
                  do ihit=2,entrack_hits_pre(itrk,1)+1
                     hit=entrack_hits_pre(itrk,ihit)
                     pln=edc2_layer_num(hit)
                     
*     note chi2 is single precision
cDK                     edc2_layer_wirecenter(itrk,pln)=edc2_wire_center(hit)
cDK                     edc2_layer_wirecoord(itrk,pln)=edc2_wire_coord(hit)
                     
                     edc2_residual_pre(itrk,pln)=
     &                    edc2_wire_coord(hit)-edc2_track_coord_pre(itrk,pln)
                     chi2=chi2+
     &                    (edc2_residual_pre(itrk,pln)
     &                    /edc2_sigma(pln))**2
c--   Debug
ccc                  print *,'track fit',pln, hit,edc2_wire_coord(hit), edc2_track_coord_pre(itrk,pln) 
c--
                  enddo
               endif
               
               ex_fp_pre2(itrk)=dray(1)
               ey_fp_pre2(itrk)=dray(2)
c               hz_fp(itrk)=0.   ! z=0 of tracking.
               exp_fp_pre2(itrk)=dray(3)
               eyp_fp_pre2(itrk)=dray(4)
            endif               ! end test on degrees of freedom
            edc2chi2_pre(itrk)=chi2
            edc2chi2perdof_pre(itrk)=chi2/enfree_fp(itrk)
cDK            if(edc2chi2perdof_fp(itrk) .le. edc2_bestchi2) then
cDK               edc2_bestchi2_index = itrk
cDK               edc2_bestchi2 = edc2chi2perdof_fp(itrk)
cDK            EndIf
cc            Write(*,*) 'End', itrk, edc2chi2perdof_pre(itrk), ex_fp_pre2(itrk), ey_fp_pre2(itrk)
         enddo                  ! end loop over tracks
      endif

      call e_fill_dc2_track_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif
      
*     test if we want to dump out trackfit results
c      if(edebugtrackprint.ne.0) then
cc         call e_dc2_print_tracks
c      endif                     ! end test on zero tracks
 1000 return
      end

