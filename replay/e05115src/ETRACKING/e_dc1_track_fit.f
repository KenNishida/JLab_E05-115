      subroutine E_DC1_TRACK_FIT(ABORT,err,ierr)
*     
*     primary track fitting routine for the HES 
*     Called by E_TRACK
*     
*     d.f. geesaman         8 Sept 1993
*     $Log: e_dc1_track_fit.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.2  2005/09/19 22:05:51  sumihama
*     Mod. EDC tracking by Akhiko and starttime from EHODO
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.7  2005/03/10 16:48:24  miyoshi
*     change tracking variable name
*
*     Revision 1.6  2005/02/11 23:01:12  sumihama
*     Mod. e-ntuple, Mod. etracking
*     
*     Revision 1.5  2005/01/25 17:21:00  miyoshi
*     change real4 variables
*     
*     Revision 1.4  2005/01/24 19:56:43  miyoshi
*     use only one drift dist calc
*     
*     Revision 1.3  2005/01/05 22:24:08  miyoshi
*     correct minor mistake
*     
*     Revision 1.2  2004/11/15 16:13:10  miyoshi
*     add track_coord in 1st loop
*     
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*     
*     
*     Revision 1.9 03/23/2004 Miyoshi
*     for E01-011
*     
*     Revision 1.8  1996/01/17 18:56:08  cdaq
*     (JRA) Fill sdc_plane_wirecenter and sdc_plane_wirecoord arrays
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
*     (SAW) Split gen_data_data_structures 
*     into gen, hms, sos, and coin parts"
*     
*     Revision 1.3  1995/04/06  19:45:16  cdaq
*     (JRA) Rename residuals variables
*     
*     Revision 1.2  1994/11/23  14:24:18  cdaq
*     (SPB) Recopied from hms file and modified names for SOS
*     
*     Revision 1.1  1994/02/21  16:42:27  cdaq
*     Initial revision
*     
      implicit none
      include "hes_data_structures.cmn"
      include "hes_tracking.cmn"
      include "hes_statistics.cmn"
      include "hes_geometry.cmn"
      include "hes_id_histid.cmn"
      include "hes_scin_parms.cmn"
      include 'gen_event_info.cmn'
      include 'gen_constants.par'
*     
*     local variables
*     
      logical ABORT
      character*11 here
      parameter (here='E_DC1_TRACK_FIT')
      character*(*) err
      integer*4 itrk,ierr       ! track loop index
      integer*4 i,j,k,l,m,n     ! loop index
      integer*4 ihit, pln, hit, nhit,jhit
      integer*4 nwire,jfit,flag_used(10),nseva_loops
      integer*4 wn,slot
      
      integer*4 temp_on_track_hits(11),set_new_track
      real*4 wc,wco,slope,layer_temp_fpt
      real*4 entracks_pre_seva
      real*4 dt
      real*8 dray(enum_fpray_param)
      real*8 TTT(enum_fpray_param)
      real*8 AA(enum_fpray_param,enum_fpray_param)
      real*4 xfp,yfp,xpfp,ypfp,ds,newdt
      real*4 chi,phi,dphidz,zsin,zcos
      real*4 wire,sw,z0,zw,sina,cosa
      real*4 sint,cost,det
      real*4 test_res1,test_res2
      real*4 sw_pln(emax_num_dc1_layers)
      real*4 zw_pln(emax_num_dc1_layers)
      
      real*4 raddeg
      parameter (raddeg = 3.14159265/180.)

c     variables for LNS routine
      real*4 sw_lns(emax_num_dc1_layers), zw_lns(emax_num_dc1_layers)
      real*4 s_lns(emax_num_dc1_layers),z_lns(emax_num_dc1_layers)
      real*4 a_lns(emax_num_dc1_layers),w_lns(emax_num_dc1_layers)
      real*4 wt_lns(emax_num_dc1_layers)
      real*4 dl_lns(emax_num_dc1_layers),dsdz_lns(emax_num_dc1_layers)
      real*4 scalw_lns(emax_num_dc1_layers)
      integer*4 iptr_lns(emax_num_dc1_layers)
      integer*4 irptr_lns(emax_num_dc1_layers)
      integer*4 ipl_lns(emax_num_dc1_layers)
      integer*4 ipln_lns(emax_num_dc1_layers)
      real*4 xcal_lns(emax_num_dc1_layers),ycal_lns(emax_num_dc1_layers)
      real*4 zold_lns(emax_num_dc1_layers)
      real*4 xcalold_lns(emax_num_dc1_layers)
      real*4 ycalold_lns(emax_num_dc1_layers)
      real*4 sold_lns(emax_num_dc1_layers)
      real*4 chisqr_lns, chiold_lns
      real*8 param_lns(enum_fpray_param)
      real*8 param_lns2(enum_fpray_param)
      real*8 paramold_lns(enum_fpray_param)
      integer*4 nd_lns,nduv_lns,nitra_lns
      integer*4 kf_lns(emax_num_dc1_layers)
      real*4 f_lns,ct_lns,st_lns
      real*4 z11_lns,z12_lns,z21_lns,z22_lns
      real*4 s11_lns,s12_lns,s21_lns,s22_lns
      real*4 z1_lns,z2_lns
      real*4 s1_lns,s2_lns
      real*4 s1cal_lns,s2cal_lns,d1_lns,d2_lns,ang0_lns,ang1_lns,ang2_lns
      real*4 ctj_lns,stj_lns,sij_lns,da1_lns,da2_lns
      real*4 rayang_lns
      real*4 dsl_lns(16)
      real*4 ds_lns(16)
      integer*4 mini_lns,mini1_lns,mini2_lns
      real*4 minads1_lns,minads2_lns
      real*4 dsc11_lns,dsc12_lns,dsc21_lns,dsc22_lns
      integer*4 MaxIteration
      parameter (MaxIteration = 3)
      real*4 IncAngleDifference
      parameter (IncAngleDifference = 5.*raddeg)
      integer*4 ii,jj,mark(entracks_pre_max)
      real*4 tmp_lns, tdc_sub,temp_time
      integer*4 la,co,num,seva_loop,hit1,hit2,la1,la2,co1,co2
      integer*4 num_la(emax_num_dc1_layers),kk
      real*4 stime,x_la,x_min,x_max,temp_on_track

      integer*4 nd_lns2
      integer*4 iptr_lns2(emax_num_dc1_layers)
      integer*4 irptr_lns2(emax_num_dc1_layers)
      integer*4 ipl_lns2(emax_num_dc1_layers)
      integer*4 ipln_lns2(emax_num_dc1_layers)
      real*4 sw_lns2(emax_num_dc1_layers), zw_lns2(emax_num_dc1_layers)
      real*4 s_lns2(emax_num_dc1_layers),z_lns2(emax_num_dc1_layers)
      real*4 a_lns2(emax_num_dc1_layers),w_lns2(emax_num_dc1_layers)
      real*4 wt_lns2(emax_num_dc1_layers)
      real*4 dl_lns2(emax_num_dc1_layers),dsdz_lns2(emax_num_dc1_layers)
      real*4 edc1_track_coord_pre2(emax_num_dc1_layers)

      real*4 fitdx,fitdy,fitdxp,fitdyp
c     save initialray,initialsteps,fitnames   ! starting ray, steps, names
      integer*4 remap(enum_fpray_param)
      data remap/5,6,3,4/
      save remap
c     
      real*4 e_drift_dist_calc
      external e_drift_dist_calc
      real*4 xx1,xx2,yy1,yy2
      real*4 ss1(16),ss2(16),zz1(16),zz2(16)
      integer*4 edc1eff_counter
      integer*4 edc1eff_start
*     
      ABORT= .FALSE.
      ierr=0

      do i=1,16
         ss1(i)=0
         ss2(i)=0
         zz1(i)=0
         zz2(i)=0
      enddo


*     --- Histogram for dc1 sp bank 
      call e_fill_dc1_pretrk_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif
      
*     --- initial check
      if(entracks_pre .le. 0) Return
      if (esevaflag.eq.0) then
         nseva_loops=0
      else
         nseva_loops=3
      endif
      seva_loop=0
      entracks_pre_seva=entracks_pre
      
*     initailize hes_tracking.cmn common block
 5555 continue
      do pln=1,edc1_num_layers         
         do itrk=1,entracks_pre
            Do i=1,enum_fitting
               edc1_residual_pre(itrk,pln,i) = 10000.
               edc1_track_coord_pre(itrk,pln,i)= 10000.
               edc1_drift_dis_pre(itrk,pln,i)= 10000.
            EndDo
         enddo
      enddo
      Do i=1,enum_fitting
         Do itrk=1,entracks_pre
            edc1chi2_pre(itrk,i) = 0.
            edc1chi2perdof_pre(itrk,i) = 0.
         EndDo
         edc1bestchi2_pre(i) = 1000000.
         edc1bestchi2_pre_index(i) = -1
      EndDo

      do itrk=1,entracks_pre
c     DK         edc1track_fit_num=itrk            
         enfree_pre(itrk)=entrack_hits_pre(itrk,1)-enum_fpray_param
         if(enfree_pre(itrk).gt.0) then 
            if (seva_loop.gt.0) goto 6789
*******************************************
*     1st loop
*******************************************
*     initialize parameters
c            do i=1,enum_fpray_param
c               TTT(i)=0.
c               do ihit=2,entrack_hits_pre(itrk,1)+1
c                  hit=entrack_hits_pre(itrk,ihit)
c                  pln=edc1_layer_num(hit)
c                  TTT(i)=TTT(i)+((edc1_wire_coord(hit)*
c     &                 edc1layer_coeff(remap(i),pln))
c     &                 /(edc1_sigma(pln)*edc1_sigma(pln)))
c               enddo
c            enddo
c            
c            do i=1,enum_fpray_param
c               do j=1,enum_fpray_param
c                  AA(i,j)=0.
c                  if(j.lt.i)then
c                     AA(i,j)=AA(j,i)
c                  else
c                     do ihit=2,entrack_hits_pre(itrk,1)+1
c                        hit=entrack_hits_pre(itrk,ihit)
c                        pln=edc1_layer_num(hit)
c                        AA(i,j)=AA(i,j) + (
c     &                       edc1layer_coeff(remap(i),pln)
c     &                       *edc1layer_coeff(remap(j),pln)
c     &                       /(edc1_sigma(pln)*edc1_sigma(pln)))
c                     enddo      ! end loop on ihit
c                  endif         ! end test on j .lt. i
c               enddo            ! end loop on j
c            enddo               ! end loop on i
*     --- fitting ---
*     solve four by four equations
c            call solve_four_by_four(TTT,AA,dray,ierr)
c            if(ierr.ne.0) write(*,*) "ierr0",ierr
*     if error...
c            if(ierr.ne.0) then
c               dray(1)=10000.
c               dray(2)=10000.
c               dray(3)=10000.
c               dray(4)=10000.
c            else                ! no error
*     calculate hit coord at each layer 
*     for chisquared and efficiency calculations.
c               do pln = 1,edc1_num_layers
c                  edc1_track_coord_pre(itrk,pln,1)=
c     &                 edc1layer_coeff(remap(1),pln)*dray(1)
c     &                 +edc1layer_coeff(remap(2),pln)*dray(2)
c     &                 +edc1layer_coeff(remap(3),pln)*dray(3)
c     &                 +edc1layer_coeff(remap(4),pln)*dray(4)
c               enddo
c               
c               do ihit = 2,entrack_hits_pre(itrk,1)+1
c                  hit = entrack_hits_pre(itrk,ihit)
c                  pln = edc1_layer_num(hit)
c                  wco = edc1_wire_coord(hit)
c                  edc1_residual_pre(itrk,pln,1)=
c     &                 wco - edc1_track_coord_pre(itrk,pln,1)
c                  edc1chi2_pre(itrk,1) = 
c     &                 edc1chi2_pre(itrk,1) +
c     &                 (edc1_residual_pre(itrk,pln,1)
c     &                 /edc1_sigma(pln))**2
c               enddo   
c            endif               ! ierr = 0 or not.
c            
c            ex_fp_pre1(itrk,1)=dray(1)
c            ey_fp_pre1(itrk,1)=dray(2)
c            exp_fp_pre1(itrk,1)=dray(3)
c            eyp_fp_pre1(itrk,1)=dray(4)   
c
c            edc1chi2perdof_pre(itrk,1) = 
c     &           edc1chi2_pre(itrk,1)/Real(enfree_pre(itrk))
c            if(edc1chi2perdof_pre(itrk,1) .le. edc1bestchi2_pre(1)) then
c               edc1bestchi2_pre_index(1) = itrk
c               edc1bestchi2_pre(1) = edc1chi2perdof_pre(itrk,1)
c            EndIF
c            write(*,*) "event",gen_event_id_number
c            write(*,*) " dray",dray
c            write(*,*) " chi2",edc1chi2perdof_pre(itrk,1)
******************************************
*     2nd loop
******************************************
 6789       continue
            jfit=2              ! 2nd loop

******************************************
*     LNS fitting routine
******************************************
            edc1eff_flag(itrk)=0

            edc1eff_counter=0
            if (edc1trackingflag.eq.0) then
               edc1eff_start=11
            else
               edc1eff_start=1
            endif

            Do edc1eff_layer=edc1eff_start,11
*     ---- Initialize ----
               Do i=1,emax_num_dc1_layers
                  sw_lns(i)=-1000.
                  zw_lns(i)=-1000.
                  s_lns(i)=-1000.
                  z_lns(i)=-1000.
                  a_lns(i)=-1000.
                  w_lns(i)=-1000.
                  wt_lns(i)=-1000.
                  dl_lns(i)=-1000.
                  dsdz_lns(i)=-1000.
                  scalw_lns(i)=-1000.
                  xcal_lns(i)=-1000.
                  ycal_lns(i)=-1000.
                  zold_lns(i)=-1000.
                  xcalold_lns(i)=-1000.
                  ycalold_lns(i)=-1000.
                  sold_lns(i)=-1000.
                  
                  iptr_lns(i)=-1
                  irptr_lns(i)=-1
                  ipl_lns(i)=-1
                  ipln_lns(i)=-1


                  if(edc1eff_counter.eq.0) then
                     sw_lns2(i)=-1000.
                     zw_lns2(i)=-1000.
                     s_lns2(i)=-1000.
                     z_lns2(i)=-1000.
                     a_lns2(i)=-1000.
                     w_lns2(i)=-1000.
                     dl_lns2(i)=-1000.
                     dsdz_lns2(i)=-1000.
                     iptr_lns2(i)=-1
                     irptr_lns2(i)=-1
                     ipl_lns2(i)=-1
                     ipln_lns2(i)=-1
                  endif
               enddo
               Do i=1,enum_fpray_param
                  param_lns(i)=-1000.
                  param_lns2(i)=-1000.
               enddo
               nitra_lns=1
               
*     ---- fill LNS variables -----
               nd_lns=0
               if(edc1eff_counter.eq.0) nd_lns2=0
               nduv_lns=0
               nhit = entrack_hits_pre(itrk,1)
               Do ihit=2,nhit+1
                  hit=entrack_hits_pre(itrk,ihit)
                  pln=edc1_layer_num(hit)
                     
                  slot=edc1_slot_num(hit)
                  wire=edc1_wire_center(hit)
                  nwire=edc1_wire_num(hit)
*     Change to subtract starttime here by MIZUKI
c                  write(*,*) "itrk,starttime(itrk)",itrk,starttime(itrk)
                  if (.not.gen_event_ts_flag(2)) then
                     dt=edc1_drift_time(hit)-starttime(itrk)
                  else
                     dt=edc1_drift_time(hit)
                  endif
c                  write(*,*) "dt,starttime",dt,starttime(itrk)
c                  if(edc1eff_layer.eq.11)
c     >                 Call HF1(eiddc1singdtime(pln),dt,1.)
                  z0=edc1_zpos(pln)
                  ds = e_drift_dist_calc(pln,slot,dt)
c                  edc1_drift_dis_pre(itrk,pln,enum_fitting)=ds
c                  write(*,*) edc1_drift_dis_pre(itrk,pln,enum_fitting)
c                  if(edc1eff_layer.eq.11)
c     >                 Call HF1(eiddc1driftdis(pln),ds,1.) ! matsu

                  if(pln.ne.edc1eff_layer) then
                     nd_lns=nd_lns+1
                     sw_lns(nd_lns)=wire
                     zw_lns(nd_lns)=z0
                     a_lns(nd_lns)=edc1_alpha_angle(pln)-90*raddeg
                     w_lns(nd_lns)=1/(edc1_sigma(pln)*edc1_sigma(pln))
                     dl_lns(nd_lns)=ds
                     ipln_lns(nd_lns)=-1
                     ipl_lns(nd_lns)=-1
                     iptr_lns(nd_lns)=pln
                     irptr_lns(nd_lns)=nd_lns
                     
                     if(pln.eq.3.or.pln.eq.4.or.
     >                    pln.eq.7.or.pln.eq.8) then
                        nduv_lns=nduv_lns+1
c     write(*,*) "pln,nduv_lns",pln,nduv_lns
                     endif
                  endif
                  if(edc1eff_counter.eq.0) then
                     nd_lns2=nd_lns2+1
                     sw_lns2(nd_lns2)=wire
                     zw_lns2(nd_lns2)=z0
                     a_lns2(nd_lns2)=edc1_alpha_angle(pln)-90*raddeg
                     w_lns2(nd_lns2)=1/(edc1_sigma(pln)*edc1_sigma(pln))
                     dl_lns2(nd_lns2)=ds
                     ipln_lns2(nd_lns2)=-1
                     ipl_lns2(nd_lns2)=-1
                     iptr_lns2(nd_lns2)=pln
                     irptr_lns2(nd_lns2)=nd_lns2
                  endif
               enddo 
               if(nduv_lns.le.2) then 
                  goto 1214
               else if(nduv_lns.ge.3.and.edc1eff_layer.le.10) then
                  edc1eff_flag(itrk)=edc1eff_flag(itrk)+edc1eff_layer
               endif
c            write(*,*) "---------------------------------------------"
c            write(*,*) "event",gen_event_id_number
c            write(*,*) "nhit,nd_lns,nduv_lns",nhit,nd_lns,nduv_lns
c            write(*,*) " irptr_lns",irptr_lns
c            write(*,*) " iptr_lns",iptr_lns
c            write(*,*) " sw_lns",sw_lns
c            write(*,*) " zw_lns",zw_lns
c            write(*,*) " a_lns",a_lns
c            write(*,*) " w_lns",w_lns
c            write(*,*) " dl_lns",dl_lns

               Do i=1,nd_lns    ! check pair layer
                  pln=iptr_lns(i)
                  if(pln-int(pln/2)*2.eq.1) then ! odd layer
                     if(i.lt.10.and.iptr_lns(i+1)-pln.eq.1) then
                        ipln_lns(i)=iptr_lns(i+1)
c     ipl_lns(i)=irptr_lns(ipln_lns(i))
                        ipl_lns(i)=ipln_lns(i)
                     endif
                  else          ! even layer
                     if(i.gt.1.and.pln-iptr_lns(i-1).eq.1) then
                        ipln_lns(i)=iptr_lns(i-1)
c     ipl_lns(i)=irptr_lns(ipln_lns(i))
                        ipl_lns(i)=ipln_lns(i)
                     endif
                  endif
               enddo

               if(edc1eff_counter.eq.0) then
                  Do i=1,nd_lns2 ! check pair layer
                     pln=iptr_lns2(i)
                     if(pln-int(pln/2)*2.eq.1) then ! odd layer
                        if(i.lt.10.and.iptr_lns2(i+1)-pln.eq.1) then
                           ipln_lns2(i)=iptr_lns2(i+1)
                           ipl_lns2(i)=ipln_lns2(i)
                        endif
                     else       ! even layer
                        if(i.gt.1.and.pln-iptr_lns2(i-1).eq.1) then
                           ipln_lns2(i)=iptr_lns2(i-1)
                           ipl_lns2(i)=ipln_lns2(i)
                        endif
                     endif
                  enddo
               endif
c      write(*,*) "event",gen_event_id_number
c      write(*,*) "edc1eff_layer",edc1eff_layer
c      write(*,*) " nd_lns",nd_lns
c      write(*,*) " ipln_lns",ipln_lns
c      write(*,*) " ipl_lns",ipl_lns
c      write(*,*) " ipl_lns2",ipl_lns2
c      write(*,*) " iptr_lns",iptr_lns
c      write(*,*) " irptr_lns",irptr_lns
c      write(*,*) " sw_lns",sw_lns
c      write(*,*) " zw_lns",zw_lns
c      write(*,*) " a_lns",a_lns
c      write(*,*) " w_lns",w_lns
c      write(*,*) " dl_lns",dl_lns
               
               if(edc1eff_counter.eq.0) then
c                  write(*,*) "10 layer fit"
                  call preSelectLR(nd_lns2,zw_lns2,sw_lns2,a_lns2,
     >                 dl_lns2,w_lns2,ipl_lns2,
     >                 z_lns2,s_lns2,wt_lns2,ierr)
                  if(ierr.ne.0) write(*,*) "ierr1",ierr
               else
c               write(*,*) "9 layer fit", edc1eff_layer
                  call preSelectLR(nd_lns,zw_lns,sw_lns,a_lns,
     >              dl_lns,w_lns,ipl_lns,
     >              z_lns,s_lns,wt_lns,ierr)
                  if(ierr.ne.0) write(*,*) "ierr1",ierr
               endif
c      write(*,*) "event",gen_event_id_number
c      write(*,*) "edc1eff_layer",edc1eff_layer
c      write(*,*) " iptr_lns",iptr_lns
c      write(*,*) " ipl_lns",ipl_lns
c      write(*,*) " z_lns",z_lns
c      write(*,*) " s_lns",s_lns
c      write(*,*) " s_lns2",s_lns2
c      write(*,*) " nd_lns",nd_lns
c      write(*,*) " wt_lns",wt_lns
c               Do i=1,nd_lns
c                  pln=iptr_lns(i)
c                  edc1_drift_dis_pre(itrk,pln,enum_fitting)=
c     >                 sqrt((s_lns(i)-sw_lns(i))**2
c     >                 +(z_lns(i)-zw_lns(i))**2)
c     >                 *(s_lns(i)-sw_lns(i))/abs(s_lns(i)-sw_lns(i))
c               enddo
               
               
               if(edc1eff_counter.eq.0) then
cDK               write(*,*) "10 layer fit"
                  call linearFit(nd_lns2,z_lns2,s_lns2,a_lns2,
     >              wt_lns2,param_lns2,ierr)
c                     print *, "s_lns2",s_lns2
c                     print *, "z_lns2",z_lns2
c                     print *, "dl_lns2",dl_lns2
c                     print *, "param_lns2",param_lns2
                  if(ierr.ne.0) write(*,*) "ierr2-1",ierr
                  call calcChiSquare(nd_lns2,z_lns2,a_lns2,zw_lns2,sw_lns2,
     >               dl_lns2,w_lns2,param_lns2,scalw_lns,dsdz_lns, 
     >               xcal_lns, ycal_lns, chisqr_lns)
c                     print *, "x2",xcal_lns
                  if(ierr.ne.0) write(*,*) "ierr2-2",ierr
                  Do i=1,enum_fpray_param
                     param_lns(i)=param_lns2(i)
                  enddo
               else
                  call linearFit(nd_lns,z_lns,s_lns,a_lns,
     >               wt_lns,param_lns,ierr)
c                     print *, "z_lns",z_lns
c                     print *, "param_lns",param_lns
                  if(ierr.ne.0) write(*,*) "ierr2-1",ierr
                  call calcChiSquare(nd_lns,z_lns,a_lns,zw_lns,sw_lns,
     >              dl_lns,w_lns,param_lns,scalw_lns,dsdz_lns, 
     >              xcal_lns, ycal_lns, chisqr_lns)
c                     print *, "x",xcal_lns
                  if(ierr.ne.0) write(*,*) "ierr2-2",ierr
               endif
c      write(*,*) "dsdz_lns",dsdz_lns
c      write(*,*) "event",gen_event_id_number
c      write(*,*) " param_lns",param_lns
c      write(*,*) " chisqr_lns",chisqr_lns
               
               chiold_lns=chisqr_lns+100
               
cDK                  write(*,*) "chi2 loop start, edc1eff_layer=",edc1eff_layer
               do while(chisqr_lns.lt.chiold_lns
     >              .and.nitra_lns.le.MaxIteration)
c     ---- next guess ----
cDK                  Do i=1,nd_lns
                  Do i=1,10
                     kf_lns(i)=1
                     zold_lns(i)=z_lns(i)
                     sold_lns(i)=s_lns(i)
                     xcalold_lns(i)=xcal_lns(i)
                     ycalold_lns(i)=ycal_lns(i)
                  enddo
                  
c                  write(*,*) "nd_lns loop start, edc1eff_layer=",edc1eff_layer,"iteration=",nitra_lns
                  Do i=1,nd_lns
                     f_lns=1./sqrt(1.+dsdz_lns(i)*dsdz_lns(i))
                     ct_lns=cos(a_lns(i))
                     st_lns=sin(a_lns(i))
c                     write(*,*)"i=",i,zw_lns(i)
                     if(kf_lns(i).gt.0.and.ipl_lns(i).ge.0
     >                    .and.kf_lns(ipl_lns(i)).gt.0) then ! Pair Hits 
                        !ii=ipl_lns(i)
                        
                        kf_lns(i)=0
                        !kf_lns(ii)=0
                        ii=i+1 !DK
                        kf_lns(i+1)=0 !DK
                        
                        z11_lns=zw_lns(i) -f_lns*dsdz_lns(i) *dl_lns(i)
                        z12_lns=zw_lns(i) +f_lns*dsdz_lns(i) *dl_lns(i)
                        z21_lns=zw_lns(ii)-f_lns*dsdz_lns(ii)*dl_lns(ii)
                        z22_lns=zw_lns(ii)+f_lns*dsdz_lns(ii)*dl_lns(ii)
                        s11_lns=sw_lns(i) +f_lns*dl_lns(i)
                        s12_lns=sw_lns(i) -f_lns*dl_lns(i)
                        s21_lns=sw_lns(ii)+f_lns*dl_lns(ii)
                        s22_lns=sw_lns(ii)-f_lns*dl_lns(ii)
                        rayang_lns=atan(dsdz_lns(i))
c                        print *, "dl_lns(i),dl_lns(ii)",i,dl_lns(i),dl_lns(ii)
c                        print *, "dsdz_lns(i),dsdz_lns(ii)",atan(dsdz_lns(i)),atan(dsdz_lns(ii))
                        
                        do j=1,16
                            if (iand(j,1).eq.0) then
                              ss1(j)=s11_lns
                            else
                              ss1(j)=s12_lns
                            endif
                            if (iand(ishft(j,-1),1).eq.0) then
                              ss2(j)=s21_lns
                            else
                              ss2(j)=s22_lns
                            endif
                            if (iand(ishft(j,-2),1).eq.0) then
                              zz1(j)=z11_lns
                            else
                              zz1(j)=z12_lns
                            endif
                            if (iand(ishft(j,-3),1).eq.0) then
                              zz2(j)=z21_lns
                            else
                              zz2(j)=z22_lns
                            endif
                           dsl_lns(j)=atan((ss1(j)-ss2(j))/(zz1(j)-zz2(j)))
     >                       -rayang_lns
                           ds_lns(j)=abs(ss1(j)-(param_lns(1)+param_lns(2)*zz1(j))
     &                      -(param_lns(3)+param_lns(4)*zz1(j)))
     &                      +abs(ss2(j)-(param_lns(1)+param_lns(2)*zz2(j))
     &                      -(param_lns(3)+param_lns(4)*zz2(j)))
c                           print *, "j,ss1,ss2,dz",j,ss1(j),ss2(j),(zz1(j)-zz2(j))
c                           print *, "j,dsl_lns(j)",j,atan((ss1(j)-ss2(j))/(zz1(j)-zz2(j))) 
c                           print *, "j,ds_lns(j)",j,ds_lns(j) 
                        enddo
                        
                        if(abs(dsl_lns(1)).lt.abs(dsl_lns(2))) then
                           mini1_lns=0
                           mini2_lns=1
                           minads1_lns=abs(dsl_lns(1))
                           minads2_lns=abs(dsl_lns(2))
                        else
                           mini1_lns=1
                           mini2_lns=0
                           minads1_lns=abs(dsl_lns(2))
                           minads2_lns=abs(dsl_lns(1))
                        endif
                        Do jj=3,16
                           tmp_lns=abs(dsl_lns(jj))
                           if(tmp_lns.lt.minads1_lns) then
                              mini2_lns=mini1_lns
                              mini1_lns=jj-1
                              minads2_lns=minads1_lns
                              minads1_lns=tmp_lns
                           else if(tmp_lns.lt.minads2_lns) then
                              mini2_lns=jj-1
                              minads2_lns=tmp_lns
                           endif
                        enddo
                       
c                        write(*,*)"edc1eff_layer=",edc1eff_layer
c                        write(*,*)"nitra_lns=",nitra_lns
c                        write(*,*)"i=",i
c                        write(*,*)"mini1_lns=",mini1_lns
c                        write(*,*)"mini2_lns=",mini2_lns
                        if( minads2_lns-minads1_lns.gt.
     >                       IncAngleDifference ) then
                           mini_lns=mini1_lns
                        else
                           do j=1,16
                              if(mini1_lns.eq.j-1) then
                                 dsc11_lns=ss1(j)
     >                             -(param_lns(1)+param_lns(2)*zz1(j))
     >                             *ct_lns
     >                             -(param_lns(3)+param_lns(4)*zz1(j))
     >                             *st_lns 
                                 dsc21_lns=ss2(j)
     >                             -(param_lns(1)+param_lns(2)*zz2(j))
     >                             *ct_lns
     >                             -(param_lns(3)+param_lns(4)*zz2(j))
     >                             *st_lns
                              endif
                              if(mini2_lns.eq.j-1) then
                                 dsc12_lns=ss1(j)
     >                             -(param_lns(1)+param_lns(2)*zz1(j))
     >                             *ct_lns
     >                             -(param_lns(3)+param_lns(4)*zz1(j))
     >                             *st_lns
                                 dsc22_lns=ss2(j)
     >                             -(param_lns(1)+param_lns(2)*zz2(j))
     >                             *ct_lns
     >                             -(param_lns(3)+param_lns(4)*zz2(j))
     >                             *st_lns
                              endif
                           enddo

                           if(abs(dsc11_lns)+abs(dsc21_lns)
     >                          .lt.abs(dsc12_lns)+abs(dsc22_lns)) then
                              mini_lns=mini1_lns
                           else 
                              mini_lns=mini2_lns
                           endif
                           
                           z_lns(i)=zz1(mini_lns+1)
                           s_lns(i)=ss1(mini_lns+1)
                           z_lns(ii)=zz2(mini_lns+1)
                           s_lns(ii)=ss2(mini_lns+1)
                           wt_lns(i)=w_lns(i)*f_lns
                           wt_lns(ii)=w_lns(ii)*f_lns 
                        endif
                     else if(kf_lns(i).gt.0) then ! Single Hit
                        kf_lns(i)=0
                        
                        z1_lns=zw_lns(i)-f_lns*dsdz_lns(i)*dl_lns(i)
                        z2_lns=zw_lns(i)+f_lns*dsdz_lns(i)*dl_lns(i)
                        s1_lns=sw_lns(i)+f_lns*dl_lns(i)
                        s2_lns=sw_lns(i)-f_lns*dl_lns(i)
                        s1cal_lns=(param_lns(1)+param_lns(2)*z1_lns)*ct_lns
     >                       +(param_lns(3)+param_lns(4)*z1_lns)*st_lns
                        s2cal_lns=(param_lns(1)+param_lns(2)*z2_lns)*ct_lns
     >                       +(param_lns(3)+param_lns(4)*z2_lns)*st_lns
c                        s1cal_lns=(param_lns(1)+param_lns(2)*z1_lns)*ct_lns
c     >                       +(param_lns(3)+param_lns(4)*z1_lns)*st_lns
c                        s2cal_lns=(param_lns(1)+param_lns(2)*z2_lns)*ct_lns
c     >                       +(param_lns(3)+param_lns(4)*z2_lns)*st_lns
                        d1_lns=abs(s1_lns-s1cal_lns)
                        d2_lns=abs(s2_lns-s2cal_lns)
                        
                        ang0_lns=atan(dsdz_lns(i))
                        ang1_lns=0.0
                        ang2_lns=0.0
                        Do j=1,nd_lns
                           if(i.ne.j) then
                              ctj_lns=cos(a_lns(j))
                              stj_lns=sin(a_lns(j))
                              sij_lns=xcalold_lns(j)*ct_lns
     >                             +ycalold_lns(j)*st_lns
                              ang1_lns=ang1_lns
     >                             +atan((s1_lns-sij_lns)
     >                             /(z1_lns-zold_lns(j)))
                              ang2_lns=ang2_lns+atan((s2_lns-sij_lns)
     >                             /(z2_lns-zold_lns(j)))
                           endif
                        enddo
                        
                        ang1_lns=ang1_lns/real(nd_lns-1)
                        ang2_lns=ang2_lns/real(nd_lns-1)
                        da1_lns=abs(ang1_lns-ang0_lns)
                        da2_lns=abs(ang2_lns-ang0_lns)

cDK                        write(*,*) edc1eff_layer
cDK                        write(*,*)"param_lns=",param_lns
cDK                        write(*,*)"param_lns2=",param_lns2
cDK                        write(*,*)"s1_lns=",s1_lns
cDK                        write(*,*)"s2_lns=",s2_lns
cDK                        write(*,*)"s1cal_lns=",s1cal_lns
cDK                        write(*,*)"s2cal_lns=",s2cal_lns
cDK                        write(*,*)"d1_lns=",d1_lns*1000
cDK                        write(*,*)"d2_lns=",d2_lns*1000
cDK                        write(*,*)"da1_lns=",da1_lns*1000
cDK                        write(*,*)"da2_lns=",da2_lns*1000
                        
                        if(da1_lns.lt.da2_lns.and.d1_lns.lt.d2_lns) then
cDK                           write(*,*)"case1"
                           z_lns(i)=z1_lns
                           s_lns(i)=s1_lns
                           wt_lns(i)=w_lns(i)*f_lns
                        else if(da2_lns.lt.da1_lns
     >                          .and.d2_lns.lt.d1_lns) then
cDK                           write(*,*)"case2"
                           z_lns(i)=z2_lns
                           s_lns(i)=s2_lns
                           wt_lns(i)=w_lns(i)*f_lns
                        else
cDK                           write(*,*)"case3"
                           if((s1_lns-s1cal_lns)*(s2_lns-s2cal_lns)
     >                          .lt.0.) then
                              if(d1_lns.lt.d2_lns) then
                                 z_lns(i)=z1_lns
                                 s_lns(i)=s1_lns
                                 wt_lns(i)=0.1*w_lns(i)
                              else
                                 z_lns(i)=z2_lns
                                 s_lns(i)=s2_lns
                                 wt_lns(i)=0.1*w_lns(i)
                              endif
                           else
                              if(d1_lns.lt.d2_lns) then
                                 z_lns(i)=z1_lns
                                 s_lns(i)=s1_lns
                                 wt_lns(i)=0.1*w_lns(i)
                              else
                                 z_lns(i)=z2_lns
                                 s_lns(i)=s2_lns
                                 wt_lns(i)=0.1*w_lns(i)
                              endif
                           endif
                        endif
c                        write(*,*) "edc1eff_layer=",edc1eff_layer
c                        write(*,*) "nitra_lns=",nitra_lns
c                        write(*,*) "nd_lns=",i
c                        write(*,*) s_lns2(i),s_lns(i)
c                        write(*,*) s1_lns,s2_lns
                     endif      ! if( ipl(i)... ), paired or single
                  enddo       ! nd_lns
                  
                  Do i=1,enum_fpray_param
                     paramold_lns(i)=param_lns(i)
                  enddo
                  chiold_lns=chisqr_lns
c                  write(*,*) "edc1eff_layer=",edc1eff_layer
c                  write(*,*) " nd_lns=",nd_lns
c                  write(*,*) " z_lns=",z_lns
c                  write(*,*) " s_lns=",s_lns
c                  write(*,*) "nitra_lns=",nitra_lns
c                  Do i=1,nd_lns
c                     if (nd_lns.eq.10) then
c                        edc1_z(itrk,i) = z_lns(i)
c                     else 
c                        edc1_z(itrk,i) = 10000.
c                     endif
c                  enddo
                  call linearFit(nd_lns,z_lns,s_lns,a_lns,
     >                 wt_lns,param_lns,ierr)
c                  print *, "param_lns",param_lns
c                  write(*,*) "n, dx,dx',dy,dy'",
c     &             nitra_lns,(param_lns(1)-param_lns2(1))*1e4,
c     &             (param_lns(2)-param_lns2(2))*1e3,
c     &             (param_lns(3)-param_lns2(3))*1e4,
c     &             (param_lns(4)-param_lns2(4))*1e3
                  if(ierr.ne.0) write(*,*) "ierr3",ierr

c                  if(edc1eff_layer.gt.10) then
c                     xx1=(param_lns(1)+z_lns(3)*param_lns(2))
c     &                *cos(a_lns(3)) 
c                     xx2=(param_lns(1)+z_lns(8)*param_lns(2))
c     &                *cos(a_lns(8)) 
c                     yy1=(sw_lns(3)-xx1)/sin(a_lns(3))
c                     yy2=(sw_lns(8)-xx2)/sin(a_lns(8))
c                     write(*,*) yy1,yy2 
c                     write(*,*) param_lns(4),(yy2-yy1)/(z_lns(8)-z_lns(3))
c                  endif

                  
                  call calcChiSquare(nd_lns,z_lns,a_lns,zw_lns,sw_lns,
     >                 dl_lns,w_lns,param_lns,scalw_lns,dsdz_lns, 
     >                 xcal_lns, ycal_lns, chisqr_lns)
                  
                  nitra_lns=nitra_lns+1
c                  write(*,*) "chiold_lns=",chiold_lns
c                  write(*,*) "chisqr_lns=",chisqr_lns
                  
               enddo            ! do while(...)
               
c        write(*,*) "event=",gen_event_id_number
c        write(*,*) " nitra_lns",nitra_lns
c        write(*,*) " param_lns",param_lns
c        write(*,*) " param_lns2",param_lns2
c        write(*,*) " chisqr_lns",chisqr_lns
               
c     call HF1(eidniteration,float(nitra_lns),1.)
               
c      write(*,*) 'a=',
c     >           ex_fp_pre1(itrk,enum_fitting)-param_lns(1)
c      write(*,*) 'b=',
c     >           ey_fp_pre1(itrk,enum_fitting)-param_lns(3)
c      write(*,*) 'c=',
c     >           exp_fp_pre1(itrk,enum_fitting)-param_lns(2)
c      write(*,*) 'd=',
c     >           eyp_fp_pre1(itrk,enum_fitting)-param_lns(4)
               
               
c               fitdx=ex_fp_pre1(itrk,enum_fitting)-param_lns(1)
c               fitdy=ey_fp_pre1(itrk,enum_fitting)-param_lns(3)
c               fitdxp=exp_fp_pre1(itrk,enum_fitting)-param_lns(2)
c               fitdyp=eyp_fp_pre1(itrk,enum_fitting)-param_lns(4)
c               call HF1(eidfitdx,fitdx,1.)
c               call HF1(eidfitdy,fitdy,1.)
c               call HF1(eidfitdxp,fitdxp,1.)
c               call HF1(eidfitdyp,fitdyp,1.)
               
               
c     ---- fill variables (only jfit=enum_fitting) ----
c               write(*,*) 'edc1eff_layer',edc1eff_layer
               do pln=1,edc1_num_layers
                  edc1_track_coord_pre(itrk,pln,enum_fitting)=
     &                 edc1layer_coeff(remap(1),pln)*param_lns(1) !x
     &                 +edc1layer_coeff(remap(2),pln)*param_lns(3)!y
     &                 +edc1layer_coeff(remap(3),pln)*param_lns(2)!x'
     &                 +edc1layer_coeff(remap(4),pln)*param_lns(4)!y'
                  edc1_track_coord_pre2(pln)=
     &                 edc1layer_coeff(remap(1),pln)*param_lns2(1) !x
     &                 +edc1layer_coeff(remap(2),pln)*param_lns2(3)!y
     &                 +edc1layer_coeff(remap(3),pln)*param_lns2(2)!x'
     &                 +edc1layer_coeff(remap(4),pln)*param_lns2(4)!y'
c                  write(*,*) 'coord',edc1_track_coord_pre(itrk,pln,enum_fitting)
               enddo
               
c               write(*,*) 'coord',edc1_track_coord_pre2
c               write(*,*) 'param_lns=',param_lns
               do ihit = 2,entrack_hits_pre(itrk,1)+1
                  hit = entrack_hits_pre(itrk,ihit)
                  pln = edc1_layer_num(hit)
c                  if(edc1eff_layer.gt.10) then
c                     dt=edc1_drift_time(hit)-starttime(itrk)
c                     edc1_drift_time_pre(itrk,pln,enum_fitting)=dt
c                     edc1_drift_dis_pre(itrk,pln,enum_fitting)=
c     &                  edc1_track_coord_pre(itrk,pln,enum_fitting) -
c     &                  edc1_wire_center(hit)
c                  endif
                  do i=1,nd_lns2
                     if(pln.eq.iptr_lns2(i)) then
                        k=irptr_lns2(i)
                     endif
                  enddo
                  edc1_z(itrk,pln) = z_lns(k)
                  edc1_spos(itrk,pln)=s_lns(k)
                  edc1_residual_pre(itrk,pln,enum_fitting)=
     &                 s_lns2(k) 
     &                 -edc1_track_coord_pre(itrk,pln,enum_fitting)
c                     write(*,*) edc1eff_layer,pln,s_lns2(k)
c     &                 ,edc1_track_coord_pre(itrk,pln,enum_fitting)
c     &                 ,edc1_residual_pre(itrk,pln,enum_fitting)*10000
                  if(edc1eff_layer.gt.10) then
                     edc1chi2_pre(itrk,enum_fitting) = 
     &                    edc1chi2_pre(itrk,enum_fitting) +
     &                    (edc1_residual_pre(itrk,pln,enum_fitting)
     &                    /edc1_sigma(pln))**2 
                  endif
               enddo
               
               if(edc1eff_layer.gt.10) then
                  edc1chi2perdof_pre(itrk,enum_fitting) = 
     &                 edc1chi2_pre(itrk,enum_fitting)
     &                 /Real(enfree_pre(itrk))
               endif
               ex_fp_pre1(itrk,enum_fitting) = param_lns(1)
               ey_fp_pre1(itrk,enum_fitting) = param_lns(3)
               exp_fp_pre1(itrk,enum_fitting) = param_lns(2)
               eyp_fp_pre1(itrk,enum_fitting) = param_lns(4)
               
               if(edc1eff_layer.gt.10) then
                  if(edc1chi2perdof_pre(itrk,enum_fitting) 
     >                 .le. edc1bestchi2_pre(enum_fitting)) then
                     edc1bestchi2_pre_index(enum_fitting) = itrk
                     edc1bestchi2_pre(enum_fitting) 
     >                    = edc1chi2perdof_pre(itrk,enum_fitting)
                  EndIF 
               endif

c     ---- choose particles that pass through EDC effective area ----
c               mark(itrk)=1
               mark(itrk)=0
               if(abs(ex_fp_pre1(itrk,enum_fitting)
     >              +16*exp_fp_pre1(itrk,enum_fitting)).le.60.and.
     >              abs(ex_fp_pre1(itrk,enum_fitting)
     >              -16*exp_fp_pre1(itrk,enum_fitting)).le.60.and.
     >              abs(ey_fp_pre1(itrk,enum_fitting)
     >              +16*eyp_fp_pre1(itrk,enum_fitting)).le.7.and.
     >              abs(ey_fp_pre1(itrk,enum_fitting)
     >              -16*eyp_fp_pre1(itrk,enum_fitting)).le.7) then
                  mark(itrk)=1
               endif
               
               if(edc1eff_layer.le.10) then
                  edc1eff_residual(itrk,edc1eff_layer,enum_fitting)=
     >                 edc1_residual_pre(itrk,edc1eff_layer,enum_fitting)
                  edc1effx_fp_pre(itrk,edc1eff_layer,enum_fitting) =
     >                 ex_fp_pre1(itrk,enum_fitting)
                  edc1effy_fp_pre(itrk,edc1eff_layer,enum_fitting) =
     >                 ey_fp_pre1(itrk,enum_fitting)
                  edc1effxp_fp_pre(itrk,edc1eff_layer,enum_fitting) =
     >                 exp_fp_pre1(itrk,enum_fitting)
                  edc1effyp_fp_pre(itrk,edc1eff_layer,enum_fitting) =
     >                 eyp_fp_pre1(itrk,enum_fitting)
               endif
                  

c               write(*,*) 'event=',gen_event_id_number,' itrk=',itrk,
c     >              ' enum_fitting',enum_fitting
c               write(*,*) ' edc1eff_layer=',edc1eff_layer,
c     >              ' iptr_lns=',iptr_lns
c               write(*,*) ' x,y,xp,yp=',
c     >              edc1effx_fp_pre(itrk,edc1eff_layer,enum_fitting),
c     >              edc1effy_fp_pre(itrk,edc1eff_layer,enum_fitting),
c     >              edc1effxp_fp_pre(itrk,edc1eff_layer,enum_fitting),
c     >              edc1effyp_fp_pre(itrk,edc1eff_layer,enum_fitting)
c               write(*,*) ' edc1eff_layer=',edc1eff_layer,
c     >              ' edc1eff_residual=',
c     >              edc1eff_residual(itrk,edc1eff_layer,enum_fitting),
c     >              ' edc1_residual_pre=',
c     >              edc1_residual_pre(itrk,edc1eff_layer,enum_fitting)
c     write(*,*) "event",gen_event_id_number
c      write(*,*) " lns x,y,xp,yp",ex_fp_pre1(itrk,enum_fitting),
c     >           ey_fp_pre1(itrk,enum_fitting),
c     >           exp_fp_pre1(itrk,enum_fitting),
c     >           eyp_fp_pre1(itrk,enum_fitting)
               
******************************************
*     LNS fitting routine end
******************************************
               
c               write(*,*) edc1eff_layer,
c     >              ' edc1eff_residual=',
c     >              edc1eff_residual(itrk,10,enum_fitting),
c     >              ' edc1_residual_pre=',
c     >              edc1_residual_pre(itrk,10,enum_fitting)
 1214          continue         ! nduv_lns<3
               edc1eff_counter=edc1eff_counter+1
            enddo
12345       continue            ! end of 2nd fitting
         EndIf                  ! enfreefp condition
c     if(echi2perdof_fp(itrk) .lt. 200) then
c     Write(*,*) 'trackfit=',itrk,echi2perdof_fp(itrk)
c     EndIf
c               write(*,*) edc1eff_layer,
c     >              ' edc1eff_residual=',
c     >              edc1eff_residual(itrk,10,enum_fitting),
c     >              ' edc1_residual_pre=',
c     >              edc1_residual_pre(itrk,10,enum_fitting)
      EndDo                     ! track loop

cCCCCCCCCCCCCCCCCCCCCCCCCCC SEVA NEW TIMING
c calculate new start time in order to get correct drift time information 
      seva_loop=seva_loop+1
      if (seva_loop.lt.nseva_loops) then
         Do i=1,entracks_pre
            slope = sqrt(1+exp_fp_pre1(i,enum_fitting)*exp_fp_pre1(i,enum_fitting)
     &           +eyp_fp_pre1(i,enum_fitting)*eyp_fp_pre1(i,enum_fitting))
            temp_time=starttime(i)
            num   = 0
            stime = 0
            on_track_hits_pre(i,1)=0
            do kk=1,emax_num_dc1_layers
               num_la(kk)=0
            enddo
            
            Do j = 1, escin_tot_hits               
               la = escin_layer_num(j)
               if((la.eq.1).or.(la.eq.2)) then
                  co = escin_counter_num(j)
                  if( mod(co,2).eq.1) then
                     x_la =  -exp_fp_pre1(i,enum_fitting) 
     &                    * ( escin_zpos(la)+1.5 ) ! EHODO downstream side
     &                    + ex_fp_pre1(i,enum_fitting)
                  else 
                     x_la =  -exp_fp_pre1(i,enum_fitting) 
     &                    * ( escin_zpos(la)-1.5 ) ! EHODO upstream side
     &                    + ex_fp_pre1(i,enum_fitting)
                  endif
c                  x_la =  -exp_fp_pre1(i,enum_fitting) *escin_zpos(la) + 
c     &              ex_fp_pre1(i,enum_fitting)
                  x_min = escin_xcenter(la,co) - 0.5*escin_width+0.25+
     &              abs(escin_thickness*0.5*(-exp_fp_pre1(i,enum_fitting)))
                  x_max = escin_xcenter(la,co) + 0.5*escin_width-0.25-
     &              abs(escin_thickness*0.5*(-exp_fp_pre1(i,enum_fitting)))
               
                  if( x_la .ge. x_min .and. x_la .le. x_max ) then
                     num_la(la)=num_la(la)+1
                     num = num + 1 
                     if( mod(co,2).eq.1) then
                        layer_temp_fpt = escin_mean_time(j) 
     &                       - ( escin_zpos(la)+1.5 ) ! EHODO downstream side
     &                       * slope / speed_of_light
                     else
                        layer_temp_fpt = escin_mean_time(j) 
     &                       - ( escin_zpos(la)-1.5 ) ! EHODO upstream side
     &                       * slope / speed_of_light
                     endif
c                     layer_temp_fpt=escin_mean_time(j) - 
c     &                   escin_zpos(la) * slope / speed_of_light
                     stime = stime + layer_temp_fpt
                     on_track_hits_pre(i,1)=num
                     on_track_hits_pre(i,num+1)=j
                  endif              
               endif
            Enddo

            if(num.gt.1) starttime(i) = stime/num
            if (num.eq.1) then
               Do j = 1, escin_tot_hits
                  la = escin_layer_num(j)
c                  if(la.eq.3.or.num_la(la).gt.0) goto 1201
                  if(((la.eq.1).or.(la.eq.2)).and.num_la(la).eq.0) then
                     co = escin_counter_num(j)
                     if( mod(co,2).eq.1 ) then
                        x_la =  -exp_fp_pre1(i,enum_fitting) 
     &                       * ( escin_zpos(la) +1.5 ) ! EHODO downstream side
     &                       + ex_fp_pre1(i,enum_fitting)
                     else
                        x_la =  -exp_fp_pre1(i,enum_fitting) 
     &                       * ( escin_zpos(la) -1.5 ) ! EHODO upstream side
     &                       + ex_fp_pre1(i,enum_fitting)
                     endif
c                     x_la =  -exp_fp_pre1(i,enum_fitting) *escin_zpos(la) + 
c     &                  ex_fp_pre1(i,enum_fitting)
                     x_min = escin_xcenter(la,co) - 0.5*escin_width-escin_xslop
                     x_max = escin_xcenter(la,co) + 0.5*escin_width+escin_xslop
c                     x_min = escin_xcenter(la,co) - 0.5*escin_width-escin_width/3
c                     x_max = escin_xcenter(la,co) + 0.5*escin_width+escin_width/3
                     if( x_la .ge. x_min .and. x_la .le. x_max ) then
                        if( mod(co,2).eq.1 ) then
                           layer_temp_fpt = escin_mean_time(j)
     &                          - ( escin_zpos(la) +1.5 ) ! EHODO downstream side
     &                          * slope / speed_of_light
                        else
                           layer_temp_fpt = escin_mean_time(j)
     &                          - ( escin_zpos(la) -1.5 ) ! EHODO upstream side
     &                          * slope / speed_of_light
                        endif
c                        layer_temp_fpt=escin_mean_time(j) -
c     &                     escin_zpos(la) * slope / speed_of_light
                        if (num.gt.0) then
                           if (abs(stime/num-layer_temp_fpt).lt.1.5*etrk_time_gate) then
                              num = num + 1 
                              stime = stime + escin_mean_time(j)
                              on_track_hits_pre(i,1)=num
                              on_track_hits_pre(i,num+1)=j
                           endif
                        else
                           num = num + 1 
                           stime = stime + escin_mean_time(j)
                           on_track_hits_pre(i,1)=num
                           on_track_hits_pre(i,num+1)=j
                        endif
                     endif                  
                  endif
               Enddo
            endif  

            if(num.gt.0) starttime(i) = stime/num
            if (num.eq.0.and.seva_loop.eq.(nseva_loops-1).and.
     &               i.le.entracks_pre_seva) then
               temp_on_track=0
               Do j = 1, escin_tot_hits
                  la = escin_layer_num(j)
                  if(((la.eq.1).or.(la.eq.2)).and.num_la(la).eq.0) then
                     co = escin_counter_num(j)
                     if( mod(co,2).eq.1 ) then
                        x_la =  -exp_fp_pre1(i,enum_fitting)
     &                       * ( escin_zpos(la)+1.5 ) ! EHODO downstream side
     &                       + ex_fp_pre1(i,enum_fitting)
                     else 
                        x_la =  -exp_fp_pre1(i,enum_fitting)
     &                       * ( escin_zpos(la)-1.5 ) ! EHODO upstream side
     &                       + ex_fp_pre1(i,enum_fitting)
                     endif
                     x_la =  -exp_fp_pre1(i,enum_fitting) *escin_zpos(la) +
     &                  ex_fp_pre1(i,enum_fitting)
                     x_min = escin_xcenter(la,co) - 0.5*escin_width-escin_xslop
                     x_max = escin_xcenter(la,co) + 0.5*escin_width+escin_xslop
c                     x_min = escin_xcenter(la,co) - 0.5*escin_width-escin_width/3
c                     x_max = escin_xcenter(la,co) + 0.5*escin_width+escin_width/3
                     if( x_la .ge. x_min .and. x_la .le. x_max ) then
                        temp_on_track=temp_on_track+1
                        temp_on_track_hits(temp_on_track)=j
                     endif 
                  endif 
               Enddo

               
               do j=1,temp_on_track
                  flag_used(j)=0
               enddo
               set_new_track=0
               
               do j=1, temp_on_track-1
                  stime=0
                  num=0
                  if ( flag_used(j).eq.0.and.entracks_pre.lt.entracks_pre_max) then
                     flag_used(j)=1
                     set_new_track=set_new_track+1
                     entracks_pre=entracks_pre_seva+set_new_track
                     num=1
                     hit1=temp_on_track_hits(j)
                     la1 = escin_layer_num(hit1)
                     co1 = escin_counter_num(hit1)
                     if( mod(co1,2).eq.1 ) then
                        layer_temp_fpt=escin_mean_time(hit1) 
     &                       - ( escin_zpos(la) +1.5 ) !EHODO downstream side
     &                       * slope / speed_of_light
                     else
                        layer_temp_fpt=escin_mean_time(hit1) 
     &                       - ( escin_zpos(la) -1.5 ) !EHODO upstream side
     &                       * slope / speed_of_light
                     endif
c                     layer_temp_fpt=escin_mean_time(hit1) - 
c     &                  escin_zpos(la) * slope / speed_of_light
                     stime=layer_temp_fpt
                     
                     on_track_hits_pre(i,1)=num        
                     on_track_hits_pre(i,num+1)=hit1        

                     stime=layer_temp_fpt
                     do k=j+1, temp_on_track
                        
                        hit2=temp_on_track_hits(k)
                        la2 = escin_layer_num(hit2)
                        co2 = escin_counter_num(hit2)
                        if( mod(co2,2).eq.1 )then
                           layer_temp_fpt = escin_mean_time(hit2) 
     &                          - ( escin_zpos(la2) +1.5 ) ! EHODO downstream side
     &                          * slope / speed_of_light 
                        else
                           layer_temp_fpt = escin_mean_time(hit2) 
     &                          - ( escin_zpos(la2) -1.5 ) ! EHODO upstream side
     &                          * slope / speed_of_light 
                        endif   
c                        layer_temp_fpt=escin_mean_time(hit2) - 
c     &                   escin_zpos(la2) * slope / speed_of_light 
                        if (abs(stime/num-layer_temp_fpt).lt.1.5*etrk_time_gate) then
                           flag_used(k)=1
                           num=num+1
                           on_track_hits_pre(i,1)=num        
                           on_track_hits_pre(i,num+1)=hit2  
                           stime=stime+layer_temp_fpt
                        endif
                     enddo
                     enfree_pre(entracks_pre)=enfree_pre(i)
                     starttime(entracks_pre)=stime/num
                     do ihit = 1,entrack_hits_pre(i,1)+1
                        entrack_hits_pre(entracks_pre,ihit)= entrack_hits_pre(i,ihit)
                     enddo
                  endif
               enddo
            endif
         enddo
      goto 5555
      endif

      
c     write(*,*) "---------------------------------------------------"
c     write(*,*) "Before",entracks_pre
c     write(*,*) "edc1bestchi2_pre_index(enum_fitting)",
c     >     edc1bestchi2_pre_index(enum_fitting)
c      Do i=1,entracks_pre
c       write(*,*) " mark",mark(i)," ex",ey_fp_pre1(i,enum_fitting)
c      enddo
      
c     ---- fill variables that pass through EDC effective area ----
      ii=0
      jj=0
      Do i=1,entracks_pre
         if(mark(i).eq.1) then
            ii=ii+1
            Do pln=1,edc1_num_layers
               edc1_track_coord_pre(ii,pln,enum_fitting)=
     >              edc1_track_coord_pre(i,pln,enum_fitting)
               edc1_residual_pre(ii,pln,enum_fitting)=
     >              edc1_residual_pre(i,pln,enum_fitting)
               edc1eff_residual(ii,pln,enum_fitting)=
     >              edc1eff_residual(i,pln,enum_fitting)
               edc1effx_fp_pre(ii,pln,enum_fitting) = 
     >              edc1effx_fp_pre(i,pln,enum_fitting)
               edc1effy_fp_pre(ii,pln,enum_fitting) = 
     >              edc1effy_fp_pre(i,pln,enum_fitting)
               edc1effxp_fp_pre(ii,pln,enum_fitting) =
     >              edc1effxp_fp_pre(i,pln,enum_fitting)
               edc1effyp_fp_pre(ii,pln,enum_fitting) =
     >              edc1effyp_fp_pre(i,pln,enum_fitting)
            enddo
            edc1chi2_pre(ii,enum_fitting) = 
     >           edc1chi2_pre(i,enum_fitting)
            edc1chi2perdof_pre(ii,enum_fitting) = 
     >           edc1chi2perdof_pre(i,enum_fitting)
            ex_fp_pre1(ii,enum_fitting) = 
     >           ex_fp_pre1(i,enum_fitting)
            ey_fp_pre1(ii,enum_fitting) = 
     >           ey_fp_pre1(i,enum_fitting)
            exp_fp_pre1(ii,enum_fitting) =
     >           exp_fp_pre1(i,enum_fitting)
            eyp_fp_pre1(ii,enum_fitting) =
     >           eyp_fp_pre1(i,enum_fitting)
            enfree_pre(ii)=enfree_pre(i)
            Do ihit=2,entrack_hits_pre(i,1)+1
               entrack_hits_pre(ii,ihit)=entrack_hits_pre(i,ihit)
            enddo
            entrack_hits_pre(ii,1)=entrack_hits_pre(i,1)
            if(edc1bestchi2_pre_index(enum_fitting).eq.i) then
               edc1bestchi2_pre_index(enum_fitting)=ii
               jj=1
            endif
         endif
      enddo
      entracks_pre=ii
      if(jj.eq.0) then
c     write(*,*) " jj=0!!!!!!!!!!!!!!!!!!!!!!!!"
         edc1bestchi2_pre_index(enum_fitting)=1
         edc1bestchi2_pre(enum_fitting)=edc1chi2perdof_pre(1,enum_fitting)
         Do i=1,entracks_pre
            if(edc1chi2perdof_pre(i,enum_fitting).le.
     >           edc1bestchi2_pre(enum_fitting)) then
               edc1bestchi2_pre_index(enum_fitting)=i
               edc1bestchi2_pre(enum_fitting)=
     >              edc1chi2perdof_pre(i,enum_fitting)
            endif
         enddo
      endif
c      write(*,*) "After",entracks_pre
c     Do i=1,entracks_pre
c     write(*,*) " ex",ex_fp_pre1(i,enum_fitting)
c     enddo
c     write(*,*) "edc1bestchi2_pre_index(enum_fitting)",
c     >     edc1bestchi2_pre_index(enum_fitting)
      
*     --- Histogram for dc1 track bank 
      call e_fill_dc1_track_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

*     --- calculate dc1 layer efficiency
      Call e_dc1_layer_eff(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif
      
c     --- test if we want to dump out trackfit results
      if(edebugtrackprint.ne.0) then
         call e_dc1_print_tracks
      endif                     ! end test on zero tracks
      
 1000 return
      
      end


**************************************************************************
      Subroutine calcChiSquare(n,z,a,zw,sw,dl,w,param,
     >     scalw,dsdz,xcal,ycal,chisqr)

      implicit none
      include "hes_data_structures.cmn"
      include "hes_tracking.cmn"
      include "hes_geometry.cmn"
      include "hes_id_histid.cmn"
      include 'gen_event_info.cmn'

      integer*4 n
      real*4 z(n),a(n),zw(n),sw(n),dl(n),w(n)
      real*4 scalw(n),dsdz(n),xcal(n),ycal(n)
      real*8 param(enum_fpray_param)
      real*4 chisqr

      integer*4 i,j,k
      real*4 ct,st
      real*4 scal,dlcal,xw,yw

      chisqr=0.
      Do i=1,n
         ct=cos(a(i))
         st=sin(a(i))
         xcal(i)=param(1) + param(2)*z(i)
         ycal(i)=param(3) + param(4)*z(i)
         scal=xcal(i)*ct + ycal(i)*st
         dlcal=sqrt((z(i)-zw(i))*(z(i)-zw(i))
     >        + (scal-sw(i))*(scal-sw(i)))
         chisqr = chisqr + (dl(i)-dlcal)*(dl(i)-dlcal)*w(i)

         xw=param(1) + param(2)*zw(i)
         yw=param(3) + param(4)*zw(i)
         dsdz(i) = param(2)*ct + param(4)*st
         scalw(i)=xw*ct + yw*st
      enddo
      chisqr = chisqr/real(n-enum_fpray_param)

      return
      end

**************************************************************************
      Subroutine linearFit(n,z,s,a,w,param,ierr)

      implicit none
      include "hes_data_structures.cmn"
      include "hes_tracking.cmn"
      include "hes_geometry.cmn"
      include "hes_id_histid.cmn"
      include 'gen_event_info.cmn'

      integer*4 n
      real*4 z(n),s(n),a(n),w(n)
      real*8 param(enum_fpray_param)
      real*8 paramx(2)
      real*8 paramy(2)
      real*8 mtpx(enum_fpray_param,enum_fpray_param)
      real*8 mtpy(enum_fpray_param,enum_fpray_param)
      integer*4 indx(enum_fpray_param)
      integer*4 indy(enum_fpray_param)
      integer*4 ipiv(enum_fpray_param)
      integer*4 ierr

      integer*4 i,j,k
      real*4 ct,st
      real*4 xx,yy

      ierr=0
      Do i=1,enum_fpray_param
         param(i)=0
         Do j=1,enum_fpray_param
            mtpx (i,j)=0
            mtpy (i,j)=0
         enddo
      enddo
      ct=0
      st=0
      xx=0
      yy=0

      Do i=1,n
         ct=cos(a(i))
         st=sin(a(i))
         if (ct.eq.1) then
            mtpx(1,1) = mtpx(1,1) + w(i)
            mtpx(1,2) = mtpx(1,2) + w(i)*z(i)
            mtpx(2,2) = mtpx(2,2) + w(i)*z(i)*z(i)
            paramx(1) = paramx(1) + w(i)*s(i)
            paramx(2) = paramx(2) + w(i)*z(i)*s(i)
         endif
      enddo
      mtpx(2,1) = mtpx(1,2)
      call GaussJordan(mtpx,2,paramx,indx,indy,ipiv,ierr)
      
      Do i=1,n
         ct=cos(a(i))
         st=sin(a(i))
         if (ct.ne.1) then
            xx=paramx(1)+paramx(2)*z(i)
            yy=(s(i)-xx*ct)/st
            mtpy(1,1) = mtpy(1,1) + w(i)
            mtpy(1,2) = mtpy(1,2) + w(i)*z(i)
            mtpy(2,2) = mtpy(2,2) + w(i)*z(i)*z(i)
            paramy(1) = paramy(1) + w(i)*yy
            paramy(2) = paramy(2) + w(i)*z(i)*yy
         endif
      enddo
      mtpy(2,1) = mtpy(1,2)
      call GaussJordan(mtpy,2,paramy,indx,indy,ipiv,ierr)
      
      param(1)=paramx(1) 
      param(2)=paramx(2) 
      param(3)=paramy(1) 
      param(4)=paramy(2) 


      return
      end


**************************************************************************
      Subroutine GaussJordan(a,n,b,indxr,indxc,ipiv,ierr )

      implicit none
      include "hes_data_structures.cmn"
      include "hes_tracking.cmn"
      include "hes_geometry.cmn"
      include "hes_id_histid.cmn"
      include 'gen_event_info.cmn'

      real*8 a(enum_fpray_param,enum_fpray_param)
      integer*4 n
c      real*8 b(enum_fpray_param)
      real*8 b(2)
      integer*4 indxr(enum_fpray_param)
      integer*4 indxc(enum_fpray_param)
      integer*4 ipiv(enum_fpray_param)
      integer*4 ierr

      integer*4 i,j,k,l
      integer*4 irow,icol
      real*8 big,ta,tb,pivinv,d,t

      ierr=0
      Do j=1,n
         indxr(j)=0
         indxc(j)=0
         ipiv(j)=0
      enddo

      Do i=1,n
         big=0.0
         irow=-1
         icol=-1
         Do j=1,n
            if(ipiv(j).ne.1) then
               Do k=1,n
                  if(ipiv(k).eq.0 ) then
                     if(abs(a(j,k)).ge.big) then
                        big=abs(a(j,k))
                        irow=j
                        icol=k
                     endif
                  else if(ipiv(k).gt.1) then
                     write(*,*) 'GaussJordan: Singular Matrix-1'
                     ierr=1
                     stop
                  endif
               enddo
            endif
         enddo
         ipiv(icol)=ipiv(icol)+1
         
         if(irow.ne.icol) then
            Do k=1,n
               ta=a(irow,k)
               a(irow,k)=a(icol,k)
               a(icol,k)=ta
            enddo
            tb=b(irow)
            b(irow)=b(icol)
            b(icol)=tb
         endif
         
         indxr(i)=irow
         indxc(i)=icol
         
c         print *, "i,icol,a1",i,icol,a(1,1),a(1,2),a(2,1),a(2,2)
         if(a(icol,icol).eq.0) then
            write(*,*) 'GaussJordan: Singular Matrix-2, n='
     &       ,n,' i=',i,' icol=',icol,'big=',big
            ierr=1
c            stop
            goto 1111
         endif
         pivinv=1./a(icol,icol)
         a(icol,icol)=1.
         Do k=1,n
            a(icol,k)=a(icol,k)*pivinv
         enddo
         b(icol)=b(icol)*pivinv
         Do k=1,n
            if(k.ne.icol) then
               d=a(k,icol)
               a(k,icol)=0.
               Do l=1,n
                  a(k,l)=a(k,l)-a(icol,l)*d
               enddo
               b(k)=b(k)-b(icol)*d
            endif
         enddo
c         print *, "i,a2",i,a(1,1),a(1,2),a(2,1),a(2,2)
      enddo

      Do l=n,1,-1
         if(indxr(l).ne.indxc(l)) then
            Do k=1,n
               t=a(k,indxr(l))
               a(k,indxr(l))=a(k,indxc(l))
               a(k,indxc(l))=t
            enddo
         endif
      enddo

1111  return
      end


**************************************************************************
      Subroutine preSelectLR(n,zw,sw,ang,dl,worig,ipl,z,s,w,ierr)

      implicit none
      include "hes_data_structures.cmn"
      include "hes_tracking.cmn"
      include "hes_geometry.cmn"
      include "hes_id_histid.cmn"
      include 'gen_event_info.cmn'

      integer*4 n
      integer*4 ipl(emax_num_dc1_layers)
      real*4 zw(emax_num_dc1_layers),sw(emax_num_dc1_layers)
      real*4 ang(emax_num_dc1_layers),dl(emax_num_dc1_layers)
      real*4 worig(emax_num_dc1_layers)
c      real*4 z(emax_num_dc1_layers),s(emax_num_dc1_layers)
c      real*4 w(emax_num_dc1_layers)
c      integer*4 ipl(n)
c      real*4 zw(n),sw(n),ang(n),dl(n),worig(n)
      real*4 z(n),s(n),w(n)
      integer*4 ierr

      real*8 mtp(enum_fpray_param,enum_fpray_param)
      real*8 p(enum_fpray_param),q(enum_fpray_param)
      real*4 zt(emax_num_dc1_layers),xt(emax_num_dc1_layers)
      real*4 wt(emax_num_dc1_layers)
      real*4 dsdz,f,z11,z12,z21,z22,s11,s12,s21,s22
      real*4 dsl(16),rayang,minads,tmp,xx
      real*4 ct,st,z1,z2,s1,s2,s1cal,s2cal,d1,d2
      integer*4 nx,ny,mini
      integer*4 indx(enum_fpray_param),indy(enum_fpray_param)
      integer*4 ipiv(enum_fpray_param)
      integer*4 i,ii,jj,kk,j
      integer*4 kf(emax_num_dc1_layers)

      real*4 SmallTiltAngle
      parameter(SmallTiltAngle=0.1)
      real*4 CellSize,xx1,xx2,yy1,yy2
      real*4 ss1(16),ss2(16),zz1(16),zz2(16)
      parameter(CellSize=0.5)

      ierr=0
      
      do i=1,16
         ss1(i)=0
         ss2(i)=0
         zz1(i)=0
         zz2(i)=0
      enddo

      nx=0
      Do i=1,emax_num_dc1_layers
         if(abs(ang(i)).lt.SmallTiltAngle) then
            nx=nx+1
            zt(nx)=zw(i)
            xt(nx)=sw(i)
            wt(nx)=worig(i)
c            write(*,*) "zw(i),sw(i)"
c     &        ,i,zw(i),sw(i)
         endif
      enddo
c      write(*,*) "nx",nx
      p(1)=0.0
      p(2)=0.0
      p(3)=0.0
      p(4)=0.0
      mtp(1,1)=0.0
      mtp(1,2)=0.0
      mtp(2,2)=0.0
      Do i=1,nx
         mtp(1,1) = mtp(1,1)+wt(i)
         mtp(1,2) = mtp(1,2)+wt(i)*zt(i)
         mtp(2,2) = mtp(2,2)+wt(i)*zt(i)*zt(i)
         
         p(1) = p(1)+wt(i)*xt(i)
         p(2) = p(2)+wt(i)*zt(i)*xt(i)
      enddo
      mtp(2,1) = mtp(1,2)
      
      call GaussJordan(mtp,2,p,indx,indy,ipiv,ierr)
      if(ierr.ne.0) write(*,*) "ierr4",ierr
c      write(*,*)"p(1),p(2)",p(1),p(2)
c     
c     ---- Second Fit With DL ----
c     
      nx=0
      Do i=1,emax_num_dc1_layers
         if(abs(ang(i)).lt.SmallTiltAngle) then
            nx=nx+1
            dsdz=p(2)
            f=1./sqrt(1.+dsdz*dsdz)
            if(ipl(i).ge.0) then
               ii=ipl(i)
               z11=zw(i) -f*dsdz*dl(i)
               z12=zw(i) +f*dsdz*dl(i)
               z21=zw(ii)-f*dsdz*dl(ii)
               z22=zw(ii)+f*dsdz*dl(ii)
               s11=sw(i) +f*dl(i)
               s12=sw(i) -f*dl(i)
               s21=sw(ii)+f*dl(ii)
               s22=sw(ii)-f*dl(ii)
               do j=1,16
                   if (iand(j,1).eq.0) then
                     ss1(j)=s11
                   else
                     ss1(j)=s12
                   endif
                   if (iand(ishft(j,-1),1).eq.0) then
                     ss2(j)=s21
                   else
                     ss2(j)=s22
                   endif
                   if (iand(ishft(j,-2),1).eq.0) then
                     zz1(j)=z11
                   else
                     zz1(j)=z12
                   endif
                   if (iand(ishft(j,-3),1).eq.0) then
                     zz2(j)=z21
                   else
                     zz2(j)=z22
                   endif
                  dsl(j)=((ss1(j)-ss2(j))/(zz1(j)-zz2(j)))
               enddo

               mini=0
               rayang=atan(dsdz)
               minads=abs(atan(dsl(1))-rayang)
               Do jj=2,16
                  tmp=abs(atan(dsl(jj))-rayang)
                  if(tmp.lt.minads) then
                     minads=tmp
                     mini=jj-1
                  endif
               enddo
               zt(nx)=zz1(mini+1)
               xt(nx)=ss1(mini+1)
               wt(nx)=worig(i)*f
c               nx=nx+1
            else
               zt(nx)=zw(i)
               xt(nx)=sw(i)
               wt(nx)=1./(CellSize*CellSize)
c               nx=nx+1
            endif
         endif
      enddo
      
      q(1)=0.0
      q(2)=0.0
      mtp(1,1)=0.0
      mtp(1,2)=0.0
      mtp(2,1)=0.0
      mtp(2,2)=0.0
      Do i=1,nx
         mtp(1,1) = mtp(1,1)+wt(i)
         mtp(1,2) = mtp(1,2)+wt(i)*zt(i)
         mtp(2,2) = mtp(2,2)+wt(i)*zt(i)*zt(i)
         q(1) = q(1)+wt(i)*xt(i)
         q(2) = q(2)+wt(i)*zt(i)*xt(i)
      enddo
      mtp(2,1)=mtp(1,2)

      call GaussJordan(mtp,2,q,indx,indy,ipiv,ierr)
      if(ierr.ne.0) write(*,*) "ierr5",ierr
      
c      nx=0
c      Do i=1,emax_num_dc1_layers
c         if(abs(ang(i)).lt.SmallTiltAngle) then
c            nx=nx+1
c            if (abs(xt(nx))<5) then
c               print *, "nx,xt,xcoord,dx",nx,xt(nx),(q(1)+zw(i)*q(2)),xt(nx)-(q(1)+zw(i)*q(2))
c            endif
c         endif
c      enddo
      
      p(1)=q(1)
      p(2)=q(2)
      ny=0
      p(3)=0.0
      Do i=1,n
         if(abs(ang(i)).gt.SmallTiltAngle) then
            ny=ny+1
            xx=p(1)+p(2)*zw(i)
c            p(3) = p(3)+(xx*cos(ang(i))-sw(i))/sin(ang(i))
            p(3) = p(3)+(sw(i)-xx*cos(ang(i)))/sin(ang(i))
c            write(*,*) "p(1),p(2),zw(i),sw(i),xx"
c     &        ,i,p(1),p(2),zw(i),sw(i),xx
c            ny=ny+1
         endif
      enddo
      p(3) = p(3)/real(ny)
      
    
c
c     End Of Second Fit
c

c     --- Third Fit ---
cDK      Do i=1,n
      Do i=1,10
         kf(i)=1
      enddo

      Do i=1,n
         ct=cos(ang(i))
         dsdz=p(2)*ct
         f=1./sqrt(1.+dsdz*dsdz)
         if(kf(i).gt.0.and.ipl(i).ge.0.and.kf(ipl(i)).gt.0) then
            !ii=ipl(i)
            kf(i)=0
            !kf(ii)=0
            ii=i+1 !DK
            kf(i+1)=0 !DK

            z11=zw(i) -f*dsdz*dl(i)
            z12=zw(i) +f*dsdz*dl(i)
            z21=zw(ii)-f*dsdz*dl(ii)
            z22=zw(ii)+f*dsdz*dl(ii)
            s11=sw(i) +f*dl(i)
            s12=sw(i) -f*dl(i)
            s21=sw(ii)+f*dl(ii)
            s22=sw(ii)-f*dl(ii)
            do j=1,16
                if (iand(j,1).eq.0) then
                  ss1(j)=s11
                else
                  ss1(j)=s12
                endif
                if (iand(ishft(j,-1),1).eq.0) then
                  ss2(j)=s21
                else
                  ss2(j)=s22
                endif
                if (iand(ishft(j,-2),1).eq.0) then
                  zz1(j)=z11
                else
                  zz1(j)=z12
                endif
                if (iand(ishft(j,-3),1).eq.0) then
                  zz2(j)=z21
                else
                  zz2(j)=z22
                endif
               dsl(j)=((ss1(j)-ss2(j))/(zz1(j)-zz2(j)))
            enddo

            mini=0
            rayang=atan(dsdz)
            minads=abs(atan(dsl(1))-rayang)
            Do jj=2,16
               tmp=abs(atan(dsl(jj))-rayang)
               if(tmp.lt.minads) then
                  minads=tmp
                  mini=jj-1
               endif
            enddo
            z(i)=zz1(mini+1)
            s(i)=ss1(mini+1)
            z(ii)=zz2(mini+1)
            s(ii)=ss2(mini+1)
            w(i)=worig(i)*f
            w(ii)=worig(ii)*f
         else if(kf(i).gt.0) then
            kf(i)=0
            if(abs(ang(i)).lt.SmallTiltAngle) then
               st=sin(ang(i))
               z1=zw(i)-f*dsdz*dl(i)
               z2=zw(i)+f*dsdz*dl(i)
               s1=sw(i)+f*dl(i)
               s2=sw(i)-f*dl(i)
               s1cal=(p(1)+p(2)*z1)*ct+p(3)*st
               s2cal=(p(1)+p(2)*z2)*ct+p(3)*st
c               write(*,*)"s1,s1cal",s1,s1cal
               d1=abs(s1-s1cal)
               d2=abs(s2-s2cal)

               if(d1.lt.d2) then
                  z(i)=z1
                  s(i)=s1
                  w(i)=worig(i)*f
               else
                  z(i)=z2
                  s(i)=s2
                  w(i)=worig(i)*f
               endif
            else
               z(i)=zw(i)
               s(i)=sw(i)
               w(i)=1./(CellSize*CellSize)
            endif
         endif
      enddo

      mtp(1,1)=0.0
      mtp(1,2)=0.0
      mtp(1,3)=0.0
      mtp(1,4)=0.0
      mtp(2,2)=0.0
      mtp(2,3)=0.0
      mtp(2,4)=0.0
      mtp(3,3)=0.0
      mtp(3,4)=0.0
      mtp(4,4)=0.0
      q(1)=0.0
      q(2)=0.0
      q(3)=0.0
      q(4)=0.0
      Do i=1,n
         ct=cos(ang(i))
         st=sin(ang(i))
         mtp(1,1) = mtp(1,1) + w(i)*ct*ct
         mtp(1,2) = mtp(1,2) + w(i)*z(i)*ct*ct
         mtp(1,3) = mtp(1,3) + w(i)*ct*st
         mtp(1,4) = mtp(1,4) + w(i)*z(i)*ct*st
         mtp(2,2) = mtp(2,2) + w(i)*z(i)*z(i)*ct*ct
         mtp(2,3) = mtp(2,3) + w(i)*z(i)*ct*st
         mtp(2,4) = mtp(2,4) + w(i)*z(i)*z(i)*ct*st
         mtp(3,3) = mtp(3,3) + w(i)*st*st
         mtp(3,4) = mtp(3,4) + w(i)*z(i)*st*st
         mtp(4,4) = mtp(4,4) + w(i)*z(i)*z(i)*st*st

         q(1) = q(1)+w(i)*s(i)*ct
         q(2) = q(2)+w(i)*s(i)*z(i)*ct
         q(3) = q(3)+w(i)*s(i)*st
         q(4) = q(4)+w(i)*s(i)*z(i)*st
      enddo
      mtp(2,1) = mtp(1,2)
      mtp(3,1) = mtp(1,3)
      mtp(3,2) = mtp(2,3)
      mtp(4,1) = mtp(1,4)
      mtp(4,2) = mtp(2,4)
      mtp(4,3) = mtp(3,4)
c      call GaussJordan(mtp,3,q,indx,indy,ipiv,ierr)
      call GaussJordan(mtp,4,q,indx,indy,ipiv,ierr)
      if(ierr.ne.0) write(*,*) "ierr6",ierr
      p(1)=q(1)
      p(2)=q(2)
      p(3)=q(3)
      p(4)=q(4)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!ang
c      if(n.eq.10) then
c         xx1=(p(1)+z(3)*p(2))*cos(ang(3)) 
c         xx2=(p(1)+z(8)*p(2))*cos(ang(8)) 
c         yy1=(s(3)-xx1)*sin(ang(3))
c         yy2=(s(8)-xx2)*sin(ang(8))
c         write(*,*) yy1,yy2,(yy2-yy1)/(z(8)-z(3))
c      endif
      

c
c      End of Third Fit
c    

cDK      Do i=1,n
      Do i=1,10
         kf(i)=1
      enddo

      kk=1 
      Do i=1,n
         ct=cos(ang(i))
         dsdz=p(2)*ct+p(4)*st
         f=1./sqrt(1.+dsdz*dsdz)
         if(kf(i).gt.0.and.ipl(i).ge.0.and.kf(ipl(i)).gt.0) then
            !ii=ipl(i)
            ii=i+1 !DK
            !kf(i)=0
            kf(i+1)=0 !DK
cDK         write(*,*) "i=",i,zw(i),sw(i),dl(i)
cDK         write(*,*) "ii=",ii,zw(ii),sw(ii),dl(ii)

            z11=zw(i) -f*dsdz*dl(i)
            z12=zw(i) +f*dsdz*dl(i)
            z21=zw(ii)-f*dsdz*dl(ii)
            z22=zw(ii)+f*dsdz*dl(ii)
            s11=sw(i) +f*dl(i)
            s12=sw(i) -f*dl(i)
            s21=sw(ii)+f*dl(ii)
            s22=sw(ii)-f*dl(ii)
            do j=1,16
                if (iand(j,1).eq.0) then
                  ss1(j)=s11
                else
                  ss1(j)=s12
                endif
                if (iand(ishft(j,-1),1).eq.0) then
                  ss2(j)=s21
                else
                  ss2(j)=s22
                endif
                if (iand(ishft(j,-2),1).eq.0) then
                  zz1(j)=z11
                else
                  zz1(j)=z12
                endif
                if (iand(ishft(j,-3),1).eq.0) then
                  zz2(j)=z21
                else
                  zz2(j)=z22
                endif
               dsl(j)=((ss1(j)-ss2(j))/(zz1(j)-zz2(j)))
            enddo
            
            mini=0
            rayang=atan(dsdz)
            minads=abs(atan(dsl(1))-rayang)
            Do jj=2,16
               tmp=abs(atan(dsl(jj))-rayang)
               if(tmp.lt.minads) then
                  minads=tmp
                  mini=jj-1
               endif
cDK               write(*,*) jj-1,"tmp=",tmp," dsl(jj)=",dsl(jj)
            enddo
cDK            write(*,*) i,"mini=",mini
            z(i)=zz1(mini+1)
            s(i)=ss1(mini+1)
            z(ii)=zz2(mini+1)
            s(ii)=ss2(mini+1)
            w(i)=worig(i)*f
            w(ii)=worig(ii)*f
         else if(kf(i).gt.0) then
            kf(i)=0
            st=sin(ang(i))
            z1=zw(i)-f*dsdz*dl(i)
            z2=zw(i)+f*dsdz*dl(i)
            s1=sw(i)+f*dl(i)
            s2=sw(i)-f*dl(i)
c            s1cal=(p(1)+p(2)*z1)*ct+p(3)*st
c            s2cal=(p(1)+p(2)*z2)*ct+p(3)*st
            s1cal=(p(1)+p(2)*z1)*ct+(p(3)+p(4)*z1)*st
            s2cal=(p(1)+p(2)*z2)*ct+(p(3)+p(4)*z2)*st
            d1=abs(s1-s1cal)
            d2=abs(s2-s2cal)
            if(d1.lt.d2) then
               z(i)=z1
               s(i)=s1
               w(i)=worig(i)*f
            else 
               z(i)=z2
               s(i)=s2
               w(i)=worig(i)*f
            endif
         endif
      enddo
      
c      write(*,*) "event",gen_event_id_number," n",n
c      write(*,*) " z",z
c      write(*,*) " s",s
c      write(*,*) " w",w

c      Do i=1,n
c         z(i)=zw(i)
c         s(i)=sw(i)
c         w(i)=1./(CellSize*CellSize)
c      enddo

      return
      end
