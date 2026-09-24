      SUBROUTINE H_PHYSICS(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods : Do final HKS physics analysis 
*     -                         on HKS only part of event.
*     -                              
*     -                         to decoded information 
*     -
*     -      Required Input BANKS     HKS_FOCAL_PLANE
*     -                               HKS_TARGET
*     -                               HKS_TRACK_TESTS
*     -
*     -      Output BANKS             HKS_PHYSICS_R4
*     -                               HKS_PHYSICS_I4
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     -   Created 19-JAN-1994   D. F. Geesaman
*     -                           Dummy Shell routine
*     
*     Modified 06-Oct-1999  Jinghua Liu for HNSS
*     
*     Revision 2.6 2009/07/11 yez
*     Add Lucite info for E05-115
*
*     Revision 2.5 2004/03/02 Miyoshi
*     for E01-011
*     
*     Revision 2.4  2000/03/19 22:03:36  jinghua
*     (JLiu) Modified the calculation of ssphi
*     
*     Revision 2.3  2000/03/09 18:39:10  jinghua
*     (JLiu) Added sstime_at_target
*     
*     Revision 2.2  2000/03/03 10:19:23  jinghua
*     (JLiu) Included Yuan's pathlength correction, 
*     added mass square calculation
*     
*     Revision 2.1  2000/02/18 10:21:34  jinghua
*     (JLiu) changed total_eloss to g_total_eloss
*     
*     Revision 2.0  1999/10/06 09:54:15  jinghua
*     (JLiu) HKS has been moved to the right side of the beam line.
*     
*     csa 11/24/98 -- replaced with a version of the new h_physics.f
*     (which contains G. Warren's cleanups).
*     
*     Revision 1.17  1999/02/10 17:46:15  csa
*     Cleanup and bugfixes (mostly G. Warren)
*     
*     Revision 1.16  1996/11/07 19:51:38  saw
*     (JRA) Correct error in mass calculation
*     
*     Revision 1.15  1996/09/05 20:13:14  saw
*     (JRA) Improved track length calculation.  Photon E calc. for (gamma,p)
*     
*     Revision 1.14  1996/04/30 17:13:48  saw
*     (JRA) Add pathlength and rf calculations
*     
*     Revision 1.13  1996/01/24 16:08:14  saw
*     (JRA) Change cpbeam/cebeam to gpbeam/gebeam
*     
*     Revision 1.12  1996/01/17 19:00:50  cdaq
*     (JRA) Calculate q, W for electrons
*     
*     Revision 1.11  1995/10/10 12:54:30  cdaq
*     (JRA) Add call to s_dump_cal, change upper to lower case
*     
*     Revision 1.10  1995/08/31 18:45:26  cdaq
*     (JRA) Add projection to cerenkov mirror pos, fill sdc_sing_res array
*     
*     Revision 1.9  1995/07/20  18:59:15  cdaq
*     (SAW) Declare sind and tand for f2c compatibility
*     
*     Revision 1.8  1995/05/22  19:45:43  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, hks, and coin parts"
*     
*     Revision 1.7  1995/05/11  17:15:15  cdaq
*     (SAW) Add additional kinematics variables
*     
*     Revision 1.6  1995/04/06  19:37:30  cdaq
*     (SAW) Fix typo
*     
*     Revision 1.5  1995/02/23  13:39:13  cdaq
*     (SAW) Moved best track selection code into S_SELECT_BEST_TRACK (new)
*     
*     Revision 1.4  1995/01/18  20:57:12  cdaq
*     (SAW) Correct some trig and check for negative arg 
*     in elastic kin calculation
*     
*     Revision 1.4  1995/01/18  20:00:04  cdaq
*     (SAW) Correct some trig and check for negative arg 
*     in elastic kin calculation
*     
*     Revision 1.3  1994/11/23  13:55:03  cdaq
*     (SPB) Recopied from hms file and modified names for HKS
*     
*     Revision 1.2  1994/06/14  03:41:10  cdaq
*     (DFG) Calculate physics quantities
*     
*     Revision 1.1  1994/02/21  16:15:43  cdaq
*     Initial revision
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*9 here
      parameter (here= 'H_PHYSICS')
*     
      logical ABORT
      character*(*) err
      integer ierr
*     
      Include 'hes_data_structures.cmn'
      include 'gen_data_structures.cmn'
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hks_physics_sing.cmn'
      INCLUDE 'hks_scin_parms.cmn'
      INCLUDE 'hks_tracking.cmn'
      INCLUDE 'hks_geometry.cmn'
      include 'gen_event_info.cmn'
      include 'hks_scin_tof.cmn'
      include 'hks_aero_parms.cmn'
      include 'hks_water_parms.cmn'
      include 'hks_lucite_parms.cmn'
      include 'hks_bypass_switches.cmn'

*     local variables 
      
      integer*4 i,j,k,ip,ihit,hp,co
      integer*4 itrk
      real*4 coshstheta,sinhstheta
      real*4 p_nonzero
      real*4 xdist,ydist,dist(12),res(12)
      real*4 tmp,W2
      real*4 hsp_z, hluc_z
      real*4 Wvec(4)
      real*4 hstheta_1st
      real*4 scalar,mink
      Real*4 chi2,chi2_2
      Real*4 xyz
      real*4 a,b,c,d,e,y,luc_zoff,luc_timeoff
      
      real*4 pionmass,protonmass,positronmass
      Parameter(pionmass=0.13957)
      Parameter(protonmass=0.938272)
      Parameter(positronmass=0.000511)
c--   Path lenght correction variables
      real*4 vf(5)
*
*--------------------------------------------------------
*
      ierr=0
      hphi_lab=0.0
      hnphysics = 0
      
      if(hntracks_fp .le. 0) Return
      
c     Write(*,*) '(phys) hntracks_fp=',hntracks_fp
      
      Do itrk = 1,hntracks_fp
         chi2 = hchi2perdof_fp(itrk)
****************************************
c     put your condition here.
****************************************
         if(
     &        htrk_beta(itrk) .gt. -10000
     &        ) then
            hnphysics = hnphysics + 1
            hp = hnphysics
            if(hp .eq. 1) then
               hschi2perdeg(1) = chi2
               hphys_ntrack(1) = itrk        
            Else if(hp .gt. 1) then
               i=hp-1
               Do while(i .gt. 0)
                  chi2_2 = hschi2perdeg(i)
                  if(chi2 .lt. chi2_2) then
                     if(i .lt. hnphysics_max) then 
                        hphys_ntrack(i+1) = hphys_ntrack(i)          
                        hschi2perdeg(i+1) = hschi2perdeg(i)
                     EndIf
                     if(i .eq. 1) then
                        hphys_ntrack(1) = itrk
                        hschi2perdeg(1) = chi2
                     EndIf
                  Else
                     if(i .lt. hnphysics_max) then
                        hphys_ntrack(i+1) = itrk          
                        hschi2perdeg(i+1) = chi2
                     endif
                     i = 1
                  EndIf
                  i = i - 1
               EndDo               
            EndIf
         EndIf
      EndDo

c     if(hntracks_fp .gt. 0) then
c     Do hp = 1,hntracks_fp
c     Write(*,*) 'ht=',hp,hchi2perdof_fp(hp)
c     EndDo
c     Do hp = 1, hnphysics
c     Write(*,*) 'hp=',hp,hphys_ntrack(hp),hschi2perdeg(hp)
c     EndDo
c     EndIf
cc      print *, 'physics',hnphysics,htrk_beta(1)

c***********OUTPUT to get hit information in KDC************
      if(hdc_hitinfo_flag.eq.1 .and.
     &     hdc_hitinfo_flag2.eq.1)then
         open(22,file='hks_cham.dat',access='append')   
         write(22,*)hnphysics
         close(22)
      endif
c***********************************************************

      Do hp = 1, hnphysics
         itrk = hphys_ntrack(hp)      
         
         hphys_scin_hit(hp,1) = 0
         Do j=1,HNUM_SCIN_LAYERS
            hphys_scin_co(itrk,j) = 0
            if(hscin_on_track(itrk,j) .gt. 0) then
               hphys_scin_hit(hp,1) = hphys_scin_hit(hp,1)+1
               hphys_scin_hit(hp,hphys_scin_hit(hp,1)+1)=
     &              hscin_on_track(itrk,j)
               if(hscin_on_track2(itrk,j) .ne. -1 ) then
                  hphys_scin_co(itrk,j) = 
     &                 (real(hscin_counter_num(hscin_on_track(itrk,j)))+
     &                 real(hscin_counter_num(hscin_on_track2(itrk,j))))
     &                 /2
               else
                  hphys_scin_co(itrk,j) = 
     &                 hscin_counter_num(hscin_on_track(itrk,j))
               endif
            EndIf
         EndDo
         
         hsdelta(hp)      = hdelta_tar(itrk)
         hsx_tar(hp)      = hx_tar(itrk)
         hsy_tar(hp)      = hy_tar(itrk)
         hsxp_tar(hp)     = hxp_tar(itrk)
         hsyp_tar(hp)     = hyp_tar(itrk)
         hsx_fp(hp)       = hx_fp(itrk)
         hsy_fp(hp)       = hy_fp(itrk)
         hsxp_fp(hp)      = hxp_fp(itrk)
         hsyp_fp(hp)      = hyp_fp(itrk)
         hsbeta(hp)       = htrk_beta(itrk)
         hsbeta1y(hp)     = htrk_beta1y(itrk)
         hstof(hp)        = htrk_tof(itrk)
         hsdedx(hp,1)     = hscin_trk_depo(itrk,1)
         hsdedx(hp,2)     = hscin_trk_depo(itrk,2)
         hsdedx(hp,3)     = hscin_trk_depo(itrk,3)
         hstime_at_fp(hp) = htrk_time_atfp(itrk)
         hsnco1(hp)       = hphys_scin_co(itrk,1)
         hsnco2(hp)       = hphys_scin_co(itrk,2)
         hsnco3(hp)       = hphys_scin_co(itrk,3)
         hsnt1(hp)        = hscin_trk_time(itrk,1)
         hsnt2(hp)        = hscin_trk_time(itrk,2)
         hsnt3(hp)        = hscin_trk_time(itrk,3)
         hsnt1m(hp)       = hscin_trk_time2(itrk,1)
         hsnt2m(hp)       = hscin_trk_time2(itrk,2)
         hsnt3m(hp)       = hscin_trk_time2(itrk,3)
         hsna1(hp)        = hscin_trk_depo(itrk,1)
         hsna2(hp)        = hscin_trk_depo(itrk,2)
         hsna3(hp)        = hscin_trk_depo(itrk,3)
         hsna1m(hp)       = hscin_trk_depo2(itrk,1)
         hsna2m(hp)       = hscin_trk_depo2(itrk,2)
         hsna3m(hp)       = hscin_trk_depo2(itrk,3)
c***********OUTPUT to get hit information in KDC************
         if(hdc_hitinfo_flag.eq.1 .and.
     &        hdc_hitinfo_flag2.eq.1)then
            open(22,file='hks_cham.dat',access='append')   
            write(22,*)hsx_fp(hp),hsy_fp(hp)
     &           ,hsxp_fp(hp),hsyp_fp(hp)
            close(22)
         endif
c*************************************************
         
*     hsp - momentum : hsenergy - kaon energy
         hsp(hp)     = hp_tar(itrk) ! GeV
         hsenergy(hp)= sqrt(hsp(hp)*hsp(hp)+hpartmass*hpartmass) 
         if(hsp(hp) .lt. 0.11) then
            hse_pi(hp)  = sqrt(hpcentral*hpcentral+mass_pion*mass_pion)             
            hse_pr(hp)  = sqrt(hpcentral*hpcentral+mass_proton*mass_proton)    
         Else
            hse_pi(hp)  = sqrt(hsp(hp)*hsp(hp)+mass_pion*mass_pion)             
            hse_pr(hp)  = sqrt(hsp(hp)*hsp(hp)+mass_proton*mass_proton)         
         EndIF

         hsx_dc1(hp) = hsx_fp(hp) +  hsxp_fp(hp) * hdc_1_zpos
         hsy_dc1(hp) = hsy_fp(hp) +  hsyp_fp(hp) * hdc_1_zpos
         hsx_dc2(hp) = hsx_fp(hp) +  hsxp_fp(hp) * hdc_2_zpos
         hsy_dc2(hp) = hsy_fp(hp) +  hsyp_fp(hp) * hdc_2_zpos
         hsx_s1x(hp)  = hsx_fp(hp) +  hsxp_fp(hp) * hscin_zpos(1)
         hsy_s1x(hp)  = hsy_fp(hp) +  hsyp_fp(hp) * hscin_zpos(1)
         hsx_s1y(hp)  = hsx_fp(hp) +  hsxp_fp(hp) * hscin_zpos(2)
         hsy_s1y(hp)  = hsy_fp(hp) +  hsyp_fp(hp) * hscin_zpos(2)
         hsx_s2x(hp)  = hsx_fp(hp) +  hsxp_fp(hp) * hscin_zpos(3)
         hsy_s2x(hp)  = hsy_fp(hp) +  hsyp_fp(hp) * hscin_zpos(3)
         
*     --- fill Cherenkov infomation.
         Do j=1,HNUM_AER_LAYERS
            hsx_aer(hp,j) = hsx_fp(hp) 
     &           + hsxp_fp(hp) * haer_box_zpos(j)

            hsy_aer(hp,j) = hsy_fp(hp) 
     &           + hsyp_fp(hp) * haer_box_zpos(j)
            hsaer_npe(hp,j) = htrk_aer_npe_sum(itrk,j)
            hsaer_npe2(hp,j) = htrk_aer_npe_sum2(itrk,j)
            hsaer_time(hp,j) = htrk_aer_time(itrk,j)
            hsaer_time2(hp,j) = htrk_aer_time2(itrk,j)
            hsaer_npe_pos(hp,j) = htrk_aer_npe_pos(itrk,j)
            hsaer_npe_pos2(hp,j) = htrk_aer_npe_pos2(itrk,j)
            hsaer_time_pos(hp,j) = htrk_aer_time_pos(itrk,j)
            hsaer_time_pos2(hp,j) = htrk_aer_time_pos2(itrk,j)
            hsaer_npe_neg(hp,j) = htrk_aer_npe_neg(itrk,j)
            hsaer_npe_neg2(hp,j) = htrk_aer_npe_neg2(itrk,j)
            hsaer_time_neg(hp,j) = htrk_aer_time_neg(itrk,j)
            hsaer_time_neg2(hp,j) = htrk_aer_time_neg2(itrk,j)
            hsaer_num(hp,j) = htrk_aer_num(itrk,j)
         EndDo
         Do j=1,HNUM_WAT_LAYERS
            hsx_wat(hp,j) = hsx_fp(hp) 
     &           + hsxp_fp(hp) * hwat_box_zpos(j)
            hsy_wat(hp,j) = hsy_fp(hp) 
     &           + hsyp_fp(hp) * hwat_box_zpos(j)
            hswat_npe(hp,j) = htrk_wat_npe_sum(itrk,j)
            hswat_npe2(hp,j) = htrk_wat_npe_sum2(itrk,j)
            hswat_npe_k_ratio(hp,j) = htrk_wat_npe_sum_k_ratio(itrk,j)
            hswat_npe_k_ratio2(hp,j) = htrk_wat_npe_sum_k_ratio2(itrk,j)
            hswat_time(hp,j) = htrk_wat_time(itrk,j)
            hswat_time2(hp,j) = htrk_wat_time2(itrk,j)
            hswat_npe_pos(hp,j) = htrk_wat_npe_pos(itrk,j)
            hswat_npe_pos2(hp,j) = htrk_wat_npe_pos2(itrk,j)
            hswat_time_pos(hp,j) = htrk_wat_time_pos(itrk,j)
            hswat_time_pos2(hp,j) = htrk_wat_time_pos2(itrk,j)
            hswat_npe_neg(hp,j) = htrk_wat_npe_neg(itrk,j)
            hswat_npe_neg2(hp,j) = htrk_wat_npe_neg2(itrk,j)
            hswat_time_neg(hp,j) = htrk_wat_time_neg(itrk,j)
            hswat_time_neg2(hp,j) = htrk_wat_time_neg2(itrk,j)
            hswat_num(hp,j) = htrk_wat_num(itrk,j)
         EndDo
           
         Do j=1,HNUM_LUC_LAYERS
            hsx_luc(hp,j) = hsx_fp(hp) 
     &           + hsxp_fp(hp) * hluc_box_zpos(j)
            hsy_luc(hp,j) = hsy_fp(hp) 
     &           + hsyp_fp(hp) * hluc_box_zpos(j)
            hsluc_npe(hp,j) = htrk_luc_npe_sum(itrk,j)
            hsluc_npe2(hp,j) = htrk_luc_npe_sum2(itrk,j)
            hsluc_time(hp,j) = htrk_luc_time(itrk,j)
            hsluc_time2(hp,j) = htrk_luc_time2(itrk,j)
            hsluc_num(hp,j) = htrk_luc_num(itrk,j)

cc =======================================================================
cc          Correct the curve shape of Lucite #1,#2,#16 from SANE.
cc          The distribution of those lucite bars along (x,y) are:
cc          z=z0+e-{a+b*(d-y)+c*(d-y)**2},where a,b fitted by the curve 
cc          of Lucites,c is the max length along positive y axis,respect
cc          to the y0 axis center definition of lucite plane, 
cc          and d is the max length of lucite surface from the whole
cc          lucite plane z0.            --Z. Ye 08/15/2009

cc          1&2 are fixed together

            co=hluc_counter_num(hp)
            if(co.eq.1.or.co.eq.2) then
               a =-0.0859
               b = 0.1996
               c =-0.002077
               d = 45.81 !cm
               e = 6.70  !cm
               
               luc_zoff=e-(a+b*(d-hsy_luc(hp,j))+c*(d-hsy_luc(hp,j))**2)
               hluc_z=hluc_box_zpos(j)+luc_zoff
               
               hsx_luc(hp,j) = hsx_fp(hp) 
     &              + hsxp_fp(hp) * hluc_z
               hsy_luc(hp,j) = hsy_fp(hp) 
     &              + hsyp_fp(hp) * hluc_z
ccc         Do we need to correct time info also?
            luc_timeoff = luc_zoff/speed_of_light/max(.001,hsbeta_k(hp)) ! use hpartmass
            hsluc_time(hp,j) = htrk_luc_time(itrk,j)+luc_timeoff
            hsluc_time2(hp,j) = htrk_luc_time2(itrk,j)+luc_timeoff

            elseif(co.eq.16) then
               a =-0.0859
               b = 0.1996
               c =-0.002077
               d = 48.05    !cm
               e = 8.10     !cm
               y = hsy_luc(hp,j)

               luc_zoff=e-(a+b*(d-y)+c*(d-y)**2)
               hluc_z=hluc_box_zpos(j)+luc_zoff
               
               hsx_luc(hp,j) = hsx_fp(hp) 
     &              + hsxp_fp(hp) * hluc_z
               hsy_luc(hp,j) = hsy_fp(hp) 
     &              + hsyp_fp(hp) * hluc_z

ccc         Do we need to correct time info also?
            luc_timeoff = luc_zoff/speed_of_light/max(.001,hsbeta_k(hp)) ! use hpartmass
            hsluc_time(hp,j) = htrk_luc_time(itrk,j)+luc_timeoff
            hsluc_time2(hp,j) = htrk_luc_time2(itrk,j)+luc_timeoff
            endif
cc ===================End of correction========================================

            
         EndDo

         hsbeta_k(hp) = hsp(hp)/max(hsenergy(hp),.00001)
         hsbeta_pi(hp) = hsp(hp)/max(hse_pi(hp),.00001)
         hsbeta_pr(hp) = hsp(hp)/max(hse_pr(hp),.00001)
         
         hspathlength(hp) = htrk_pathlength(itrk)

cc         print *, 'PATHL',hspathlength(hp)
         hstime_at_tar(hp) = hstime_at_fp(hp)
     &        - hspathlength(hp)
     &        /speed_of_light/max(.001,hsbeta_k(hp)) ! use hpartmass


c     Write(*,*) '(hphys)htt,betak,',
c     &        hstime_at_tar(hp),hsbeta_k(hp)
c--

ccc hsscin_elem_hit is no used anywhere, then I commented out DK
c         do i = 1, hnum_scin_layers
c            if ( hscin_on_track(itrk,i).gt.0 ) then
c               hsscin_elem_hit(hp,i) = 
c     &              hscin_counter_num(hscin_on_track(itrk,i))
c            else                ! more than 1 hit in layer
*     --- it was 18. I(Miyoshi) increased the number.
c               hsscin_elem_hit(hp,ip) = 20
c            endif
c         enddo
         
         hschi2perdeg(hp)   = hchi2_fp(itrk) / float(hnfree_fp(itrk))
         hsnfree_fp(hp)     = hnfree_fp(itrk)
         

         
         do ip = 1, hdc_num_layers
            hsdc_sing_res(hp,ip)    = hdc_single_residual(itrk,ip)
            hsdc_track_coord(hp,ip) = hdc_track_coord(itrk,ip)
            hsdc_wire_coord(hp,ip)  = hdc_layer_wirecoord(itrk,ip)
            hsdc_wire_center(hp,ip) = hdc_layer_wirecenter(itrk,ip)
            hsdc_wire_numb(hp,ip)   = hdc_wire_numb(itrk,ip)
            hsdc_layer_sigma(hp,ip) = hdc_layer_sigma(itrk,ip)
            hsdc_drift_distance(hp,ip)  = hdc_drift_distance(itrk,ip)
            hsdc_layer_drift_time(hp,ip)= hdc_layer_drift_time(itrk,ip)
c            Write(*,*) 'DRIFT DISTANCE =',hsdc_drift_distance(hp,ip)
c            write(22,*)"wire",hsdc_wire_numb(hp,ip)
         enddo
c***********OUTPUT to get hit information in KDC************
         if(hdc_hitinfo_flag.eq.1 .and.
     &        hdc_hitinfo_flag2.eq.1)then
            open(22,file='hks_cham.dat',access='append')
            write(22,*)hdc_wire_numb(itrk,1),hdc_wire_numb(itrk,2),
     &           hdc_wire_numb(itrk,3),hdc_wire_numb(itrk,4),
     &           hdc_wire_numb(itrk,5),hdc_wire_numb(itrk,6),
     &           hdc_wire_numb(itrk,7),hdc_wire_numb(itrk,8),
     &           hdc_wire_numb(itrk,9),hdc_wire_numb(itrk,10),
     &           hdc_wire_numb(itrk,11),hdc_wire_numb(itrk,12)
            write(22,*)hdc_track_coord(itrk,1),hdc_track_coord(itrk,2),
     &           hdc_track_coord(itrk,3),hdc_track_coord(itrk,4),
     &           hdc_track_coord(itrk,5),hdc_track_coord(itrk,6),
     &           hdc_track_coord(itrk,7),hdc_track_coord(itrk,8),
     &           hdc_track_coord(itrk,9),hdc_track_coord(itrk,10),
     &           hdc_track_coord(itrk,11),hdc_track_coord(itrk,12)
            write(22,*)hdc_single_residual(itrk,1),
     &           hdc_single_residual(itrk,2),
     &           hdc_single_residual(itrk,3),
     &           hdc_single_residual(itrk,4),
     &           hdc_single_residual(itrk,5),
     &           hdc_single_residual(itrk,6),
     &           hdc_single_residual(itrk,7),
     &           hdc_single_residual(itrk,8),
     &           hdc_single_residual(itrk,9),
     &           hdc_single_residual(itrk,10),
     &           hdc_single_residual(itrk,11),
     &           hdc_single_residual(itrk,12)
            write(22,*)hschi2perdeg(hp)
            write(22,*)hsnco1(hp),hsnco2(hp),hsnco3(hp)
            write(22,*)hsaer_num(hp,1),hsaer_num(hp,2),hsaer_num(hp,3)
            write(22,*)hswat_num(hp,1),hswat_num(hp,2)
            write(22,*)hsp(hp)
c            write(22,*)"data"
            close(22)
         endif
c*************************************************
         hsdc_track_numb(hp)  = itrk!added for test
         
c     if (hntrack_hits(itrk,1).eq.12 .and. hschi2perdeg.le.4) then
c     xdist = hsx_dc1
c     ydist = hsy_dc1
c     do ip = 1,12
c     if (hdc_readout_x(ip)) then
c     dist(ip) = ydist*hdc_readout_corr(ip)
c     else          !readout from top/bottom
c     dist(ip) = xdist*hdc_readout_corr(ip)
c     endif
c     res(ip) = hdc_sing_res(ip)
c     tmp = hdc_layer_wirecoord(itrk,ip)
c     $                 - hdc_layer_wirecenter(itrk,ip)
c     if (tmp.eq.0) then !drift dist = 0
c     res(ip) = abs(res(ip))
c     else
c     res(ip) = res(ip) * (abs(tmp)/tmp) !convert +/- res to near/far res
c     endif
c     enddo
c     write(37,'(12f7.2,12f8.3,12f8.5)') (hsdc_track_coord(ip),ip=1,12),
c     &           (dist(ip),ip=1,12),(res(ip),ip=1,12)
c     endif
            
*     Do energy loss, which is particle specific
            
         hstheta_1st = htheta_lab*TT/180. + atan(hsyp_tar(hp)) ! rough scat angle
         
         if (hpartmass .lt. 2.*mass_electron) then ! for electron
            if (gtarg_z(gtarg_num).gt.0.) then
               call g_total_eloss(2,.true.,gtarg_z(gtarg_num),
     $              gtarg_a(gtarg_num),
     $              gtarg_thick(gtarg_num),gtarg_dens(gtarg_num),
     $              hstheta_1st,gtarg_theta,1.0,hseloss(hp))
            else
               hseloss(hp)=0.
            endif
         else                   ! not an electron
            if (gtarg_z(gtarg_num).gt.0.) then
               call g_total_eloss(2,.false.,gtarg_z(gtarg_num),
     $              gtarg_a(gtarg_num),
     $              gtarg_thick(gtarg_num),gtarg_dens(gtarg_num),
     $              hstheta_1st,gtarg_theta,hsbeta,hseloss(hp))
            else
               hseloss(hp)=0.
            endif
         endif                  ! particle specific stuff
         
*     Correct hsenergy and hsp for eloss at the target
         
         hscorre(hp) = hsenergy(hp) + hseloss(hp)
         hscorrp(hp) = sqrt(hscorre(hp)**2-hpartmass**2)
         
*     Angles for Scattered particle. Theta and phi are conventional
*     polar/azimuthal angles defined w.r.t. coordinate system defined
*     above. In rad, of course. Note that phi is around -pi/2 for HMS,
*     +pi/2 for HKS.

         xyz = 1. + hsxp_tar(hp)**2 + hsyp_tar(hp)**2

c     --- analysis coordinate system
c     --- theta>0 with hsxp_tar<0
c     --- we accept only positive.
         if (sqrt(xyz).ge.1.) then
            if(hsxp_tar(hp) .lt. 0) then
               hstheta(hp) = acos(1/sqrt(xyz))
            Else
               hstheta(hp) = 10.
            EndIf
         else
            hstheta(hp) = 10.
         endif
         
         
*     Begin Kinematic stuff
         
*     coordinate system :
*     z points downstream along beam
*     x points downward 
*     y points toward beam left (away from HMS)
*     
*     This coordinate system is a just a simple rotation away from the
*     TRANSPORT coordinate system used in the spectrometers
         
         hsp_z = hsp(hp)/sqrt(xyz)
         
*     Initial Electron
         
         hs_kvec(hp,1) = gebeam ! after energy loss in target
         hs_kvec(hp,2) = 0
         hs_kvec(hp,3) = 0
         hs_kvec(hp,4) = gebeam 
         
*     Scattered Electron (not meaningful if hadron is in HKS!)
*     calculation without small angle approximation - gaw 98/10/5 csa
*     12/21/98 -- notice assumption of no out-of-layer offset
         
         hs_kpvec(hp,1) =  hscorre(hp)
         hs_kpvec(hp,2) =  hsp_z*hsxp_tar(hp)
C***  JLiu modified for HNSS
C***  changed the signs of following two terms containing "sinsthetas"
         hs_kpvec(hp,3) =  hsp_z*hsyp_tar(hp)
         hs_kpvec(hp,4) =  hsp_z
 
         hsphi(hp) =atan2(hs_kpvec(hp,3), hs_kpvec(hp,2))      
         
         sinhstheta = sin(hstheta(hp))
         coshstheta = cos(hstheta(hp))
         
         hsphi(hp) = hphi_lab + hsphi(hp)
         
         if (hsphi(hp) .lt. 0.) hsphi(hp) = hsphi(hp) + 2*tt
         
*     hszbeam is the intersection of the beam ray with the
*     spectrometer as measured along the z axis.
         
         if( sinhstheta .eq. 0.) then
            hszbeam(hp) = 0.
         else
            hszbeam(hp) = sin(hsphi(hp)) 
     $           * ( -hsy_tar(hp) + gbeam_y * coshstheta) /
     $           sinhstheta  
        endif                  ! end test on sinsstheta=0
         
C***  JLiu Added to calculate the mass square of the particle in HKS
C***  The mass square can be negtive if hsbeta>1.
         if(hsbeta(hp).gt.0.) then
            hsmass2(hp) = hsp(hp)*hsp(hp)
     &           *(1./hsbeta(hp)/hsbeta(hp)-1.)
         else
            hsmass2(hp) = 40000. ! just a random big number (in GeV)
         endif
         
*     Target particle 4-momentum
         
         hs_tvec(hp,1) = gtarg_mass(gtarg_num)*m_amu
         hs_tvec(hp,2) = 0.
         hs_tvec(hp,3) = 0.
         hs_tvec(hp,4) = 0.
         
*     Initialize the electron-specific variables
         
         do i=1,4
            hs_qvec(hp,i) = -1000.
            Wvec(i)    = -1000.
         enddo 
         
         hsq3     = -1000.
         hsbigq2  = -1000.
         W2       = -1000.
         sinvmass = -1000.
         
*     Calculate quantities that are meaningful only if 
*     the particle in the HKS is an electron.
         
c     if (hpartmass .lt. 2.*mass_electron) then
c     do i=1,4
c     hs_qvec(hp,i) = hs_kvec(hp,i) - hs_kpvec(hp,i)
c     Wvec(hp,i)    = hs_qvec(hp,i) + hs_tvec(hp,i) ! Q+P 4 vector
c     enddo 
*     Magnitudes
c     hsq3    = sqrt(scalar(hs_qvec,hs_qvec))
c     hsbigq2 = -mink(hs_qvec,hs_qvec) 
c     W2      = mink(Wvec,Wvec)
c     if(W2.ge.0 ) then
c     sinvmass = SQRT(W2)
c     else
c     sinvmass = 0.
c     endif
c     endif               ! positron
         
         if (.false.) then
*     if (.true.) then
            write(6,*)' ****hnphycics = ',hp,'********'
            write(6,*)' h_phys: htheta_lab, hphi_lab =',
     &           htheta_lab,hphi_lab
            write(6,*)' h_phys: hsdelta            =',hsdelta(hp)
            write(6,*)' h_phys: hsx_tar, hsy_tar   =',
     &           hsx_tar(hp),hsy_tar(hp)
            write(6,*)' h_phys: hsxp_tar, hsyp_tar =',
     &           hsxp_tar(hp),hsyp_tar(hp)
            write(6,*)' h_phys: hsbeta, hsbeta_p   =',
     &           hsbeta(hp),hsbeta_k(hp)
            write(6,*)' h_phys: hsenergy, hsp      =',
     &           hsenergy(hp),hsp(hp)
            write(6,*)' h_phys: hseloss            =',hseloss(hp)
            write(6,*)' h_phys: hscorre, hscorrp   =',
     &           hscorre(hp),hscorrp(hp)
            write(6,*)' h_phys: hstheta_1st        =',hstheta_1st
            write(6,*)' h_phys: hsp_z              =',hsp_z
            write(6,*)' h_phys: hs_kvec            =',
     &           (hs_kvec(hp,j),j=-1,4)
            write(6,*)' h_phys: cos/sinsthetas     =',
     &           coshthetas,sinhthetas
            write(6,*)' h_phys: hs_kpvec           =',
     &           (hs_kpvec(hp,j),j=1,4)
            write(6,*)' h_phys: hs_tvec            =',
     &           (hs_tvec(hp,j),j=1,4)
            write(6,*)' h_phys: hs_qvec            =',
     &           (hs_qvec(hp,j),j=1,4)
c     write(6,*)' h_phys: Wvec               =',Wvec
c     write(6,*)' h_phys: hsq3               =',hsq3
c     write(6,*)' h_phys: hsbigq2, W2        =',hsbigq2,W2
            write(6,*)' h_phys: hstheta, hsphi     =',
     &           hstheta(hp),hsphi(hp)
         endif

c         if(hsp(hp).ge.1.0 .and. hsp(hp).lt.1.1)then ! Mom Region1 (test:20/Oct/2011, T.Gogami)
c            write(*,*) hsp(hp)
!         if(hsp(hp).ge.1.1 .and. hsp(hp).lt.1.2)then ! Mom Region2 (test:20/Oct/2011, T.Gogami)
!         if(hsp(hp).ge.1.2 .and. hsp(hp).lt.1.3)then ! Mom Region3 (test:20/Oct/2011, T.Gogami)
!         if(hsp(hp).ge.1.3 .and. hsp(hp).lt.1.4)then ! Mom Region3 (test:20/Oct/2011, T.Gogami)
!         if(hsp(hp).ge.1.2 .and. hsp(hp).lt.1.4)then ! Mom Region3 (test:20/Oct/2011, T.Gogami)
!         if(hsp(hp).ge.1.0 .and. hsp(hp).lt.1.2)then ! Mom Region3 (test:20/Oct/2011, T.Gogami)
c         if( hsx_fp(hp)-(hsxp_fp(hp)*50.0)
c     &        .le. -10.0)then             ! test
c     &        .ge. -10.0)then             ! test
         call h_dc_eff_count(abort,err,hp) ! KDC efficiency (T.Gogami,3Mar2011)
c         write(*,*) "TEST__"
c         endif
!         endif
      EndDo                     ! hnphysics loop

c***********OUTPUT to get hit information in KDC************
      if(hdc_hitinfo_flag.eq.1 
     &     .and. hdc_hitinfo_flag2.eq.1
     &     .and. hnphysics.gt.0)then
         open(22,file='hks_cham.dat',access='append')   
         write(22,*)"data"
         close(22)
      endif
c**********************************************************

*     --- Histogram for phys bank ---
      call h_fill_phys_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif
      
      
*     Calculate physics statistics and wire chamber efficencies.
      
      call h_physics_stat(ABORT,err)
      ABORT= ierr.ne.0 .or. ABORT
      IF(ABORT) THEN
         call G_add_path(here,err)
      ENDIF
      
      return
      end
      
***************************************************
      
      real*4 function scalar(vec1,vec2)
      
*     scalar product of vec1 and vec2
      
      real*4 vec1(4)
      real*4 vec2(4)
      
      scalar = 0
      
      do i=2,4
         scalar=vec1(i)*vec2(i)+scalar
      enddo

      return
      end
      
***************************************************
      
      real*4 function mink(vec1,vec2)
      
*     Minkowski product
      
      implicit none
      
      real*4 vec1(4),vec2(4)
      real scalar
      
      mink=vec1(1)*vec2(1)-scalar(vec1,vec2)
      return
      end
      
