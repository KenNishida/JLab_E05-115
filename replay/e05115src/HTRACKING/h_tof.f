      SUBROUTINE H_TOF(ABORT,errmsg)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods : Analyze HKS scintillator info. for each track 
*     -
*     -      Required Input BANKS     HKS_RAW_SCIN
*     -                               HKS_DECODED_SCIN
*     -                               HKS_FOCAL_LAYER
*     -
*     -      Output BANKS             HKS_TRACK_TESTS
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     -   Created 22-FEB-1994   John Arrington
*     
*     $Log: h_tof.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.5  2005/08/23 17:32:09  cdaq
*     Not use 1Y to determine time at fp.
*
*     Revision 1.4  2005/07/09 00:37:37  yuan
*     Add variable hsnum_pmt_hit to dump TOF info for calibration use
*
*     Revision 1.6  2005/04/19 17:50:45  miyoshi
*     add new hist
*
*     Revision 1.5  2005/04/08 20:55:02  miyoshi
*     add histogram
*
*     Revision 1.4  2005/01/11 00:44:10  sumihama
*     Mod HKS-TOF
*
*     Revision 1.3  2005/01/07 23:38:13  sumihama
*     Mod. htof
*
*     Revision 1.2  2004/12/24 21:35:57  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
*     new dir
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*50 here
      parameter (here= 'H_TOF')
*     
      logical ABORT
      character*(*) errmsg
*     
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hks_scin_parms.cmn'
      include 'hks_scin_tof.cmn'
      include 'hks_tracking.cmn'
      include 'hks_id_histid.cmn'

      integer*4 hit, trk, good, num
      integer*4 i,pl,co,j,k
      real*4 xhit_coord,yhit_coord
      real*4 p,betap, temp,zcor  
      real*4 longmin,longmax,xyz
      real*4 scin_fp_time, beta_tmp(2)        ! L.Y.   
      integer*4 good_beta(2)     ! TOF beta within range
c--   Path correction variables   L.Y.  
      real*4 corpos, corneg, path
      real*4 hscin_2y_right,hscin_2y_left
      save
*     
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      errmsg = ' '
      
      if(hntracks_fp.le.0) return

c     Temperatly give 2Y left and right postion -- Z.Ye 09/10/2009
   
       hscin_2y_right = -70.0
       hscin_2y_left = 70.0

      
*     *    MAIN LOOP:  Loop over all tracks. 
      do trk = 1 , hntracks_fp
*     --- Initialize 
         p = hp_tar(trk)
c     --- targ_trans is not good, p=0.1.
c     --- thus, if p<0.11 then here we use p=hpcentral
         if(p .lt. 0.11) then
            p = hpcentral
         EndIF
         betap = p/sqrt(p*p+hpartmass*hpartmass)

         htrk_tof(trk) = -10000
         htrk_beta(trk) = -10000
         htrk_beta1y(trk) = -10000
         hsnum_scin_hit(trk) = 0
         hsnum_pmt_hit(trk) = 0
c--
         beta_tmp(1) = -10000
         beta_tmp(2) = -10000
         good_beta(1) = 0
         good_beta(2) = 0

         xyz = hxp_fp(trk)*hxp_fp(trk) + 1
         xyz = xyz + hyp_fp(trk) * hyp_fp(trk)
         xyz = sqrt(xyz)
         
         do pl = 1 , hnum_scin_layers
            hscin_on_track(trk,pl) = -1
            hscin_on_track2(trk,pl) = -1
            hscin_trk_good(trk,pl) = -100
            hscin_trk_depo(trk,pl) = -10000
            hscin_trk_time(trk,pl) = -10000
            hscin_trk_depo2(trk,pl) = -10000
            hscin_trk_time2(trk,pl) = -10000
            hscin_trk_diff(trk,pl) = -10000
            hscin_trk_fptime(trk,pl) = -10000
         enddo
         
         if(hscin_tot_hits.le.0) goto 100

         do hit = 1 , hscin_tot_hits
            pl = hscin_layer_num(hit)
            co = hscin_counter_num(hit)
*     *    Find hit position
            xhit_coord = hx_fp(trk) + hxp_fp(trk)*hscin_zpos(pl)
            yhit_coord = hy_fp(trk) + hyp_fp(trk)*hscin_zpos(pl)
            if (pl.eq.1 .or. pl.eq.3) then !x layer
               hscin_trans_coord(hit) = xhit_coord
               hscin_long_coord(hit)  = yhit_coord
               if(pl .eq. 1) then
                  longmin = hscin_1x_bot - hscin_slope(pl)*2.0
                  longmax = hscin_1x_top + hscin_slope(pl)*2.0
               Else if(pl .eq. 3) then
                  longmin = hscin_2x_bot - hscin_slope(pl)*2.0
                  longmax = hscin_2x_top + hscin_slope(pl)*2.0
               EndIf
            else if (pl.eq.2) then !y layer
               hscin_trans_coord(hit) = yhit_coord
               hscin_long_coord(hit)  = xhit_coord
               longmin = hscin_1y_right - hscin_slope(pl)*2.0
               longmax = hscin_1y_left + hscin_slope(pl)*2.0
            else if (pl.eq.4) then !y layer
               hscin_trans_coord(hit) = yhit_coord
               hscin_long_coord(hit)  = xhit_coord
               longmin = hscin_2y_right - hscin_slope(pl)*2.0
               longmax = hscin_2y_left + hscin_slope(pl)*2.0
            else                !bad layer #.
               abort = .true.
               write(errmsg,*) '(h_tof) bad layer num (',hit,') = ',pl
               call g_prepend(here,errmsg)
               return
            endif
            
*     --- Check if the track is in the scintillator area
cc           print *, 'track test',hscin_center(pl,co), hscin_trans_coord(hit),
cc     &           hscin_long_coord(hit), hscin_width(pl)/2., pl, co
c--
            if (abs(hscin_center(pl,co)-hscin_trans_coord(hit)) .lt.
     &           (hscin_width(pl)/2.+hscin_slope(pl)) .and.
     &           (hscin_long_coord(hit) .ge. longmin .and.
     &           hscin_long_coord(hit) .le. longmax) ) then

cc               Write(*,*) pl,hscin_trans_coord(hit),hscin_center(pl,co)
c--   calculate hsnum_scin_hit and hsnum_pmt_hit  number of hits on track
c--   L.Y.  05/24/2005
               hgood_tdc_pos(trk,hit) = hgood_pos(hit)
               hgood_tdc_neg(trk,hit) = hgood_neg(hit)
               if ( hgood_pos(hit).or.hgood_neg(hit) ) then
                  hsnum_scin_hit(trk) = hsnum_scin_hit(trk)+1
                  hscin_hit(trk,hsnum_scin_hit(trk)) = hit
               endif   
               if ( hgood_pos(hit).and.hgood_neg(hit) ) then
                  good = 2
                  hsnum_pmt_hit(trk) = hsnum_pmt_hit(trk)+2
               elseif( hgood_pos(hit) ) then
                  good = 1
                  hsnum_pmt_hit(trk) = hsnum_pmt_hit(trk)+1
               elseif( hgood_neg(hit) ) then
                  hsnum_pmt_hit(trk) = hsnum_pmt_hit(trk)+1
                  good = -1
               endif               
c--   correction for propogation of light thru scintillator. Pulse height
c--   and time offsets already done in h_trans_scin.f. 
c--   Add by L.Yuan  05/23/2003


               zcor =  hscin_zpos(pl)*xyz/(speed_of_light*betap)

               if (hgood_pos(hit)) then
                  path = hscin_pos_coord(hit) - hscin_long_coord(hit)
                  corpos = hscin_pos_time(hit) - path/hscin_vel_light(pl,co)
                  hscin_time(hit) = corpos
cc ============  Add output data fort.37 for TOF calibration ===Z.Ye 09/07/2009
  
cc                  write(*,'(1x,''1'',2i3,5f10.3)') 
cc     >                 pl,co,
cc     >                 hscin_tdc_pos(hit) * hscin_tdc_to_time,
cc     >                 path,zcor,
cc     >                 hscin_pos_time(hit)-zcor, hscin_adc_pos(hit)
               endif
               if (hgood_neg(hit)) then
                  path = hscin_long_coord(hit) - hscin_neg_coord(hit)  
                  corneg = hscin_neg_time(hit) - path/hscin_vel_light(pl,co)
                  hscin_time(hit) = corneg
cc ============  Add output data fort.37 for TOF calibration ===Z.Ye 09/07/2009
                  
cc                  write(37,'(1x,''2'',2i3,5f10.3)') 
cc     >                 pl,co,
cc     >                 hscin_tdc_neg(hit) * hscin_tdc_to_time,
cc     >                 path,zcor,
cc     >                 hscin_neg_time(hit)-zcor, hscin_adc_neg(hit)
               endif
               if ( hgood_pos(hit).and.hgood_neg(hit) ) then
                  hscin_time(hit) = 0.5*(corneg+corpos)
               endif
c--   Clean up noise hits in multihit TDCs by FP time gate
c--   L.Yuan   10/02/2009
               scin_fp_time = hscin_time(hit)-zcor
               if (abs(scin_fp_time-hstart_time_center).gt.hstart_time_slop) then
                  good = 0
               endif
c--   debug
cc               print *,'hit ', trk, hsnum_scin_hit(trk),  
cc     &              hscin_hit(trk,hsnum_scin_hit(trk)), hsnum_pmt_hit(trk)
c--   end of path correction part
c     --- not on track (first time) 
c     --- or we had good one but previous one has only one hit
               
               if( (hscin_on_track(trk,pl).lt.0.and.abs(good).ge.1) .or.
     &              (good.eq.2.and.abs(hscin_trk_good(trk,pl)).eq.1) 
     &              ) then
                  hscin_on_track(trk,pl) = hit
                  hscin_trk_good(trk,pl) = good
                  hscin_trk_depo(trk,pl) = 
     &                 sqrt( max(hscin_adc_pos(hit),1.0)*
     &                 max(hscin_adc_neg(hit),1.0))
                  hscin_trk_time(trk,pl) = hscin_time(hit)
 
                  hscin_trk_diff(trk,pl) = 
     &                 hscin_pos_time(hit) - hscin_neg_time(hit)
                  hscin_trk_fptime(trk,pl) = hscin_time(hit)
     &                 - hscin_zpos(pl) * xyz 
     &                 /(speed_of_light*betap)
cc       Write(*,*) '(tof1)',hscin_time(hit),
cc     &                 hscin_zpos(pl),xyz,betap

               elseif( abs(good).eq.1.and.
     &                 hscin_trk_good(trk,pl).eq.2) then
c     --- current one is not better.
               elseif((abs(good).eq.1.and.
     &                 abs(hscin_trk_good(trk,pl)).eq.1).or.
     &                 (abs(good).eq.2.and.
     &                 hscin_trk_good(trk,pl).eq.2)) then
                  
                  hscin_on_track2(trk,pl) = hit
                  hscin_trk_good(trk,pl) = good + 100
                  temp = hscin_trk_depo(trk,pl)
                  hscin_trk_depo(trk,pl) = sqrt(temp*
     &                 sqrt( max(hscin_adc_pos(hit),1.0)*
     &                 max(hscin_adc_neg(hit),1.0)))
                  hscin_trk_depo2(trk,pl) = 
     &                 sqrt( max(hscin_adc_pos(hit),1.0)*
     &                 max(hscin_adc_neg(hit),1.0))
c--   Take the average in case of multiple hits within gate   L.Y.
                  temp = hscin_trk_time(trk,pl) 
                  hscin_trk_time(trk,pl) = 0.5*(temp + hscin_time(hit)) 
                  hscin_trk_time2(trk,pl) = hscin_time(hit)
                  hscin_trk_diff(trk,pl) = 
     &                 hscin_pos_time(hit) - hscin_neg_time(hit)
                  hscin_trk_fptime(trk,pl) = hscin_time(hit)
     &                 - hscin_zpos(pl) * xyz 
     &                 /(speed_of_light*betap)
c     Write(*,*) '(tof2)',hscin_time(hit),
c     &                 hscin_zpos(pl),xyz,betap
               endif                  

            endif               !end of 'if scintillator was on the track'
         enddo                  !end of loop over hit scintillators

c--   Select TOF beta within range [0.5,1.3]
c--   L.Yuan  10/02/2009
         if(hscin_on_track(trk,3).gt.0) then
            if(hscin_on_track(trk,1).gt.0) then
               htrk_tof(trk)  = hscin_trk_time(trk,3) 
     &              - hscin_trk_time(trk,1)
               htrk_leng(trk) = xyz * (hscin_zpos(3) - hscin_zpos(1))
               if(htrk_tof(trk) .gt. 0) then
                  beta_tmp(1) = htrk_leng(trk) / htrk_tof(trk) 
     &                 /speed_of_light
                  beta_tmp(1) = max(0.00001,beta_tmp(1))
                  beta_tmp(1) = min(100000.0,beta_tmp(1))
               Else
                  beta_tmp(1) = -1.
               EndIF
               htrk_depo(trk) = 
     &              sqrt(hscin_trk_depo(trk,3)*hscin_trk_depo(trk,1))
cc               htrk_id(trk) = 1
c -sho            endif               !    1X-2X
c--   debug
cc            if (abs(hscin_trk_fptime(trk,1)-hscin_trk_fptime(trk,3)).le.5.and.beta_tmp(1).le.0.5) 
cc     &           print *, 'tof', beta_tmp(1), zcor,
cc     &      (hscin_trk_fptime(trk,pl),pl=1,3), (hscin_trk_time(trk,pl),pl=1,3)
cc     &       ( hscin_zpos(pl),pl=1,3)
            elseif(hscin_on_track(trk,2).gt.0) then
               htrk_tof(trk)  = hscin_trk_time(trk,3) 
     &              - hscin_trk_time(trk,2)
               htrk_leng(trk) = xyz * (hscin_zpos(3) - hscin_zpos(2))
               if(htrk_tof(trk) .gt. 0) then
                  beta_tmp(2) = htrk_leng(trk) / htrk_tof(trk) 
     &                 /speed_of_light
                  beta_tmp(2) = max(0.00001,beta_tmp(2))
                  beta_tmp(2) = min(100000.0,beta_tmp(2))
               Else
                  beta_tmp(2) = -1
               EndIf
               htrk_depo(trk) = 
     &              sqrt(hscin_trk_depo(trk,3)*hscin_trk_depo(trk,2))
cc               htrk_id(trk) = 2
            endif
c--   debug
cc            if (abs(hscin_trk_fptime(trk,2)-hscin_trk_fptime(trk,3)).le.5.and.beta_tmp(1).le.0.5) 
cc     &        print *, 'tof', beta_tmp(1), betap, xyz,
cc     &     (hscin_trk_fptime(trk,pl),pl=1,3), (hscin_trk_time(trk,pl),pl=1,3),
cc     &      (hscin_zpos(pl),pl=1,3)
c--  
c            if (beta_tmp(1).ge.0.5.and.beta_tmp(1).le.1.3) then
c               good_beta(1) = 1
c               htrk_beta(trk) = beta_tmp(1)
c            elseif (beta_tmp(2).ge.0.5.and.beta_tmp(2).le.1.3) then
c               good_beta(2) = 1
c               htrk_beta(trk) = beta_tmp(2)
c            else
c               good_beta(1) = 1
c               htrk_beta(trk) = 2
c            endif
c --- divide beta_1X & beta_1Y   S.Nagao
            if (beta_tmp(1).ge.0.5.and.beta_tmp(1).le.1.3) then
               htrk_beta(trk) = beta_tmp(1)
            else
               htrk_beta(trk) = 2
            endif
            if (beta_tmp(2).ge.0.5.and.beta_tmp(2).le.1.3) then
               htrk_beta1y(trk) = beta_tmp(2)
            else
               htrk_beta1y(trk) = 2
            endif
c--   
         endif                      !  2X has hit

         num = 0
         htrk_time_atfp(trk) = 0
         i = 1
         if(hscin_on_track(trk,i).gt.0) then
            num = num + 1
            htrk_time_atfp(trk) = htrk_time_atfp(trk) 
     &           + hscin_trk_fptime(trk,i) 
         endif
         i = 3
         if(hscin_on_track(trk,i).gt.0) then
            num = num + 1
            htrk_time_atfp(trk) = htrk_time_atfp(trk) 
     &           + hscin_trk_fptime(trk,i) 
         endif
c         do i = 1, 3
c            if(hscin_on_track(trk,i).gt.0) then
c               num = num + 1
c               htrk_time_atfp(trk) = htrk_time_atfp(trk) 
c     &              + hscin_trk_fptime(trk,i) 
c            endif
c         enddo
         if(num .gt. 0) then
            htrk_time_atfp(trk) = htrk_time_atfp(trk)/Real(num)
         Else
            htrk_time_atfp(trk) = -10000.
         EndIf

cc      Write(*,*) '(htof) htrk_time_atfp=',htrk_time_atfp(trk)
 
*     Dump tof common blocks if (sdebugprinttoftracks is set
         
         if(hdebugprinttoftracks.ne.0 ) call h_prt_tof(trk)
         
c     --- fill histogram
c     
         Call HF1(hidtrktof,htrk_tof(trk),1.)
         Call HF1(hidtrkleng,htrk_leng(trk),1.)

c         Write(*,*) '(tof)betap=',hidtrkbetap,betap
         Call HF1(hidtrkp,p,1.)
         Call HF1(hidtrkbetap,betap,1.)

         Call HF1(hidtrkbetaall,htrk_beta(trk),1.)
c         Call HF1(hidtrkbeta(htrk_id(trk)),htrk_beta(trk),1.) !htrk_id is not well defined DK 
         Call HF1(hidtrkdepo,htrk_depo(trk),1.)
         Call HF1(hidtrkid,float(htrk_id(trk)),1.)
         Call HF2(hidtrkdeltabeta,htrk_beta(trk),hdelta_tar(trk),1.)

         Do j=1,hnum_scin_layers
            Call HF1(hidscintrktime(j),hscin_trk_time(trk,j),1.)
         EndDo
         Do j=1,hnum_scin_layers
            Call HF1(hidscintrkfptime(j),hscin_trk_fptime(trk,j),1.)
         EndDo

         Call HF1(hidtrktimeatfpall,htrk_time_atfp(trk),1.)
c         Call HF1(hidtrktimeatfp(htrk_id(trk)),htrk_time_atfp(trk),1.) !htrk_id is not well defined DK

 100  enddo                     !end of loop over tracks

*     Write raw timing information for fitting.
      if(hdebugdumptof.ne.0) call h_dump_tof
cc      write(37,'(1x,''0'')') 
      RETURN
      END
