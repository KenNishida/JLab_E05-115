      subroutine h_trans_scin(abort,errmsg)
*--------------------------------------------------------
*     author: John Arrington
*     created: 2/22/94
*     
*     s_trans_scin fills the sos_decoded_scin common block
*     with track independant corrections and parameters
*     needed for the drift chamber and tof analysis.
*     
c--   Correct mean time calculation part
c--   L.Yuan 05/15/2005

*     $Log: h_trans_scin.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.5  2005/07/09 00:35:32  yuan
*     Modify calculation of mean time taking into account X scintillator centers are off from Z axis
*
*     Revision 1.11  2005/04/19 17:54:57  miyoshi
*     change minor one
*
*     Revision 1.10  2005/04/08 20:40:56  miyoshi
*     change names
*
*     Revision 1.9  2005/03/14 19:50:49  miyoshi
*     change variables names
*
*     Revision 1.8  2005/03/02 17:28:02  miyoshi
*     add hist
*
*     Revision 1.7  2005/01/07 23:38:13  sumihama
*     Mod. htof
*
*     Revision 1.6  2005/01/06 23:27:09  sumihama
*     Mod of escin & hscin
*
*     Revision 1.5  2005/01/05 23:37:39  sumihama
*     Mod. of escin and hscin
*
*     Revision 1.4  2005/01/05 20:11:05  sumihama
*     h_trans updata
*
*     Revision 1.3  2004/12/24 21:35:57  miyoshi
*     change name plane to layer
*
*     Revision 1.2  2004/12/23 19:38:01  sumihama
*     Add and delete Hbooks, add h_fill_scin_dec_hist.f
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*     
*     Revision 2.1 2004/04 Miyoshi
*     for E01-011
*     
*     Revision 2.0   2000/03/10 09:20:15  jinghua
*     (JLiu) Removed the beta calculation which never been used. 
*     The real beta calculation is in s_tof.f
*     
*     Revision 1.13  1999/06/10 16:57:58  csa
*     (JRA) Cosmetic changes
*     
*     Revision 1.12  1996/04/30 17:15:39  saw
*     (JRA) Cleanup
*     
*     Revision 1.11  1996/01/17 18:21:56  cdaq
*     (JRA) Misc. fixes.
*     
*     Revision 1.10  1995/05/22 19:46:03  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.9  1995/05/17  16:48:22  cdaq
*     (JRA) Add hscintimes user histogram
*     
*     Revision 1.8  1995/05/11  15:10:59  cdaq
*     (JRA) Replace hardwired TDC offsets with ctp variables.  Fix latent hmsism.
*     
*     Revision 1.7  1995/04/06  19:52:59  cdaq
*     (JRA) Change hardwired TDC offset to 100
*     
*     Revision 1.6  1995/02/23  13:25:28  cdaq
*     (JRA) Add a calculation of beta without finding a track
*     
*     Revision 1.5  1995/01/18  21:00:24  cdaq
*     (SAW) Catch negative ADC values in argument of square root
*     
*     Revision 1.4  1994/11/23  15:08:24  cdaq
*     * (SPB) Recopied from hms file and modified names for SOS
*     
*     Revision 1.3  1994/04/13  20:07:02  cdaq
*     (SAW) Fix a typo
*     
*     Revision 1.2  1994/04/13  19:00:06  cdaq
*     (DFG) 3/24  Add s_prt_scin_raw    raw bank dump routine
*     Add s_prt_scin_dec    decoded print routine
*     Add test for zero hits and skip all but initialization
*     Commented out setting abort = .true.
*     Add ABORT and errmsg to arguements
*     (DFG) 4/5   Move prt_scin_raw to s_raw_dump_all routine
*     (DFG) 4/12  Add call to s_fill_scin_raw_hist
*     
*     Revision 1.1  1994/02/21  16:43:53  cdaq
*     Initial revision
*     
*--------------------------------------------------------
      
      implicit none
      
      include 'hks_data_structures.cmn'
      include 'hks_scin_parms.cmn'
      include 'hks_scin_tof.cmn'
      include 'hks_id_histid.cmn'
      include 'hks_tracking.cmn'
      include 'gen_decode_common.cmn'
      include 'gen_rocid.cmn'
      include 'gen_f1tdc.cmn'
      include 'gen_run_info.cmn'
      include 'gen_event_info.cmn'
       INCLUDE 'hks_geometry.cmn'
      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'h_trans_scin')
      
      integer*4 i,j, ihit, nhit, la, co
      integer*4 time_num, have_tdc(hmax_scin_hits), num
      real*4 time_sum
      real*4 fptime,corpos,corneg
      real*4 scint_center
      real*4 hit_position           ! add by L.Y.
      real*4 dist_from_center
      Real*4    adcpos, adcneg
      Integer*4 tdcpos, tdcneg
      Integer*4 tmin,tmax,amin,amax
c--   temporary variables              ! add by L.Yuan
      real*4 fptraw(4), fptraw_sum(4),h1tsum,h2tsum
      integer*4 t_num(4)
      integer*4 f1time,ttime
c      real*4 hstart_pro(hmax_scin_hits,3)
      real*4 hsm_counter_num(hmax_scin_hits,3)
      real*4 h11time,h21time,t1x2x, t1x2x_new,t1xh1,t1xh2
      real*4 h12time,h22time,t2xh1,t2xh2,h1time_pre,h2time_pre
      real*4 t1x1y,t1y2x
      real*4 deltax,prelen,deltaz,deltay !deltaz is distance between 1x and 2x which can be calculated somewhere once
      real*4 deltaz1x1y,deltaz1y2x,deltaz1x2x,deltazh11x,deltazh12x  
      real*4 deltazh11y,deltazh21x,deltazh22x,deltazh21y
      integer*4 h1num,h2num,ntof2
      real*4 t1yh1,t1yh2
c--------------------------------------
c give the counter number and time to the new array which after cut
      integer*4 skipflag1x(hmax_scin_hits),skipflag2x(hmax_scin_hits)
      integer*4 skipflag1y(hmax_scin_hits)
      integer*4 nhitscut1x,nhitscut2x,nhitscut1y
    
      save 
      
       abort = .false.



      IF(hscin_raw_tot_hits .le. 0 ) return
      

c     --- tdc scan
c      Do i=1,hscin_raw_tot_hits
c         tdcpos = hscin_raw_tdc_pos(i)
c         tdcneg = hscin_raw_tdc_neg(i)
c         if((tdcpos.le.0.or.tdcneg.le. 0)) then !for F1 test
c            have_tdc(i) = 1
c         EndIf
c      EndDo

      num=0
      nhits1x=0
      nhits2x=0
      nhits1y=0
      ntof=0
      ntof2=0
      ntof1x2x=0
      ntof1x1y=0
      ntof1y2x=0
      h1num=0
      h2num=0
      h1tsum=0.0
      h2tsum=0.0

c     For test (T.Gogami, 24/Oct/2011)
      if(gen_event_ID_number.eq.1) then 
         open(22,file='test.dat',status='replace')
         close(22)
      endif
      open(22,file='test.dat',access='append')
      write(22,*)hscin_raw_tot_hits
      close(22)
      
      open(22,file='test.dat',access='append') !For test (T.Gogami, 24/Oct/2011)
     
      Do i=1,hscin_raw_tot_hits
         ttime=g_f1_trigger_time(HR_VME_ROCID)
         f1time=g_f1_refer_time(HR_VME_ROCID,1,1)
         la = hscin_raw_layer_num(i)
         co = hscin_raw_counter_num(i)
         tdcpos = hscin_raw_tdc_pos(i)
         tdcneg = hscin_raw_tdc_neg(i)
c     For test (T.Gogami, 24/Oct/2011)
!      open(22,file='test.dat',access='append')   
!      write(22,*)la,co,tdcpos,tdcneg
!      close(22)
         

c--   in EEL test, VMEID is 1. In Hall C, this may be changed to 2.
*      if(  f1time .lt. 0 .and. have_tdc .eq. 1) then
*         if(have_tdc(i).eq.0) then !for F1 test
         if((tdcpos.gt.0.or.tdcneg.gt. 0)) then !for F1 test
c     --- -70000 is initial value.
c     --- pos 
            if(tdcpos .le. -70000) then
               hscin_rawtdc_pos_sub_trig(i) = -70000
            Else
               if(tdcpos.lt.ttime) then
                  tdcpos = tdcpos+hscin_f1tdc_gate_width
               endif
               if(f1time.lt.ttime) then
                  f1time = f1time+hscin_f1tdc_gate_width
               endif
               hscin_rawtdc_pos_sub_trig(i) = 
     &            tdcpos-f1time+1444/hscin_tdc_to_time
            EndIf            ! tdcpos<=-70000
c     --- neg
            if(tdcneg .le. -70000) then
               hscin_rawtdc_neg_sub_trig(i) = -70000
            Else
               if(tdcneg.lt.ttime) then
                  tdcneg = tdcneg+hscin_f1tdc_gate_width
               endif
               if(f1time.lt.ttime) then
                  f1time = f1time+hscin_f1tdc_gate_width
               endif
               hscin_rawtdc_neg_sub_trig(i) = 
     &            tdcneg-f1time+1444/hscin_tdc_to_time
            EndIf
            num = num + 1
c            Write(*,*) "ev,f1,la,co,tpos,tneg,tpossub,tnegsub,gate="
c            write(*,*)
c     &              gen_event_id_number,
c     &              ttime,
c     &              hscin_raw_layer_num(i),hscin_raw_counter_num(i),
c     &              tdcpos,
c     &              tdcneg,
c     &              hscin_rawtdc_pos_sub_trig(i),
c     &              hscin_rawtdc_neg_sub_trig(i),
c     &              hscin_f1tdc_gate_width
         else 
            hscin_rawtdc_pos_sub_trig(i) = -70000
            hscin_rawtdc_neg_sub_trig(i) = -70000
         EndIF                  ! good trigger time
      EndDo               ! hscin_raw_tot_hits loop
*     --- Histogram raw scin
      call h_fill_scin_raw_hist(abort,errmsg)
      if (abort) then
         call g_prepend(here,errmsg)
         return
      endif

      if(num.eq.0) then
         Write(*,*) 'No HSCIN trigger signal : ev=',
     &     gen_event_ID_number, ' Number of events=', num
         hscin_raw_tot_hits = 0 
         return
      endif
      
      
      IF(hscin_raw_tot_hits .le. 0 ) return
      
      do i = 1, hnum_scin_layers
         hscin_hits_per_layer(i) = 0
         hscin_sing_counter(i) = -1
      enddo
      
      hscin_tot_hits = 0
      time_num = 0
      time_sum = 0.
c--   temporary
      do i =1,3 
         fptraw_sum(i) = 0.0
         t_num(i) = 0
         fptraw(i) = -10000.
      enddo
c--
      amin = hscin_adc_min
      amax = hscin_adc_max
      tmin = hscin_tdc_min
      tmax = hscin_tdc_max

      if(amin .le. 0) amin = 0. 
      if(amax .le. 0) amax = 8000. 
      if(tmin .le. 0) tmin = 10000. 
      if(tmax .le. 0) tmax = 50000. 
      
c      write(*,*)"HKS f1 trigger time",f1time
      
c      do i=1,18
c         write(*,*) hscin_pos_time_offset(1,i),
c     &      hscin_pos_time_offset(2,i),
c     &      hscin_pos_time_offset(3,i),
c     &      hscin_pos_time_offset(4,1)
c      enddo
c      do i=1,18
c         write(*,*) hscin_neg_time_offset(1,i),
c     &      hscin_neg_time_offset(2,i),
c     &      hscin_neg_time_offset(3,i),
c     &      hscin_neg_time_offset(4,1)
c      enddo
      

      do ihit = 1 , hscin_raw_tot_hits
         la = hscin_raw_layer_num(ihit)
         co = hscin_raw_counter_num(ihit)
cc         print *, 'ped',la, co, hscin_raw_ped_pos(la,co)
         adcpos = real(hscin_raw_adc_pos(ihit)) 
     &        - hscin_raw_ped_pos(la,co)
         adcneg = real(hscin_raw_adc_neg(ihit)) 
     &        - hscin_raw_ped_neg(la,co)
         tdcpos = hscin_rawtdc_pos_sub_trig(ihit)
         tdcneg = hscin_rawtdc_neg_sub_trig(ihit)

         write(22,*)la,co,tdcpos,tdcneg,adcpos,adcneg

C     Search good hits
         if(  
c     &        (tdcpos .gt. tmin .and. tdcpos .lt. tmax) 
c     &        .OR.
c     &        (tdcneg .gt. tmin .and. tdcneg .lt. tmax) 
     &        (tdcpos .gt. tmin .and. tdcpos .lt. tmax) 
     &        .and.
     &        (tdcneg .gt. tmin .and. tdcneg .lt. tmax) 
     &        .and.
     &        abs(tdcpos-tdcneg).lt.500.
     &        ) then

            hscin_tot_hits = hscin_tot_hits + 1
            nhit = hscin_tot_hits 
            hscin_layer_num(nhit) = la
            hscin_counter_num(nhit) = co
            hscin_hits_per_layer(hscin_layer_num(nhit)) = 
     &           hscin_hits_per_layer(hscin_layer_num(nhit)) + 1
            htwo_good_times(nhit) = .false.
            hscin_time(nhit) = -1000.
            hscin_cor_time(nhit) = -1000.
            
c     --- store position info.

c     Write(*,*) '(htransscin)centercoord=',
c     &           la,co,hscin_center_coord(nhit)
          
            if (la .eq. 1) then !1x
               hscin_pos_coord(nhit) = hscin_1x_top
               hscin_neg_coord(nhit) = hscin_1x_bot
            else if (la .eq. 2) then !1y
               hscin_pos_coord(nhit) = hscin_1y_left
               hscin_neg_coord(nhit) = hscin_1y_right
            else if (la .eq. 3) then !2x
               hscin_pos_coord(nhit) = hscin_2x_top
               hscin_neg_coord(nhit) = hscin_2x_bot
            else if (la.eq.4) then ! temperally for 2y
               hscin_pos_coord(nhit) = hscin_1y_left
               hscin_neg_coord(nhit) = hscin_1y_right
            else                ! unknown layer
               abort = .true.
               write(errmsg,*) 'Trying to init. hks hodoscope layer',la
               call g_prepend(here,errmsg)
               return
            endif

*     --- for positive tubes
            if( adcpos .gt. amin .and. adcpos .lt. amax .and.
     &          tdcpos .gt. tmin .and. tdcpos .lt. tmax ) then
               hgood_pos(nhit) =  .true.
               hscin_adc_pos(nhit)  = adcpos
               hscin_tdc_pos(nhit)  = tdcpos
               if(adcpos .gt. 0) then
c                  hscin_pos_time(nhit) = tdcpos*hscin_tdc_to_time 
c     &                 - hscin_pos_phc_coeff(la,co)/sqrt(adcpos)
c     &                 - hscin_pos_time_offset(la,co)
                  hscin_pos_time(nhit) = (tdcpos
     &                 - hscin_pos_phc_coeff(la,co)/sqrt(adcpos)
     &                 + hscin_pos_phc_coeff2(la,co)*adcpos)
     &                 * hscin_tdc_to_time
     &                 - hscin_pos_time_offset(la,co)
               Else
                  hscin_pos_time(nhit) = tdcpos*hscin_tdc_to_time 
     &                 - hscin_pos_time_offset(la,co)
               EndIF
            Else      
               hgood_pos(nhit) = .false.
               hscin_adc_pos(nhit) = -1
               hscin_tdc_pos(nhit) = -70000
               hscin_pos_time(nhit) = -1000.
            EndIF               ! positive tubes.
            
*     --- for negative tubes
            if( adcneg .gt. amin .and. adcneg .lt. amax .and.
     &           tdcneg .gt. tmin .and. tdcneg .lt. tmax ) then
               hgood_neg(nhit) = .true.
               hscin_adc_neg(nhit)  = adcneg
               hscin_tdc_neg(nhit)  = tdcneg
               if(adcneg .gt. 0) then
c                  hscin_neg_time(nhit) = tdcneg*hscin_tdc_to_time
c     &                 - hscin_neg_phc_coeff(la,co)/sqrt(adcneg)
c     &                 - hscin_neg_time_offset(la,co) 
                  hscin_neg_time(nhit) = (tdcneg
     &                 - hscin_neg_phc_coeff(la,co)/sqrt(adcneg)
     &                 + hscin_neg_phc_coeff2(la,co)*adcneg)
     &                 * hscin_tdc_to_time
     &                 - hscin_neg_time_offset(la,co)
               Else
                  hscin_neg_time(nhit) = tdcneg*hscin_tdc_to_time
     &                 - hscin_neg_time_offset(la,co) 
               EndIf
            Else      
               hgood_neg(nhit) = .false.
               hscin_adc_neg(nhit) = -1
               hscin_tdc_neg(nhit) = -70000
               hscin_neg_time(nhit) = -1000.
            EndIf               ! negative tubes.
            
*     --- calculate mean time.
            if( hgood_pos(nhit) .and. hgood_neg(nhit) ) then
               htwo_good_times(nhit) = .true.
               hscin_time(nhit) = 0.5 *( hscin_pos_time(nhit) + 
     &              hscin_neg_time(nhit))
*     Find hit position. 
c--   original code assume central Z axis at the center of scintillator,
c--   abs(hscin_neg_coord)=abs(hscin_pos_coord). This is not the case.
c--   L.Y.
               dist_from_center = 
     &              0.5*( hscin_neg_time(nhit) - hscin_pos_time(nhit) ) 
     &              * hscin_vel_light(la,co)
               scint_center = (hscin_pos_coord(nhit)+hscin_neg_coord(nhit))/2.
               hit_position = scint_center + dist_from_center
               hit_position = min(hscin_pos_coord(nhit),hit_position)
               hit_position = max(hscin_neg_coord(nhit),hit_position)
c     dist_from_center  = min(hscin_pos_coord(nhit),
c     &              dist_from_center)
c     dist_from_center  = max(hscin_neg_coord(nhit),
c     &              dist_from_center)
               hscin_hit_coord(nhit) = hit_position 
*     Get corrected time.
               corpos = hscin_pos_time(nhit) 
     &              - (hscin_pos_coord(nhit) - hit_position)
     &              /hscin_vel_light(la,co)
               corneg = hscin_neg_time(nhit) 
     &              - (hit_position-hscin_neg_coord(nhit))
     &              /hscin_vel_light(la,co)
               hscin_cor_time(nhit) = 0.5 * (corpos + corneg)
             if(la.eq.1) then
             nhits1x=nhits1x+1
c           print*,la,co,nhit,nhits1x,hscin_cor_time(nhit)
              hstart_pro(nhits1x,1)=hscin_cor_time(nhit)
              hpro_counter_num(nhits1x,1)=co
            

             endif
            if(la.eq.2) then
              nhits1y=nhits1y+1
c           print*,la,co,nhit,nhits1y,hscin_cor_time(nhit)
              hstart_pro(nhits1y,2)=hscin_cor_time(nhit)
              hpro_counter_num(nhits1y,2)=co
           
             endif
             if(la.eq.3) then
             nhits2x=nhits2x+1
             hstart_pro(nhits2x,3)=hscin_cor_time(nhit)
             hpro_counter_num(nhits2x,3)=co
c             print*,la,co,nhit,nhits2x,hscin_cor_time(nhit)
c             write(37,*) hstart_pro(nhits2x,3)
            endif
*     start time calculation.  assume xp=yp=0 radians.  project all
*     time values to focal layer.  use average for start time.

*     need expected particle velocity for start time calculation.
         hbeta_pcent = hpcentral/sqrt(hpcentral*hpcentral+
     &      hpartmass*hpartmass)
c        print*,hbeta_pcent 
               fptime  = hscin_cor_time(nhit) - hscin_zpos(la)
     &              /(29.979*hbeta_pcent)
               call hf1(hidscinfptime,fptime,1.)
               if (abs(fptime-hstart_time_center).le.hstart_time_slop) then
                  time_sum = time_sum + fptime
                  time_num = time_num + 1
c--   temporary
                  fptraw_sum(la) = fptraw_sum(la) +fptime
                  t_num(la) = t_num(la) + 1
               endif
            Else if( hgood_pos(nhit) ) then
               hscin_time(nhit) = hscin_pos_time(nhit)
               hscin_hit_coord(nhit) = 0.
               hscin_cor_time(nhit) = hscin_pos_time(nhit)
     &              - abs(hscin_neg_coord(nhit) - hscin_pos_coord(nhit))
     &              /hscin_vel_light(la,co) * 0.5
            Else if( hgood_neg(nhit) ) then
               hscin_time(nhit) = hscin_neg_time(nhit)
               hscin_hit_coord(nhit) = 0.
               hscin_cor_time(nhit) = hscin_neg_time(nhit)
     &              - abs(hscin_neg_coord(nhit) - hscin_pos_coord(nhit))
     &              /hscin_vel_light(la,co) * 0.5
            EndIF               ! mean time.
         endif                  ! one good hit
      enddo                     ! tot hits
      
      hgood_start_time = .false.
      
*     --- Decoded data filling ---
      call h_fill_scin_dec_hist(ABORT,errmsg)
c------------------------------------------------------------------------------      
       if (time_num.eq.0) then
         hgood_start_time = .false.
         hstart_time = hstart_time_center
      else
         hgood_start_time = .true.
         hstart_time = time_sum / float(time_num)
         do i=1,3
            if (t_num(i).ge.1) fptraw(i) = fptraw_sum(i)/t_num(i)
         enddo
c         write(32,*) (i,fptraw(i),i=1,3)
        endif
cc---------------------------------------------------------------------------
c--------------------------------------------------------------------------
c C.CHEN Jan.25,2010
c calculate the hstart_time by project tof from 1x-2x to HDC1-1X and HDC2-2x
c---------------------------------------------------------------------------
c---for data transform after cut

         deltaz1x2x=hscin_zpos(1)-hscin_zpos(3)
         deltaz1x1y=hscin_zpos(1)-hscin_zpos(2)
         deltaz1y2x=hscin_zpos(2)-hscin_zpos(3)
         deltazh11x= hdc_1_zpos-hscin_zpos(1)
         deltazh12x= hdc_1_zpos-hscin_zpos(3)
         deltazh11y= hdc_1_zpos-hscin_zpos(2)
         deltazh21x= hdc_2_zpos-hscin_zpos(1)
         deltazh22x= hdc_2_zpos-hscin_zpos(3)
         deltazh21y= hdc_2_zpos-hscin_zpos(2)
         do j=1,nhits2x
             skipflag2x(j)=1
         enddo

         do j=1,nhits1x
            skipflag1x(j)=1
         enddo

         do j=1,nhits1y
            skipflag1y(j)=1
         enddo

             nhitscut1x=0
             nhitscut2x=0
             nhitscut1y=0    
c----------------------------------------------------
c-1x 2x pretracking
c  
         if(nhits1x.gt.0.and.nhits2x.gt.0) then
           do i=1,nhits1x
              do j=1,nhits2x
              
            t1x2x=hstart_pro(i,1)-hstart_pro(j,3)
            deltax=(hscin_1x_center(hpro_counter_num(i,1))
     +                -hscin_2x_center(hpro_counter_num(j,3)))
            deltaz=deltaz1x2x     !hscin_zpos(1)-hscin_zpos(3),it is different from survey data
            prelen=sqrt(deltax*deltax+deltaz*deltaz)
            t1x2x=abs(deltaz)*t1x2x/prelen
        
            if(t1x2x.lt.0.0.and.t1x2x.gt.-10.0) then
            
              
              ntof1x2x=ntof1x2x+1
              hsm_counter_num(ntof1x2x,1)=hpro_counter_num(i,1)
              hsm_counter_num(ntof1x2x,3)=hpro_counter_num(j,3)

c             write(15,*) t1x2x,t1x2x_new,hstart_pro(j,3),hstart_pro(i,1)
              t1xh2=t1x2x*deltazh21x/deltaz1x2x  ! time_h2-timex1 it is negative
           
                                       !19.2cm is the distance between 1x and HDC2, 125.6 is the distance between 1x and 2x from survey data
              h21time=hstart_pro(i,1)+t1xh2 !h21time=time_x1-(time_x1-time_h2)=time_x1+(time_h2-time_x1)
              t1xh1=t1x2x*deltazh11x/deltaz1x2x !119.371cm is the distance between 1x and HDC1 all those number could get from parameter files
              h11time=hstart_pro(i,1)+t1xh1
              t2xh1=t1x2x*deltazh12x/deltaz1x2x 
              t2xh2=t1x2x*deltazh22x/deltaz1x2x
              h12time=hstart_pro(j,3)+t2xh1
              h22time=hstart_pro(j,3)+t2xh2
              h1time_pre=(h11time+h12time)/2
              h2time_pre=(h21time+h22time)/2

              
              if(abs(h1time_pre-h1start_time_center).le.hstart_time_slop) then   ! -72.16 is the mean value of h1time_pre, need to be write it to outside as a parameter
              h1tsum=h1tsum+h1time_pre
              h1num=h1num+1
              endif
              if(abs(h2time_pre-h2start_time_center).le.hstart_time_slop) then
              h2tsum=h2tsum+h2time_pre
              h2num=h2num+1
              endif
              if(skipflag2x(j).gt.0) then
                skipflag2x(j)=0
               
              endif

              if(skipflag1x(i).gt.0) then
                skipflag1x(i)=0
               
               endif


             endif
             enddo  !nhits2x loop
            enddo !nhits1x loop
   
             if(h1num.gt.0) then
             h1start_pre=h1tsum/h1num
             else
             h1start_pre=h1start_time_center
             endif
             if(h2num.gt.0) then
             h2start_pre=h2tsum/h2num
             else
             h2start_pre=h2start_time_center
             endif           
             ntof=ntof1x2x
          
             do i=1,nhits1x
              if(skipflag1x(i).eq.0) then
               nhitscut1x=nhitscut1x+1
               hpro_counter_num(nhitscut1x,1)=hpro_counter_num(i,1)
            
               hstart_pro(nhitscut1x,1)=hstart_pro(i,1)
               endif
             enddo
            
              do i=1,nhits2x
              if(skipflag2x(i).eq.0) then
               nhitscut2x=nhitscut2x+1
               
               hpro_counter_num(nhitscut2x,3)=hpro_counter_num(i,3)
               hstart_pro(nhitscut2x,3)=hstart_pro(i,3)
               endif
             enddo
           
             nhits1x=nhitscut1x
             nhits2x=nhitscut2x
             
             

            endif  ! nhits1x>0, nhits2x>0
c----------------------------------------------------------------------------------------          
             

              if(nhits1x.eq.0) then
              if(nhits2x.gt.0.and.nhits1y.gt.0) then
               do i=1,nhits1y
                 do j=1,nhits2x
                
                t1y2x=hstart_pro(i,2)-hstart_pro(j,3)
                deltaz=deltaz1y2x    !hscin_zpos(2)-hscin_zpos(3)
               deltax=hscin_1y_center(hpro_counter_num(i,2))
               deltay=hscin_2x_center(hpro_counter_num(j,3))
               prelen=sqrt(deltax*deltax+deltaz*deltaz+deltay*deltay)
               t1y2x=t1y2x*abs(deltaz)/prelen
              
              if(t1y2x.le.-2.and.t1y2x.ge.-7) then  !if 0.7<beta<1.2 then -5.0ns<delta<-2.91ns d2x1y=104.6cm
               
              ntof1y2x=ntof1y2x+1
              hsm_counter_num(ntof1y2x,2)=hpro_counter_num(i,2)
              hsm_counter_num(ntof1y2x,3)=hpro_counter_num(j,3)


              t1yh2=t1y2x*deltazh21y/deltaz1y2x  !19.2cm is the distance between 1x and HDC2, 125.6 is the distance between 1x and 2x from survey data
              h21time=hstart_pro(i,2)+t1yh2
              t1yh1=t1y2x*deltazh11y/deltaz1y2x !119.371cm is the distance between 1x and HDC1 all those number could get from parameter files
              h11time=hstart_pro(i,2)+t1yh1
              t2xh1=t1y2x*deltazh12x/deltaz1y2x 
              t2xh2=t1y2x*deltazh22x/deltaz1y2x
              h12time=hstart_pro(j,3)+t2xh1
              h22time=hstart_pro(j,3)+t2xh2
              h1time_pre=(h11time+h12time)/2
              h2time_pre=(h21time+h22time)/2
              
                if(abs(h1time_pre-h1start_time_center).le.hstart_time_slop) then   ! -72.16 is the mean value of h1time_pre, need to be write it to outside as a parameter
                h1tsum=h1tsum+h1time_pre
                h1num=h1num+1
                endif
                if(abs(h2time_pre-h2start_time_center).le.hstart_time_slop) then
                h2tsum=h2tsum+h2time_pre
                h2num=h2num+1
                endif
               if(skipflag2x(j).gt.0) then
                skipflag2x(j)=0

              endif
              if(skipflag1y(i).gt.0) then
                skipflag1y(i)=0

               endif
             endif 
    
             enddo
             enddo

             if(h1num.gt.0) then
             h1start_pre=h1tsum/h1num
             else
             h1start_pre=h1start_time_center
             endif
             if(h2num.gt.0) then
             h2start_pre=h2tsum/h2num
             else
             h2start_pre=h2start_time_center
             endif           

             ntof=ntof1y2x

            do i=1,nhits1y
              if(skipflag1y(i).eq.0) then
               nhitscut1y=nhitscut1y+1
               hpro_counter_num(nhitscut1y,2)=hpro_counter_num(i,2)
               hstart_pro(nhitscut1y,2)=hstart_pro(i,2)
               endif
             enddo
            
              do i=1,nhits2x
              if(skipflag2x(i).eq.0) then
               nhitscut2x=nhitscut2x+1
               hpro_counter_num(nhitscut2x,3)=hpro_counter_num(i,3)
               hstart_pro(nhitscut2x,3)=hstart_pro(i,3)
               endif
             enddo
             nhits1y=nhitscut1y
             nhits2x=nhitscut2x


            endif
           endif
           
           if(nhits2x.eq.0) then
           if(nhits1x.gt.0.and.nhits1y.gt.0) then
            do i=1,nhits1x
             do j=1,nhits1y
                       
             t1x1y=hstart_pro(i,1)-hstart_pro(j,2)
             deltaz=deltaz1x1y     !hscin_zpos(1)-hscin_zpos(2)
             deltax=hscin_1x_center(hpro_counter_num(i,1))
             deltay=hscin_1y_center(hpro_counter_num(j,2))
             prelen=sqrt(deltax*deltax+deltaz*deltaz+deltay*deltay)
             t1x1y=t1x1y*abs(deltaz)/prelen
              
             if(t1x1y.ge.-2.and.t1x1y.le.1) then  !d1x1y=21cm, -1ns<delta<-0.583ns
             ntof1x1y=ntof1x1y+1
             hsm_counter_num(ntof1x1y,1)=hpro_counter_num(i,1)
             hsm_counter_num(ntof1x1y,2)=hpro_counter_num(j,2)
              t1xh2=t1x1y*deltazh21x/deltaz1x1y  !19.2cm is the distance between 1x and HDC2, 125.6 is the distance between 1x and 2x from survey data
              h21time=hstart_pro(i,1)+t1xh2
              t1xh1=t1x1y*deltazh11x/deltaz1x1y !119.371cm is the distance between 1x and HDC1 all those number could get from parameter files
              h11time=hstart_pro(i,1)+t1xh1
              t1yh1=t1x1y*deltazh11y/deltaz1x1y 
              t1yh2=t1x1y*deltazh21y/deltaz1x1y
              h12time=hstart_pro(j,2)+t1yh1
              h22time=hstart_pro(j,2)+t1yh2
              h1time_pre=(h11time+h12time)/2
              h2time_pre=(h21time+h22time)/2
            
             if(abs(h1time_pre-h1start_time_center).le.hstart_time_slop) then   ! -72.16 is the mean value of h1time_pre, need to be write it to outside as a parameter
              h1tsum=h1tsum+h1time_pre 
              h1num=h1num+1              
              endif
              if(abs(h2time_pre-h2start_time_center).le.hstart_time_slop) then
              h2tsum=h2tsum+h2time_pre
              h2num=h2num+1            
              endif
             if(skipflag1x(j).gt.0) then
                skipflag1x(j)=0

              endif
              if(skipflag1y(i).gt.0) then
                skipflag1y(i)=0

               endif
             endif
             enddo
            enddo

             if(h1num.gt.0) then
             h1start_pre=h1tsum/h1num
             else
             h1start_pre=h1start_time_center
             endif
             if(h2num.gt.0) then
             h2start_pre=h2tsum/h2num
             else
             h2start_pre=h2start_time_center
             endif           
             ntof=ntof1x1y

              do i=1,nhits1x
              if(skipflag1x(i).eq.0) then
               nhitscut1x=nhitscut1x+1
               hpro_counter_num(nhitscut1x,1)=hpro_counter_num(i,1)
               hstart_pro(nhitscut1x,1)=hstart_pro(i,1)
               endif
             enddo
            
              do i=1,nhits1y
              if(skipflag1y(i).eq.0) then
               nhitscut1y=nhitscut1y+1
               hpro_counter_num(nhitscut1y,2)=hpro_counter_num(i,2)
               hstart_pro(nhitscut1y,2)=hstart_pro(i,2)
               endif
             enddo
             nhits1x=nhitscut1x
             nhits1y=nhitscut1y

            
            endif  
           endif
c           if(ntof.gt.0) then
c          count2=count2+1
c           print*,count2
c           endif
c         
c           print*,ntof,ntof1x2x    
           if(ntof.eq.0) then  ! ntof1x2x only for test before put all the conditions in
c right now only use 1x-2x tracking  this is only for test 
                 h1start_time=-70000
                 h2start_time=-70000     
          endif  
       
         
        

c      write(16,*) ntof,h1start_time,h2start_time          
*     Dump decoded bank if sdebugprintscindec is set
      if( hdebugprintscindec .ne. 0) call h_prt_dec_scin(ABORT,errmsg)
      
      close(22)                 !For test (T.Gogami, 24/Oct/2011)
      
      return
      end

