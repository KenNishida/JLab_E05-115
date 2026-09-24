      subroutine h_pid_ntuple_keep(ABORT,err)
*-----------------------------------------------------------------
*     
*     Purpose : Add entry to the HKS pid Ntuple
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='h_pid_ntuple_keep')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'hks_tracking.cmn'
      INCLUDE 'hks_physics_sing.cmn'
      INCLUDE 'hks_scin_parms.cmn'
      INCLUDE 'hks_scin_tof.cmn'
      INCLUDE 'hks_water_parms.cmn'
      INCLUDE 'hks_aero_parms.cmn'
      INCLUDE 'hks_lucite_parms.cmn'
      include 'hks_track_histid.cmn'
      Include 'hks_bypass_switches.cmn'
*      INCLUDE 'gen_data_structures.cmn'
*     
      logical HEXIST            !CERNLIB function
*     
      integer m,i,j,k
      integer la, co 
      integer preco1x(5), preco2x(5)
      real*4  preadc1xp(5), preadc1xn(5)
      real*4  preadc2xp(5), preadc2xn(5)
      real*4  pretdc1xp(5), pretdc1xn(5)
      real*4  pretdc2xp(5), pretdc2xn(5)

      integer co1x(25), co1y, co2x(25), co2y
      integer coac1(25), coac2(25), coac3(25)
      integer cowc1(25), cowc2(25), colc(25), cogt(25) 
      integer mh1x(17), mh1y(9), mh2x(18), mh2y(1)
      integer mhac1(7), mhac2(7), mhac3(7), mhwc1(12), mhwc2(12) 
      integer mhlc(16), mhgt(8) 
      integer flag1x, flag1y, flag2x, flag2y
      integer flagac1, flagac2, flagac3, flagwc1, flagwc2 
      integer flaglc, flaggt
      integer pidtrk
      real*4  adc1xp(25), adc1xn(25), adc1yp, adc1yn
      real*4  adc2xp(25), adc2xn(25), adc2yp, adc2yn
      real*4  tdc1xp(25), tdc1xn(25), tdc1yp, tdc1yn
      real*4  tdc2xp(25), tdc2xn(25), tdc2yp, tdc2yn
      real*4  adcwc1p(25), adcwc1n(25), adcwc2p(25), adcwc2n(25)
      real*4  tdcwc1(25), tdcwc2(25)
      real*4  adcac1p(25), adcac1n(25)
      real*4  adcac2p(25), adcac2n(25)
      real*4  adcac3p(25), adcac3n(25)
      real*4  tdcac1p(25), tdcac1n(25)
      real*4  tdcac2p(25), tdcac2n(25)
      real*4  tdcac3p(25), tdcac3n(25)
      real*4  adclcp(25), adclcn(25)
      real*4  tdclcp(25), tdclcn(25)
      real*4  tdcgt(8)
      real*4  slope,z0,x0
      real*4  xwc1(25),xwc2(25)
      real*4  xac1(25),xac2(25),xac3(25)
      real*4  xlc(25)
      
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
c      write(*,*)"ev=",gen_event_ID_number
c      write(*,*)"hit=",hpid_raw_tot_hits
      IF(.NOT.h_pid_ntuple_exists) RETURN !nothing to do
*     
      if(hscin_raw_tot_hits .le. 0 .or.
     &   hwat_raw_tot_hits .le. 0 .or.
     &   haer_raw_tot_hits .le. 0 ) RETURN
      
      do i=1,17
         mh1x(i)=0
      enddo
      do i=1,9
         mh2y(i)=0
      enddo
      do i=1,18
         mh2x(i)=0
      enddo
      mh2y(1)=0
      do i=1,7
         mhac1(i)=0
         mhac2(i)=0
         mhac3(i)=0
      enddo
      do i=1,12
         mhwc1(i)=0
         mhwc2(i)=0
      enddo
      do i=1,16
         mhlc(i)=0
      enddo
      do i=1,8
         mhgt(i)=0
         tdcgt(i)=-10000.
      enddo
      
      flag1x=1
      flag1y=1
      flag2x=1 
      flag2y=1
      flagac1=1
      flagac2=1 
      flagac3=1 
      flagwc1=1 
      flagwc2=1 
      flaglc=1
      flaggt=1
      adc1yp=-10000.
      adc1yn=-10000.
      adc2yp=-10000.
      adc2yn=-10000.
      tdc1yp=-10000.
      tdc1yn=-10000.
      tdc2yp=-10000.
      tdc2yn=-10000.
      do i=1,5 
         preco1x(i)=-10000
         preco2x(i)=-10000
         preadc1xp(i)=-10000.
         preadc1xn(i)=-10000.
         preadc2xp(i)=-10000.
         preadc2xn(i)=-10000.
         pretdc1xp(i)=-10000.
         pretdc1xn(i)=-10000.
         pretdc2xp(i)=-10000.
         pretdc2xn(i)=-10000.
      enddo
      do i=1,25 
         co1x(i)=-10000
         co2x(i)=-10000
         coac1(i)=-10000
         coac2(i)=-10000
         coac3(i)=-10000
         cowc1(i)=-10000
         cowc2(i)=-10000
         colc(i)=-10000
         adc1xp(i)=-10000.
         adc1xn(i)=-10000.
         adc2xp(i)=-10000.
         adc2xn(i)=-10000.
         tdc1xp(i)=-10000.
         tdc1xn(i)=-10000.
         tdc2xp(i)=-10000.
         tdc2xn(i)=-10000.
         adcwc1p(i)=-10000.
         adcwc1n(i)=-10000.
         adcwc2p(i)=-10000.
         adcwc2n(i)=-10000.
         tdcwc1(i)=-10000.
         tdcwc2(i)=-10000.
         adcac1p(i)=-10000.
         adcac1n(i)=-10000.
         adcac2p(i)=-10000.
         adcac2n(i)=-10000.
         adcac3p(i)=-10000.
         adcac3n(i)=-10000.
         tdcac1p(i)=-10000.
         tdcac1n(i)=-10000.
         tdcac2p(i)=-10000.
         tdcac2n(i)=-10000.
         tdcac3p(i)=-10000.
         tdcac3n(i)=-10000.
         adclcp(i)=-10000.
         adclcn(i)=-10000.
         tdclcp(i)=-10000.
         tdclcn(i)=-10000.
         xwc1(i)=-10000.
         xwc2(i)=-10000.
         xac1(i)=-10000.
         xac2(i)=-10000.
         xac3(i)=-10000.
         xlc(i)=-10000.
      enddo
      pidtrk=1
             
      if(h_debugnt(1).eq.0) then
         
         do i=1,hscin_tot_hits
            la=hscin_layer_num(i)
            if (la.eq.1.and.flag1x.le.5) then
               co=hscin_counter_num(i)
               mh1x(co)=mh1x(co)+1
               if (hscin_pos_time(i).gt.-1000.and.
     &             hscin_neg_time(i).gt.-1000.and.      
     &             abs(hscin_pos_time(i)).lt.100.and.
     &             abs(hscin_pos_time(i)
     &                 -hscin_neg_time(i)).lt.10) then
                     preco1x(flag1x)=hscin_counter_num(i)
                     preadc1xp(flag1x)=hscin_adc_pos(i)
                     preadc1xn(flag1x)=hscin_adc_neg(i)
                     pretdc1xp(flag1x)=hscin_pos_time(i)
                     pretdc1xn(flag1x)=hscin_neg_time(i)
                     flag1x=flag1x+1
               endif
            endif
            if (la.eq.3.and.flag2x.le.5) then
               co=hscin_counter_num(i)
               mh2x(co)=mh2x(co)+1
               if (hscin_pos_time(i).gt.-1000.and.
     &             hscin_neg_time(i).gt.-1000.and.      
     &             abs(hscin_pos_time(i)).lt.100.and.
     &             abs(hscin_pos_time(i)
     &                 -hscin_neg_time(i)).lt.10) then
                     preco2x(flag2x)=hscin_counter_num(i)
                     preadc2xp(flag2x)=hscin_adc_pos(i)
                     preadc2xn(flag2x)=hscin_adc_neg(i)
                     pretdc2xp(flag2x)=hscin_pos_time(i)
                     pretdc2xn(flag2x)=hscin_neg_time(i)
                     flag2x=flag2x+1
               endif
            endif
         enddo
         
         do i=1,flag1x
            do j=1,flag2x
               if (preco1x(i).gt.-10000.and.preco2x(j).gt.-10000.and.
     &                  abs(preco1x(i)-preco2x(j)).le.5) then
                  co1x(pidtrk)=preco1x(i)
                  co2x(pidtrk)=preco2x(j)
                  adc1xp(pidtrk)=preadc1xp(i) 
                  adc1xn(pidtrk)=preadc1xn(i) 
                  tdc1xp(pidtrk)=pretdc1xp(i) 
                  tdc1xn(pidtrk)=pretdc1xn(i) 
                  adc2xp(pidtrk)=preadc2xp(j) 
                  adc2xn(pidtrk)=preadc2xn(j) 
                  tdc2xp(pidtrk)=pretdc2xp(j) 
                  tdc2xn(pidtrk)=pretdc2xn(j)
                  slope=(hscin_2x_center(co2x(pidtrk))-
     &               hscin_1x_center(co1x(pidtrk)))/
     &               (hscin_zpos(3)-hscin_zpos(1))
                  z0=hscin_zpos(1)
                  x0=hscin_1x_center(co1x(pidtrk))
                  xwc1(pidtrk)=slope*(hwat_box_zpos(1)-z0)+x0
                  xwc2(pidtrk)=slope*(hwat_box_zpos(2)-z0)+x0
                  xac1(pidtrk)=slope*(haer_box_zpos(1)-z0)+x0
                  xac2(pidtrk)=slope*(haer_box_zpos(2)-z0)+x0
                  xac3(pidtrk)=slope*(haer_box_zpos(3)-z0)+x0
                  xlc(pidtrk)=slope*(hluc_box_zpos(1)-z0)+x0
c                  write(*,*) gen_event_ID_number,
c     &             pidtrk,co1x(pidtrk),co2x(pidtrk),
c     &             adc1xp(pidtrk),adc1xn(pidtrk),
c     &             tdc1xp(pidtrk),tdc1xn(pidtrk), 
c     &             adc2xp(pidtrk),adc2xn(pidtrk),
c     &             tdc2xp(pidtrk),tdc2xn(pidtrk), 
c     &             xwc1(pidtrk)
                  pidtrk=pidtrk+1
               endif
            enddo
         enddo


         
         do i=1,hscin_tot_hits
            la=hscin_layer_num(i)
            if (la.eq.2.and.flag1y.le.5) then
               co=hscin_counter_num(i)
               mh1y(co)=mh1y(co)+1
               if (
     &            hscin_pos_time(i).gt.-1000.and.
     &            hscin_neg_time(i).gt.-1000.and. 
     &            abs(hscin_pos_time(i)
     &                -hscin_neg_time(i)).lt.10
     &            ) then 
                     co1y=hscin_counter_num(i)
                     adc1yp=hscin_adc_pos(i)
                     adc1yn=hscin_adc_neg(i)
                     tdc1yp=hscin_pos_time(i)
                     tdc1yn=hscin_neg_time(i)
                     flag1y=flag1y+1
               endif
            endif
            if (la.eq.4.and.flag2y.le.5) then
               co=hscin_counter_num(i)
               mh2y(co)=mh2y(co)+1
               if (hscin_adc_pos(i).gt.1000
     &             .and.hscin_adc_neg(i).gt.1000) then
                     co2y=hscin_counter_num(i)
                     adc2yp=hscin_adc_pos(i)
                     adc2yn=hscin_adc_neg(i)
                     tdc2yp=hscin_pos_time(i)
                     tdc2yn=hscin_neg_time(i)
                     flag2y=flag2y+1
               endif
            endif
         enddo

         do j=1,pidtrk
            do i=1,hwat_tot_hits
               la=hwat_layer_num(i)
               if (la.eq.1.and.flagwc1.le.25) then
                  co=hwat_counter_num(i)
                  mhwc1(co)=mhwc1(co)+1
                  if (hwat_pos_time(i).gt.9000..and.
     &               hwat_pos_time(i).lt.15000..and.
     &               abs(hwat_box_xcenter(1,co)-xwc1(j)).lt.8
     &               ) then
                     cowc1(j)=hwat_counter_num(i)
                     adcwc1p(j)=hwat_pos_npe(i)
                     adcwc1n(j)=hwat_neg_npe(i)
                     tdcwc1(j)=hwat_pos_time(i)
                     flagwc1=flagwc1+1
                  endif
               else if (la.eq.2.and.flagwc2.le.25) then
                  co=hwat_counter_num(i)
                  mhwc2(co)=mhwc2(co)+1
                  if (hwat_pos_time(i).gt.9000..and.
     &               hwat_pos_time(i).lt.15000..and.     
     &               abs(hwat_box_xcenter(2,co)-xwc2(j)).lt.8
     &               ) then
                     cowc2(j)=hwat_counter_num(i)
                     adcwc2p(j)=hwat_pos_npe(i)
                     adcwc2n(j)=hwat_neg_npe(i)
                     tdcwc2(j)=hwat_pos_time(i)
                     flagwc2=flagwc2+1
                  endif
               endif
            enddo

         
            do i=1,haer_tot_hits
               la=haer_layer_num(i)
               if (la.eq.1.and.flagac1.le.25) then
                  co=haer_counter_num(i)
                  mhac1(co)=mhac1(co)+1
                  if (
     &             abs(haer_box_xcenter(1,co)-xac1(j)).lt.12.5
     &            ) then
                     coac1(j)=haer_counter_num(i)
                     adcac1p(j)=haer_pos_npe(i)
                     adcac1n(j)=haer_neg_npe(i)
                     tdcac1p(j)=haer_pos_time(i)
                     tdcac1n(j)=haer_neg_time(i)
                     flagac1=flagac1+1
                  endif
               else if (la.eq.2.and.flagac2.le.25) then
                  co=haer_counter_num(i)
                  mhac2(co)=mhac2(co)+1
                  if (
     &             abs(haer_box_xcenter(2,co)-xac2(j)).lt.12.5
     &            ) then
                     coac2(j)=haer_counter_num(i)
                     adcac2p(j)=haer_pos_npe(i)
                     adcac2n(j)=haer_neg_npe(i)
                     tdcac2p(j)=haer_pos_time(i)
                     tdcac2n(j)=haer_neg_time(i)
                     flagac2=flagac2+1
                  endif
               else if (la.eq.3.and.flagac3.le.25) then
                  co=haer_counter_num(i)
                  mhac3(co)=mhac3(co)+1
                  if (
     &             abs(haer_box_xcenter(3,co)-xac3(j)).lt.12.5
     &            ) then
                     coac3(j)=haer_counter_num(i)
                     adcac3p(j)=haer_pos_npe(i)
                     adcac3n(j)=haer_neg_npe(i)
                     tdcac3p(j)=haer_pos_time(i)
                     tdcac3n(j)=haer_neg_time(i)
                     flagac3=flagac3+1
                  endif
               endif
            enddo
         
            do i=1,hluc_tot_hits
               la=hluc_layer_num(i)
               if (la.eq.1.and.flaglc.le.25) then
                  co=hluc_counter_num(i)
                  mhlc(co)=mhlc(co)+1
                  if (hluc_pos_npe(i).gt.10
     &             .and.hluc_neg_npe(i).gt.10
     &             .and.abs(hluc_box_xcenter(1,co)-xlc(j)).lt.10
     &               ) then
                     colc(j)=hluc_counter_num(i)
                     adclcp(j)=hluc_pos_npe(i)
                     adclcn(j)=hluc_neg_npe(i)
                     tdclcp(j)=hluc_pos_time(i)
                     tdclcn(j)=hluc_neg_time(i)
                     flaglc=flaglc+1
                  endif
               endif
            enddo
         enddo
         
            do i=1,gtrig_tot_hits
               la=gtrig_channel_num(i)
               if (mhgt(la).eq.0.and.gtrig_tdc(i).gt.0) then
                  tdcgt(la)=gtrig_tdc(i)
                  mhgt(la)=mhgt(la)+1
               endif
            enddo
              
         do i=1,pidtrk 
               m= 0
               m= m+1
               h_pid_ntuple_contents(m)= gen_run_number        ! 1
               m= m+1
               h_pid_ntuple_contents(m)= gen_event_ID_number   ! 2
               m= m+1
               h_pid_ntuple_contents(m)= co1x(i)                  ! 4
               m= m+1
               h_pid_ntuple_contents(m)= adc1xp(i)                ! 5
               m= m+1
               h_pid_ntuple_contents(m)= adc1xn(i)                ! 6
               m= m+1
               h_pid_ntuple_contents(m)= tdc1xp(i)                ! 7
               m= m+1
               h_pid_ntuple_contents(m)= tdc1xn(i)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= co1y                  ! 4
               m= m+1
               h_pid_ntuple_contents(m)= adc1yp                ! 5
               m= m+1
               h_pid_ntuple_contents(m)= adc1yn                ! 6
               m= m+1
               h_pid_ntuple_contents(m)= tdc1yp                ! 7
               m= m+1
               h_pid_ntuple_contents(m)= tdc1yn                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= co2x(i)                  ! 4
               m= m+1
               h_pid_ntuple_contents(m)= adc2xp(i)                ! 5
               m= m+1
               h_pid_ntuple_contents(m)= adc2xn(i)                ! 6
               m= m+1
               h_pid_ntuple_contents(m)= tdc2xp(i)                ! 7
               m= m+1
               h_pid_ntuple_contents(m)= tdc2xn(i)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= co2y                  ! 4
               m= m+1
               h_pid_ntuple_contents(m)= adc2yp                ! 5
               m= m+1
               h_pid_ntuple_contents(m)= adc2yn                ! 6
               m= m+1
               h_pid_ntuple_contents(m)= tdc2yp                ! 7
               m= m+1
               h_pid_ntuple_contents(m)= tdc2yn                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= coac1(i)                 ! 4
               m= m+1
               h_pid_ntuple_contents(m)= adcac1p(i)                ! 5
               m= m+1
               h_pid_ntuple_contents(m)= adcac1n(i)                ! 6
               m= m+1
               h_pid_ntuple_contents(m)= tdcac1p(i)                ! 7
               m= m+1
               h_pid_ntuple_contents(m)= tdcac1n(i)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= coac2(i)                 ! 4
               m= m+1
               h_pid_ntuple_contents(m)= adcac2p(i)                ! 5
               m= m+1
               h_pid_ntuple_contents(m)= adcac2n(i)                ! 6
               m= m+1
               h_pid_ntuple_contents(m)= tdcac2p(i)                ! 7
               m= m+1
               h_pid_ntuple_contents(m)= tdcac2n(i)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= coac3(i)                 ! 4
               m= m+1
               h_pid_ntuple_contents(m)= adcac3p(i)                ! 5
               m= m+1
               h_pid_ntuple_contents(m)= adcac3n(i)                ! 6
               m= m+1
               h_pid_ntuple_contents(m)= tdcac3p(i)                ! 7
               m= m+1
               h_pid_ntuple_contents(m)= tdcac3n(i)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= cowc1(i)                 ! 4
               m= m+1
               h_pid_ntuple_contents(m)= adcwc1p(i)                ! 5
               m= m+1
               h_pid_ntuple_contents(m)= adcwc1n(i)                ! 6
               m= m+1
               h_pid_ntuple_contents(m)= tdcwc1(i)                ! 7
               m= m+1
               h_pid_ntuple_contents(m)= cowc2(i)                 ! 4
               m= m+1
               h_pid_ntuple_contents(m)= adcwc2p(i)                ! 5
               m= m+1
               h_pid_ntuple_contents(m)= adcwc2n(i)                ! 6
               m= m+1
               h_pid_ntuple_contents(m)= tdcwc2(i)                ! 7
               m= m+1
               h_pid_ntuple_contents(m)= colc(i)                 ! 4
               m= m+1
               h_pid_ntuple_contents(m)= adclcp(i)                ! 5
               m= m+1
               h_pid_ntuple_contents(m)= adclcn(i)                ! 6
               m= m+1
               h_pid_ntuple_contents(m)= tdclcp(i)                ! 7
               m= m+1
               h_pid_ntuple_contents(m)= tdclcn(i)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= tdcgt(1)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= tdcgt(2)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= tdcgt(3)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= tdcgt(4)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= tdcgt(5)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= tdcgt(6)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= tdcgt(7)                ! 8
               m= m+1
               h_pid_ntuple_contents(m)= tdcgt(8)                ! 8
               
         
*     Fill ntuple for this event
               ABORT= .NOT.HEXIST(h_pid_ntuple_ID)
               IF(ABORT) THEN
                  call G_build_note(':Ntuple ID#$ does not exist',
     &               '$',h_pid_ntuple_ID,' ',0.,' ',err)
                  call G_add_path(here,err)
               ELSE
                  call HFN(h_pid_ntuple_ID,h_pid_ntuple_contents)
               ENDIF
         enddo

      elseif(h_debugnt(1).eq.1) then

*     Fill ntuple for this event
         ABORT= .NOT.HEXIST(h_pid_ntuple_ID)
         IF(ABORT) THEN
            call G_build_note(':Ntuple ID#$ does not exist',
     &           '$',h_pid_ntuple_ID,' ',0.,' ',err)
            call G_add_path(here,err)
         ELSE
              call HFNT(h_pid_ntuple_ID)
         ENDIF

      endif
*
      RETURN
      END      
