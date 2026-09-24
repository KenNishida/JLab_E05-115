      Subroutine e_trans_scin(ABORT,err)
*--------------------------------------------------------
*     Translate HODOSCOPE raw data to decoded information
*     
*     $Log: e_trans_scin.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.15  2005/09/08 17:00:08  cdaq
*     Change for Ehodo 2-10 and 1-16
*
*     Revision 1.14  2005/08/23 22:03:13  sumihama
*     Mod. for the 2-8 counter
*
*     Revision 1.13  2005/08/21 21:48:18  cdaq
*     Change a sign of t0
*
*     Revision 1.12  2005/07/09 02:11:46  cdaq
*     Mod. rf calc.
*
*     Revision 1.11  2005/07/08 15:34:35  sumihama
*     Mod. for rf
*
*     Revision 1.10  2005/07/08 13:45:08  sumihama
*     Mod. Max hit number
*
*     Revision 1.9  2005/07/07 20:00:31  sumihama
*     Delte print
*
*     Revision 1.8  2005/07/06 19:20:40  sumihama
*     Mod.hrf/erf
*
*     Revision 1.7  2005/07/03 11:14:12  sumihama
*     Add Factor for 57ps/60ps
*
*     Revision 1.6  2005/06/26 03:31:52  cdaq
*     escin mod.
*
*     Revision 1.5  2005/06/12 07:14:02  cdaq
*     Mod. NO trigger case
*
*     Revision 1.4  2005/06/05 02:00:35  cdaq
*     do nothing when tdc has initial value
*
*     Revision 1.3  2005/05/28 00:43:03  cdaq
*     change/add f1trigtime hist
*
*     Revision 1.2  2005/05/23 17:15:12  cdaq
*     scan tdc to comment out trigger
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.13  2005/04/19 17:49:03  miyoshi
*     remove small bug
*
*     Revision 1.12  2005/04/08 19:04:00  miyoshi
*     precheck position is moved
*     
*     Revision 1.11  2005/02/19 16:40:25  sumihama
*     Mod. Method of ehodo-multihit
*     
*     Revision 1.10  2005/02/10 19:09:05  miyoshi
*     delete adc info for cut condition
*     
*     Revision 1.9  2005/02/02 20:06:14  miyoshi
*     fix bug for tdc dec neg value
*     
*     Revision 1.8  2005/01/24 19:58:37  miyoshi
*     correct bugs
*     
*     Revision 1.7  2005/01/14 22:53:27  miyoshi
*     change default cut condition and change adc correction
*     
*     Revision 1.6  2005/01/06 23:27:09  sumihama
*     Mod of escin & escin
*     
*     Revision 1.5  2005/01/05 23:37:39  sumihama
*     Mod. of escin and escin
*     
*     Revision 1.4  2005/01/05 20:11:05  sumihama
*     h_trans updata
*     
*     Revision 1.3  2005/01/05 18:41:00  sumihama
*     hes-scinti mod.
*     
*     Revision 1.2  2004/12/24 21:29:52  miyoshi
*     add tdc variables to read f1tdc
*     
*     Revision 1.1.1.1  2004/08/30 21:21:41  miyoshi
*     new dir
*     
*     
*     Revision 1.4 03/19/2004 Miyoshi
*     for E01-011
*     
*     Revision 1.3  2000/03/09 01:32:42  ysato
*     Update in the production run Mar.8
*     
*     Revision 1.2  2000/02/11 20:44:08  ysato
*     Local update on KTRACKING
*     
*     Revision 1.1  1999/12/23 19:59:29  ysato
*     Compiled on Redhat Linux
*     
*     
*     Novermber 4, 1999        Y.Sato       A first draft
*     
*     
*     Input Banks    hnss_raw_scin
*     Output Banks   hnss_decoded_scin
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_trans_scin')
*     
      Logical ABORT
      Character*(*) err
      
      Include 'hes_data_structures.cmn'
      Include 'hes_scin_parms.cmn'
      Include 'gen_event_info.cmn'
      Include 'gen_run_info.cmn'
      Include 'gen_rocid.cmn'
      Include 'gen_f1tdc.cmn'
      Include "hes_id_histid.cmn"
      
      Integer*4 ihit,nhit,i,j,k,num
      Integer*4 la,co,good(ENUM_SCIN_LAYERS,ENUM_SCIN_COUNTERS)
      Integer*4 tmin,tmax,amin,amax
      Real*4    adcpos, adcneg, minph
      Real*4    timemin, timemax 
      Real*4    timepos, timeneg
      Integer*4 tdcpos, tdcneg
      Integer*4 temp_rocid,have_tdc
      real*4 f1time,ttime
      
      ABORT= .FALSE.
      err= ' '
     

c      Write(*,*) 'ev,nscin=',gen_event_ID_number,escin_raw_tot_hits
     
c      if(gen_run_number .lt. 5999) then
c         temp_rocid = 1
c         f1time  =-1. !for FB DK
c         escin_f1tdc_gate_width = 0. ! for FB DK
c      Else
c         f1time = g_f1_trigger_time(HR_VME_ROCID,1,1)
c      EndIF
      if(escin_raw_tot_hits .le. 0) return
      
      

c     --- tdc scan
c      have_tdc = 0
c      Do i=1,escin_raw_tot_hits
c         tdcpos = escin_rawtdc_pos(i)
*         write(*,*) "tdcpos=",tdcpos
c         tdcneg = escin_rawtdc_neg(i)
c          if((tdcpos.gt.0.or.tdcneg.gt. 0)) then 
c            have_tdc = 1
c         EndIf
c      EndDo
c--   in EEL test, VMEID is 1. In Hall C, this may be changed to 2.

      num=0
      Do i=1,escin_raw_tot_hits
         ttime = g_f1_trigger_time(HR_VME_ROCID)
         f1time = g_f1_refer_time(HR_VME_ROCID,1,1)
         la = escin_raw_layer_num(i)
         co = escin_raw_counter_num(i)
         tdcpos = escin_rawtdc_pos(i)
         tdcneg = escin_rawtdc_neg(i)
         if(tdcpos.gt.0.or.tdcneg.gt.0) then 
c     --- -70000 is initial value.
c     --- pos
            if(tdcpos .le. -70000) then
               escin_rawtdc_pos_sub_trig(i) = -70000
            Else
               if(tdcpos.lt.ttime) then
                  tdcpos = tdcpos+escin_f1tdc_gate_width
               endif
               if(f1time.lt.ttime) then
                  f1time = f1time+escin_f1tdc_gate_width
               endif
               escin_rawtdc_pos_sub_trig(i) = 
     &            tdcpos-f1time+1444/escin_tdc_to_time
            EndIf            ! tdcpos<=-70000
c     --- neg
            if(tdcneg .le. -70000) then
               escin_rawtdc_neg_sub_trig(i) = -70000
            Else
               if(tdcneg.lt.ttime) then
                  tdcneg = tdcneg+escin_f1tdc_gate_width
               endif
               if(f1time.lt.ttime) then
                  f1time = f1time+escin_f1tdc_gate_width
               endif
               escin_rawtdc_neg_sub_trig(i) = 
     &            tdcneg-f1time+1444/escin_tdc_to_time
            EndIf
            num = num + 1
c               Write(*,*) 'ev,f1time,la,co,t1,t2,a1,a2=',gen_event_id_number,
c     &              f1time,
c     &              escin_raw_layer_num(i),escin_raw_counter_num(i),
c     &              escin_rawtdc_pos(i),
c     &              escin_rawtdc_neg(i),
c     &              escin_rawtdc_pos_sub_trig(i),
c     &              escin_rawtdc_neg_sub_trig(i),
c     &              escin_rawadc_pos(i),
c     &              escin_rawadc_neg(i)
         else 
            escin_rawtdc_pos_sub_trig(i) = -70000
            escin_rawtdc_neg_sub_trig(i) = -70000
         EndIF                  ! good trigger time
      EndDo               ! escin_raw_tot_hits loop

*     --- Histogram for raw scin bank ---
      call e_fill_scin_raw_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

      if(  num .eq. 0) then
c         Write(*,*) 'No ESCIN trigger signal : ev=',
c     &        gen_event_ID_number, ' Number of events=', num
         escin_raw_tot_hits = 0 
         return
      endif
      
c      do i=1,29
c         write(*,*) escin_pos_time_offset(1,i),
c     &      escin_pos_time_offset(2,i)
c      enddo
c      do i=1,29
c         write(*,*) escin_neg_time_offset(1,i),
c     &      escin_neg_time_offset(2,i)
c      enddo
      
      
*     --- pre-check ---
      if(escin_raw_tot_hits.le.0) return
      
*     --- reset local variables
      Do ihit=1,EMAX_SCIN_DEC_HITS
         escin_good_hits(ihit) = 0
      EndDo
      
      Do ihit=1,ENUM_SCIN_COUNTERS
         good(1,ihit) = 0
         good(2,ihit) = 0
      EndDo
      
      amin = escin_adcpos_min
      amax = escin_adcpos_max
      tmin = escin_tdcpos_min
      tmax = escin_tdcpos_max
      
      if(amin .le. 0) amin = 0.   
      if(amax .le. 0) amax = 8000.
      if(tmin .le. 0) tmin = 0.   
      if(tmax .le. 0) tmax = 8000.

      timemin=-100.
      timemax=100.

*     --- Hodoscope raw ADC and TDC cut ---
      do ihit = 1, escin_raw_tot_hits
         la = escin_raw_layer_num(ihit)
         co = escin_raw_counter_num(ihit)
         adcpos = real(escin_rawadc_pos(ihit)) 
     &        - escin_pos_ped_mean(la,co)
         adcneg = real(escin_rawadc_neg(ihit)) 
     &        - escin_neg_ped_mean(la,co)
         tdcpos = escin_rawtdc_pos_sub_trig(ihit)
         tdcneg = escin_rawtdc_neg_sub_trig(ihit)

ccc RF signals           
c         if(la.eq.3.and.co.eq.2) then            
c            if(tdcpos .gt. -70000.and.tdcpos .lt. -50000.) then
c               esrftime = tdcpos*escin_tdc_to_time
c               if(tdcneg .gt. -70000.and.tdcneg .lt. -50000.) then
c                  esrfdiff = (tdcpos - tdcneg)*escin_tdc_to_time
c               else
c                  esrfdiff = -100
c               endif
c            elseif(tdcneg .gt. -70000.and.tdcneg .lt. -50000.
c     +              .and.esrftime.eq.-70000) then
c               esrfdiff = -100
c               esrftime = tdcneg*escin_tdc_to_time
c            elseif(esrftime.eq.-70000) then
c               esrfdiff = -70000
c               esrftime = -70000
c            endif
c            
c            Call HF1(eidsrfdiff,esrfdiff,1.)
c            Call HF1(eidsrftime,esrftime,1.)
ccc HODO
c         else
            
            if(la.eq.3) then
               tmin = escin_tdcneg_min
               tmax = escin_tdcneg_max  
            else
               tmin = escin_tdcpos_min
               tmax = escin_tdcpos_max            
            endif
            
*     --- if one end has a hit, keep the hit.
*     --- if both ends have hits, good hits = 1.

            if(
     &         ( (escin_miss_channel(la,co).eq.0).and.
     &           (tdcpos .gt. tmin .and. tdcpos .lt. tmax).and. 
     &           (tdcneg .gt. tmin .and. tdcneg .lt. tmax))
     &           .OR.
     &         ( (escin_miss_channel(la,co).eq.1).and.
     &           (tdcpos .gt. tmin .and. tdcpos .lt. tmax) )
     &           .OR.
     &         ( (escin_miss_channel(la,co).eq.2).and.
     &           (tdcneg .gt. tmin .and. tdcneg .lt. tmax) )
     &        ) then
               escin_tot_hits = escin_tot_hits + 1
               nhit = escin_tot_hits
               escin_layer_num(nhit)   = la
               escin_counter_num(nhit) = co
               escin_good_hits(nhit) = 1
c               write(*,*)"escin_layer_num(nhit)",escin_layer_num(nhit)
            
            
c     --- if you already have hit and it is one-end hit, do not add new.
               if(good(la,co) .eq. 1 .and. 
     &            escin_good_hits(nhit) .ne. 1) then
                  escin_tot_hits = escin_tot_hits - 1               
                  goto 100
               endif
            
               if(
     &            tdcpos .gt. tmin .and. tdcpos .lt. tmax
     &         ) then
                  escin_adc_pos(nhit)  = adcpos
                  escin_tdc_pos(nhit)  = tdcpos
                  minph = escin_adc_pos(nhit) - escin_pos_minph(la,co)
                  if(minph .gt. 0) then
                     escin_time_pos(nhit) = (tdcpos
     &                  - estart_time_center 
     &                  + escin_pos_phc_coeff(la,co)/sqrt(minph+200))
c     &                  + 2*escin_pos_phc_coeff(la,co)/sqrt(minph))
     &                  *escin_tdc_to_time
     &                  - escin_pos_time_offset(la,co)
c                  write(*,*) estart_time_center,escin_pos_time_offset(la,co),
c     =                 escin_pos_phc_coeff(la,co),escin_pos_phc_coeff(la,co)
c                   write(*,*) "pos",la,escin_time_pos(nhit) 
                  Else
                     escin_time_pos(nhit) = tdcpos*escin_tdc_to_time
     &                  - estart_time_center 
     &                  - escin_pos_time_offset(la,co)
                  EndIf
               Else                ! not a good hit
                  escin_adc_pos(nhit) = -1
                  escin_tdc_pos(nhit) = -70000
                  escin_time_pos(nhit) = -1000.
               EndIF               ! positive tubes.
            
*     --- for negative tubes
               if(
     &            tdcneg .gt. tmin .and. tdcneg .lt. tmax
     &            ) then
               
                  escin_adc_neg(nhit)  = adcneg
                  escin_tdc_neg(nhit)  = tdcneg
                  minph = escin_adc_neg(nhit) - escin_neg_minph(la,co)
                  if(minph .gt. 0) then
                     escin_time_neg(nhit) = (tdcneg
     &                  - estart_time_center 
     &                  + escin_neg_phc_coeff(la,co)/sqrt(minph+200))
c     &                  + 2*escin_neg_phc_coeff(la,co)/sqrt(minph))
     &                  *escin_tdc_to_time
     &                  - escin_neg_time_offset(la,co)
c                   write(*,*) "neg",la,escin_time_neg(nhit) 
                  Else
                     escin_time_neg(nhit) = tdcneg*escin_tdc_to_time
     &                  - estart_time_center 
     &                  - escin_neg_time_offset(la,co)
                  EndIf
               Else                ! not a good hit
                  escin_adc_neg(nhit) = -1
                  escin_tdc_neg(nhit) = -70000
                  escin_time_neg(nhit) = -1000.
               EndIf               ! negative tubes.

               timepos=escin_time_pos(nhit)
               timeneg=escin_time_neg(nhit)
            
*     --- calculate mean time.
               if(
     &            (timepos.ge.timemin.and.timepos.le.timemax) 
     &            .AND.
     &            (timeneg.ge.timemin.and.timeneg.le.timemax) 
     &            ) then
                     escin_mean_time(nhit) = 0.5 *( escin_time_pos(nhit) + 
     &               escin_time_neg(nhit))
               Else if(adcpos .ge. amin .and. adcpos .le. amax .and.
     &            timepos .ge. timemin .and. timepos .le. timemax) then
                     escin_mean_time(nhit) = escin_time_pos(nhit)
               Else
                  escin_mean_time(nhit) = escin_time_neg(nhit)
               EndIF               ! mean time.
c               print *, "la,co,mean,pos,neg",la,co,
c     &          escin_mean_time(nhit),escin_time_pos(nhit),escin_time_neg(nhit)

c     --- fill 'good' here.
c     --- good hits<0, This is one-end hit
                  if(escin_good_hits(nhit).eq.1) good(la,co)=1
            EndIF                  ! one tube has hits.      
c         endif      
 100  continue
      if (escin_tot_hits.ge.emax_scin_dec_hits) goto 200
      enddo                     ! raw hit loop
 200  continue
cc 200  write(*,*) 'Max ESCIN-hits',emax_scin_dec_hits,gen_event_ID_number
 
*     --- Decoded data filling ---
      call e_fill_scin_dec_hist(ABORT,err)
      
*     --- Debug print ---
      If (edumpdecodedscin .ne. 0) 
     &     call e_prt_dec_scin(ABORT,err)

      Return
      end
