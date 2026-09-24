      subroutine g_analyze_trig(abort,errmsg)
*-------------------------------------------------------------------
* author: Miyoshi
* created: 6/9/05
*
* g_analyze_trig just cut trig hits by tdc window.
*
*--------------------------------------------------------

      implicit none

      include 'gen_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_event_info.cmn'
      include 'gen_f1tdc.cmn'
      include 'gen_rocid.cmn'

      logical abort
      character*(*) errmsg
      character*20 here
      parameter (here = 'g_analyze_trig')
      Integer*4 i,j,k,tdcmin,tdcmax
      Integer*4 local_trig_debug
      real*4 ttime,f1time,tdc

      save
      
      abort = .false.
      errmsg = ' '

      local_trig_debug = 0

c     --- reset flag
      gtrig_flag_hes=0
      gtrig_flag_kaonsum=0
      gtrig_flag_kaonseg1=0
      gtrig_flag_kaonseg2=0
      gtrig_flag_kaonseg3=0
      gtrig_flag_kaonseg4=0
      gtrig_flag_kaonseg5=0
      gtrig_flag_kaonseg6=0
      gtrig_flag_prepion=0
      gtrig_flag_unbiased=0
      gtrig_flag_hks1=0
      gtrig_flag_main=0
      gtrig_flag_hks2=0
      gtrig_flag_kaonor=0

c---------------------------------------------------------------

      Call HF1(gidtrigrawtothits,float(gtrig_raw_tot_hits),1.)
      if(gtrig_raw_tot_hits .le .0) Return
      
      tdcmin = gtrig_tdc_min
      tdcmax = gtrig_tdc_max
      if(tdcmax .eq. 0) tdcmax = 10000
      gtrig_tot_hits = 0

      Do i = 1, gtrig_raw_tot_hits
         
         ttime = g_f1_trigger_time(LR_VME_ROCID)
         f1time = g_f1_refer_time(LR_VME_ROCID,1,1)
         tdc=gtrig_raw_tdc(i)
         
         Call HF1(gidtrigrawchannels,float(gtrig_raw_channel_num(i)),1.)
         Call HF1(gidtrigrawtdc,float(gtrig_raw_tdc(i)),1.)
          
         if(tdc .gt.0) then
            if (tdc.lt.ttime) then
               tdc=tdc+f1tdc_LR_gate_width
            endif
            if (f1time.lt.ttime) then
               f1time=f1time+f1tdc_LR_gate_width
            endif
            gtrig_tdc(gtrig_tot_hits) = tdc-f1time
     &        +1444/f1tdc_LR_tdc_to_time
            gtrig_tot_hits = gtrig_tot_hits + 1
            gtrig_channel_num(gtrig_tot_hits) = gtrig_raw_channel_num(i)
            Call HF1(gidtrigchannels,float(gtrig_channel_num(i)),1.)
c            write(*,*) gtrig_channel_num(gtrig_tot_hits)
c     &       ,gtrig_tdc(gtrig_tot_hits)

c     --- full trigger flag
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 1) then
               gtrig_flag_hes = gtrig_tdc(gtrig_tot_hits)
c               gtrig_flag_hes = tdc
               Call HF1(gidtrigtdc1,float(gtrig_tdc(gtrig_tot_hits)),1.)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 2) then
               gtrig_flag_kaonsum = gtrig_tdc(gtrig_tot_hits)
c               gtrig_flag_kaonsum = tdc
               Call HF1(gidtrigtdc2,float(gtrig_tdc(gtrig_tot_hits)),1.)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 3) then
               gtrig_flag_kaonseg1 = gtrig_tdc(gtrig_tot_hits)
               Call HF1(gidtrigtdc3,float(gtrig_tdc(gtrig_tot_hits)),1.)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 4) then
               gtrig_flag_kaonseg2 = gtrig_tdc(gtrig_tot_hits)
               Call HF1(gidtrigtdc4,float(gtrig_tdc(gtrig_tot_hits)),1.)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 5) then
               gtrig_flag_kaonseg3 = gtrig_tdc(gtrig_tot_hits)
               Call HF1(gidtrigtdc5,float(gtrig_tdc(gtrig_tot_hits)),1.)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 6) then
               gtrig_flag_kaonseg4 = gtrig_tdc(gtrig_tot_hits)
c               write(*,*) float(gtrig_tdc(gtrig_tot_hits)) 
               Call HF1(gidtrigtdc6,float(gtrig_tdc(gtrig_tot_hits)),1.)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 7) then
               gtrig_flag_kaonseg5 = gtrig_tdc(gtrig_tot_hits)
               Call HF1(gidtrigtdc7,float(gtrig_tdc(gtrig_tot_hits)),1.)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 8) then
               gtrig_flag_kaonseg6 = gtrig_tdc(gtrig_tot_hits)
               Call HF1(gidtrigtdc8,float(gtrig_tdc(gtrig_tot_hits)),1.)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 9) then
               gtrig_flag_prepion = gtrig_tdc(gtrig_tot_hits)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 10) then
               gtrig_flag_unbiased = gtrig_tdc(gtrig_tot_hits)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 11) then
               gtrig_flag_hks1 = gtrig_tdc(gtrig_tot_hits)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 12) then
               gtrig_flag_main = gtrig_tdc(gtrig_tot_hits)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 13) then
               gtrig_flag_hks2 = gtrig_tdc(gtrig_tot_hits)
            EndIf
            if(gtrig_channel_num(gtrig_tot_hits) .eq. 14) then
               gtrig_flag_kaonor = gtrig_tdc(gtrig_tot_hits)
            EndIf
         EndIf
      EndDo
      Call HF1(gidtrigtothits,float(gtrig_tot_hits),1.)
      if (gtrig_flag_hes.gt.0.and.gtrig_flag_kaonsum.gt.0)then
         Call HF1(gidtrigcoin,float(gtrig_flag_kaonsum-gtrig_flag_hes),1.)
      endif

      if(local_trig_debug .eq. 1) then
         Write(*,*) '(ganatrig)EV=',gen_event_ID_number,' TRIGGER#='
         Write(*,*) '-HES=',gtrig_flag_hes
         Write(*,*) '-KAONSUM=',gtrig_flag_kaonsum
         Write(*,*) '-KAONSEG1=',gtrig_flag_kaonseg1
         Write(*,*) '-KAONSEG2=',gtrig_flag_kaonseg2
         Write(*,*) '-KAONSEG3=',gtrig_flag_kaonseg3
         Write(*,*) '-KAONSEG4=',gtrig_flag_kaonseg4
         Write(*,*) '-KAONSEG5=',gtrig_flag_kaonseg5
         Write(*,*) '-KAONSEG6=',gtrig_flag_kaonseg6
         Write(*,*) '-PREPION=',gtrig_flag_prepion
         Write(*,*) '-UNBIASED=',gtrig_flag_unbiased
         Write(*,*) '-HKS1=',gtrig_flag_hks1
         Write(*,*) '-MAIN=',gtrig_flag_main
         Write(*,*) '-HKS2=',gtrig_flag_hks2
         Write(*,*) '-KAONOR=',gtrig_flag_kaonor
      EndIf  

      return
      end
