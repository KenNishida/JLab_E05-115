       SUBROUTINE H_LUCITE(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze lucite aerogel information for each track
*-
*-      Required Input BANKS     HKS_RAW_LUC
*-
*-      Output BANKS             HKS_DECODED_LUC
*-
*-   Output: ABORT           - always success
*-         : err             - never failure
*-
* $Log: h_lucite.f,v $
* Revision 1.1.1.1  2009/08/17 2:42:44  yez
* Add Lucite sum channels, to distinguish with software
* sum, I named the hardware sum as tot (=neg+pos)
* 
* Revision 1.1.1.1  2009/07/09 2:42:44  yez
* Convert Lucite info from Water Cerenkov 
*
* Revision 1.4  2005/08/04 22:23:27  cdaq
* Mod. to remove double counts of AC/WC due to multihits in TDC
*
* Revision 1.3  2005/07/29 17:50:20  cdaq
* Add init. of both_hits
*
* Revision 1.2  2005/07/08 13:47:07  sumihama
* Mod. AC hist
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.3  2005/04/08 20:40:06  miyoshi
* add calling subroutine
*
* Revision 1.2  2004/12/24 19:47:07  miyoshi
* change minor
*
* Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
* new dir
*
* Revision 2.1  2000/03/12 17:44:21  jinghua
* (JLiu) Added tracking info
*
* Revision 2.0  2000/02/18 10:09:45  jinghua
* (JLiu) a brand new version
*
* Revision 1.1  1996/11/07 19:50:56  saw
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*8 here
      parameter (here= 'H_LUCITE')
*
      logical ABORT
      character*(*) err
*
     
      integer*4 ind, npmt, trk, hit, gh,i
      integer*4 layer,counter,la,co
      integer*4 tdcpos,tdcneg,tdctot,adcpos,adcneg,adctot
      integer*4 have_tdc,num,numsum
      real*4 npepos,npeneg,npetot

      real*4 xhit_coord, yhit_coord
      real*4 f1timeH,f1timeL,ttimeH, ttimeL 
*
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'hks_pedestals.cmn'
      INCLUDE 'hks_lucite_parms.cmn'
      INCLUDE 'hks_scin_tof.cmn' ! for start time center
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_event_info.cmn' ! matsu
      include 'gen_f1tdc.cmn'
      include 'gen_rocid.cmn'
      integer*4 counter_hits(HNUM_LUC_LAYERS,HNUM_LUC_COUNTERS)
      integer*4 countersum_hits(HNUM_LUC_LAYERS,HNUM_LUC_COUNTERS)

*
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '


      if(hluc_raw_tot_hits .le. 0 .and. hlucsum_raw_tot_hits .le. 0)
     $     Return
      

      hluc_tot_hits = 0
      hlucsum_tot_hits = 0

      gh = 0
      
*     initialize layer and counter hits
      Do la=1,HNUM_LUC_LAYERS
         hluc_raw_layer_hits(la) = 0
         hluc_layer_hits(la) = 0 
         hlucsum_raw_layer_hits(la) = 0
         hlucsum_layer_hits(la) = 0 
         Do co=1,HNUM_LUC_COUNTERS
            counter_hits(la,co) = 0
            countersum_hits(la,co) = 0
         EndDo
      EndDo

*     if the parameters are missing, set the largest value.
c      if(hluc_tdc_pos_max .eq. 0) then
c         hluc_tdc_pos_max = 8000
c      EndIf
c      if(hluc_tdc_neg_max .eq. 0) then
c         hluc_tdc_neg_max = 8000
c      EndIf
      
      num = 0
      numsum = 0

      ttimeH=g_f1_trigger_time(HR_VME_ROCID)
      ttimeL=g_f1_trigger_time(LR_VME_ROCID)
      f1timeH=g_f1_refer_time(HR_VME_ROCID,1,1)
      f1timeL=g_f1_refer_time(LR_VME_ROCID,1,1)

c     --- Correct reference TDCs for clock rollover
      if(f1timeH.lt.ttimeH) then ! HighRes reference rollover
         f1timeH = f1timeH+f1tdc_HR_gate_width
      endif
      if(f1timeL.lt.ttimeL) then ! LowRes reference rollover
         f1timeL = f1timeL+f1tdc_LR_gate_width
      endif

c     --- Lucite CP/TOF Signals ---
      Do i=1,hluc_raw_tot_hits
         hluc_rawtdc_pos_sub_trig(i) = -70000 ! Init rollover and reference corrected TDC
         hluc_rawtdc_neg_sub_trig(i) = -70000
         la = hluc_raw_layer_num(i)
         co = hluc_raw_counter_num(i)
         tdcpos = hluc_rawtdc_pos(i)
         tdcneg = hluc_rawtdc_neg(i)
         
         if(tdcpos.gt.0 .or. tdcneg.gt.0) then
            num = num + 1       ! Counter for good raw TDC hits
c     --- pos 
            if(tdcpos .gt. -70000) then
               if(tdcpos.lt.ttimeH) then ! tdcpos rollover 
                  tdcpos = tdcpos+f1tdc_HR_gate_width
               endif
               hluc_rawtdc_pos_sub_trig(i) = 
     &              tdcpos-f1timeH+1444/f1tdc_HR_tdc_to_time
            EndIf               ! tdcpos>-70000
c     --- neg
            if(tdcneg .gt. -70000) then
               if(tdcneg.lt.ttimeH) then ! tdcneg rollover
                  tdcneg = tdcneg+f1tdc_HR_gate_width
               endif
               hluc_rawtdc_neg_sub_trig(i) = 
     &            tdcneg-f1timeH+1444/f1tdc_HR_tdc_to_time
            EndIf               ! tdcneg>-70000
            
         endif                  ! good trigger time
      enddo                     ! hluc_raw_tot_hits loop 
      
c     --- Lucite PID Signals ---
      Do i=1,hlucsum_raw_tot_hits
         hluc_rawtdc_tot_sub_trig(i) = -70000 ! Init rollover and reference corrected TDC
         la = hlucsum_raw_layer_num(i)
         co = hlucsum_raw_counter_num(i)
         tdctot = hluc_rawtdc_tot(i)
         if(tdctot .gt. -70000) then
            numsum = numsum + 1 ! Counter for good raw TDC hits
            if(tdctot.lt.ttimeL) then ! tdctot rollover 
               tdctot = tdctot+f1tdc_LR_gate_width
            endif
            hluc_rawtdc_tot_sub_trig(i) = 
     &           tdctot-f1timeL+1444/f1tdc_LR_tdc_to_time
         EndIf                  ! tdctot>-70000
      enddo                     ! hluc_raw_tot_hits loop 

*     fill the histogram as needed
      Call h_fill_lucite_raw_hist(ABORT,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

      if(num .eq. 0) then
         hluc_raw_tot_hits = 0 
         return
      endif                   ! good trigger signal

      do ind = 1,HLUC_RAW_TOT_HITS
         la = hluc_raw_layer_num(ind)
         co = hluc_raw_counter_num(ind)
         tdcpos = hluc_rawtdc_pos_sub_trig(ind)
         tdcneg = hluc_rawtdc_neg_sub_trig(ind)
         adcpos= hluc_rawadc_pos(ind)
         adcneg= hluc_rawadc_neg(ind)

c     --- Calculate Npe no matter what!
         npepos = (adcpos-hluc_pos_ped_mean(la,co))/hluc_pos_gain(la,co)
         npeneg = (adcneg-hluc_neg_ped_mean(la,co))/hluc_neg_gain(la,co)
         
         hluc_raw_layer_hits(la) = hluc_raw_layer_hits(la) + 1
         
         if(counter_hits(la,co).eq.0)then ! this will only keep 1 hit per counter, right??
            hluc_tot_hits = hluc_tot_hits + 1
            gh = hluc_tot_hits
            counter_hits (la,co) = gh
            hluc_both_hits(gh) = 0
            hluc_layer_num(gh) = la
            hluc_counter_num(gh) = co
            hluc_pos_npe(gh) = 0.
            hluc_neg_npe(gh) = 0.
            hluc_npe_sum(gh) = 0.
            hluc_pos_time(gh) = -1
            hluc_neg_time(gh) = -1
            hluc_layer_hits(la) = hluc_layer_hits(la) + 1
         endif
         
C     ADCs have been evaluated. Now check for valid TDC hits. 


*     --- the positive tube
         if(tdcpos .gt. HLUC_TDC_POS_MIN .and. 
     &        tdcpos .lt. HLUC_TDC_POS_MAX) then
            hluc_pos_npe(counter_hits(la,co)) = npepos
            hluc_npe_sum(counter_hits(la,co)) = npepos         ! chiba 2010/10/20
*     &           hluc_npe_sum(counter_hits(la,co)) + npepos
            hluc_pos_time(counter_hits(la,co)) = 
     &           tdcpos*hluc_tdc_to_time
     &           -hluc_pos_time_offset(la,co) 
         endif                  ! Good pos TDC time
         
*     --- the negative tube
         if(tdcneg .gt. HLUC_TDC_NEG_MIN .and. 
     &        tdcneg .lt. HLUC_TDC_NEG_MAX) then
            hluc_neg_npe(counter_hits(la,co)) = npeneg
            hluc_npe_sum(counter_hits(la,co)) = npeneg         ! chiba 2010/10/20
*     &           hluc_npe_sum(counter_hits(la,co)) + npeneg
            hluc_neg_time(counter_hits(la,co)) =  
     &           tdcneg*hluc_tdc_to_time
     &           -hluc_neg_time_offset(la,co) 
         endif                  ! Good neg TDC time
      EndDo                     ! Loop over hluc_raw_tot_hits

*     --- both tubes have hits
      
      if((tdcpos .gt. HLUC_TDC_POS_MIN .and. 
     &     tdcpos .lt. HLUC_TDC_POS_MAX) 
     &     .AND.
     &     (tdcneg .gt. HLUC_TDC_NEG_MIN .and. 
     &     tdcneg .lt. HLUC_TDC_NEG_MAX)
     &     ) then
         hluc_both_hits(counter_hits(la,co)) = 1
         hluc_npe_sum(counter_hits(la,co)) = npepos + npeneg ! chiba 2010/10/20
      EndIF
      
      do ind = 1,HLUCSUM_RAW_TOT_HITS
         la = hlucsum_raw_layer_num(ind)
         co = hlucsum_raw_counter_num(ind)
         tdctot = hluc_rawtdc_tot_sub_trig(ind)
         adctot= hluc_rawadc_tot(ind)

c     --- Calculate Npe no matter what!
         npetot = (adctot-hluc_tot_ped_mean(la,co))/hluc_tot_gain(la,co)
         
         hlucsum_raw_layer_hits(la) = hlucsum_raw_layer_hits(la) + 1
         
         if(countersum_hits(la,co).eq.0)then
            hlucsum_tot_hits = hlucsum_tot_hits + 1
            gh = hlucsum_tot_hits
            countersum_hits (la,co) = gh
            hlucsum_layer_num(gh) = la
            hlucsum_counter_num(gh) = co
            hluc_tot_npe(gh) = 0.
            hluc_tot_time(gh) = -1
            hlucsum_layer_hits(la) = hlucsum_layer_hits(la) + 1
         endif    
         
C     ADCs have been evaluated. Now check for valid TDC hits. 

         if(tdctot .gt. HLUC_TDC_TOT_MIN .and. 
     &        tdctot .lt. HLUC_TDC_TOT_MAX) then
            hluc_tot_npe(countersum_hits(la,co)) = npetot
            hluc_tot_time(countersum_hits(la,co)) = 
     &           tdctot*hluc_tdc_to_time*2.0
     &           -hluc_pos_time_offset(la,co) 
         endif                  ! Good tot TDC time
      EndDo                     ! Loop over hlucsum_raw_tot_hits

*     fill the raw histogram as needed
      call h_fill_lucite_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

      return
      end
