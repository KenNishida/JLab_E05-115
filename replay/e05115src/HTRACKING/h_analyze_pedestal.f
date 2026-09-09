      subroutine h_analyze_pedestal(ABORT,err)
*********************************************************************     
* File: e05115src/HTRACKING/h_analyze_pedestal.f
*
* Date: 10/07/09 Add calls to pedestal histograms (JR)
*
* Credits: Original file had contributions from numerous folks,
*     among others Zhihong Ye, Daisuke Kawama, Mizuki Sumihama, Miyoshi,
*     Jinhua, csa, Steve Wood, John Arrington.
*
* WARNING: The *sum and *sum2 variables are currently defined as
*     Integer*4 in hks_pedestals.cmn. Thus, the largest value they can
*     hold is (2**31)-1 = 2,147,483,647. This will impose a limit on how
*     many pedestal events can be handled by the code.  E.g. for an
*     average pedestal of 1000. the sum2 would be limited to 2147
*     events.
*********************************************************************
      implicit none
      save
*     
      character*18 here
      parameter (here='h_analyze_pedestal')
*     
      logical ABORT
      character*(*) err
*     
      integer*4 ihit
      integer*4 pln,cnt
      integer*4 row,col
*     
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'hks_pedestals.cmn'
*     
      integer*4 hsci_cnt_hits(HNUM_SCIN_LAYERS,HNUM_SCIN_ELEMENTS)
      integer*4 haer_cnt_hits(HNUM_AER_LAYERS,HNUM_AER_COUNTERS)
      integer*4 haer_pln_hits(HNUM_AER_LAYERS)
      integer*4 hwat_cnt_hits(HNUM_WAT_LAYERS,HNUM_WAT_COUNTERS)
      integer*4 hluc_cnt_hits(HNUM_LUC_LAYERS,HNUM_LUC_COUNTERS)
      integer*4 hlucsum_cnt_hits(HNUM_LUC_LAYERS,HNUM_LUC_COUNTERS)

      integer*4 adcpos,adcneg

      real*4 histval
      include 'hks_id_histid.cmn'          

*      write(*,*) 'ENTER analyse pedestal'

*     
*     HODOSCOPE PEDESTALS
*     

*     clear hit counter
      Do pln=1,HNUM_RAW_SCIN_LAYERS
         Do cnt=1,HNUM_SCIN_ELEMENTS
            hsci_cnt_hits(pln,cnt)=0
         EndDo
      EndDo 

      do ihit = 1 , hscin_raw_tot_hits ! Loop over RAW hits
         pln = hscin_raw_layer_num(ihit)
         cnt = hscin_raw_counter_num(ihit)
         hsci_cnt_hits(pln,cnt) = hsci_cnt_hits(pln,cnt) + 1
         if(hsci_cnt_hits(pln,cnt).eq.1)then ! Use 1st hit only; ignore multiple hits due to MHTDC
*     --- ped.pos
            if (hscin_raw_adc_pos(ihit) .le. 
     $           hscin_pos_ped_limit(pln,cnt)) then ! Ignore large values

               histval = float(hscin_raw_adc_pos(ihit))
               if(hturnon_scin_raw_hist .ne. 0 ) call
     $              hf1(hidscinrawposped(pln,cnt),histval,1.)

               hscin_pos_ped_sum2(pln,cnt) = hscin_pos_ped_sum2(pln,cnt) 
     &              + hscin_raw_adc_pos(ihit)*hscin_raw_adc_pos(ihit)
               hscin_pos_ped_sum(pln,cnt) = hscin_pos_ped_sum(pln,cnt) 
     &              + hscin_raw_adc_pos(ihit)
               hscin_pos_ped_num(pln,cnt) = hscin_pos_ped_num(pln,cnt) + 1
               if (hscin_pos_ped_num(pln,cnt) .eq. 
     &              nint(hscin_min_peds/5.)) then
                  hscin_pos_ped_limit(pln,cnt) = 100 +
     &                 hscin_pos_ped_sum(pln,cnt) 
     &                 / hscin_pos_ped_num(pln,cnt)
               endif
            endif               ! Ignore large values
*     --- ped.neg
            if (hscin_raw_adc_neg(ihit) .le. 
     &           hscin_neg_ped_limit(pln,cnt)) then ! Ignore large values

               histval = float(hscin_raw_adc_neg(ihit))
               if(hturnon_scin_raw_hist .ne. 0 ) call
     $              hf1(hidscinrawnegped(pln,cnt),histval,1.)

               hscin_neg_ped_sum2(pln,cnt) = hscin_neg_ped_sum2(pln,cnt) +
     &              hscin_raw_adc_neg(ihit)*hscin_raw_adc_neg(ihit)
               hscin_neg_ped_sum(pln,cnt) = hscin_neg_ped_sum(pln,cnt) +
     &              hscin_raw_adc_neg(ihit)
               hscin_neg_ped_num(pln,cnt) = hscin_neg_ped_num(pln,cnt) + 1
               if (hscin_neg_ped_num(pln,cnt) .eq. 
     &              nint(hscin_min_peds/5.)) then
                  hscin_neg_ped_limit(pln,cnt) = 100 +
     &                 hscin_neg_ped_sum(pln,cnt) 
     &                 / hscin_neg_ped_num(pln,cnt)
               endif
            endif               ! Ignore large values
         endif                  ! Use 1st hit only
      enddo                     ! Loop over RAW hits
      
*     
*     AEROGEL CERENKOV PEDESTALS
*     

*     clear hit counter
      Do pln=1,HNUM_AER_LAYERS
         haer_pln_hits(pln)=0
         Do cnt=1,HNUM_AER_COUNTERS
            haer_cnt_hits(pln,cnt)=0
         EndDo
      EndDo 

      do ihit = 1 , haer_raw_tot_hits
         pln = haer_raw_layer_num(ihit)
         cnt = haer_raw_counter_num(ihit)
         adcpos = haer_rawadc_pos(ihit)
         adcneg = haer_rawadc_neg(ihit)
         haer_cnt_hits(pln,cnt) = haer_cnt_hits(pln,cnt) + 1
         if(haer_rawtdc_pos(ihit).ge.0 .or. haer_rawtdc_neg(ihit).ge.0)then
            haer_pln_hits(pln)     = haer_pln_hits(pln)     + 1
         endif
         if(haer_cnt_hits(pln,cnt).eq.1)then !ignore multiple hits
*     --- pos.ped
            if (adcpos .lt. haer_pos_ped_limit(pln,cnt) .and. adcpos.gt.0) then
               histval = float(adcpos)
               if(hturnon_ac_raw_hist .eq. 1) call hf1(hidaerrawposped(pln,cnt),histval,1.)
               haer_pos_ped_sum2(pln,cnt) = haer_pos_ped_sum2(pln,cnt) 
     &              + adcpos*adcpos
               haer_pos_ped_sum(pln,cnt) = haer_pos_ped_sum(pln,cnt)
     $              +adcpos
               haer_pos_ped_num(pln,cnt) = haer_pos_ped_num(pln,cnt) + 1
               if (haer_pos_ped_num(pln,cnt) .eq. 
     &              nint(haer_min_peds/5.)) then
                  haer_pos_ped_limit(pln,cnt) = 100 +
     &                 haer_pos_ped_sum(pln,cnt) 
     &                 / haer_pos_ped_num(pln,cnt)
               endif
            endif
*     --- neg.ped
            if (adcneg .lt. haer_neg_ped_limit(pln,cnt) .and. adcneg.gt.0) then
               histval = float(adcneg)
               if(hturnon_ac_raw_hist .eq. 1) call hf1(hidaerrawnegped(pln,cnt),histval,1.)
               haer_neg_ped_sum2(pln,cnt) = haer_neg_ped_sum2(pln,cnt) 
     &              + haer_rawadc_neg(ihit)*haer_rawadc_neg(ihit)
               haer_neg_ped_sum(pln,cnt) = haer_neg_ped_sum(pln,cnt) 
     &              + haer_rawadc_neg(ihit)
               haer_neg_ped_num(pln,cnt) = haer_neg_ped_num(pln,cnt) + 1
               if (haer_neg_ped_num(pln,cnt) .eq. 
     &              nint(haer_min_peds/5.)) then
                  haer_neg_ped_limit(pln,cnt) = 100 +
     &                 haer_neg_ped_sum(pln,cnt) 
     &                 / haer_neg_ped_num(pln,cnt)
               endif
            endif
         endif
      enddo
*      write(*,*) 'AERPED:', haer_raw_tot_hits,haer_pln_hits(1),haer_pln_hits(2),haer_pln_hits(3)
*     
*     WATER CERENKOV PEDESTALS
*     

*     clear hit counter
      Do pln=1,HNUM_WAT_LAYERS
         Do cnt=1,HNUM_WAT_COUNTERS
            hwat_cnt_hits(pln,cnt)=0
         EndDo
      EndDo 


      do ihit = 1 , hwat_raw_tot_hits
         pln = hwat_raw_layer_num(ihit)
         cnt = hwat_raw_counter_num(ihit)
         hwat_cnt_hits(pln,cnt) = hwat_cnt_hits(pln,cnt) + 1
         if(hwat_cnt_hits(pln,cnt).eq.1)then !ignore multiple hits
            if (hwat_rawadc_pos(ihit) .le. hwat_pos_ped_limit(pln,cnt)) then

               histval = float(hwat_rawadc_pos(ihit))
               if(hturnon_wc_raw_hist .eq. 1) call hf1(hidwatrawposped(pln,cnt),histval,1.)
               
               hwat_pos_ped_sum2(pln,cnt) = hwat_pos_ped_sum2(pln,cnt) 
     &              + hwat_rawadc_pos(ihit)*hwat_rawadc_pos(ihit)
               hwat_pos_ped_sum(pln,cnt) = hwat_pos_ped_sum(pln,cnt) 
     &              + hwat_rawadc_pos(ihit)
               hwat_pos_ped_num(pln,cnt) = hwat_pos_ped_num(pln,cnt) + 1
               if (hwat_pos_ped_num(pln,cnt) .eq. 
     &              nint(hwat_min_peds/5.)) then
                  hwat_pos_ped_limit(pln,cnt) = 100 +
     &                 hwat_pos_ped_sum(pln,cnt) 
     &                 / hwat_pos_ped_num(pln,cnt)
               endif
            endif
            if (hwat_rawadc_neg(ihit) .le. hwat_neg_ped_limit(pln,cnt)) then
               
               histval = float(hwat_rawadc_neg(ihit))
               if(hturnon_wc_raw_hist .eq. 1) call hf1(hidwatrawnegped(pln,cnt),histval,1.)
               
               hwat_neg_ped_sum2(pln,cnt) = hwat_neg_ped_sum2(pln,cnt) 
     &              + hwat_rawadc_neg(ihit)*hwat_rawadc_neg(ihit)
               hwat_neg_ped_sum(pln,cnt) = hwat_neg_ped_sum(pln,cnt) 
     &              + hwat_rawadc_neg(ihit)
               hwat_neg_ped_num(pln,cnt) = hwat_neg_ped_num(pln,cnt) + 1
               if (hwat_neg_ped_num(pln,cnt) .eq. 
     &              nint(hwat_min_peds/5.)) then
                  hwat_neg_ped_limit(pln,cnt) = 100 +
     &                 hwat_neg_ped_sum(pln,cnt) 
     &                 / hwat_neg_ped_num(pln,cnt)
               endif
            endif
         endif
      enddo


*     
*     LUCITE CERENKOV PEDESTALS
*     2009/07/09 Z.Ye

*     clear hit counter
      Do pln=1,HNUM_LUC_LAYERS
         Do cnt=1,HNUM_LUC_COUNTERS
            hluc_cnt_hits(pln,cnt)=0
         EndDo
      EndDo 

*     --- Single Lucite PMT hits ---
      do ihit = 1 , hluc_raw_tot_hits
         pln = hluc_raw_layer_num(ihit)
         cnt = hluc_raw_counter_num(ihit)
         hluc_cnt_hits(pln,cnt) = hluc_cnt_hits(pln,cnt) + 1
         if(hluc_cnt_hits(pln,cnt).eq.1)then !ignore multiple hits
            if(hluc_rawadc_pos(ihit) .le. hluc_pos_ped_limit(pln,cnt)) then ! Positive

               histval = float(hluc_rawadc_pos(ihit))
               if(hturnon_lc_raw_hist .eq. 1) call hf1(hidlucrawposped(pln,cnt),histval,1.)
               
               hluc_pos_ped_sum2(pln,cnt) = hluc_pos_ped_sum2(pln,cnt) 
     &              + hluc_rawadc_pos(ihit)*hluc_rawadc_pos(ihit)
               hluc_pos_ped_sum(pln,cnt) = hluc_pos_ped_sum(pln,cnt) 
     &              + hluc_rawadc_pos(ihit)
               hluc_pos_ped_num(pln,cnt) = hluc_pos_ped_num(pln,cnt) + 1
               if (hluc_pos_ped_num(pln,cnt) .eq. 
     &              nint(hluc_min_peds/5.)) then
                  hluc_pos_ped_limit(pln,cnt) = 100 +
     &                 hluc_pos_ped_sum(pln,cnt) 
     &                 / hluc_pos_ped_num(pln,cnt)
               endif
            endif               ! Positive
            if (hluc_rawadc_neg(ihit) .le. hluc_neg_ped_limit(pln,cnt)) then ! Negative
               
               histval = float(hluc_rawadc_neg(ihit))
               if(hturnon_lc_raw_hist .eq. 1) call hf1(hidlucrawnegped(pln,cnt),histval,1.)
               
               hluc_neg_ped_sum2(pln,cnt) = hluc_neg_ped_sum2(pln,cnt) 
     &              + hluc_rawadc_neg(ihit)*hluc_rawadc_neg(ihit)
               hluc_neg_ped_sum(pln,cnt) = hluc_neg_ped_sum(pln,cnt) 
     &              + hluc_rawadc_neg(ihit)
               hluc_neg_ped_num(pln,cnt) = hluc_neg_ped_num(pln,cnt) + 1
               if (hluc_neg_ped_num(pln,cnt) .eq. 
     &              nint(hluc_min_peds/5.)) then
                  hluc_neg_ped_limit(pln,cnt) = 100 +
     &                 hluc_neg_ped_sum(pln,cnt) 
     &                 / hluc_neg_ped_num(pln,cnt)
               endif
            endif               ! Negative
         endif                  ! ignore multiple hits
      enddo                     ! Loop over hluc_raw_tot_hits
      

*     --- Lucite PMT SUM hits ---
*     clear hit counter
      Do pln=1,HNUM_LUC_LAYERS
         Do cnt=1,HNUM_LUC_COUNTERS
            hlucsum_cnt_hits(pln,cnt)=0
         EndDo
      EndDo 

      do ihit = 1 , hlucsum_raw_tot_hits
         pln = hlucsum_raw_layer_num(ihit)
         cnt = hlucsum_raw_counter_num(ihit)
         hlucsum_cnt_hits(pln,cnt) = hlucsum_cnt_hits(pln,cnt) + 1
         if(hlucsum_cnt_hits(pln,cnt).eq.1)then !ignore multiple hits
            if (hluc_rawadc_tot(ihit) .le. hluc_tot_ped_limit(pln,cnt)) then
               
               histval = float(hluc_rawadc_tot(ihit))
               if(hturnon_lc_raw_hist .eq. 1) call hf1(hidlucrawtotped(pln,cnt),histval,1.)

               hluc_tot_ped_sum2(pln,cnt) = hluc_tot_ped_sum2(pln,cnt) 
     &              + hluc_rawadc_tot(ihit)*hluc_rawadc_tot(ihit)
               hluc_tot_ped_sum(pln,cnt) = hluc_tot_ped_sum(pln,cnt) 
     &              + hluc_rawadc_tot(ihit)
               hluc_tot_ped_num(pln,cnt) = hluc_tot_ped_num(pln,cnt) + 1
               if (hluc_tot_ped_num(pln,cnt) .eq. 
     &              nint(hluc_min_peds/5.)) then
                  hluc_tot_ped_limit(pln,cnt) = 100 +
     &                 hluc_tot_ped_sum(pln,cnt) 
     &                 / hluc_tot_ped_num(pln,cnt)
               endif
            endif               ! .LT. ped_limit
         endif                  ! ignore multiple hits
      enddo                     ! Loop over hlucsum_raw_tot_hits
      
      return
      end

*********************************************************************
* eventually will combine this into one uniform algorithm (JR)
*********************************************************************
c      subroutine analyze_pedestal(
c     & NUM_DET_LAYERS,NUM_DET_ELEMENTS,
c     & det_raw_tot_hits,det_raw_layer_num,det_raw_counter_num,
cposlimit,neglimit
c     & ABORT,err)
*********************************************************************
c     &           hscin_pos_ped_limit(pln,cnt)) then
c               hscin_neg_ped_sum2(pln,cnt) = hscin_neg_ped_sum2(pln,cnt) 
c     &              + hscin_raw_adc_neg(ihit)*hscin_raw_adc_neg(ihit)
c               hscin_neg_ped_sum(pln,cnt) = hscin_neg_ped_sum(pln,cnt) 
c     &              + hscin_raw_adc_neg(ihit)
c             hscin_neg_ped_num(pln,cnt) = hscin_neg_ped_num(pln,cnt) + 1
c               if (hscin_pos_ped_num(pln,cnt) .eq. 
c     &              nint(hscin_min_peds/5.)) then
c                  hscin_pos_ped_limit(pln,cnt) = 100 +
c     &                 hscin_pos_ped_sum(pln,cnt) 
c     &                 / hscin_pos_ped_num(pln,cnt)
c               endif
*********************************************************************
c      implicit none
c      save
*     
c      character*18 here
c      parameter (here='analyze_pedestal')
*     
c      logical ABORT
c      character*(*) err
*     
c      integer*4 ihit
c      integer*4 pln,cnt
c      integer*4 row,col
*     
c      INCLUDE 'hks_data_structures.cmn'
c      INCLUDE 'hks_pedestals.cmn'
*     
c      integer*4 NUM_DET_LAYERS,NUM_DET_ELEMENTS
c      integer*4 cnt_hits(NUM_DET_LAYERS,NUM_DET_ELEMENTS)
c      integer*4 det_raw_tot_hits,det_raw_layer_num(*),det_raw_counter_num(*)
c
*     clear hit counter
c      Do pln=1,NUM_DET_LAYERS
c         Do cnt=1,NUM_DET_ELEMENTS
c           cnt_hits(pln,cnt)=0
c        EndDo
c      EndDo 
c
*     loop over number of raw hits
c      do ihit = 1 , det_raw_tot_hits
c         pln = det_raw_layer_num(ihit)
c         cnt = det_raw_counter_num(ihit)
c         cnt_hits(pln,cnt) = cnt_hits(pln,cnt) + 1
c         if(cnt_hits(pln,cnt).eq.1)then !ignore multiple hits
c
*     --- ped.pos
c            if (adcpos(ihit) .le. poslimit(pln,cnt)) then
c               possum2(pln,cnt) = possum2(pln,cnt) + adcpos(ihit)*adcpos(ihit)
c               possum(pln,cnt)  = possum(pln,cnt)  + adcpos(ihit)
c               posnum(pln,cnt)  = posnum(pln,cnt)  + 1
c               if (posnum(pln,cnt) .eq. nint(minpeds/5.)) then
c                  poslimit(pln,cnt) = 100 + possum(pln,cnt)/posnum(pln,cnt)
c               endif
c            endif
*     --- ped.neg
c            if (adcneg(ihit) .le. neglimit(pln,cnt)) then
c               negsum2(pln,cnt) = negsum2(pln,cnt) + adcneg(ihit)*adcneg(ihit)
c               negsum(pln,cnt)  = negsum(pln,cnt)  + adcneg(ihit)
c               negnum(pln,cnt)  = negnum(pln,cnt)  + 1
c               if (negnum(pln,cnt) .eq. nint(minpeds/5.)) then
c                  neglimit(pln,cnt) = 100 + negsum(pln,cnt)/negnum(pln,cnt)
c               endif
c            endif
c      
c         endif
c      enddo
c      
c      return
c      end
