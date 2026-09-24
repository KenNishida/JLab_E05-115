      SUBROUTINE h_reset_event(ABORT,err)
*--------------------------------------------------------
*     -       Prototype C analysis routine
*     -
*     -
*     -   Purpose and Methods : 
*     - Resets all HKS quantities before event is processed.
*     -
*     - 
*     -   Output: ABORT	- success or failure
*     -         : err	- reason for failure, if any
*     - 
*     -   Created  2-Nov-1993   Kevin B. Beard
*     -   Modified 20-Nov-1993   KBB for new errors
*     
*     Revision 2.2 2009/08/11 Z.Ye
*     for E05-115: Add Lucite info
*     
*     -      Revision 1.16 2004/03/02 Miyoshi
*     for E01-011
*
*     -      Revision 1.15  1999/08/20 14:52:18  saw
*     -      Put in warning if Xscin_tdc_max is bigger than 4094
*     -
*     -      Revision 1.14  1999/02/03 21:13:04  saw
*     -      Code for new Shower counter tubes
*     -
*     -      Revision 1.13  1996/11/05 21:43:16  saw
*     -      (WH) Add lucite counter
*     -
*     -      Revision 1.12  1996/09/04 15:18:54  saw
*     -      (JRA) Zero out some misc scalers
*     -
*     -      Revision 1.11  1996/04/30 12:29:55  saw
*     -      (JRA) Change SAER_ADC_LEFT/RIGHT to POS/NEG
*     -
*     -      Revision 1.10  1995/10/09 18:09:01  cdaq
*     -      (JRA) Add clear of SCER_RAW_ADC
*     -
*     -      Revision 1.9  1995/07/27 19:44:17  cdaq
*     -      (JRA) Zero out pedestal arrays
*     -
*     -      Revision 1.8  1995/05/22  20:50:48  cdaq
*     -(SAW) Split gen_data_data_structures into gen, 
*     hms, sos, and coin parts"
*     
*     -Revision 1.7  1995/05/11  15:08:32  cdaq
*     -(SAW) Change SDEDXn vars to an array.  
*     Add reset of Aerogel structure.
*     
*     -Revision 1.6  1994/11/22  20:15:35  cdaq
*     -(SPB) Bring up to date with h_reset_event
*     
*     -Revision 1.5  1994/06/22  20:51:22  cdaq
*     -(SAW) Zero out the miscleaneous hits array
*     
*     Revision 1.4  1994/03/24  22:01:43  cdaq
*     Reflect changes in gen_data_structures.cmn
*     
*     Revision 1.3  1994/02/22  19:43:15  cdaq
*     (SAW) SNUM_DC_LAYERS  --> SMAX_NUM_DC_LAYERS
*     
*     Revision 1.2  1994/02/11  04:12:30  cdaq
*     Change var names to reflect current gen_data_structures
*     
*     Revision 1.1  1994/02/04  22:16:02  cdaq
*     Initial revision
*     
*     - 
*     -
*     - All standards are from "Proposal for Hall C Analysis Software
*     - Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*     -
*     -
*--------------------------------------------------------
      
      IMPLICIT NONE
      SAVE
*     
      character*50 here
      parameter (here= 'h_reset_event')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'hks_tracking.cmn'
      INCLUDE 'hks_pedestals.cmn'
      include 'hks_scin_parms.cmn'
      include 'hks_scin_tof.cmn'
*
      INTEGER i,j,k
*     
*--------------------------------------------------------
*     
*     HKS DCs HITS
*     
      HDC_RAW_TOT_HITS = 0
      DO i = 1,HMAX_DC_HITS
         HDC_RAW_LAYER_NUM(i)= 0
         HDC_RAW_WIRE_NUM(i)= 0
         HDC_RAW_TDC(i)= 0
         HDC_DRIFT_TIME(i)= 0.
         HDC_DRIFT_DIS(i)= 0.
         HDC_WIRE_CENTER(i)= 0.
         HDC_WIRE_COORD(i)= 0.
         HDC_LAYER_NUM(i)= 0.
         HDC_WIRE_NUM(i)= 0.
         HDC_TDC(i)= 0.
      ENDDO
      HDC_TOT_HITS= 0
      DO i = 1,HMAX_NUM_DC_LAYERS
         HDC_HITS_PER_LAYER(i)= 0
      ENDDO

*     
*     HKS SCINTILLATOR HITS
*     
      HSCIN_RAW_TOT_HITS= 0
      HSCIN_TOT_HITS= 0
      DO i= 1,HMAX_SCIN_HITS
         HSCIN_HIT_COORD(i)=0
         HSCIN_COR_ADC(i)= 0.0
         HSCIN_COR_TIME(i)= 0.0
         HSCIN_LAYER_NUM(i)= 0
         HSCIN_COUNTER_NUM(i)= 0
         HSCIN_ADC_POS(i)= 0
         HSCIN_ADC_NEG(i)= 0
         HSCIN_TDC_POS(i)= 0
         HSCIN_TDC_NEG(i)= 0
         HSCIN_RAW_LAYER_NUM(i)= 0
         HSCIN_RAW_COUNTER_NUM(i)= 0
         HSCIN_RAW_ADC_POS(i)= 0
         HSCIN_RAW_ADC_NEG(i)= 0
         HSCIN_RAW_TDC_POS(i)= 0
         HSCIN_RAW_TDC_NEG(i)= 0
      ENDDO
      DO i = 1,HNUM_SCIN_LAYERS
         HSCIN_HITS_PER_LAYER(i)= 0
      ENDDO
      HSTART_TIME = 0
      HSTART_HITNUM = -1
      HSTART_HITSIDE = 0
*
*     HKS AEROGEL HITS
*     
      HAER_RAW_TOT_HITS = 0
      haer_tot_hits = 0
      DO i= 1,HMAX_AER_HITS
         HAER_RAW_LAYER_NUM(i) = 0
         HAER_RAW_COUNTER_NUM(i) = 0
         HAER_RAWADC_POS(i) = -1
         HAER_RAWADC_NEG(i) = -1
         HAER_RAWTDC_POS(i) = -10000
         HAER_RAWTDC_NEG(i) = -10000
         haer_pos_npe(i)    = -1  
         haer_neg_npe(i)    = -1
         haer_pos_time(i)   = -1000
         haer_neg_time(i)   = -1000
         haer_npe_sum(i)    = -1
         haer_counter_num(i)= -1
         haer_layer_num(i)  = -1 
         HAER_BOTH_HITS(i)  = -1
      ENDDO
      Do i=1,HNUM_AER_LAYERS
         haer_raw_layer_hits(i) = 0
         haer_layer_hits(i) = 0 
      EndDo
      
*
*     HKS WATER HITS
*     
      HWAT_RAW_TOT_HITS = 0
      hwat_tot_hits = 0
      DO i= 1,HMAX_WAT_HITS
         HWAT_RAW_LAYER_NUM(i) = 0
         HWAT_RAW_COUNTER_NUM(i) = 0
         HWAT_RAWADC_POS(i) = 0
         HWAT_RAWADC_NEG(i) = 0
         HWAT_RAWTDC_POS(i) = 0
         HWAT_RAWTDC_NEG(i) = 0
         hwat_pos_npe(i)    = -1
         hwat_neg_npe(i)    = -1
         hwat_pos_time(i)   = -10000
         hwat_neg_time(i)   = -10000
         hwat_npe_sum(i)    = -1
         hwat_npe_sum_k_ratio(i)    = -1
         hwat_layer_num(i)  = -1
         hwat_counter_num(i)= -1
         hwat_both_hits(i)  = -1
       ENDDO
*
*     HKS LUCITE HITS
*     
      HLUC_RAW_TOT_HITS = 0
      HLUCSUM_RAW_TOT_HITS = 0
      hluc_tot_hits = 0
      DO i= 1,HMAX_LUCSUM_HITS
         HLUCSUM_RAW_LAYER_NUM(i) = 0
         HLUCSUM_RAW_COUNTER_NUM(i) = 0
         HLUC_RAWADC_TOT(i) = 0
         HLUC_RAWTDC_TOT(i) = 0
      ENDDO
      DO i= 1,HMAX_LUC_HITS
         HLUC_RAW_LAYER_NUM(i) = 0
         HLUC_RAW_COUNTER_NUM(i) = 0
         HLUC_RAWADC_POS(i) = 0
         HLUC_RAWADC_NEG(i) = 0
         HLUC_RAWTDC_POS(i) = 0
         HLUC_RAWTDC_NEG(i) = 0
         hluc_pos_npe(i)    = -1
         hluc_neg_npe(i)    = -1
         hluc_tot_npe(i)    = -1
         hluc_pos_time(i)   = -10000
         hluc_neg_time(i)   = -10000
         hluc_tot_time(i)   = -10000
         hluc_npe_sum(i)    = -1
         hluc_layer_num(i)  = -1
         hluc_counter_num(i)= -1
         hluc_both_hits(i)  = -1
       ENDDO     
*     
*     HKS Miscleaneous hits
*     
      hmisc_tot_hits = 0
      do i=1,HMAX_MISC_HITS
         HMISC_RAW_ADDR1(i) = 0
         HMISC_RAW_ADDR2(i) = 0
         HMISC_RAW_DATA(i) = 0
         do j=1,hnum_misc_layers
            hmisc_scaler(i,j)=0
         enddo
      enddo
*     
*     HKS DETECTOR TRACK QUANTITIES
*     
      HNTRACKS_FP= 0
      DO i= 1,HNTRACKS_MAX
         HX_FP(i)= 0.
         HY_FP(i)= 0.
         HZ_FP(i)= 0.
         HXP_FP(i)= 0.
         HYP_FP(i)= 0.
         HCHI2_FP(i)= 0.
         HNFREE_FP(i)= 0.
*         Do j= 1,4
*            do i= 1,4
*               HDEL_FP(i,j,i)= 0.
*            enddo
*         EndDo
         Do j= 1,HNTRACKHITS_MAX
            HNTRACK_HITS(i,j)= 0
         EndDo
      ENDDO
      
*     from hks_tracking.cmn
*     for matching tracking output to gen_event_ID_number 

      h_kept_track_ID_number = 0
      h_kept_ntrack = 0
      h_kept_track_index = 0
*     
*     HKS TARGET QUANTITIES
*     
      DO i= 1,HNTRACKS_MAX
         HX_TAR(i)= 0.
         HY_TAR(i)= 0.
         HZ_TAR(i)= 0.
         HXP_TAR(i)= 0.
         HYP_TAR(i)= 0.
         HDELTA_TAR(i)= 0.
         HP_TAR(i)= 0.
         HCHI2_TAR(i)= 0.
         HDEL_TAR(5,5,i)= 0.
         HNFREE_TAR(i)= 0.
         HLINK_TAR_FP(i)= 0.
         Do j= 1,5
            do k= 1,5
               HDEL_TAR(k,j,i)= 0.
            enddo
         EndDo
      ENDDO

      HNTRACKS_TAR= 0
      DO i=1, HNTRACKS_MAX
         do j = 1, HMAX_SCIN_HITS
            HSCIN_HIT(i,j)= 0
         enddo
         HTRK_BETA(i)=0
         HSBETA_CHISQ(i)=0
         HTRK_time_ATFP(i)=0
      ENDDO
      
      hnphysics = 0
      Do i=1,hnphysics_max
         hsp(i) = 0
         HSENERGY(i)=0
         HSDELTA(i)=0
         HSTHETA(i)=0
         HSPHI(i)=0
         HSZBEAM(i)=0
         do j = 1 , HNUM_SCIN_LAYERS
            HSDEDX(i,j) = 0.
         enddo
         HSBETA(i)=0
         HSTIME_AT_FP(i)=0
         HSX_FP(i)=0
         HSY_FP(i)=0
         HSXP_FP(i)=0
         HSYP_FP(i)=0
         HSCHI2PERDEG(i)=0
         HSX_TAR(i)=0
         HSY_TAR(i)=0
         HSXP_TAR(i)=0
         HSYP_TAR(i)=0
      EndDo

*     
*     HKS DECODED DATA
*     
      do i=1,hnum_scin_layers
         do j=1,hnum_scin_elements
            hscin_pos_ped_num(i,j) = 0
            hscin_pos_ped_sum2(i,j) = 0
            hscin_pos_ped_sum(i,j) = 0
            hscin_neg_ped_num(i,j) = 0
            hscin_neg_ped_sum2(i,j) = 0
            hscin_neg_ped_sum(i,j) = 0
         enddo
      enddo
      
      do i=1,hnum_aer_layers
         Do j=1,hnum_aer_counters
            haer_pos_ped_num(i,j)=0
            haer_pos_ped_sum2(i,j)=0
            haer_pos_ped_sum(i,j)=0
            haer_neg_ped_num(i,j)=0
            haer_neg_ped_sum2(i,j)=0
            haer_neg_ped_sum(i,j)=0
         enddo
      EndDo
      
      do i=1,hnum_wat_layers
         Do j=1,hnum_wat_counters
            hwat_pos_ped_num(i,j)=0
            hwat_pos_ped_sum2(i,j)=0
            hwat_pos_ped_sum(i,j)=0
            hwat_neg_ped_num(i,j)=0
            hwat_neg_ped_sum2(i,j)=0
            hwat_neg_ped_sum(i,j)=0
         enddo
      EndDo

      do i=1,hnum_luc_layers
         Do j=1,hnum_luc_counters
            hluc_pos_ped_num(i,j)=0
            hluc_pos_ped_sum2(i,j)=0
            hluc_pos_ped_sum(i,j)=0
            hluc_neg_ped_num(i,j)=0
            hluc_neg_ped_sum2(i,j)=0
            hluc_neg_ped_sum(i,j)=0
            hluc_tot_ped_num(i,j)=0
            hluc_tot_ped_sum2(i,j)=0
            hluc_tot_ped_sum(i,j)=0
         enddo
      EndDo
      htul_raw_tot_hits = 0
      htul_tot_hits = 0

*     
c      if(hscin_tdc_max.gt.4094) then
c         print *,' '
c         print *,'WARNING!!: hscin_tdc_max is ',hscin_tdc_max
c         print *,
c     &        'We usually run our high resolution TDCs with 12 bit'
c         print *,
c     &        'ranges.  If hscin_tdc_max is set to a value higher than'
c         print *,
c     &        'the TDCs overflow channel, then overflowed TDC channels'
c         print *,
c     &        'will not be rejected.  Under high rate conditions, this'
c         print *,'can result in bad beta and timing calculations'
c         print *,' '
c      endif
*     
      ABORT= .FALSE.
      err= ' '

      RETURN
      END
