       SUBROUTINE H_AERO(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze aerogel information for each track
*-
*-      Required Input BANKS     SOS_RAW_AER
*-
*-      Output BANKS             SOS_TRACK_TESTS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*-
*-   Created 13-MAY-1995     H. Breuer and R. Mohring, UMD
*-                           SOS Aerogel detector calibration routine
*-
* $Log: h_aero.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.6  2005/08/04 22:23:00  cdaq
* Mod. to remove double counts of AC/WC due to multihits in TDC
*
* Revision 1.5  2005/07/29 17:50:16  cdaq
* Add init. of both_hits
*
* Revision 1.4  2005/07/08 13:47:47  sumihama
* Mod. AC hist
*
* Revision 1.3  2005/07/02 17:22:29  sumihama
* Bug fix foe AERO
*
* Revision 1.2  2005/06/27 21:54:29  cdaq
* Change AER-gain multi->devide
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.3  2005/04/08 20:39:11  miyoshi
* add calling subroutine
*
* Revision 1.2  2004/12/24 19:47:07  miyoshi
* change minor
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 2.1 03/16/2004 Miyoshi
* for E01-011
*
* Revision 2.0  2000/03/05 02:43:13 jinghua
* (JLiu) Cleanup. changed hot channel's npe from +100 to -100.
*
* Revision 1.3  1996/11/07 19:48:28  saw
* (JRA) Handle over and underflows
*
* Revision 1.2  1996/09/05 13:13:14  saw
* (JRA) ??
*
* Revision 1.1  1996/04/30 17:12:40  saw
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*6 here
      parameter (here= 'H_AERO')
*
      logical ABORT
      character*(*) err
*
      integer*4 ind,npmt,gh,i
      integer*4 layer,counter,la,co
      integer*4 tdcpos,tdcneg,adcpos,adcneg
      integer*4 have_tdc,num 
      real*4 npepos,npeneg,f1time,ttime
*
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'hks_pedestals.cmn'
      INCLUDE 'hks_aero_parms.cmn'
      INCLUDE 'hks_scin_tof.cmn' ! for start time center
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_event_info.cmn' 
      include 'gen_f1tdc.cmn'
      include 'gen_rocid.cmn'
      
      integer*4 counter_hits(HNUM_AER_LAYERS,HNUM_AER_COUNTERS)
*     
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '
      

      if(haer_raw_tot_hits .le. 0) return
      

      haer_tot_hits = 0
      gh = 0
      
*     initialize layer and counter hits
      Do la=1,HNUM_AER_LAYERS
         haer_raw_layer_hits(la) = 0
         haer_layer_hits(la) = 0 
         Do co=1,HNUM_AER_COUNTERS
            counter_hits(la,co) = 0
         EndDo
      EndDo
      
*     if the parameters are missing, set the largest value.
      if(haer_tdc_pos_max .eq. 0) then
         haer_tdc_pos_max = 8000
      EndIf
      if(haer_tdc_neg_max .eq. 0) then
         haer_tdc_neg_max = 8000
      EndIf



      num=0
      Do i=1,haer_raw_tot_hits
         ttime=g_f1_trigger_time(LR_VME_ROCID)
         f1time=g_f1_refer_time(LR_VME_ROCID,1,1)
         la = haer_raw_layer_num(i)
         co = haer_raw_counter_num(i)
         tdcpos = haer_rawtdc_pos(i)
         tdcneg = haer_rawtdc_neg(i)
          if((tdcpos.gt.0).or.(tdcneg.gt.0)) then 
c     --- -70000 is initial value.
c     --- pos 
            if(tdcpos .le. -70000) then
               haer_rawtdc_pos_sub_trig(i) = -70000
            Else
               if(tdcpos.lt.ttime) then
                  tdcpos = tdcpos+f1tdc_LR_gate_width
               endif
               if(f1time.lt.ttime) then
                  f1time = f1time+f1tdc_LR_gate_width
               endif
               haer_rawtdc_pos_sub_trig(i) = 
     &            tdcpos-f1time+1444/f1tdc_LR_tdc_to_time
            EndIf            ! tdcpos<=-70000
c     --- neg
            if(tdcneg .le. -70000) then
               haer_rawtdc_neg_sub_trig(i) = -70000
            Else
               if(tdcneg.lt.ttime) then
                  tdcneg = tdcneg+f1tdc_LR_gate_width
               endif
               if(f1time.lt.ttime) then
                  f1time = f1time+f1tdc_LR_gate_width
               endif
               haer_rawtdc_neg_sub_trig(i) = 
     &            tdcneg-f1time+1444/f1tdc_LR_tdc_to_time
            EndIf
            num=num+1
c               Write(*,*) 'ev,t1,t2,i,t,la,wi,cort=',gen_event_id_number,
c     &              f1time,
c     &              haer_raw_layer_num(i),haer_raw_counter_num(i),
c     &              haer_rawtdc_pos_sub_trig(i),
c     &              haer_rawtdc_neg_sub_trig(i),
c     &              haer_rawadc_pos(i),
c     &              haer_rawadc_neg(i)
               
         else 
            haer_rawtdc_pos_sub_trig(i) = -70000
            haer_rawtdc_neg_sub_trig(i) = -70000
         endif               ! good trigger time
      enddo                  ! haer_raw_tot_hits loop

*     fill the raw histogram as needed
      call h_fill_aer_raw_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

      if(  num .eq. 0) then
         haer_raw_tot_hits = 0 
         return
      endif                  ! good trigger signal


*     --- check raw data
c      Write(*,*) 'in h_aero.f tot=',haer_raw_tot_hits
c      Do ind=1, haer_raw_tot_hits
c         Write(*,*) 'la,co,pos,neg=',haer_raw_layer_num(ind),
c     &        haer_raw_counter_num(ind),haer_rawadc_pos(ind),
c     &        haer_rawadc_neg(ind)
c      EndDo
      
      
C     In E01-011, the AC ADC are read not sparsified, i.e. there are
C     always at least 21 hits. Because the AC discriminator hits will
C     veto the data acquisition, most ADC values will not be associated
C     with valid TDC hits. Thus the calibration and calculation of
C     number of photoelectrons has to be done independent of TDC values.

      do ind = 1, haer_raw_tot_hits
         la = haer_raw_layer_num(ind)
         co = haer_raw_counter_num(ind)
         tdcpos= haer_rawtdc_pos_sub_trig(ind)
         tdcneg= haer_rawtdc_neg_sub_trig(ind)
         adcpos= haer_rawadc_pos(ind)
         adcneg= haer_rawadc_neg(ind)

C     Calculate Npe no matter what!
         npepos = (adcpos-haer_pos_ped_mean(la,co))/haer_pos_gain(la,co)
         npeneg = (adcneg-haer_neg_ped_mean(la,co))/haer_neg_gain(la,co)

         haer_raw_layer_hits(la) = haer_raw_layer_hits(la) + 1
         
         if(counter_hits(la,co).eq.0) then
            haer_tot_hits = haer_tot_hits + 1
            gh = haer_tot_hits
            counter_hits(la,co) = gh
            haer_both_hits(gh) = 0
            haer_layer_num(gh) = la
            haer_counter_num(gh) = co
            haer_pos_npe(gh) = 0.
            haer_neg_npe(gh) = 0.
            haer_npe_sum(gh) = 0.
            haer_neg_time(gh) = -1
            haer_pos_time(gh) = -1
         endif 

C     ADCs have been evaluated. Now check for valid TDC hits. 

         
*     --- the positive tube
         if(tdcpos .gt. HAER_TDC_POS_MIN .and. 
     &        tdcpos .lt. HAER_TDC_POS_MAX) then 
            haer_pos_npe(counter_hits(la,co)) = npepos
            haer_npe_sum(counter_hits(la,co)) =  npepos    ! chiba 2010/10/20
*     +           haer_npe_sum(counter_hits(la,co)) + npepos
            haer_pos_time(counter_hits(la,co)) = Real(tdcpos)
         EndIF
            
*     --- the negative tube
            
         if(tdcneg .gt. HAER_TDC_NEG_MIN .and. 
     &        tdcneg .lt. HAER_TDC_NEG_MAX) then 
            haer_neg_npe(counter_hits(la,co)) =   npeneg  
            haer_npe_sum(counter_hits(la,co)) =  npeneg    ! chiba 2010/10/20
*     +           haer_npe_sum(counter_hits(la,co)) + npeneg
            haer_neg_time(counter_hits(la,co)) = Real(tdcneg)
         EndIF

*     --- both tubes have hits
            
         if((tdcpos .gt. HAER_TDC_POS_MIN .and. 
     &        tdcpos .lt. HAER_TDC_POS_MAX) 
     &        .AND.
     &        (tdcneg .gt. HAER_TDC_NEG_MIN .and. 
     &        tdcneg .lt. HAER_TDC_NEG_MAX)
     &        ) then
            haer_npe_sum(counter_hits(la,co)) = npepos + npeneg     ! chiba 2010/10/20
            haer_both_hits(counter_hits(la,co)) = 1
            haer_layer_hits(la) = haer_layer_hits(la) + 1
         endif
      EndDo      

*     fill the raw histogram as needed
      call h_fill_aero_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

      return
      end
      
