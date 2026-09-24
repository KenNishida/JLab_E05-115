       SUBROUTINE H_WATER(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze lucite aerogel information for each track
*-
*-      Required Input BANKS     HKS_RAW_WAT
*-
*-      Output BANKS             HKS_DECODED_WAT
*-
*-   Output: ABORT           - always success
*-         : err             - never failure
*-
* $Log: h_water.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
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
      parameter (here= 'H_WATER')
*
      logical ABORT
      character*(*) err
*
     
      integer*4 ind, npmt, trk, hit, gh,i
      integer*4 layer,counter,la,co
      integer*4 tdcpos,tdcneg,adcpos,adcneg
      integer*4 have_tdc,num
      real*4 npepos,npeneg

      real*4 xhit_coord, yhit_coord
      real*4 f1time,ttime
*
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'hks_pedestals.cmn'
      INCLUDE 'hks_water_parms.cmn'
      INCLUDE 'hks_scin_tof.cmn' ! for start time center
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_event_info.cmn' ! matsu
      include 'gen_f1tdc.cmn'
      include 'gen_rocid.cmn'
      integer*4 counter_hits(HNUM_WAT_LAYERS,HNUM_WAT_COUNTERS)
*calculate normalized kaon NPE in this run
      INTEGER start_time
      real*4 k_gain(HNUM_WAT_LAYERS,HNUM_WAT_COUNTERS)
      real*4 k_sigma(HNUM_WAT_LAYERS,HNUM_WAT_COUNTERS)

      start_time = gen_run_UTC_start - 1250989200  !using this value, "beam start time" 8/23 1:00(GMT) equals zero
      Do la=1,HNUM_WAT_LAYERS
         Do co=1,HNUM_WAT_COUNTERS
           k_gain(la,co)=hwat_k_gain_par1(la,co) + start_time * hwat_k_gain_par2(la,co)
           k_sigma(la,co)=hwat_k_sigma_par1(la,co) + start_time * hwat_k_sigma_par2(la,co)
          EndDo
      EndDo

*
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '


      if(hwat_raw_tot_hits .le. 0) Return
      

      hwat_tot_hits = 0
      gh = 0
      
*     initialize layer and counter hits
      Do la=1,HNUM_WAT_LAYERS
         hwat_raw_layer_hits(la) = 0
         hwat_layer_hits(la) = 0 
         Do co=1,HNUM_WAT_COUNTERS
            counter_hits(la,co) = 0
         EndDo
      EndDo

       num=0

       Do i=1,hwat_raw_tot_hits
          ttime=g_f1_trigger_time(LR_VME_ROCID)
          f1time=g_f1_refer_time(LR_VME_ROCID,1,1)
          la = hwat_raw_layer_num(i)
          co = hwat_raw_counter_num(i)
          tdcpos = hwat_rawtdc_pos(i)
c          write(*,*) la,co,tdcpos,f1time,ttime
          if((tdcpos.gt.0)) then 
c     --- -70000 is initial value.
c     --- WC doesn't have NEG TDC
c     --- pos 
            if(tdcpos.lt.ttime) then
               tdcpos = tdcpos+f1tdc_LR_gate_width
            endif
            if(f1time.lt.ttime) then
               f1time = f1time+f1tdc_LR_gate_width
            endif
            hwat_rawtdc_pos_sub_trig(i) = 
     &         tdcpos-f1time+1444./f1tdc_LR_tdc_to_time
            num=num+1
         else 
            hwat_rawtdc_pos_sub_trig(i) = -70000
         endif               ! good trigger time
c            Write(*,*) 'ev,t1,t2,i,t,la,wi,cort=',gen_event_id_number,
c     &              ttime,f1time,
c     &              hwat_raw_layer_num(i),hwat_raw_counter_num(i),
c     &              hwat_rawtdc_pos_sub_trig(i),
c     &              hwat_rawtdc_pos(i),
c     &              hwat_rawadc_pos(i),
c     &              hwat_rawadc_neg(i)
      enddo                  ! hwat_raw_tot_hits loop
*     fill the histogram as needed
      Call h_fill_water_raw_hist(ABORT,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

      if(  num.eq. 0) then
         hwat_raw_tot_hits = 0 
         return
      endif                   ! good trigger signal



      do ind = 1,HWAT_RAW_TOT_HITS
         la = hwat_raw_layer_num(ind)
         co = hwat_raw_counter_num(ind)
         tdcpos = hwat_rawtdc_pos_sub_trig(ind)
         adcpos= hwat_rawadc_pos(ind)
         adcneg= hwat_rawadc_neg(ind)
c         write(*,*) 'la,co,tdcpos,tdcneg,adcpos,adcneg',
c     >        la,co,tdcpos,tdcneg,adcpos,adcneg ! matsu
C     Calculate Npe no matter what!
         npepos = (adcpos-hwat_pos_ped_mean(la,co))/hwat_pos_gain(la,co)
         npeneg = (adcneg-hwat_neg_ped_mean(la,co))/hwat_neg_gain(la,co)
         hwat_raw_layer_hits(la) = hwat_raw_layer_hits(la) + 1

         if(counter_hits(la,co).eq.0)then
            hwat_tot_hits = hwat_tot_hits + 1
            gh = hwat_tot_hits
            counter_hits (la,co) = gh
            hwat_both_hits(gh) = 0
            hwat_layer_num(gh) = la
            hwat_counter_num(gh) = co
            hwat_pos_npe(gh) = 0.
            hwat_neg_npe(gh) = 0.
            hwat_npe_sum(gh) = 0.
            hwat_npe_sum_k_ratio(gh) = 0.
            hwat_pos_time(gh) = -1
         endif
         
C     ADCs have been evaluated. Now check for valid TDC hits. 

*     --- both tubes have hits
            

*     --- the positive tube
         
         if(tdcpos .gt. HWAT_TDC_POS_MIN .and. 
     &        tdcpos .lt. HWAT_TDC_POS_MAX) then
            hwat_pos_npe(counter_hits(la,co)) = npepos
            hwat_neg_npe(counter_hits(la,co)) = npeneg
            hwat_npe_sum(counter_hits(la,co)) = npepos + npeneg    ! chiba 2010/10/20
*     &           hwat_npe_sum(counter_hits(la,co)) + npepos + npeneg
*calculate normalized NPE
            hwat_npe_sum_k_ratio(counter_hits(la,co)) = 
     &           ((hwat_npe_sum(counter_hits(la,co)) / k_gain(la,co)) - 1.)*(0.2/k_sigma(la,co)) +1.
            hwat_pos_time(counter_hits(la,co)) = Real(tdcpos) 
            hwat_both_hits(counter_hits(la,co)) = 1
            hwat_layer_hits(la) = hwat_layer_hits(la) + 1
       endif
      EndDo
 


      
*     fill the raw histogram as needed
      call h_fill_water_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

      return
      end
