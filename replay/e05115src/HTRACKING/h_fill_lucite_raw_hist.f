      subroutine h_fill_lucite_raw_hist(Abort,err)
*
*     routine to fill lucite raw data histograms and hit pattern
*
*     Author:	Jinghua Liu
*     Date:     18 Feb. 2000
* Revision 1.2 2009/08/17 yez
* Add Lucite sum channels
*
* Revision 1.2 2009/07/09 yez
* Convert Water info into Lucite info for E05-115
*
* Revision 1.0  2000/02/18 10:51:23  Jinghua
* Initial version
*
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*20 here
      parameter (here='h_fill_lucite_raw_hist')
*
      logical ABORT
      character*(*) err

      real*4 histval
      integer*4 ind
      integer*4 la,co,adcpos,adcneg,tdcpos,tdcneg,tdctot,adctot
      include 'hks_data_structures.cmn'
      include 'hks_lucite_parms.cmn'
      include 'hks_pedestals.cmn'
      include 'hks_id_histid.cmn'          
      include 'gen_event_info.cmn'
      integer*4 counter_hits(HNUM_LUC_LAYERS,HNUM_LUC_COUNTERS)
      integer*4 countersum_hits(HNUM_LUC_LAYERS,HNUM_LUC_COUNTERS)
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
      
      Call HF1(hidlucrawtothits,float(hluc_raw_tot_hits),1.)

*     Make sure there is at least 1 hit
      if(hluc_raw_tot_hits .lt. 1 .and. hlucsum_raw_tot_hits .lt. 1)  Return
      
*     clear hit counter
      Do la=1,HNUM_LUC_LAYERS
         Do co=1,HNUM_LUC_COUNTERS
            counter_hits(la,co) = 0
            countersum_hits(la,co) = 0
         EndDo
      EndDo

c     Write(*,*) '-ev,tot=',gen_event_ID_number,hwat_raw_tot_hits
*     Loop over all counters
      do ind = 1, hluc_raw_tot_hits
         la = hluc_raw_layer_num(ind)
         co = hluc_raw_counter_num(ind)
         counter_hits(la,co)=counter_hits(la,co)+1
         adcpos = hluc_rawadc_pos(ind)
         adcneg = hluc_rawadc_neg(ind)
         tdcpos = hluc_rawtdc_pos_sub_trig(ind)
         tdcneg = hluc_rawtdc_neg_sub_trig(ind)
c     Write(*,*) 'fillwcraw,la,co,apan,tptn=',
c     &        la,co,adcpos,adcneg,adctot,tdcpos,tdcneg,tdctot
c     --- hit > 0, fill raw hit pattern.
         Call HF1(hidlucrawhitpat(la),float(co),1.)         
c     --- layer hit
         Call HF1(hidlucrawlayer,float(la),1.)

         if(tdcpos .ne. -1) then
            call HF1(hidlucrawtdchitpatpos(la),float(co),1.)
            call HF1(hidlucsumpostdc(la),float(tdcpos),1.)
         EndIf
         if(tdcneg .ne. -1) then
            call HF1(hidlucrawtdchitpatneg(la),float(co),1.)
            call HF1(hidlucsumnegtdc(la),float(tdcneg),1.)
         EndIf
         if(adcpos .ne. -1) then
            call HF1(hidlucrawadchitpatpos(la),float(co),1.)
            call HF1(hidlucsumposadc(la),float(adcpos),1.)
         EndIf
         if(adcneg .ne. -1) then
            call HF1(hidlucrawadchitpatneg(la),float(co),1.)
            call HF1(hidlucsumnegadc(la),float(adcneg),1.)
         EndIf
         if(hturnon_lc_raw_hist .eq. 1) then
*     Fill ADC histograms.
            if(counter_hits(la,co).eq.1) then ! ignore double hits in ADC
               histval = float(hluc_rawadc_pos(ind))
               call hf1(hidlucrawposadc(la,co),histval,1.)
               histval = float(hluc_rawadc_neg(ind))
               call hf1(hidlucrawnegadc(la,co),histval,1.)
            endif
*     Fill TDC histograms.
            histval = float(hluc_rawtdc_pos_sub_trig(ind))
            call hf1(hidlucrawpostdc(la,co),histval,1.)
            histval = float(hluc_rawtdc_neg_sub_trig(ind))
            call hf1(hidlucrawnegtdc(la,co),histval,1.)
         EndIf
      EndDo

c     --- Lucite PID sum signals
      do ind = 1, hlucsum_raw_tot_hits
         la = hlucsum_raw_layer_num(ind)
         co = hlucsum_raw_counter_num(ind)
         countersum_hits(la,co)=countersum_hits(la,co)+1
         adctot = hluc_rawadc_tot(ind)
         tdctot = hluc_rawtdc_tot_sub_trig(ind)

c     --- hit > 0, fill raw hit pattern.
*         Call HF1(hidlucrawhitpat(la),float(co),1.)         
c     --- layer hit
*         Call HF1(hidlucrawlayer,float(la),1.)

         if(tdctot .ne. -1) then
            call HF1(hidlucrawtdchitpattot(la),float(co),1.)
            call HF1(hidlucsumtottdc(la),float(tdctot),1.)
         EndIf
         if(adctot .ne. -1) then
            call HF1(hidlucrawadchitpattot(la),float(co),1.)
            call HF1(hidlucsumtotadc(la),float(adctot),1.)
         EndIf
         if(hturnon_lc_raw_hist .eq. 1) then
*     Fill ADC histograms.
            if(countersum_hits(la,co).eq.1) then ! ignore double hits in ADC
               histval = float(hluc_rawadc_tot(ind))
               call hf1(hidlucrawtotadc(la,co),histval,1.)
            endif
*     Fill TDC histograms.
            histval = float(hluc_rawtdc_tot_sub_trig(ind))
            call hf1(hidlucrawtottdc(la,co),histval,1.)
         EndIf
      EndDo                     ! Loop over hlucsum_raw_tot_hits

      return
      end
