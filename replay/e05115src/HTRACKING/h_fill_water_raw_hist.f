      subroutine h_fill_water_raw_hist(Abort,err)
*
*     routine to fill lucite raw data histograms and hit pattern
*
*     Author:	Jinghua Liu
*     Date:     18 Feb. 2000
*
* Revision 1.0  2000/02/18 10:51:23  Jinghua
* Initial version
*
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*20 here
      parameter (here='h_fill_water_raw_hist')
*
      logical ABORT
      character*(*) err

      real*4 histval
      integer*4 ind
      integer*4 la,co,adcpos,adcneg,tdcpos,tdcneg
      include 'hks_data_structures.cmn'
      include 'hks_water_parms.cmn'
      include 'hks_pedestals.cmn'
      include 'hks_id_histid.cmn'          
      include 'gen_event_info.cmn'
      integer*4 counter_hits(HNUM_WAT_LAYERS,HNUM_WAT_COUNTERS)
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
      
      Call HF1(hidwatrawtothits,float(hwat_raw_tot_hits),1.)

*     Make sure there is at least 1 hit
      if(hwat_raw_tot_hits .lt. 1 )  Return
      
*     clear hit counter
      Do la=1,HNUM_WAT_LAYERS
         Do co=1,HNUM_WAT_COUNTERS
            counter_hits(la,co) = 0
         EndDo
      EndDo

c     Write(*,*) '-ev,tot=',gen_event_ID_number,hwat_raw_tot_hits
*     Loop over all counters
      do ind = 1, hwat_raw_tot_hits
         la = hwat_raw_layer_num(ind)
         co = hwat_raw_counter_num(ind)
         counter_hits(la,co)=counter_hits(la,co)+1
         adcpos = hwat_rawadc_pos(ind)
         adcneg = hwat_rawadc_neg(ind)
         tdcpos = hwat_rawtdc_pos_sub_trig(ind)
         tdcneg = hwat_rawtdc_neg_sub_trig(ind)
c     Write(*,*) 'fillwcraw,la,co,apan,tptn=',
c     &        la,co,adcpos,adcneg,tdcpos,tdcneg
c     --- hit > 0, fill raw hit pattern.
         Call HF1(hidwatrawhitpat(la),float(co),1.)         
c     --- layer hit
         Call HF1(hidwatrawlayer,float(la),1.)

         if(tdcpos .ne. -1) then
            call HF1(hidwatrawtdchitpatpos(la),float(co),1.)
            call HF1(hidwatsumpostdc(la),float(tdcpos),1.)
         EndIf
         if(tdcneg .ne. -1) then
            call HF1(hidwatrawtdchitpatneg(la),float(co),1.)
            call HF1(hidwatsumnegtdc(la),float(tdcneg),1.)
         EndIf
         if(adcpos .ne. -1) then
            call HF1(hidwatrawadchitpatpos(la),float(co),1.)
            call HF1(hidwatsumposadc(la),float(adcpos),1.)
         EndIf
         if(adcneg .ne. -1) then
            call HF1(hidwatrawadchitpatneg(la),float(co),1.)
            call HF1(hidwatsumnegadc(la),float(adcneg),1.)
         EndIf

         if(hturnon_wc_raw_hist .eq. 1) then
*     Fill ADC histograms.
            if(counter_hits(la,co).eq.1) then ! ignore double hits in ADC
               histval = float(hwat_rawadc_pos(ind))
               call hf1(hidwatrawposadc(la,co),histval,1.)
               histval = float(hwat_rawadc_neg(ind))
               call hf1(hidwatrawnegadc(la,co),histval,1.)
            endif
*     Fill TDC histograms.
            histval = float(hwat_rawtdc_pos_sub_trig(ind))
            call hf1(hidwatrawpostdc(la,co),histval,1.)
            histval = float(hwat_rawtdc_neg_sub_trig(ind))
            call hf1(hidwatrawnegtdc(la,co),histval,1.)
         EndIf
      EndDo

      return
      end
