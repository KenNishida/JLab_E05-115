      subroutine h_fill_aer_raw_hist(Abort,err)
*     
*     routine to fill Aerogel raw data histograms and hit pattern
*     
*     Revision 1.1 03/16/2004 Miyoshi
*     copy from h_fill_water_raw_hist
*     
*     Revision 1.0  2000/02/18 10:51:23  Jinghua
*     Initial version
*     
*     
*--------------------------------------------------------
      IMPLICIT NONE
*     
      character*20 here
      parameter (here='h_fill_aer_raw_hist')
*     
      logical ABORT
      character*(*) err
      
      real*4 histval
      integer*4 ind,la,co,adcpos,adcneg,tdcpos,tdcneg

      include 'hks_data_structures.cmn'
      include 'hks_aero_parms.cmn'
      include 'hks_pedestals.cmn'
      include 'hks_id_histid.cmn'          
      include 'gen_event_info.cmn'
      integer*4 counter_hits(HNUM_AER_LAYERS,HNUM_AER_COUNTERS)
*     
      SAVE
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '
      
      Call HF1(hidaerrawtothits,float(haer_raw_tot_hits),1.)

*     Make sure there is at least 1 hit

      if(haer_raw_tot_hits .lt. 1 )  Return

*     clear hit counter
      Do la=1,HNUM_AER_LAYERS
         Do co=1,HNUM_AER_COUNTERS
            counter_hits(la,co) = 0
         EndDo
      EndDo

c     Write(*,*) '(fillaerraw)-ev=',gen_event_id_number,haer_raw_tot_hits
*     Loop over all counters
      do ind = 1, haer_raw_tot_hits
         la = haer_raw_layer_num(ind)
         co = haer_raw_counter_num(ind)
         counter_hits(la,co)=counter_hits(la,co)+1
         adcpos = haer_rawadc_pos(ind)
         adcneg = haer_rawadc_neg(ind)
         tdcpos = haer_rawtdc_pos_sub_trig(ind)
         tdcneg = haer_rawtdc_neg_sub_trig(ind)
c         Write(*,*) 'l,c,apan,tptn=',la,co,adcpos,adcneg,tdcpos,tdcneg
c     --- hit > 0, fill raw hit pattern.
         Call HF1(hidaerrawhitpat(la),float(co),1.) 
c     --- layer hit
         Call HF1(hidaerrawlayer,float(la),1.)
         if(tdcpos .ne. -1) then
            call HF1(hidaerrawtdchitpatpos(la),float(co),1.)
            call HF1(hidaersumpostdc(la),float(tdcpos),1.)
         EndIf
         if(tdcneg .ne. -1) then
            call HF1(hidaerrawtdchitpatneg(la),float(co),1.)
            call HF1(hidaersumnegtdc(la),float(tdcneg),1.)
         EndIf
         if(adcpos .ne. -1) then
            call HF1(hidaerrawadchitpatpos(la),float(co),1.)
            call HF1(hidaersumposadc(la),float(adcpos),1.)
         EndIf
         if(adcneg .ne. -1) then
            call HF1(hidaerrawadchitpatneg(la),float(co),1.)
            call HF1(hidaersumnegadc(la),float(adcneg),1.)
         EndIf

         if(hturnon_ac_raw_hist .eq. 1) then
*     Fill ADC histograms.
          if(counter_hits(la,co).eq.1) then ! ignore double hits in ADC
             histval = float(haer_rawadc_pos(ind))
             call hf1(hidaerrawposadc(la,co),histval,1.)
             histval = float(haer_rawadc_neg(ind))
             call hf1(hidaerrawnegadc(la,co),histval,1.)
          endif
*     Fill TDC histograms.
            histval = float(haer_rawtdc_pos_sub_trig(ind))
            call hf1(hidaerrawpostdc(la,co),histval,1.)
            histval = float(haer_rawtdc_neg_sub_trig(ind))
            call hf1(hidaerrawnegtdc(la,co),histval,1.)
         EndIf
      enddo
      
      return
      end
