      Subroutine h_fill_scin_dec_hist(ABORT,err)
*--------------------------------------------------------
* Fill HKS-HTOF decoded data histograms
*
* Revision 1.1  2004/12/23 Sumihama
*
* This routine is called from h_trans_scin.f
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Logical ABORT
      Character*(*) err
*--------------------------------------------------------
      Include "hks_data_structures.cmn"
      Include "hks_id_histid.cmn"
      Include "hks_scin_tof.cmn"

      character*15 here
      parameter (here='h_fill_scin_dec_hist')

      Integer*4 i,n,iscin,la,co
      Real*4 val
      
*     --- Decoded data ---
      Call Hf1(hidscindectothits,Float(hscin_tot_hits),1.)
      
      if(hscin_tot_hits .le. 0) return
      
      Do i=1,hnum_scin_layers
         val = Real(hscin_hits_per_layer(i))
         Call HF1(hidscindeclayerhits(i),val,1.)
      EndDo

      Do i=1,hscin_tot_hits
         la = hscin_layer_num(i)
         co = hscin_counter_num(i)
         if(  hscin_tdc_pos(i) .gt. hscin_tdc_min .and.
     &        hscin_tdc_pos(i) .lt. hscin_tdc_max) then
            Call Hf1(hidscinpattdcpos(la),Float(co),1.)
         EndIf
         if(  hscin_tdc_neg(i) .gt. hscin_tdc_min .and.
     &        hscin_tdc_neg(i) .lt. hscin_tdc_max) then
            Call Hf1(hidscinpattdcneg(la),Float(co),1.)
         EndIf
         if(  hscin_tdc_pos(i) .gt. hscin_tdc_min .and.
     &        hscin_tdc_pos(i) .lt. hscin_tdc_max .and.
     &        hscin_tdc_neg(i) .gt. hscin_tdc_min .and.
     &        hscin_tdc_neg(i) .lt. hscin_tdc_max) then
            Call Hf1(hidscinpattdcboth(la),Float(co),1.)
         EndIf
         
         if(  hscin_adc_pos(i) .gt. 0 ) then
            Call Hf1(hidscinpatadcpos(la),Float(co),1.)
         EndIf
         if(  hscin_adc_neg(i) .gt. 0 ) then
            Call Hf1(hidscinpatadcneg(la),Float(co),1.)
         EndIf
         if(  hscin_adc_pos(i) .gt. 0 .and.
     &        hscin_adc_neg(i) .gt. 0 ) then
            Call Hf1(hidscinpatadcboth(la),Float(co),1.)
         EndIf
         
         if(htwo_good_times(i)) then
            Call Hf1(hidscinlayer,float(la),1.)
            Call Hf1(hidscincounters(la),float(co),1.)
         EndIf

         if(hturnon_scin_dec_hist .ne. 0 ) then
            Call Hf1(hidscinposadc(la,co),hscin_adc_pos(i),1.)
            Call Hf1(hidscinnegadc(la,co),hscin_adc_neg(i),1.)
            Call Hf1(hidscinpostime(la,co),hscin_pos_time(i),1.)
            Call Hf1(hidscinnegtime(la,co),hscin_neg_time(i),1.)
         endif
      EndDo

      Return
      End

