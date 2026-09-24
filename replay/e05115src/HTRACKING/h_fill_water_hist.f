      subroutine h_fill_water_hist(Abort,err)
*
*     routine to fill water data histograms and hit pattern
*
*     Author:	Jinghua Liu
*     Date:     18 Feb. 2000
*
* Revision 1.1 2004/4 Miyoshi
* for E01-011
*
* Revision 1.0  2000/02/18 10:51:23  Jinghua
* Initial version
*
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*20 here
      parameter (here='h_fill_water_hist')
*
      logical ABORT
      character*(*) err

      real*4 histval
      integer*4 ind
      integer*4 la,co
      include 'hks_data_structures.cmn'
      include 'hks_water_parms.cmn'
      include 'hks_pedestals.cmn'
      include 'hks_id_histid.cmn'          
      integer*4 counter_hits(HNUM_WAT_LAYERS,HNUM_WAT_COUNTERS)
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
      
      Call HF1(hidwattothits,float(hwat_tot_hits),1.)
*     Make sure there is at least 1 hit
      if(hwat_tot_hits .LT. 1 ) then
         return
      endif

*     clear hit counter
      Do la=1,HNUM_WAT_LAYERS
         Do co=1,HNUM_WAT_COUNTERS
            counter_hits(la,co) = 0
         EndDo
      EndDo

*     Loop over all counters
      do ind=1, HWAT_TOT_HITS
         la=hwat_layer_num(ind)
         co=hwat_counter_num(ind)
         counter_hits(la,co)=counter_hits(la,co)+1

         Call HF1(hidwatlayer,float(la),1.)
         Call HF1(hidwathitpat(la),float(co),1.)

*     Fill ADC and npe histograms.
         if(counter_hits(la,co).eq.1) then ! ignore double hits in Npe
            histval = hwat_pos_npe(ind)
            call hf1(hidwatposnpe(la,co),histval,1.)
            histval = hwat_neg_npe(ind)
            call hf1(hidwatnegnpe(la,co),histval,1.)         
         endif

         if(hturnon_wc_dec_hist .eq. 1) then
*     Fill time histograms.
            histval = hwat_pos_time(ind)
            call hf1(hidwatpostime(1,1),histval,1.)
c            call hf1(hidwatpostime(la,co),histval,1.)
            histval = hwat_neg_time(ind)
c            call hf1(hidwatnegtime(la,co),histval,1.)
         EndIf
         
         histval = hwat_npe_sum(ind)
         call hf1(hidwatnpesum(la),histval,1.)       

      enddo

      Do ind=1,hnum_wat_layers
         call HF1(hidwatlayerhits(ind),float(hwat_layer_hits(ind)),1.)
      EndDo

      return
      end
