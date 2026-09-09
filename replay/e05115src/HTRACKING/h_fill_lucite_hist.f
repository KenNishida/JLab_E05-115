      subroutine h_fill_lucite_hist(Abort,err)

*     routine to fill lucite data histograms and hit pattern
*
*     Author:	Jinghua Liu
*     Date:     18 Feb. 2000
*
* Revision 1.2 2009/08/17 yez
* Add Lucite sum channels for E05-115
*
* Revision 1.2 2009/07/09 yez
* Convert Water info into Lucite info for E05-115
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
      parameter (here='h_fill_lucite_hist')
*
      logical ABORT
      character*(*) err

      real*4 histval
      integer*4 ind
      integer*4 la,co
      include 'hks_data_structures.cmn'
      include 'hks_lucite_parms.cmn'
      include 'hks_pedestals.cmn'
      include 'hks_id_histid.cmn'          
      integer*4 counter_hits(HNUM_LUC_LAYERS,HNUM_LUC_COUNTERS)
      integer*4 countersum_hits(HNUM_LUC_LAYERS,HNUM_LUC_COUNTERS)
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
      
      Call HF1(hidluctothits,float(hluc_tot_hits),1.)
*     Make sure there is at least 1 hit
      if(hluc_tot_hits .LT. 1 ) then
         return
      endif

*     clear hit counter
      Do la=1,HNUM_LUC_LAYERS
         Do co=1,HNUM_LUC_COUNTERS
            counter_hits(la,co) = 0
            countersum_hits(la,co) = 0
         EndDo
      EndDo

*     Loop over all counters
      do ind=1, HLUC_TOT_HITS
         la=hluc_layer_num(ind)
         co=hluc_counter_num(ind)
         counter_hits(la,co)=counter_hits(la,co)+1

         Call HF1(hidluclayer,float(la),1.)
         Call HF1(hidluchitpat(la),float(co),1.)

*     Fill ADC and npe histograms.
         if(counter_hits(la,co).eq.1) then ! ignore double hits in Npe
            histval = hluc_pos_npe(ind)
            call hf1(hidlucposnpe(la,co),histval,1.)
            histval = hluc_neg_npe(ind)
            call hf1(hidlucnegnpe(la,co),histval,1.)        
            histval = hluc_npe_sum(ind)
            call hf1(hidlucsumnpe(la,co),histval,1.)        
          endif

          if(hturnon_lc_dec_hist .eq. 1) then
*     Fill time histograms.
             histval = hluc_pos_time(ind)
             call hf1(hidlucpostime(la,co),histval,1.)
             histval = hluc_neg_time(ind)
             call hf1(hidlucnegtime(la,co),histval,1.)
          EndIf
          
          histval = hluc_npe_sum(ind)
          call hf1(hidlucnpesum(la),histval,1.)       

      enddo                     ! Loop over hluc_tot_hits

*     Loop over PID sums
      do ind=1, HLUCSUM_TOT_HITS
         la=hlucsum_layer_num(ind)
         co=hlucsum_counter_num(ind)
         countersum_hits(la,co)=countersum_hits(la,co)+1

c         Call HF1(hidluclayer,float(la),1.)
c         Call HF1(hidluchitpat(la),float(co),1.)

*     Fill ADC and npe histograms.
         if(countersum_hits(la,co).eq.1) then ! ignore double hits in Npe
            histval = hluc_tot_npe(ind)
            call hf1(hidluctotnpe(la,co),histval,1.)        
         endif
         
         if(hturnon_lc_dec_hist .eq. 1) then
*     Fill time histograms.
            histval = hluc_tot_time(ind)
            call hf1(hidluctottime(la,co),histval,1.)
         EndIf
      enddo                     ! Loop over hlucsum_tot_hits

      Do ind=1,hnum_luc_layers
         call HF1(hidluclayerhits(ind),float(hluc_layer_hits(ind)),1.)
      EndDo

      return
      end
