      subroutine h_fill_aero_hist(Abort,err)
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
      parameter (here='h_fill_aero_hist')
*     
      logical ABORT
      character*(*) err
      
      real*4 histval
      integer*4 ind,la,co
      include 'hks_data_structures.cmn'
      include 'hks_aero_parms.cmn'
      include 'hks_pedestals.cmn'
      include 'hks_id_histid.cmn'          
      integer*4 counter_hits(HNUM_AER_LAYERS,HNUM_AER_COUNTERS)
*     
      SAVE
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '
      
      Call HF1(hidaertothits,float(haer_tot_hits),1.)
*     Make sure there is at least 1 hit
      if(haer_tot_hits .lt. 1 ) then
         return
      endif

*     clear hit counter
      Do la=1,HNUM_AER_LAYERS
         Do co=1,HNUM_AER_COUNTERS
            counter_hits(la,co) = 0
         EndDo
      EndDo

*     Loop over all counters
      do ind=1, haer_tot_hits
         la=haer_layer_num(ind)
         co=haer_counter_num(ind)
         counter_hits(la,co)=counter_hits(la,co)+1

         call HF1(hidaerlayer,float(la),1.)
         call HF1(hidaerhitpat(la),float(co),1.)

         if(counter_hits(la,co).eq.1) then ! ignore double hits in Npe
            histval = haer_pos_npe(ind)
            call hf1(hidaerposnpe(la,co),histval,1.)
            histval = haer_neg_npe(ind)
            call hf1(hidaernegnpe(la,co),histval,1.)
            histval = haer_npe_sum(ind)
            call hf1(hidaersumnpe(la,co),histval,1.)
         endif

         if(hturnon_ac_dec_hist .eq. 1) then
*     Fill time histograms.
            histval = haer_pos_time(ind)
c            call hf1(hidaerpostime(la,co),histval,1.)
            call hf1(hidaerpostime(1,1),histval,1.)
            histval = haer_neg_time(ind)
c            call hf1(hidaernegtime(la,co),histval,1.)         
            call hf1(hidaerpostime(1,1),histval,1.)         
         EndIf
         
         histval = haer_npe_sum(ind)
         call hf1(hidaernpesum(la),histval,1.)         
      enddo
      
      Do ind=1,HNUM_AER_LAYERS
         Call HF1(hidaerlayerhits(ind),float(haer_layer_hits(ind)),1.)
      EndDo

      return
      end
