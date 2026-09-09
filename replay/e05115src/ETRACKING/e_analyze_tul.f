      subroutine e_analyze_tul(abort,errmsg)
*-------------------------------------------------------------------
* author: Miyoshi
* created: 6/9/05
*
* e_analyze_tul just cut tul hits by tdc window.
*
*--------------------------------------------------------

      implicit none

      include 'hes_data_structures.cmn'
      include 'hes_id_histid.cmn'

      logical abort
      character*(*) errmsg
      character*20 here
      parameter (here = 'e_analyze_tul')
      Integer*4 i,j,k,tdcmin,tdcmax

      save

      abort = .false.
      errmsg = ' '

      Call HF1(eidtulrawtothits,float(etul_raw_tot_hits),1.)
      if(etul_raw_tot_hits .le .0) Return
      
      tdcmin = etul_tdc_min
      tdcmax = etul_tdc_max
      if(tdcmax .eq. 0) tdcmax = 10000

      etul_tot_hits=0
      Do i=1,etul_raw_tot_hits
         Call HF1(eidtulrawtotchannelnum,
     &        float(etul_raw_channel_num(i)),1.)
         Call HF1(eidtulrawtdc,float(etul_raw_tdc(i)),1.)
         if(etul_raw_tdc(i) .gt. tdcmin .and.
     &        etul_raw_tdc(i) .lt. tdcmax) then
            etul_tot_hits = etul_tot_hits + 1
            etul_channel_num(etul_tot_hits) =
     &           etul_raw_channel_num(i)
            etul_tdc(etul_tot_hits) = 
     &           etul_raw_tdc(i)
            Call HF1(eidtultotchannelnum,
     &           float(etul_channel_num(i)),1.)
         EndIf
      EndDo
      Call HF1(eidtultothits,float(etul_tot_hits),1.)

      return
      end
