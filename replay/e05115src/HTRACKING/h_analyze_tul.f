      subroutine h_analyze_tul(abort,errmsg)
*-------------------------------------------------------------------
* author: Miyoshi
* created: 6/9/05
*
* e_analyze_tul just cut tul hits by tdc window.
*
*--------------------------------------------------------

      implicit none

      include 'hks_data_structures.cmn'
      include 'hks_id_histid.cmn'

      logical abort
      character*(*) errmsg
      character*20 here
      parameter (here = 'h_analyze_tul')
      Integer*4 i,j,k,tdcmin,tdcmax
      Integer*4 totch

      save

      abort = .false.
      errmsg = ' '

      Call HF1(hidtulrawtothits,float(htul_raw_tot_hits),1.)
      if(htul_raw_tot_hits .le .0) Return
      
      tdcmin = htul_tdc_min
      tdcmax = htul_tdc_max
      if(tdcmax .eq. 0) tdcmax = 10000
      
      htul_tot_hits=0
      Do i=1,htul_raw_tot_hits
         totch = 32*(htul_raw_module_num(i)-1)+htul_raw_channel_num(i)
         Call HF1(hidtulrawtotchannelnum,float(totch),1.)
         Call HF1(hidtulrawtdc,float(htul_raw_tdc(i)),1.)
         if(htul_raw_tdc(i) .gt. tdcmin .and.
     &        htul_raw_tdc(i) .lt. tdcmax) then
            htul_tot_hits = htul_tot_hits + 1
            htul_module_num(htul_tot_hits) =
     &           htul_raw_module_num(i) 
            htul_channel_num(htul_tot_hits) =
     &           htul_raw_channel_num(i)
            htul_tdc(htul_tot_hits) = 
     &           htul_raw_tdc(i)
            Call HF1(hidtultotchannelnum,float(totch),1.)
         EndIf
      EndDo
      Call HF1(hidtultothits,float(htul_tot_hits),1.)

      return
      end
