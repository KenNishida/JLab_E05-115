      subroutine h_fill_scin_raw_hist(Abort,err)
*
*     routine to fill histograms with hks_raw and hks_raw_scin varibles
*
*     Author:	D. F. Geesaman
*     Date:     4 April 1994
*
*     Modified  9 April 1994     DFG
*                                Add CTP flag to turn on histogramming
*                                id's in sos_id_histid
* $Log: h_fill_scin_raw_hist.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.3  2005/05/30 22:28:39  miyoshi
* add/delete histogram
*
* Revision 1.2  2005/05/30 18:11:55  miyoshi
* change histname
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.7  2005/03/14 19:50:49  miyoshi
* change variables names
*
* Revision 1.6  2005/03/02 17:25:30  miyoshi
* fix minor bug
*
* Revision 1.5  2005/02/11 18:53:12  miyoshi
* clean up used/unused hist
*
* Revision 1.4  2004/12/24 21:37:10  miyoshi
* change name plane to layer
*
* Revision 1.3  2004/12/24 19:47:07  miyoshi
* change minor
*
* Revision 1.2  2004/12/23 19:38:01  sumihama
* Add and delete Hbooks, add h_fill_scin_dec_hist.f
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.7  1996/01/17 19:04:54  cdaq
* (JRA)
*
* Revision 1.6  1995/10/10 13:27:45  cdaq
* (JRA) Remove some unneeded validity tests
*
* Revision 1.5  1995/07/20 14:52:20  cdaq
* (JRA) Fill hist's from "all" data structures
*
* Revision 1.4  1995/05/22  19:45:39  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/11  21:04:14  cdaq
* (JRA) Modifications to user histograms
*
* Revision 1.2  1995/02/10  19:11:36  cdaq
* (JRA) Change sscin_num_counters to snum_scin_counters
*
* Revision 1.1  1994/04/13  20:07:48  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      external thgetid
      integer*4 thgetid
      character*20 here
      parameter (here='h_fill_scin_raw_hist')
*
      logical ABORT
      character*(*) err
      real*4 histval
      real*4 rcnt
      integer*4 t1
      integer*4 pln,cnt,ihit
      integer*4 adcpos,adcneg,tdcpos,tdcneg
c     --- fastbus data limit
      Integer*4 adcmin,adcmax
      Integer*4 tdcmin,tdcmax
      Parameter(adcmin=0)
      Parameter(adcmax=8192)
      Parameter(tdcmin=15000)
      Parameter(tdcmax=50000)

      include 'hks_data_structures.cmn'
      include 'hks_scin_parms.cmn'
      include 'hks_id_histid.cmn'          
      Include "gen_f1tdc.cmn"
      include 'gen_rocid.cmn'          
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
      
      Call HF1(hidscinrawtothits,float(hscin_raw_tot_hits),1.)
      
*     Do we want to histogram raw scintillators      
*     Make sure there is at least 1 hit
      if(hscin_raw_tot_hits .le. 0 ) Return
      t1 = g_f1_refer_time(HR_VME_ROCID,1,1)
      Call Hf1(gidf1trigtimehist(1,1,1),float(t1),1.)
      
      tdcpos=-70000 
      tdcneg=-70000 
*     Loop over raw hits
      do ihit=1,hscin_raw_tot_hits
         pln=hscin_raw_layer_num(ihit)
         cnt=hscin_raw_counter_num(ihit)
         adcpos=hscin_raw_adc_pos(ihit)
         adcneg=hscin_raw_adc_neg(ihit)
         tdcpos=hscin_rawtdc_pos_sub_trig(ihit) 
         tdcneg=hscin_rawtdc_neg_sub_trig(ihit) 
*     Fill layer map                  
         histval = float(pln)
         call HF1(hidscinrawlayer,histval,1.)
*     Fill counter map
         histval = float(cnt)
         call hf1(hidscinrawcounters(pln),histval,1.)
*     Fill ADC and TDC histograms for positive tubes.
         if(adcpos .ne. -1) then
            histval = real(hscin_raw_adc_pos(ihit))
     &           -hscin_raw_ped_pos(pln,cnt)
            call hf1(hidscinsumposadc(pln),histval,1.)
            if(adcpos .gt. adcmin .and. adcpos .lt. adcmax) then
               call hf1(hidscinrawadchitpatpos(pln),float(cnt),1.)
            EndIF
         EndIf
         
         if (tdcpos .ne. -1) then !tube was hit.
            histval = float(tdcpos)
            call hf1(hidscinsumpostdc(pln),histval,1.)
            if(tdcpos .gt. tdcmin .and. tdcpos .lt. tdcmax) then
               call hf1(hidscinrawtdchitpatpos(pln),float(cnt),1.)
            EndIF
         endif                  ! tdc ne 1 
         
*     Fill ADC and TDC histograms for negative tubes.
         if(adcneg .ne. -1) then
            histval = Real(hscin_raw_adc_neg(ihit))
     &           -hscin_raw_ped_neg(pln,cnt)
            call hf1(hidscinsumnegadc(pln),histval,1.)
            if(adcneg .gt. adcmin .and. adcneg .lt. adcmax) then
               call hf1(hidscinrawadchitpatneg(pln),float(cnt),1.)
            EndIF
         EndIf
         if (tdcneg .ne. -1) then !tube was hit.
            histval = float(tdcneg)
            call hf1(hidscinsumnegtdc(pln),histval,1.)
            if(tdcneg .gt. tdcmin .and. tdcneg .lt. tdcmax) then
               call hf1(hidscinrawtdchitpatneg(pln),float(cnt),1.)
            EndIF
         endif
         
         if(hturnon_scin_raw_hist .ne. 0 ) then
            histval = float(adcpos)
            call hf1(hidscinrawposadc(pln,cnt),histval,1.)
            histval = float(adcneg)
            call hf1(hidscinrawnegadc(pln,cnt),histval,1.)
            histval = float(tdcpos)
            call hf1(hidscinrawpostdc(pln,cnt),histval,1.)
            histval = float(tdcneg)
            call hf1(hidscinrawnegtdc(pln,cnt),histval,1.)
         endif                  ! end test on histogramming flag
      enddo                     ! end loop over hits

      return
      end
