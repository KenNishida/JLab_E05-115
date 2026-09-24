      subroutine h_fill_dc_raw_hist(Abort,err)
*     
*     routine to fill histograms with hks_raw_dc varibles
*     
*     Author:	D. F. Geesaman
*     Date:     30 March 1994
*     Modified:  9 April 1994     D. F. Geesaman
*     Put id's in sos_tracking_histid
*     implement flag to turn block off
*     $Log: h_fill_dc_raw_hist.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.3  2005/05/30 22:28:39  miyoshi
*     add/delete histogram
*
*     Revision 1.2  2005/05/28 00:42:20  cdaq
*     change f1trigtime hist
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.4  2005/03/07 16:39:09  miyoshi
*     correct histogramming
*
*     Revision 1.3  2005/03/02 22:31:23  miyoshi
*     modify histogramming
*
*     Revision 1.2  2005/03/02 17:22:38  miyoshi
*     move histid params
*
*     Revision 1.1  2005/03/01 23:52:02  miyoshi
*     initial version
*
*     Revision 1.2  2004/12/24 21:37:10  miyoshi
*     change name plane to layer
*
*     
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'h_fill_dc_raw_hist')
*     
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 layer,ihit,la,wi,sl,t1
      integer*4 layerhit(12)
*     
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'hks_geometry.cmn'
      include 'hks_id_histid.cmn'
      include 'gen_event_info.cmn'
      Include 'gen_f1tdc.cmn'
      Include 'gen_rocid.cmn'
*     
      SAVE
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '
*     
      Call Hf1(hiddcrawtothit,Float(hdc_raw_tot_hits),1.)
      Call Hf1(hiddcrawtothitzoom,Float(hdc_raw_tot_hits),1.)

      
*     Is histogramming flag set
*     Make sure there is at least 1 hit
      if(HDC_RAW_TOT_HITS .le. 0 ) Return
      
      do layer=1,hmax_num_dc_layers
        layerhit(layer)=0
      enddo
      
*     Loop over all hits
      do ihit=1,HDC_RAW_TOT_HITS 
         Call HF1(hiddcrawtdcall,float(hdc_raw_tdc_sub_trig(ihit)),1.)
         la=HDC_RAW_LAYER_NUM(ihit)
         wi=HDC_RAW_WIRE_NUM(ihit)
         Call HF1(hiddcrawhitpat(la),float(wi),1.)
         Call HF1(hiddcrawlayertdc(la),
     &        float(hdc_raw_tdc_sub_trig(ihit)),1.)
         if(hturnon_dc_raw_hist.ne.0 ) then
            if(la .eq. 1 .or. la .eq. 2) then
               sl = 6*(la-1) 
     &              + int((wi-hdc_wire_offset_low(la)-1)/16) + 1
            Else if(la .eq. 3 .or. la .eq. 4) then
               sl = 12 + 8*(la-3) 
     &              + int((wi-hdc_wire_offset_low(la)-1)/16) + 1
            Else if(la .eq. 5 .or. la .eq. 6) then
               sl = 28 + 6*(la-5) 
     &              + int((wi-hdc_wire_offset_low(la)-1)/16) + 1
            Else if(la .eq. 7 .or. la .eq. 8) then
               sl = 40 + 6*(la-7) 
     &              + int((wi-hdc_wire_offset_low(la)-1)/16) + 1
            Else if(la .eq. 9 .or. la .eq. 10) then
               sl = 52 + 8*(la-9) 
     &              + int((wi-hdc_wire_offset_low(la)-1)/16) + 1
            Else if(la .eq. 11 .or. la .eq. 12) then
               sl = 68 + 6*(la-11) 
     &              + int((wi-hdc_wire_offset_low(la)-1)/16) + 1
            EndIf
            Call HF1(hiddcrawtdc(sl),
     &           float(hdc_raw_tdc_sub_trig(ihit)),1.)
         endif                  ! end test on histogram block turned on.
         layerhit(la)=layerhit(la)+1         
      EndDo                     ! end loop over hits
      
      do layer=1,hmax_num_dc_layers
         Call HF1(hiddcrawlayerhit(layer),float(layerhit(layer)),1.)
      enddo
      
      RETURN
      END

