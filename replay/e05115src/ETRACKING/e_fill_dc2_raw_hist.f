      subroutine e_fill_dc2_raw_hist(Abort,err)
*     
*     routine to fill histograms with hks_raw_dc varibles
*     
*     Author:	D. F. Geesaman
*     Date:     30 March 1994
*     Modified:  9 April 1994     D. F. Geesaman
*     Put id's in sos_tracking_histid
*     implement flag to turn block off
*     $Log: e_fill_dc2_raw_hist.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
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
      parameter (here= 'e_fill_dc2_raw_hist')
*     
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 layer,ihit,la,wi,sl,t1,i
      integer*4 layerhit(6)
*     
      include 'hes_data_structures.cmn'
      include 'hes_tracking.cmn'
      include 'hes_geometry.cmn'
      include 'hes_id_histid.cmn'
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
      do i=1,6
         layerhit(i)=0
      enddo

      Call Hf1(eiddc2rawtothit,Float(edc2_raw_tot_hits),1.)
      Call Hf1(eiddc2rawtothitzoom,Float(edc2_raw_tot_hits),1.)

*     Is histogramming flag set
*     Make sure there is at least 1 hit
      if(EDC2_RAW_TOT_HITS .le. 0 ) Return
      
*     Loop over all hits
      do ihit=1,EDC2_RAW_TOT_HITS 
         Call HF1(eiddc2rawtdcall,float(edc2_raw_tdc_sub_trig(ihit)),1.)
         la=EDC2_RAW_LAYER_NUM(ihit)
         wi=EDC2_RAW_WIRE_NUM(ihit)
         layerhit(la)=layerhit(la)+1
         Call HF1(eiddc2rawhitpat(la),float(wi),1.)
         Call HF1(eiddc2rawlayertdc(la),
     &        float(edc2_raw_tdc_sub_trig(ihit)),1.)
         if(eturnon_dc2_raw_hist.ne.0 ) then
            if(la .eq. 1 .or. la .eq. 2) then
               sl = 6*(la-1) 
     &              + int((wi-edc2_wire_offset_low(la)-1)/16) + 1
            Else if(la .eq. 3 .or. la .eq. 4) then
               sl = 12 + 8*(la-3) 
     &              + int((wi-edc2_wire_offset_low(la)-1)/16) + 1
            Else if(la .eq. 5 .or. la .eq. 6) then
               sl = 28 + 6*(la-5) 
     &              + int((wi-edc2_wire_offset_low(la)-1)/16) + 1
            Else if(la .eq. 7 .or. la .eq. 8) then
               sl = 40 + 6*(la-7) 
     &              + int((wi-edc2_wire_offset_low(la)-1)/16) + 1
            Else if(la .eq. 9 .or. la .eq. 10) then
               sl = 52 + 8*(la-9) 
     &              + int((wi-edc2_wire_offset_low(la)-1)/16) + 1
            Else if(la .eq. 11 .or. la .eq. 12) then
               sl = 68 + 6*(la-11) 
     &              + int((wi-edc2_wire_offset_low(la)-1)/16) + 1
            EndIf
            Call HF1(eiddc2rawtdc(sl),
     &           float(edc2_raw_tdc_sub_trig(ihit)),1.)
         endif                  ! end test on histogram block turned on.         
      EndDo                     ! end loop over hits
      
      do i=1,6
         Call HF1(eiddc2rawlayerhit(i),float(layerhit(i)),1.)
      enddo

      Call Hf1(eiddc2rawtothit,Float(edc2_raw_tot_hits),1.)
      Call Hf1(eiddc2rawtothitzoom,Float(edc2_raw_tot_hits),1.)
      
      RETURN
      END

