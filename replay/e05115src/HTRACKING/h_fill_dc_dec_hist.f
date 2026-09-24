      subroutine h_fill_dc_dec_hist(Abort,err)
*     
*     routine to fill histograms with sos_decoded_dc varibles
*     
*     Author:	D. F. Geesaman
*     Date:     30 March 1994
*     Modified:  9 April 1994     D. F. Geesaman
*     Put id's in sos_tracking_histid
*     implement flag to turn block off
*     $Log: h_fill_dc_dec_hist.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
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
*     Revision 1.2  2004/12/24 21:37:10  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.5  1996/04/30 17:12:04  saw
*     (JRA) Comment out SDC_DRIFT_DIS and SDC_DRIFT_TIME histograms
*     
*     Revision 1.4  1995/08/31 18:42:28  cdaq
*     (JRA) Comment out filling of siddcwirecent (wire center) histogram
*     
*     Revision 1.3  1995/05/22  19:45:38  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.2  1994/08/18  04:33:13  cdaq
*     (SAW) Indentation changes
*     
*     Revision 1.1  1994/04/13  18:10:22  cdaq
*     Initial revision
*     
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'h_fill_dc_dec_hist')
*     
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 layer,ihit,la,wi,i,j,sl,cs
      real*4 wc,dt,val,val2
*     
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'hks_id_histid.cmn'
      include 'hks_geometry.cmn'
      include 'gen_event_info.cmn'
*     
      SAVE
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '
*     
      Call Hf1(hiddcdectothit,Float(hdc_tot_hits),1.)
      Call Hf1(hiddcdectothitzoom,Float(hdc_tot_hits),1.)
      
      if(hdc_tot_hits .le. 0) Return
      
*     --- debug histogram
      Do i=1,hmax_num_dc_layers
         Do j=1,hdc_max_wires_per_layer
            val = float(hwire_early_mult(j,i))
            val2 = float(hdc_max_wires_per_layer*(i-1)+j)
            if(val .gt. 0 .and. hiddcearlymult .gt. 0) then
               Call Hf1(hiddcearlymult,val2,val)
            EndIf
            val = float(hwire_late_mult(j,i))
            val2 = float(hdc_max_wires_per_layer*(i-1)+j)
            if(val .gt. 0 .and. hiddclatemult .gt. 0) then
               Call Hf1(hiddclatemult,val2,val)
            EndIf
            val = float(hwire_extra_mult(j,i))
            val2 = float(hdc_max_wires_per_layer*(i-1)+j)
            if(val .gt. 0 .and. hiddcextramult .gt. 0) then
               Call Hf1(hiddcextramult,val2,val)
            EndIf
         EndDo
      EndDo

*     
      Do i=1,hdc_tot_hits
         la=hdc_layer_num(i)
         wi=hdc_wire_num(i)
         wc=hdc_wire_center(i)
         dt=hdc_drift_time(i) 
         cs = hdc_cluster_size(i)
         Call HF1(hiddcdectdcall,float(hdc_tdc(i)),1.)
         Call HF1(hiddcwirecenter(la),wc,1.)
c         Write(*,*) '(hfilldcdec,la,wi,wc,dt=)',la,wi,wc,dt

         if(hturnon_dc_dec_hist .ne. 0 ) then
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
            Call HF1(hiddcdrifttimesl(sl),dt,1.)
         EndIF
         
         if(i .lt. hdc_tot_hits .and.
     &        hdc_cluster_size(i+1) .eq. 1)
     &        Call HF1(hiddccluster(la),float(cs),1.)
         if(i .eq. hdc_tot_hits) 
     &        Call HF1(hiddccluster(la),float(cs),1.)
         
c         call hf1(hiddcdrifttime(la),dt,1.)
         
      EndDo                     ! hdc_tot_hits
      
      Do i=1,hmax_num_dc_layers
         Call Hf1(hiddclayerhit(i),float(hdc_hits_per_layer(i)),1.)
      EndDo
      
      RETURN
      END

