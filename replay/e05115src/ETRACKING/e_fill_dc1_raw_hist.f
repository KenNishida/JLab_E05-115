      Subroutine e_fill_dc1_raw_hist(ABORT,err)
*--------------------------------------------------------
*     Routine to fill histograms with hnss_raw_dc1 varibles
*     03/21/2004 Miyoshi
*     for E01-011
*     
*     Input Banks    hes_raw_dc1
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_fill_dc1_raw_hist')
*     
      Logical ABORT
      Character*(*) err
      
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"
      Include "hes_geometry.cmn"
      Include 'gen_f1tdc.cmn'
      Include 'gen_rocid.cmn'

      Integer*4 i,la,wi,sl,t1
      Integer*4 layerhit(10) 

      ABORT= .FALSE.
      err= ' '
      
      Do i = 1,10
         layerhit(i)=0
      enddo

      Call Hf1(eiddc1rawtothit,Float(edc1_raw_tot_hits),1.)
      Call Hf1(eiddc1rawtothitzoom,Float(edc1_raw_tot_hits),1.)
      
      if(edc1_raw_tot_hits .le. 0) Return
      
      Do i = 1,edc1_raw_tot_hits
         Call HF1(eiddc1rawtdcall,float(edc1_raw_tdc_sub_trig(i)),1.)
         la = edc1_raw_layer_num(i)
         wi = edc1_raw_wire_num(i)
         layerhit(la)=layerhit(la)+1
*     --- ex.) layer=1&offset=2
*     --- 3-18 is slot=1,19-34 is slot=2...
*     --- layer2&offset2,3-18 is slot=8,...
         sl = 7*(la-1) + int((wi-edc1_wire_offset_low(la)-1)/16) 
     &        + 1
         Call HF1(eiddc1rawhitpat(la),float(wi),1.)
         Call HF1(eiddc1rawlayertdc(la),
     &        float(edc1_raw_tdc_sub_trig(i)),1.)
*** MIZUKI
         if(eturnon_dc1_raw_hist .eq. 1) then
            Call HF1(eiddc1rawtdc(sl),float(edc1_raw_tdc_sub_trig(i)),1.)
         EndIf

      EndDo
      
      Do i = 1,10
         Call HF1(eiddc1rawlayerhit(i),float(layerhit(i)),1.)
      enddo

      Return
      end
