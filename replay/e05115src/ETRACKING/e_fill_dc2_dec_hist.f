      Subroutine e_fill_dc2_dec_hist(ABORT,err)
*--------------------------------------------------------
*     Routine to fill histograms with hes_dec_dc2 varibles
*     03/21/2004 Miyoshi
*     for E01-011
*     
*     Input Banks    hes_raw_decodec_dc2
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_fill_dc2_dec_hist')
*     
      Logical ABORT
      Character*(*) err
      
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"
      Include "hes_tracking.cmn"
      Include "hes_geometry.cmn"
      
      Integer*4 i,j,la,wi,sl,cs
      Real*4 wc,dt,val,val2
      
      ABORT= .FALSE.
      err= ' '
      
      Call Hf1(eiddc2dectothit,Float(edc2_tot_hits),1.)
      Call Hf1(eiddc2dectothitzoom,Float(edc2_tot_hits),1.)
      
      if(edc2_tot_hits .le. 0) Return

*     --- debug histogram
      Do i=1,emax_num_dc2_layers
         Do j=1,edc2_max_wires_per_layer
            val = float(edc2wire_early_mult(j,i))
            val2 = float(edc2_max_wires_per_layer*(i-1)+j)
            if(val .gt. 0 .and. eiddc2earlymult .gt. 0) then
               Call Hf1(eiddc2earlymult,val2,val)
            EndIf
            val = float(edc2wire_late_mult(j,i))
            val2 = float(edc2_max_wires_per_layer*(i-1)+j)
            if(val .gt. 0 .and. eiddc2latemult .gt. 0) then
               Call Hf1(eiddc2latemult,val2,val)
            EndIf
            val = float(edc2wire_extra_mult(j,i))
            val2 = float(edc2_max_wires_per_layer*(i-1)+j)
            if(val .gt. 0 .and. eiddc2extramult .gt. 0) then
               Call Hf1(eiddc2extramult,val2,val)
            EndIf
         EndDo
      EndDo

*     
      Do i=1,edc2_tot_hits
         la=edc2_layer_num(i)
         wi=edc2_wire_num(i)
*     --- ex.) layer=1&offset=2
*     --- 3-18 is slot=1,19-34 is slot=2...
*     --- layer2&offset2,3-18 is slot=8,...
         if(eturnon_dc2_dec_hist .ne. 0 ) then
            if(la .eq. 1 .or. la .eq. 2) then
               sl = 6*(la-1) 
     &              + int((wi-edc2_wire_offset_low(la)-1)/16) + 1
            Else if(la .eq. 3 .or. la .eq. 4) then
               sl = 12 + 8*(la-3) 
     &              + int((wi-edc2_wire_offset_low(la)-1)/16) + 1
            Else if(la .eq. 5 .or. la .eq. 6) then
               sl = 28 + 6*(la-5) 
     &              + int((wi-edc2_wire_offset_low(la)-1)/16) + 1
            EndIf
         EndIF
         wc=edc2_wire_center(i)
         dt=edc2_drift_time(i)

c     Write(*,*) la,wi,sl,wc,dt

         Call HF1(eiddc2wirecenter(la),wc,1.)
         Call HF1(eiddc2drifttime(sl),dt,1.)
c         Call HF1(eiddc2singdtime(la),dt,1.) ! move to e_track_fit

         cs = edc2_cluster_size(i)
         if(i .lt. edc2_tot_hits .and.
     &        edc2_cluster_size(i+1) .eq. 1)
     &        Call HF1(eiddc2cluster(la),float(cs),1.)
         if(i .eq. edc2_tot_hits) 
     &        Call HF1(eiddc2cluster(la),float(cs),1.)
      EndDo
      
      Do i=1,emax_num_dc2_layers
         Call Hf1(eiddc2layerhit(i),float(edc2_hits_per_layer(i)),1.)
      EndDo

      Return
      end
