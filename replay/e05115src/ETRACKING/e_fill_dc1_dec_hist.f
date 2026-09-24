      Subroutine e_fill_dc1_dec_hist(ABORT,err)
*--------------------------------------------------------
*     Routine to fill histograms with hes_dec_dc1 varibles
*     03/21/2004 Miyoshi
*     for E05-115
*     
*     Input Banks    hes_raw_decodec_dc1
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_fill_dc1_dec_hist')
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
      
      Call Hf1(eiddc1dectothit,Float(edc1_tot_hits),1.)
      Call Hf1(eiddc1dectothitzoom,Float(edc1_tot_hits),1.)
      
      if(edc1_tot_hits .le. 0) Return

*     --- debug histogram
      Do i=1,emax_num_dc1_layers
         Do j=1,edc1_max_wires_per_layer
            val = float(edc1wire_early_mult(j,i))
            val2 = float(edc1_max_wires_per_layer*(i-1)+j)
            if(val .gt. 0 .and. eiddc1earlymult .gt. 0) then
               Call Hf1(eiddc1earlymult,val2,val)
            EndIf
            val = float(edc1wire_late_mult(j,i))
            val2 = float(edc1_max_wires_per_layer*(i-1)+j)
            if(val .gt. 0 .and. eiddc1latemult .gt. 0) then
               Call Hf1(eiddc1latemult,val2,val)
            EndIf
            val = float(edc1wire_extra_mult(j,i))
            val2 = float(edc1_max_wires_per_layer*(i-1)+j)
            if(val .gt. 0 .and. eiddc1extramult .gt. 0) then
               Call Hf1(eiddc1extramult,val2,val)
            EndIf
         EndDo
      EndDo

*     
      Do i=1,edc1_tot_hits
         la=edc1_layer_num(i)
         wi=edc1_wire_num(i)
*     --- ex.) layer=1&offset=2
*     --- 3-18 is slot=1,19-34 is slot=2...
*     --- layer2&offset2,3-18 is slot=8,...
         sl = 7*(la-1) + int((wi-edc1_wire_offset_low(la)-1)/16) 
     &        + 1
         wc=edc1_wire_center(i)
         dt=edc1_drift_time(i)

c         Write(*,*) 'dec',la,wi,sl,wc,dt

         Call HF1(eiddc1wirecenter(la),wc,1.)
c         if(eturnon_dc1_dec_hist .eq. 1) then
c            Call HF1(eiddc1drifttime(sl),dt,1.)
c         EndIf
c         Call HF1(eiddc1singdtime(la),dt,1.) ! move to e_track_fit

         cs = edc1_cluster_size(i)
         if(i .lt. edc1_tot_hits .and.
     &        edc1_cluster_size(i+1) .eq. 1)
     &        Call HF1(eiddc1cluster(la),float(cs),1.)
         if(i .eq. edc1_tot_hits) 
     &        Call HF1(eiddc1cluster(la),float(cs),1.)
      EndDo
      
      Do i=1,emax_num_dc1_layers
         Call Hf1(eiddc1layerhit(i),float(edc1_hits_per_layer(i)),1.)
      EndDo

      Return
      end
