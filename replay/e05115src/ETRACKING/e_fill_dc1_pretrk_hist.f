      Subroutine e_fill_dc1_pretrk_hist(ABORT,err)
*--------------------------------------------------------
*     Routine to fill histogra with DC1 space points
*     03/21/2004 Miyoshi
*     for E01-011
*     
*     Input Banks    hes_dc1_space_points
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_fill_dc1_pretrk_hist')
*     
      Logical ABORT
      Character*(*) err
      
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"
      Include "hes_tracking.cmn"

      
      Integer*4 i,j,la,wi,ihit,hits
      real*4 da,wcoord

      ABORT= .FALSE.
      err= ' '
      
      Call HF1(eiddc1ntrackspre,float(entracks_pre),1.)
      Call HF1(eiddc1ntracksprezoom,float(entracks_pre),1.)
      
      if(entracks_pre .le. 0) Return
      Do i=1,entracks_pre
         ihit = entrack_hits_pre(i,1)
         Do j=1,ihit
            hits = entrack_hits_pre(i,j+1)
            la = edc1_layer_num(hits)
            wcoord = EDC1_WIRE_COORD(hits)
            da = wcoord-EDC1_WIRE_CENTER(hits)
c       Write(*,*) 'pretrk',edc1_wire_coord(hits),edc1_wire_center(hits),
c     &           da,la
c            Call Hf1(eiddc1driftdis(la),da,1.)
            Call Hf1(eiddc1wcoord(la),wcoord,1.)
         EndDo
      EndDo




      Return
      end

