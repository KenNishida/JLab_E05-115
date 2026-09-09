      Subroutine e_fill_dc2_pretrk_hist(ABORT,err)
*--------------------------------------------------------
*     Routine to fill histogra with DC1 space points
*     03/21/2004 Miyoshi
*     for E01-011
*     
*     Input Banks    hes_dc2_space_points
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_fill_dc2_pretrk_hist')
*     
      Logical ABORT
      Character*(*) err
      
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"
      Include "hes_tracking.cmn"

      
      Integer*4 i,j,la,wi,ihit,hits
      real*4 da

      ABORT= .FALSE.
      err= ' '
      
      Call HF1(eidentrackspre,float(entracks_pre),1.)
      Call HF1(eidentracksprezoom,float(entracks_pre),1.)
      
      if(entracks_pre .le. 0) Return
      Do i=1,entracks_pre
         ihit = entrack_hits_pre(i,1)
         Do j=1,ihit
            hits = entrack_hits_pre(i,j+1)
            la = edc2_layer_num(hits)
            da = EDC2_WIRE_COORD(hits)-EDC2_WIRE_CENTER(hits)
c     Write(*,*) edc2_wire_coord(hits),edc2_wire_center(hits),
c     &           da,la
            Call Hf1(eiddc2driftdis(la),da,1.)
         EndDo
      EndDo




      Return
      end

