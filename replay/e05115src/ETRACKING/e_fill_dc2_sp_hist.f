      Subroutine e_fill_dc2_sp_hist(ABORT,err)
*--------------------------------------------------------
*     Routine to fill histograms with DC space points
*     03/21/2004 Miyoshi
*     for E01-011
*     
*     Input Banks    hks_dc_space_points
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_fill_dc2_sp_hist')
*     
      Logical ABORT
      Character*(*) err
      
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"
      Include "hes_tracking.cmn"
      
      Integer*4 i,la,wi

      ABORT= .FALSE.
      err= ' '
      
      Call HF1(eiddc2nspacepoint,float(edc2nspace_points_tot),1.)
      Call HF1(eiddc2nspacepointzoom,float(edc2nspace_points_tot),1.)
      if(edc2nspace_points_tot .le. 0) Return
      Do i=1,edc2nspace_points_tot
         Call HF1(eiddc2nspacepointhits,float(edc2space_point_hits(i,1)),1.)
      EndDo
      
      Return
      end
