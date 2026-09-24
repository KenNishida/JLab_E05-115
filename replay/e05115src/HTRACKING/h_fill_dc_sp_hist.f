      Subroutine h_fill_dc_sp_hist(ABORT,err)
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
      Parameter (here='h_fill_dc_sp_hist')
*     
      Logical ABORT
      Character*(*) err
      
      Include "hks_data_structures.cmn"
      Include "hks_id_histid.cmn"
      Include "hks_tracking.cmn"
      
      Integer*4 i,la,wi

      ABORT= .FALSE.
      err= ' '
      
      Call HF1(hiddcnspacepoint,float(hnspace_points_tot),1.)
      Call HF1(hiddcnspacepointzoom,float(hnspace_points_tot),1.)
      if(hnspace_points_tot .le. 0) Return
      Do i=1,hnspace_points_tot
         Call HF1(hiddcnspacepointhits,float(hspace_point_hits(i,1)),1.)
      EndDo
      
      Return
      end
