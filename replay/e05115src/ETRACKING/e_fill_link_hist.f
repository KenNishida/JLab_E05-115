      Subroutine e_fill_link_hist(ABORT,err)
*--------------------------------------------------------
* This routine is called from e_link_tracks.f
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Logical ABORT
      Character*(*) err
*--------------------------------------------------------
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"

      character*15 here
      parameter (here='e_fill_link_hist')

      Integer*4 i
      
      Do i=1,entracks_fp
         Call HF1(eidtrkhodohit,float(etrk_hodo_hit(i,1)),1.)
         Call HF1(eidtrkfptime,etrk_fptime(i),1.)
      EndDo
      
      Return
      End

