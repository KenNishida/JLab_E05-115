      Subroutine e_fill_dc1_sp_hist(ABORT,err)
*--------------------------------------------------------
*     Routine to fill histograms with DC space points
*     03/21/2004 Miyoshi
*     for E01-011
*     
*     Input Banks    hes_dc1_space_points
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_fill_dc1_sp_hist')
*     
      Logical ABORT
      Character*(*) err
      
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"
      Include "hes_tracking.cmn"
      
      Integer*4 i,la,wi,ihit,hit,pln
      Integer*4 have_hit(10)
           
      Do pln=1,10
         have_hit(pln)=0
      EndDo

      ABORT= .FALSE.
      err= ' '
      
      Call HF1(eiddc1nspacepoint,float(edc1nspace_points_tot),1.)
      Call HF1(eiddc1nspacepointzoom,float(edc1nspace_points_tot),1.)
c      write(*,*) "sp",edc1nspace_points_tot
      if(edc1nspace_points_tot .le. 0) Return
      Do i=1,edc1nspace_points_tot
         Call HF1(eiddc1nspacepointhits,float(edc1space_point_hits(i,1)),1.)
*DK from here
         Do ihit=1,edc1space_point_hits(i,1)
            hit = edc1space_point_hits(i,ihit+2)
            la = edc1_layer_num(hit)
            Do pln=1,10
               if(pln.eq.la) then
                  have_hit(pln)=1
               endif
            EndDo
         EndDO
         Do pln=1,10
            Call HF1(5100+pln,float(have_hit(pln)),1.)
         EndDo
*DK to here
      EndDo
      
      Return
      end
