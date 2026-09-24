      Subroutine h_fill_dc_track_hist(ABORT,err)
*--------------------------------------------------------
*     Fill HES tracking results histograms
*     
*     $Log: h_fill_dc_track_hist.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.2  2005/07/06 02:32:32  sumihama
*     Mod. hist
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.4  2005/04/08 19:08:56  miyoshi
*     add histogram filling parts
*
*     Revision 1.3  2005/03/14 19:52:17  miyoshi
*     change tracking variables and histogram names
*
*     Revision 1.2  2005/03/07 16:39:09  miyoshi
*     correct histogramming
*
*     Revision 1.1  2005/03/02 22:29:55  miyoshi
*     initial version
*
*     Revision 1.1.1.1  2004/08/30 21:21:41  miyoshi
*     new dir
*
*     Revision 1.3  2000/03/09 01:32:40  ysato
*     Update in the production run Mar.8
*     
*     Revision 1.2  2000/02/11 20:44:07  ysato
*     Local update on KTRACKING
*     
*     Revision 1.1  1999/12/23 19:59:25  ysato
*     Compiled on Redhat Linux
*     
*
* This routine is called from k_track.f
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Character*50 here
      Parameter (here='h_fill_dc_track_hist')
*     
      Logical ABORT
      Character*(*) err
*     
*     
      Include "hks_data_structures.cmn"
      Include "hks_id_histid.cmn"
      Include "hks_tracking.cmn"
      
      Integer*4 i,j,k,l,itrk,hit,la,ihit
      Real*4 re,dt
      
      ABORT= .FALSE.
      err= ' '
      
      itrk = 0
*     --- First, fill the best one.
      itrk=hdc_bestchi2_index
c     --- for dummy data
      if(hntracks_fp .gt. 0 .and. hdc_bestchi2_index .eq. 0) 
     &     itrk = hntracks_fp
      
      if(itrk .gt. 0) then
         Do ihit = 2,hntrack_hits(itrk,1)+1
            hit = hntrack_hits(itrk,ihit)
            la = hdc_layer_num(hit)
            re = hdc_track_coord(itrk,la)-hdc_wire_center(hit)
            dt = hdc_drift_time(hit)
c     Write(*,*) dt,re
            Call HF2(hiddcdistime(la),dt,re,1.)
         EndDo
         
         Call HF1(hidchi2perdofpre,hchi2perdof_fp(itrk),1.)
         Call HF1(hidchi2perdofprezoom,hchi2perdof_fp(itrk),1.)

         Do i=1,hmax_num_dc_layers
            re=hdc_single_residual(itrk,i)
            Call HF1(hidresidualpre(i),re,1.)
         EndDo
      EndIf                     ! itrk > 0

      return
      end
