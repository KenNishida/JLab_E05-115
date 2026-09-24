      Subroutine e_fill_dc2_track_hist(ABORT,err)
*--------------------------------------------------------
*     Fill Enge tracking results histograms
*     
*     $Log: e_fill_dc2_track_hist.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.2  2005/09/19 22:05:51  sumihama
*     Mod. EDC1 tracking by Akhiko and starttime from EHODO
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.2  2005/03/10 16:48:23  miyoshi
*     change tracking variable name
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
      Parameter (here='e_fill_dc2_track_hist')
*     
      Logical ABORT
      Character*(*) err
*     
*     
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"
      Include "hes_tracking.cmn"
      Include "hes_statistics.cmn"
      
      Integer*4 i,j,k,l,itrk,hit,la,ihit
      Real*4 re,dt,res

      ABORT= .FALSE.
             err= ' '
      
*     --- First, fill the best one.
      itrk=edc2bestchi2_index
      
      if (itrk.le.0) return
c      Call HF1(eiddc2ntracksfp,float(entracks_fp),1.)
c      Call HF1(eiddc2ntracksfpzoom,float(entracks_fp),1.)
      
      Do ihit = 2,edc2ntrack_hits_pre(itrk,1)+1
         hit = edc2ntrack_hits_pre(itrk,ihit)
         la = edc2_layer_num(hit)
         re = edc2_track_coord_pre(itrk,la)
     &        -edc2_wire_center(hit)
* MIZUKI
c         print *, 'fill', hit,edc2_drift_time(hit)
         dt = edc2_drift_time(hit)
* for Ntuple DK
         edc2_drift_time_pre(itrk,la)=dt
         edc2_drift_dis_pre(itrk,la)=re

c      Write(*,*) dt,re
         Call HF2(eiddc2distime(la),dt,re,1.)
cDK --- for dldt func from here        
         Call HF1(eiddc2singdtime(la),dt,1.)
c         Call HF1(eiddc2driftdis(la),re,1.)
c         Call HF1(5110+la,edc2_track_coord_pre(itrk,la),1.)
c         Call HF2(5000+la,re,dt,1.)
c         Call HF2(4990+la,abs(re),dt,1.)
c         if(re .ge. 0) then
c            Call HF2(5020+la,re,dt,1.)
c         else
c            Call HF2(5010+la,re,dt,1.)
c         endif
cDK --- to here      
        res = edc2_residual_pre(itrk,la)
        Call HF1(eiddc2singleresidual(la),res,1.)
      EndDo

      Call HF1(eiddc2chi2perdoffp,edc2chi2perdof(itrk),1.)
      Call HF1(eiddc2chi2perdoffpzoom,edc2chi2perdof(itrk),1.)
      
      
      Call HF1(eiddc2chi2perdoffp,echi2perdof_fp(itrk),1.)
      Call HF1(eiddc2chi2perdoffpzoom,echi2perdof_fp(itrk),1.)
      Call HF1(eiddc2fpx,ex_fp_pre2(itrk),1.)
      Call HF1(eiddc2fpy,ey_fp_pre2(itrk),1.)
      Call HF1(eiddc2fpxp,exp_fp_pre2(itrk),1.)
      Call HF1(eiddc2fpyp,eyp_fp_pre2(itrk),1.)
      Call HF2(eiddc2fpxy,ex_fp_pre2(itrk),ey_fp_pre2(itrk),1.)
      Call HF2(eiddc2fpxpyp,exp_fp_pre2(itrk),eyp_fp_pre2(itrk),1.)
      Call HF2(eiddc2fpxxp,ex_fp_pre2(itrk),exp_fp_pre2(itrk),1.)
      Call HF2(eiddc2fpxyp,ex_fp_pre2(itrk),eyp_fp_pre2(itrk),1.)
      Call HF2(eiddc2fpyxp,ey_fp_pre2(itrk),exp_fp_pre2(itrk),1.)
      Call HF2(eiddc2fpyyp,ey_fp_pre2(itrk),eyp_fp_pre2(itrk),1.)

      return
      end
