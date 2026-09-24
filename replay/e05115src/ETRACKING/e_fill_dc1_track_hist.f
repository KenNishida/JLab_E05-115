      Subroutine e_fill_dc1_track_hist(ABORT,err)
*--------------------------------------------------------
*     Fill Enge tracking results histograms
*     
*     $Log: e_fill_dc1_track_hist.f,v $
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
      Parameter (here='e_fill_dc1_track_hist')
*     
      Logical ABORT
      Character*(*) err
*     
*     
      Include "gen_event_info.cmn"
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"
      Include "hes_tracking.cmn"
      Include "hes_statistics.cmn"
      
      Integer*4 i,j,k,l,itrk,hit,la,ihit,sl
      Real*4 re,dt,wire
      real*4 e_drift_dist_calc
      external e_drift_dist_calc

      ABORT= .FALSE.
             err= ' '
      
*     --- First, fill the best one.
      itrk=edc1bestchi2_pre_index(enum_fitting)
      
      Do ihit = 2,entrack_hits_pre(itrk,1)+1
         hit = entrack_hits_pre(itrk,ihit)
         la = edc1_layer_num(hit)
         sl=edc1_slot_num(hit)
         wire=edc1_wire_center(hit)
         re = edc1_track_coord_pre(itrk,la,enum_fitting)
     &        -edc1_wire_center(hit)
         if (.not.gen_event_ts_flag(2)) then
            dt=edc1_drift_time(hit)-starttime(itrk)
         else
            dt=edc1_drift_time(hit)
         endif
         
* for Ntuple DK
         edc1_drift_time_pre(itrk,la,enum_fitting)=dt
         edc1_drift_dis_pre(itrk,la,enum_fitting)=e_drift_dist_calc(la,sl,dt)
         edc1_wire_center_pre(itrk,la,enum_fitting)=wire

c      Write(*,*) dt,re
         Call HF2(eiddc1distime(la),dt,re,1.)
cDK --- for dldt func from here        
         Call HF1(eiddc1drifttime(sl),dt,1.)
         Call HF1(eiddc1singdtime(la),dt,1.)
         Call HF1(eiddc1driftdis(la),re,1.)
         Call HF1(5200+la,edc1_track_coord_pre(itrk,la,enum_fitting),1.)
         Call HF2(5100+la,re,dt,1.)
         Call HF2(5000+la,abs(re),dt,1.)
         if(re .ge. 0) then
            Call HF2(5140+la,re,dt,1.)
         else
            Call HF2(5120+la,re,dt,1.)
         endif
cDK --- to here      
      EndDo

      Call HF1(eiddc1chi2perdofpre,edc1chi2perdof_pre(itrk,enum_fitting),1.)
      Call HF1(eiddc1chi2perdofprezoom,edc1chi2perdof_pre(itrk,enum_fitting),1.)
      
      Do i=1,emax_num_dc1_layers
*         re=edc1_residual_pre(itrk,i,enum_fitting)
         re=edc1eff_residual(itrk,i,enum_fitting)
cDK         write(*,*) re,edc1_residual_pre(itrk,i,enum_fitting)
cDK         if(edc1eff_flag(itrk).eq.55) then
            Call HF1(eiddc1residualpre(i),re,1.)
cDK         endif
      EndDo
      

      return
      end
