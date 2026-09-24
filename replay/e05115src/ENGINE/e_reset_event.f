      Subroutine e_reset_event(ABORT,err)
*--------------------------------------------------------
* 
* March 2, 1999        Y.Fujii        A first draft
* July  9, 1999        Y.Fujii        Revised
*
* Resets all ENGE quantities before event is processed.
*
* This routine will be called from ENGINE/g_reset_event.f.
* Called once at the begin of the analysis.
*
* $Log: e_reset_event.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.3  2005/07/08 13:55:21  sumihama
* Mod. Max hit number
*
* Revision 1.2  2005/06/06 21:54:55  cdaq
* reset fission variables
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.9  2005/04/19 17:43:05  miyoshi
* change variables
*
* Revision 1.8  2005/03/10 16:42:25  miyoshi
* add new variables for EDC
*
* Revision 1.7  2005/02/11 23:01:12  sumihama
* Mod. e-ntuple, Mod. etracking
*
* Revision 1.6  2005/01/24 19:48:11  miyoshi
* add/delete some common blocks
*
* Revision 1.5  2005/01/20 21:26:55  sumihama
* Mod. clear/reset
*
* Revision 1.4  2005/01/19 22:53:24  sumihama
* Mod. clear_event
*
* Revision 1.3  2005/01/05 18:41:00  sumihama
* enge-scinti mod.
*
* Revision 1.2  2004/12/24 21:22:43  miyoshi
* change name plane to layer
*
* Revision 1.1.1.1  2004/08/30 21:21:38  miyoshi
* new dir
*
* Revision 1.5 2004/03/03 Miyoshi
* for E01-011
* Revision 1.4  2000/03/09 01:32:37  ysato
* Update in the production run Mar.8
*
* Revision 1.3  1999/12/23 19:59:19  ysato
* Compiled on Redhat Linux
*
* Revision 1.2  1999/11/02 15:55:04  ysato
* Upgrade for ENGE
*
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Character*50 here
      Parameter (here='e_reset_event')
*
      Logical ABORT
      Character*(*) err
*--------------------------------------------------------
      Include "hes_data_structures.cmn"
      Include "hes_pedestals.cmn"
      Include "hes_tracking.cmn"

      Integer i,j,k
      
      ABORT=.FALSE.
      err=' '
      
*     Initialize common/hes_scin_pedestals/
      do i=1,enum_scin_layers
         do j=1,enum_scin_counters
            escin_pos_ped_num(i,j) = 0
            escin_pos_ped_sum2(i,j) = 0
            escin_pos_ped_sum(i,j) = 0
            escin_pos_ped_num(i,j) = 0
            escin_pos_ped_sum2(i,j) = 0
            escin_pos_ped_sum(i,j) = 0
            escin_neg_ped_num(i,j) = 0
            escin_neg_ped_sum2(i,j) = 0
            escin_neg_ped_sum(i,j) = 0
            escin_neg_ped_num(i,j) = 0
            escin_neg_ped_sum2(i,j) = 0
            escin_neg_ped_sum(i,j) = 0
         enddo
      enddo

C-----------------------|
C         EDC1          |
C-----------------------| 
*     Initialize common/hes_raw_dc/
      edc1_raw_tot_hits = 0
      Do i = 1, emax_dc1_hits
         edc1_raw_layer_num(i)   = 0
         edc1_raw_wire_num(i)    = 0
         edc1_raw_tdc(i)         = 0
         edc1_raw_tdc_sub_trig(i)= 0
      EndDo
      
*     Initialize common/hes_decoded_dc/
      edc1_tot_hits=0
c      Do i=1,EMAX_DC_HITS
      Do i=1,EMAX_DC1_DEC_HITS
         edc1_layer_num(i) = 0
         edc1_group_num(i) = 0
         edc1_wire_num(i)  = 0
         edc1_slot_num(i)  = 0
         edc1_tdc(i) = 0
         edc1_cluster_size(i) = 0
         edc1_drift_time(i)  = 0.0
         edc1_drift_dis(i)   = -10000. 
         edc1_wire_center(i) = 0.0
         edc1_wire_coord(i)  = -10000.
      EndDo

      Do i = 1, EMAX_NUM_DC1_LAYERS
         edc1_hits_per_layer(i) = 0
      EndDo      

*     Initialize common /hes_fitting_test in hes_tracking.cmn
      
      entracks_pre = 0
      entracks_fp = 0
      Do i=1,entracks_max
         Do j=1,emax_num_dc1_layers
            edc1_single_residual(i,j) = -100000.
            edc1_track_coord(i,j) = -100000.
         endDo
      EndDo

*     Initialize common/ENGE_TRACKING/
      edc1nspace_points_tot = 0
      Do i=1,edc1max_space_points
         edc1space_points(i,1) = -10000.
         edc1space_points(i,2) = -10000.
         Do j=1,edc1max_hits_per_point+2
            edc1space_point_hits(i,j) = 0
         EndDo
      EndDo

C-----------------------|
C         EDC2          |
C-----------------------| 
*     Initialize common/hes_raw_dc/
      edc2_raw_tot_hits = 0
      Do i = 1, emax_dc2_hits
         edc2_raw_layer_num(i)   = 0
         edc2_raw_wire_num(i)    = 0
         edc2_raw_tdc(i)         = 0
         edc2_raw_tdc_sub_trig(i)= 0
      EndDo
      
*     Initialize common/hes_decoded_dc/
      edc2_tot_hits=0
c      Do i=1,EMAX_DC_HITS
      Do i=1,EMAX_DC1_DEC_HITS
         edc2_layer_num(i) = 0
         edc2_group_num(i) = 0
         edc2_wire_num(i)  = 0
         edc2_slot_num(i)  = 0
         edc2_tdc(i) = 0
         edc2_cluster_size(i) = 0
         edc2_drift_time(i)  = 0.0
         edc2_drift_dis(i)   = -10000. 
         edc2_wire_center(i) = 0.0
         edc2_wire_coord(i)  = -10000.
      EndDo

      Do i = 1, EMAX_NUM_DC2_LAYERS
         edc2_hits_per_layer(i) = 0
      EndDo      

*     Initialize common /hes_fitting_test in hes_tracking.cmn
      
*DK      entracks_pre = 0
*DK      entracks_fp = 0
      Do i=1,entracks_max
         Do j=1,emax_num_dc2_layers
            edc2_single_residual(i,j) = -100000.
            edc2_track_coord(i,j) = -100000.
         endDo
      EndDo

*     Initialize common/ENGE_TRACKING/
      edc2nspace_points_tot = 0
      Do i=1,edc2max_space_points
         edc2space_points(i,1) = -10000.
         edc2space_points(i,2) = -10000.
         Do j=1,edc2max_hits_per_point+2
            edc2space_point_hits(i,j) = 0
         EndDo
      EndDo
      
C-----------------------|
C         Hodo          |
C-----------------------| 
*     Initialize common/hes_raw_scin/
      escin_raw_tot_hits=0
      Do i=1,EMAX_SCIN_HITS
         escin_raw_layer_num(i)=0
         escin_raw_counter_num(i)=0
         escin_rawadc_pos(i)=0
         escin_rawtdc_pos(i)=-70000
         escin_rawadc_neg(i)=0
         escin_rawtdc_neg(i)=-70000
         escin_rawtdc_pos_sub_trig(i)=-70000
         escin_rawtdc_neg_sub_trig(i)=-70000
      EndDo
      
*     Initialize common/hes_decoded_scin/
      escin_tot_hits = 0
      Do i=1,EMAX_SCIN_DEC_HITS
         escin_layer_num(i)  = 0
         escin_counter_num(i)= 0
         escin_good_hits(i)  = -1
         escin_adc_pos(i)    = -1
         escin_adc_neg(i)    = -1
         escin_tdc_pos(i)    = -70000
         escin_tdc_neg(i)    = -70000
         escin_time_pos(i)   = -1000.0
         escin_time_neg(i)   = -1000.0
         escin_mean_time(i)  = -1000.0
      EndDo

*     ---- fission ----
      efis_pos_raw_hits = 0
      efis_time_raw_hits = 0
      
C-----------------------|
C         MISC          |
C-----------------------| 
*     Initialize common/hes_misc_raw/
      emisc_raw_tot_hits=0
      Do i=1,EMAX_MISC_HITS
         emisc_raw_adc(i)=0
         emisc_raw_tdc(i)=0
         emisc_raw_layer_num(i)=0
         emisc_raw_counter_num(i)=0
         emisc_raw_dummy(i)=0
      EndDo
      
C-----------------------|
C         Physics       |
C-----------------------| 
*     Initialize common /hes_focal_plane/
      
      entracks_fp = 0
      Do i=1,entracks_max
         ex_fp(i) = -100000
         ey_fp(i) = -100000
         ez_fp(i) = -100000
         exp_fp(i) = -100000
         eyp_fp(i) = -100000
         echi2_fp(i) = 10000000
         echi2perdof_fp(i) =10000000
         Do j=1,entrackhits_max+1
            entrack_hits(i,j) = 0
         EndDo
      EndDo

*     Initialize common/hes_taregt/
      
      entracks_tar = 0
      Do i=1,entracks_max
         ex_tar(i)  = -100000
         ey_tar(i)  = -100000
         ez_tar(i)  = -100000
         exp_tar(i) = -100000
         eyp_tar(i) = -100000
         edelta_tar(i) = -1000
         ep_tar(i)     = -1000
         echi2_tar(i)  = -100000
         do j = 1,5
            do k = 1,5
               edel_tar(j,k,i)   = -1000
            enddo
         enddo
         enfree_tar(i) = -1
         elink_tar_fp(i) = -1
      EndDo

*     Initialize common/hes_phsics/
      enphysics=0
      Do i=1,ENPHYSICS_MAX
         estime_at_fp(i) = 0 
         estime_at_tar(i) = 0
         esdelta(i) = 0
         esx_fp(i) = 0
         esx_tar(i) = 0
         esxp_fp(i) = 0
         esyp_fp(i) = 0
         esy_tar(i) = 0
         esxp_tar(i) = 0
         esyp_tar(i) = 0
      EndDo

      Return
      End

