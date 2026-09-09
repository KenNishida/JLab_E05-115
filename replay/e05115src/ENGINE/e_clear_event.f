      Subroutine e_clear_event(ABORT,err)
*--------------------------------------------------------
*     
*     August 12, 1999        Y.Fujii        Initial version
*     
*     Resets all HES quantities before event is processed.
*     
*     This routine will be called from ENGINE/g_clear_event.f.
*     
*     $Log: e_clear_event.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:46  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.4  2005/06/10 18:56:06  cdaq
*     add trig,tul,fission ntuple variables
*
*     Revision 1.3  2005/06/06 21:53:30  cdaq
*     reset fission variables
*
*     Revision 1.2  2005/06/05 01:59:02  cdaq
*     reset Escin tdc value to -70000
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.6  2005/04/19 17:43:05  miyoshi
*     change variables
*
*     Revision 1.5  2005/03/10 16:42:25  miyoshi
*     add new variables for EDC
*
*     Revision 1.4  2005/01/20 21:26:55  sumihama
*     Mod. clear/reset
*
*     Revision 1.3  2005/01/19 22:53:24  sumihama
*     Mod. clear_event
*
*     Revision 1.2  2005/01/03 19:27:58  miyoshi
*     add edc ntuple
*
*     Revision 1.1.1.1  2004/08/30 21:21:38  miyoshi
*     new dir
*
*     Revision 1.4  2000/03/09 01:32:36  ysato
*     Update in the production run Mar.8
*     
*     Revision 1.3  1999/12/23 19:59:18  ysato
*     Compiled on Redhat Linux
*     
*     Revision 1.2  1999/11/02 15:55:02  ysato
*     Upgrade for HNSS
*     
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*13 here
      Parameter (here='e_clear_event')
*     
      Logical ABORT
      Character*(*) err
      Integer*4 i

*--------------------------------------------------------
      Include "hes_data_structures.cmn"
      Include "hes_tracking.cmn"

      ABORT=.FALSE.
      err=' '
    
*     Initialize common/hes_raw_dc/
      edc1_raw_tot_hits=0
      edc2_raw_tot_hits=0
      
*     Initialize common/hes_decoded_dc/
      edc1_tot_hits=0
      edc2_tot_hits=0
      
*     Initialize common/hes_raw_scin/
      escin_raw_tot_hits=0

      Do i=1,EMAX_SCIN_HITS
         escin_rawtdc_pos(i)=-70000
         escin_rawtdc_neg(i)=-70000
         escin_rawtdc_pos_sub_trig(i)=-70000
         escin_rawtdc_neg_sub_trig(i)=-70000
      EndDo

      
*     Initialize common/hes_decoded_scin/
      escin_tot_hits=0

*     Initialize fission chamber tot hits
      efis_pos_raw_hits = 0
      efis_time_raw_hits = 0
      
*     Initialize common/hes_misc_raw/
      emisc_raw_tot_hits=0
      
*     Initialize common/hes_track_test/
      edc1nspace_points_tot = 0
      edc2nspace_points_tot = 0
      entracks_pre = 0
      entracks_fp = 0
      
*     Initialize common/hes_physics/
      enphysics = 0

*     
      etul_raw_tot_hits = 0
      etul_tot_hits = 0

  
      Return
      End

