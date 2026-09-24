      Subroutine e_reconstruction(ABORT,err)
*--------------------------------------------------------
*     
*     $Log: e_reconstruction.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.2  2005/06/10 18:57:53  cdaq
*     add tul hist and ana
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.5  2005/04/08 18:51:12  miyoshi
*     change bypass condition
*
*     Revision 1.4  2005/03/10 16:49:38  miyoshi
*     change tracking variable name
*
*     Revision 1.3  2005/01/31 23:44:37  miyoshi
*     add trigger time calculation code
*
*     Revision 1.2  2005/01/17 16:07:18  miyoshi
*     correct the number of parameters for subroutine
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.6 03/22/04 Miyoshi
*     for E01-011
*     
*     Revision 1.5  2000/02/11 20:44:08  ysato
*     Local update on KTRACKING
*     
*     Revision 1.4  1999/12/23 19:59:28  ysato
*     Compiled on Redhat Linux
*     
*     Revision 1.3  1999/11/03 16:42:49  ysato
*     Internal update
*     
*     Revision 1.2  1999/11/02 15:55:27  ysato
*     Upgrade for HNSS
*     
*     March 2, 1999        Y.Fujii        A first draft
*     July 12, 1999        Y.Fujii        First working draft
*     
*     This routine will be called from analysis engine.
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_reconstruction')
*     
      Logical ABORT
      Character*(*) err
      integer*4 istat      

      Include 'hes_data_structures.cmn'
      Include 'hes_bypass_swiches.cmn'
      Include 'hes_id_histid.cmn'

*--------------------------------------------------------
      
      ABORT=.FALSE.
      err=' '
      

*     
*     Dump all raw data
*     
      call e_raw_dump_all(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif

*
*     Analyze tul
*
      call e_analyze_tul(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif
      

*     
*     TRANSLATE SCINTILATORS AND CALCULATE TIME
*     HNSS_RAW_SCIN ====> HNSS_DECODED_SCIN
*     
      If(ebypass_trans_scin.eq.0) then
         call E_TRANS_MISC(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
         endif                  ! end test on MISC ABORT
         
         call E_TRANS_SCIN(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
         endif                  ! end test on SCIN ABORT
c     --- calculate trigger time
         
         call e_calc_trigtime(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
         endif                  ! end test on trigger time abort
      endif                     ! end test on ebypass_trans_scin

      
*     
*     HES DC TRACKING 
*     
      if(ebypass_track.eq.0) then
         call E_TRACK(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                  ! end test on E_TRACK ABORT
c     if(ebypass_track_eff.eq.0) then
c     call e_track_tests
c     endif                  ! end test on ebypass_trackeff
      endif                     ! end test on ebypass_track
      
*     Select good tracks by checking chi square
*     and comparing closer tracks
*     !HES_FOCAL_PLANE common block is arrenged again!
*     note : existing SOS/HMS code is not adopted for
*     multi-track analysis in default.
*     

c      if(ebypass_good_tracks.eq. 0) then
c         call E_SELECT_GOOD_TRACKS(ABORT,err)
c         if(ABORT) then
c            call G_add_path(here,err)
c            return
c         endif                  ! end test on E_SELECT_GOOD_TRACKS ABORT
c      endif                     ! end test on ebypass_good_tracks
      
*     
*     Project tracks back to target
*     HES_FOCAL_PLANE  ====>  HES_TARGET
*     
      if(ebypass_targ_trans.eq. 0) then
         call E_TARG_TRANS(ABORT,err,istat)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                  ! end test on E_TARG_TRANS ABORT
      endif                     ! end test on ebypass_target_trans
      
*
*     link tracking results and hodoscope data.
*     
*     
      if(ebypass_link_tracks.eq.0) then
         call E_LINK_TRACKS(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                  ! end test on E_LINK_TRACKS ABORT
      endif                     ! end test on ebypass_link_tracks

*     
      if(ebypass_physics.eq.0) then
         call E_PHYSICS(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                  ! end test on e_PHYSICS ABORT
      endif                     ! end test on ebypass_physics      

      Return
      End
