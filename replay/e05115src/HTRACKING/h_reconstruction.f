      SUBROUTINE h_reconstruction(ABORT,err)
*--------------------------------------------------------
*     -       Prototype C analysis routine
*     -
*     -
*     -   Purpose and Methods : reconstruction of HKS quantities 
*     -
*     -   Output: ABORT              - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     $Log: h_reconstruction.f,v $
*     Revision 1.1.1.1  2009/07/12 20:03:01  yez
*     Add Lucite info
*
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.4  2005/08/23 21:11:18  cdaq
*     Mod. use RT/ERIKA for mome/angles
*
*     Revision 1.3  2005/07/06 19:21:33  sumihama
*     Mod.hrf/erf
*
*     Revision 1.2  2005/06/10 19:01:09  cdaq
*     add tul ana
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.5  2005/04/19 17:52:05  miyoshi
*     add option for targ trans
*
*     Revision 1.4  2005/04/08 20:43:20  miyoshi
*     change subroutine location
*
*     Revision 1.3  2005/01/31 23:45:25  miyoshi
*     add trigger time calculation code for test
*
*     Revision 1.2  2004/12/24 19:47:07  miyoshi
*     change minor
*
*     Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
*     new dir
*
*     
*     Revision 1.14 2004/03/02 Miyoshi
*     for E01-011 
*     
*     Revision 1.13  1996/11/07 19:53:12  saw
*     (WH) Add lucite information
*     
*     Revision 1.12  1996/09/05 20:13:45  saw
*     (JRA) Add sbypass_track_eff
*     
*     Revision 1.11  1996/04/30 17:14:36  saw
*     (JRA) Add call to aerogel routine
*     
*     Revision 1.10  1995/10/10 17:33:31  cdaq
*     (JRA) Don't make an error just because no track is found
*     
*     Revision 1.9  1995/08/31 20:43:03  cdaq
*     (JRA) Add call to s_trans_cer
*     
*     Revision 1.8  1995/05/22  19:45:54  cdaq
*     (SAW) Split gen_data_data_structures into gen, 
*     hms, sos, and coin parts"
*     
*     Revision 1.7  1995/05/11  21:07:26  cdaq
*     (JRA) Add call to s_trans_misc
*     
*     Revision 1.6  1995/04/06  19:42:47  cdaq
*     (JRA) Add call to s_select_best_track before s_physics
*     
*     Revision 1.5  1994/06/07  04:46:21  cdaq
*     (DFG) add s_recon_num and bypass switches
*     
*     Revision 1.4  1994/05/13  03:34:52  cdaq
*     (DFG) Put s_prt_track_tests here. Remove from s_tof
*     
*     Revision 1.3  1994/04/13  18:30:40  cdaq
*     (DFG) add call to s_raw_dump_all 
*     and comment out some returns after ABORT's
*     
*     Revision 1.2  1994/02/22  15:56:17  cdaq
*     (DFG) Replace with real version
*     (SAW) Move to TRACKING directory
*     
*     Revision 1.1  1994/02/04  22:16:44  cdaq
*     Initial revision
*     
*     -
*     - All standards are from "Proposal for Hall C Analysis Software
*     - Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*     -
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*16 here
      parameter (here= 'h_reconstruction')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hks_scin_parms.cmn'
      include 'hks_bypass_switches.cmn'
      include 'hks_statistics.cmn'
      Include 'hks_recon_elements.cmn'
*
*     local variables
      integer*4 istat
*--------------------------------------------------------
*     
ccc   ABORT= .TRUE.
ccc   err= ':no events analyzed!'
*     increment reconstructed number
c     s_recon_num= s_recon_num + 1
*     
*     dump all raw data
      call h_raw_dump_all(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif

*     analyze tul
      call h_analyze_tul(ABORT,err)
      if(ABORT) then
         call g_add_path(here,err)
         return
      endif

*     
*     TRANSLATE SMISC TDC HITS.
*     H_RAW_MISC ====> HKS_DECODED_MISC
*     
      If(hbypass_trans_scin.eq.0) then
         call H_TRANS_MISC(ABORT,err)
         if(ABORT)  then
            call G_add_path(here,err)
*     return
         endif                  ! end test on SCIN ABORT
      endif                     ! end test on hbypass_trans_scin
      
*     
*     TRANSLATE SCINTILATORS AND CALCULATE START TIME
*     HKS_RAW_SCIN ====> HKS_DECODED_SCIN
*     
      If(hbypass_trans_scin.eq.0) then
         call H_TRANS_SCIN(ABORT,err)
         if(ABORT)  then
            call G_add_path(here,err)
         endif                  ! end test on SCIN ABORT
c         call h_calc_trigtime(ABORT,err)
c         if(ABORT)  then
c            call G_add_path(here,err)
c         endif                  ! end test on SCIN ABORT
      endif                     ! end test on hbypass_trans_scin
c
c     ////////////////////////////////////////////
*     TEST by gogami
      if(hbypass_aero.eq.0) then
         call H_AERO(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
*     return
         endif                  ! end test of H_AERO ABORT
      endif                     ! end test on hbypass_aero
c     ////////////////////////////////////////////
*     
*     TRANSLATE DRIFT CHAMBERS
*     HKS_RAW_DC + HKS_DECODED_SCIN ====>  HKS_DECODED_DC

      if(hbypass_trans_dc.eq.0) then
         call H_TRANS_DC(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                  ! end test on H_TRANS_DC ABORT
      endif                     ! end test on hbypass_trans_dc
*     
      if(hbypass_track.eq.0) then
         call H_TRACK(ABORT,err)
         if(ABORT)  then
            call G_add_path(here,err)
            return
         endif                  ! end test on H_TRACK ABORT
c     if(hbypass_track_eff.eq.0) then
c     call h_track_tests
c     endif                  ! end test on hbypass_trackeff
      endif                     ! end test on hbypass_track
      

*     
*     Select good tracks by checking chi square
*     and comparing closer tracks
*     !HKS_FOCAL_PLANE common block is arrenged again!
*     note : existing SOS/HMS code is not adopted for
*     multi-track analysis in default.
*     
      
      if(hbypass_track.eq.0) then
         call h_select_good_tracks(abort,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif
      endif


*     Project tracks back to target
*     HKS_FOCAL_PLANE  ====>  HKS_TARGET
*     
      if(hbypass_targ_trans.eq. 0) then
c         if(h_recon_switch .eq. 1) then
            Call H_TARG_TRANS_RT(ABORT,err,istat)
c         Else
c            call H_TARG_TRANS(ABORT,err,istat)
c         EndIf
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                  ! end test on H_TARG_TRANS ABORT
      endif                     ! end test on hbypass_target_trans
      
*     Now begin to process particle identification information
*     First scintillator and time of flight
*     HKS_RAW_SCIN ====> HKS_TRACK_TESTS
      
      if(hbypass_tof.eq.0) then
         call H_TOF(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                  ! end test of H_TOF ABORT
      endif                     ! end test on hbypass_tof
      
*     
*     Next Aerogel Cerenkov information
*     HKS_DECODED_AER ====> HKS_TRACK_TESTS
*     
ccc      if(hbypass_aero.eq.0) then
ccc         call H_AERO(ABORT,err)
ccc         if(ABORT) then
ccc            call G_add_path(here,err)
ccc*     return
ccc         endif                  ! end test of H_AERO ABORT
ccc      endif                     ! end test on hbypass_aero
*     
*     Next Water Cerenkov information
*     HKS_DECODED_WAT ====> HKS_TRACK_TESTS
*     
      if(hbypass_water.eq.0) then
         call H_WATER(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
*     return
         endif                  ! end test of H_WATER ABORT
      endif                     ! end test on hbypass_water

*     Next Lucite Cerenkov information
*     HKS_DECODED_LUC ====> HKS_TRACK_TESTS
*     
      if(hbypass_lucite.eq.0) then
         call H_LUCITE(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
*     return
         endif                  ! end test of H_LUCITE ABORT
      endif                     ! end test on hbypass_water
      
*     
*     combine AC and WC data
*     
      
      if(hbypass_track .eq. 0) then
         call H_LINK_TRACKS(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
*     return
         endif                  ! end test of H_LINK_TRACKS ABORT
      endif                     ! end test on hbypass_water
      
*     
*     Dump HKS_TRACK_TESTS if hdebugprinttracktests is set
c     if( hdebugprinttracktests .ne. 0 ) then
c     call h_prt_track_tests
c     endif
*     Combine results in HKS physics analysis
*     HKS_TARGET + HKS_TRACK_TESTS ====>  HKS_PHYSICS
*     
      
      if(hbypass_physics.eq.0) then
         call h_physics(ABORT,err)
c         call h_dc_eff_out(ABORT,err) ! ( 3Mar2011 , T.Gogami )
         if(ABORT) then
            call G_add_path(here,err)
         endif                  ! end test of H_PHYSICS ABORT
      endif                     ! end test on hbypass_physics

      
*     
*     Successful return
      ABORT=.FALSE.
      RETURN
      END








