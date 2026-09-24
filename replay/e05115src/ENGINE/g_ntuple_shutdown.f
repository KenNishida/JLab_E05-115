      SUBROUTINE g_ntuple_shutdown(ABORT,err)
*--------------------------------------------------------
*     -       Close all ntuples
*     -
*     -
*     -   Purpose and Methods : Close ntuples.
*     -          Taken from ?_keep_results routines so that g_keep_results can
*     -          be called without closing out ntuples.
*     - 
*     -   Output: ABORT	- success or failure
*     -         : err	- reason for failure, if any
*     - 
*     -   Created  30-June-1995 SAW
*     -   Modified 2-Jul-1999   Jinghua Liu for HNSS
*     
*     Revision 2.1  2000/02/28 16:34:23  jinghua
*     Added beam montitor Ntuple, removed SOS sieve slit ntuple.
*     
*     Revision 2.0  1999/07/02 15:50:00  jinghua
*     Change HMS to HNSS
*     
*     Revision 1.2  1995/09/01 15:46:13  cdaq
*     (JRA) Add call to sos sieve slit ntuple
*     
*     Revision 1.1  1995/07/27  19:00:55  cdaq
*     Initial revision
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*17 here
      parameter (here= 'g_ntuple_shutdown')
*     
      logical ABORT
      character*(*) err
*--------------------------------------------------------
      call e_ntuple_shutdown(ABORT,err)
*---  for EDC ntuple
      call e_dc_ntuple_shutdown(ABORT,err)
      call e_track_ntuple_shutdown(ABORT,err)
      call e_scin_ntuple_shutdown(ABORT,err)
      call e_fis_ntuple_shutdown(ABORT,err)
*     
*     call e_sv_nt_shutdown(ABORT,err)
*     
      call h_ntuple_shutdown(ABORT,err)
      call h_dc_ntuple_shutdown(ABORT,err)
      call h_track_ntuple_shutdown(ABORT,err) !commented out for test GOGAMI
      call h_scin_ntuple_shutdown(ABORT,err)
      call h_ch_ntuple_shutdown(ABORT,err)
      call h_wat_ntuple_shutdown(ABORT,err)
      call h_aer_ntuple_shutdown(ABORT,err)
      call h_luc_ntuple_shutdown(ABORT,err)
      call h_pid_ntuple_shutdown(ABORT,err)
*
      call h_dc_eff_out(ABORT,err) ! KDC efficiency (3Mar2011,Gogami)
      call e_dc_eff_out(ABORT,err) ! EDC efficiency (3Mar2011,Gogami)

*     call h_sv_nt_shutdown(ABORT,err)
*     
      call g_bm_nt_shutdown(ABORT,err)
*     
      call c_ntuple_shutdown(ABORT,err)
*
      return
      end


