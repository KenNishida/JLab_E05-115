      subroutine h_Ntuple_register(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for the SOS Ntuples 
*
*     Purpose : Register output filename for SOS Ntuple; temporary
*     implementation to be superceeded by CTP Ntuples
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, HU: added Ntuples
*
* Revision 1.2  1994/06/17 02:56:26  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:16:38  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='h_Ntuple_register')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      integer ierr
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call G_reg_C('hks_Ntuple',h_Ntuple_file,ABORT,err)
      call G_reg_C('hks_DCntuple',h_dc_Ntuple_file,ABORT,err)
      call G_reg_C('hks_TRACKntuple',h_track_Ntuple_file,ABORT,err)
      call G_reg_C('hks_SCINntuple',h_scin_Ntuple_file,ABORT,err)
      call G_reg_C('hks_WATntuple',h_wat_Ntuple_file,ABORT,err)
      call G_reg_C('hks_AERntuple',h_aer_Ntuple_file,ABORT,err)
      call G_reg_C('hks_LUCntuple',h_luc_Ntuple_file,ABORT,err)
      call G_reg_C('hks_PIDntuple',h_pid_Ntuple_file,ABORT,err)
*
      IF(ABORT) THEN
        call G_prepend(':unable to register-',err)
        call G_add_path(here,err)
      ENDIF
*
      return
      end
