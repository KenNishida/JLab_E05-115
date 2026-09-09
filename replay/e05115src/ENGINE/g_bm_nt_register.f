      subroutine g_bm_Nt_register(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for the GEN BEAM Ntuples 
*
*     Purpose : Register output filename for GEN BEAM Ntuple; temporary
*     implementation to be superceeded by CTP Ntuples
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 28-Feb-2000
*
* Revision 1.1 2004/03/02 Miyoshi
* 
*
* Revision 1.0  2000/02/28 11:43:06  jinghua
* Initial revision
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='g_bm_Nt_register')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'g_beam_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      integer ierr
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call G_reg_C('beam_ntuple',g_beam_Ntuple_file,ABORT,err)
*
      IF(ABORT) THEN
        call G_prepend(':unable to register-',err)
        call G_add_path(here,err)
      ENDIF
*
      return
      end
