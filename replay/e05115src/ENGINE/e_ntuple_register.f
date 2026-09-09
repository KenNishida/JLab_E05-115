      subroutine e_Ntuple_register(ABORT,err)
*----------------------------------------------------------------------
*
*     CTP variable registration routine for the HNSS Ntuples 
*
*     Purpose : Register output filename for HNSS Ntuple; temporary
*     implementation to be superceeded by CTP Ntuples
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, HU: added Ntuples
* $Log: e_ntuple_register.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:46  kawama
*
* e05115 src repository for software development
*
* Revision 1.2  2005/06/10 18:56:06  cdaq
* add trig,tul,fission ntuple variables
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.3  2005/01/04 23:56:21  miyoshi
* add detector ntuple
*
* Revision 1.2  2005/01/03 19:27:58  miyoshi
* add edc ntuple
*
* Revision 1.1.1.1  2004/08/30 21:21:38  miyoshi
* new dir
*
* Revision 1.4 2004/03/02 Miyoshi
* for E01-011
* Revision 1.1.1.1  1999/11/01 13:54:25  ysato
* Upgrade for HNSS
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
      parameter (here='e_Ntuple_register')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'e_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      integer ierr
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      call G_reg_C('hes_Ntuple',e_Ntuple_file,ABORT,err)
      call G_reg_C('hes_DCntuple',e_dc_ntuple_file,ABORT,err)
      call G_reg_C('hes_TRACKntuple',e_track_ntuple_file,ABORT,err)
      call G_reg_C('hes_SCINntuple',e_scin_ntuple_file,ABORT,err)
      call G_reg_C('hes_FISNtuple',e_fis_ntuple_file,ABORT,err)
*
      IF(ABORT) THEN
        call G_prepend(':unable to register-',err)
        call G_add_path(here,err)
      ENDIF
*
      return
      end
