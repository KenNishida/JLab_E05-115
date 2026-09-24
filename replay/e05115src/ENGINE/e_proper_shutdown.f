      SUBROUTINE e_proper_shutdown(lunout,ABORT,err)
*--------------------------------------------------------
*     -       Prototype C analysis routine
*     -
*     -
*     -   Purpose and Methods : Closes files properly, flushes, etc.
*     - 
*     -   Output: ABORT		- success or failure
*     -         : err	- reason for failure, if any
*     - 
*     
*     - All standards are from "Proposal for Hall C Analysis Software
*     - Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*     -
*     $Log: e_proper_shutdown.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.1.1.1  2004/08/30 21:21:38  miyoshi
*     new dir
*
*     Revision 1.4 2004/03/03 Miyoshi
*     for E01-011
*     
*     Revision 1.3  1999/12/23 19:59:18  ysato
*     Compiled on Redhat Linux
*     
*     Revision 1.2  1999/11/02 15:55:03  ysato
*     Upgrade for ENGE
*     
*     Updated 08/16/99 Y.Fujii
*     Modified for ENGE 07/15/99 S.Danagoulian
*     
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      include 'gen_routines.dec'
      include 'gen_run_info.cmn'
      include 'hes_data_structures.cmn'
      include 'hes_filenames.cmn'
      include 'hes_bypass_swiches.cmn'
*     
      character*17 here
      parameter (here= 'e_proper_shutdown')
*
      logical ABORT, report_abort
      character*(*) err
*
      integer ierr
      character*132 file
      integer lunout
*--------------------------------------------------------
*-chance to flush any statistics, etc.
*
*
      ABORT= .FALSE.
      err= ' '
*     
      if (ebypass_track_eff.eq.0) then
*         call e_track_eff_shutdown(lunout,ABORT,err)
      endif
*     
      if (ebypass_scin_eff.eq.0) then
         call e_scin_eff_shutdown(lunout,ABORT,err)
      endif
*     
      if(e_report_blockname.ne.' '.and.
     $     e_report_output_filename.ne.' ') then
         
         file = e_report_output_filename
         call g_sub_run_number(file, gen_run_number)
         
         ierr = threp(e_report_blockname, file)
         if(ierr.ne.0) then
            call g_append(err,'& threp failed to create report in file'//file)
            report_abort = .true.
         endif
      endif
*
      IF(ABORT.or.report_abort) THEN
         call G_add_path(here,err)
      ELSE
         err= ' '
      ENDIF
*
      RETURN
      END
