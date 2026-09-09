      Subroutine e_register_variables(ABORT,err)
*--------------------------------------------------------
*     
*     CTP variable registration routine for the ENGE
*     
*     This routine will be called from g_register_variables
*     
*     Revision 1.4 2003/03/02 Miyoshi
*     
*     
*     $Log: e_register_variables.f,v $
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
*     Revision 1.3  1999/12/23 19:59:19  ysato
*     Compiled on Redhat Linux
*     
*     Revision 1.2  1999/11/02 15:55:04  ysato
*     Upgrade for HNSS
*     
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_register_variables')
*     
      Logical ABORT
      Character*(*) err
      Logical FAIL
      Character*1000 why
*--------------------------------------------------------
      err=' '
      ABORT=.FALSE.
      
      Call r_hes_data_structures
      
      Call r_hes_filenames
      
      Call r_e_ntuple
      
      Call e_register_param(FAIL,why)
      IF(err.NE.' ' .and. why.NE.' ') THEN !keep warnings
         call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
         err= why
      ENDIF
      ABORT= ABORT .or. FAIL
*     
      call e_ntuple_register(FAIL,why) ! Remove this when ctp files fixed
      IF(err.NE.' ' .and. why.NE.' ') THEN !keep warnings
         call G_append(err,' & '//why)
      ELSEIF(why.NE.' ') THEN
         err= why
      ENDIF
      ABORT= ABORT .or. FAIL

      Return
      End

