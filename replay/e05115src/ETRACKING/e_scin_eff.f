      Subroutine e_scin_eff(ABORT,err)
*--------------------------------------------------------
*     Estimate HODOSCOPE efficiency relative to SSD hit
*     
*     $Log: e_scin_eff.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.1.1.1  2004/08/30 21:21:41  miyoshi
*     new dir
*
*     Revision 1.3  2000/03/09 01:32:41  ysato
*     Update in the production run Mar.8
*     
*     Revision 1.2  1999/12/23 19:59:28  ysato
*     Compiled on Redhat Linux
*     
*     Revision 1.1  1999/11/03 16:47:52  ysato
*     Internal modification
*     
*     
*     November 3, 1999        Y.Sato        A first draft
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_scin_eff')
*     
      Logical ABORT
      Character*(*) err
      
      Include "hes_data_structures.cmn"
      Include "hes_statistics.cmn"
      Include "hes_scin_parms.cmn"
      

      ABORT= .FALSE.
      err= ' '


      return
      end
