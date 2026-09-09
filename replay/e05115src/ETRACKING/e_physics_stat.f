      Subroutine e_physics_stat(ABORT,err)
*----------------------------------------------------------------
* Select best tracks and store information to data banks
* 
* $Log: e_physics_stat.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.1  1999/12/23 19:59:26  ysato
* Compiled on Redhat Linux
*
*
*----------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Character*10 here
      Parameter (here='e_physics_stat')
*
      Logical ABORT
      Character*(*) err

      Include "hes_bypass_swiches.cmn"

*--------------------------------------------------------
      ABORT=.FALSE.
      err=' '

*     --- EDC efficiencies ---
c     if(ebypass_dc_eff.eq.0) call e_dc_eff(ABORT,err)
c     if(ebypass_dc_trk_eff.eq.0) call e_dc_trk_eff(ABORT,err)
      
*     --- Hodoscope efficiencies ---
      if(ebypass_scin_eff.eq.0) call e_scin_eff(ABORT,err)

      Return
      End
