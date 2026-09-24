       subroutine g_total_eloss(arm,prt,z,a,tgthick,dens,angle,tgangle,
     &                          beta,e_loss)

*------------------------------------------------------------------------------
*-         Prototype C routine
*- 
*-    Output: e_loss            -   energy loss for the arm requested
*-    Created   17-Feb-2000   by Jinghua for HNSS
*
* $Log: g_total_eloss.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:38  miyoshi
* new dir
*
* Initial Version
* (JLiu) At the this point we don't care about the energy loss.
*        We may try to implement the detail later.
*------------------------------------------------------------------------------
     
      IMPLICIT NONE
*
      INTEGER arm                       ! 0 : incident beam
                                        ! 1 : HNSS
                                        ! 2 : SOS
      LOGICAL prt                       ! .true. : electron
                                        ! .false. : non-electron (beta .lt. 1)
      REAL*4 z,a,tgthick,dens,angle,tgangle,beta
      REAL*4 e_loss

********************INITIALIZE ENERGY LOSS VARIABLES*****************
      e_loss         = 0.0
*********************ENABLE SWITCH***********************************
C      if(gen_eloss_enable.eq.0.) goto 100  !if 0 don't do eloss correction.
***********************SETUP OF PARAMETERS****************************
      RETURN
      END
