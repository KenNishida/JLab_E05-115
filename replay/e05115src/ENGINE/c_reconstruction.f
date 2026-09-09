      SUBROUTINE C_reconstruction(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : reconstruction of coincidence/physics
*-
*-   Output: ABORT              - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created  8-Nov-1993   Kevin B. Beard, HU
*-   Modified 20-Nov-1993   KBB for new errors
*-   Modified 07-Oct-1999   Jinghua Liu for HNSS
*
* Revision 2.0  1999/10/07 12:45:03  jinghua
* (JLiu) Change include filename and variable name
*
* Revision 1.5  1995/05/22 20:50:44  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.4  1995/05/11  15:48:15  cdaq
* (SAW) Add call to c_physics for coincidence variables
*
* Revision 1.3  1994/06/17  03:16:29  cdaq
* (KBB) Remove not yet written warning
*
* Revision 1.2  1994/02/04  21:09:43  cdaq
* Fix indentation
*
* Revision 1.1  1994/02/04  21:08:21  cdaq
* Initial revision
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*16 here
      parameter (here= 'C_reconstruction')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'coin_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
*
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err = ' '
*
      if(c_coin_bypass.eq.0) then
        call c_physics(abort,err)
        IF(ABORT) call G_add_path(here,err)
        return
      endif
*
*     Successful return
*
      abort = .false.
*
      RETURN
      END

