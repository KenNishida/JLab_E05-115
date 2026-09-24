      SUBROUTINE e_init_physics(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods : Initialize constants for s_physics
*     -                              
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     -   Created 6-6-94          D. F. Geesaman
*     $Log: e_init_physics.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.1.1.1  1999/11/01 13:54:40  ysato
*     Upgrade for HNSS
*     
*     Revision 1.5  1996/09/05 19:54:16  saw
*     (JRA) avoid setting p=0??
*     
*     Revision 1.4  1996/01/24 16:07:34  saw
*     (JRA) Change upper case to lower case, cebeam to gebeam
*     
*     Revision 1.3  1995/05/22 19:45:41  cdaq
*     (SAW) Split gen_data_data_structures into gen, 
*     hms, sos, and coin parts"
*     
*     Revision 1.2  1995/05/11  17:07:14  cdaq
*     (SAW) Fix SOS to be in plane, beam left
*     
*     Revision 1.1  1994/06/14  04:09:12  cdaq
*     Initial revision
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*50 here
      parameter (here= 'e_init_physics')
*     
      logical ABORT      
      character*(*) err

      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'hes_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hes_physics_sing.cmn'

*     
*     
*     local variables 
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '
      
*
*     Fix HKS to be in plane, beam left
*     
      ephi_lab = tt/2
*     
      if (emomentum_factor .gt. 0.1) then !avoid setting p=0
         epcentral = epcentral * emomentum_factor
      endif
*     
      cosethetae = cos(etheta_lab*degree)
      sinethetae = sin(etheta_lab*degree)
*     Constants for elastic kinematics calcultion
      ephysicsa = 2.*gebeam*gtarg_mass(gtarg_num) -
     $     mass_electron**2 - epartmass**2
      ephysicsb = 2. * (gtarg_mass(gtarg_num) - gebeam)
      ephysicab2 = ephysicsa**2 * ephysicsb**2
      ephysicsm3b = epartmass**2 * ephysicsb**2
      
      return
      end
