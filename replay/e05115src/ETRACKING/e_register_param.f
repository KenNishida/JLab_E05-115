      Subroutine e_register_param(ABORT,err)
*--------------------------------------------------------
* $Log: e_register_param.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:41  miyoshi
* new dir
*
* Revision 1.5  2000/03/09 01:32:41  ysato
* Update in the production run Mar.8
*
* Revision 1.4  1999/12/23 19:59:28  ysato
* Compiled on Redhat Linux
*
* Revision 1.3  1999/11/03 16:42:49  ysato
* Internal update
*
* Revision 1.2  1999/11/02 15:55:27  ysato
* Upgrade for HNSS
*
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Character*50 here
      Parameter (here='e_register_param')
*
      Logical ABORT
      Character*(*) err
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.

c     these are on e_registor_variables
c     Call r_hes_data_structures
c     Call r_hes_filenames
c     Call r_e_ntuple


*     
*     register tracking variables
*     
      
      call r_hes_tracking
      call r_hes_geometry
      call r_hes_recon_elements
      call r_hes_physics_sing ! ???? (6/June/2011)

      Call r_hes_scin_parms

      Call r_hes_id_histid

      Call r_hes_bypass_swiches

      Call r_hes_pedestals ! ???? (6/June/2011)

      Call r_hes_statistics

      Return
      End
