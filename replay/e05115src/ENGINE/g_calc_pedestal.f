      subroutine g_calc_pedestal(ABORT,err)
*     
*     Revision 1.2  1996/01/22 15:12:35  saw
*     (JRA) Add call to g_calc_beam_pedestal
*     
*     Revision 1.1  1995/04/01 19:37:06  cdaq
*     Initial revision
*     
*     
      implicit none
*     
      character*18 here
      parameter (here='g_calc_pedestal')
*     
*     we use common unit id for all pedestal code.
      Integer*4 SPAREID
      Parameter(SPAREID=67)
      
      logical ABORT
      character*(*) err
*     
      call g_calc_beam_pedestal(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         close(SPAREID)
         return
      endif
*     
      call e_calc_pedestal(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         close(SPAREID)
         return
      endif
*
      call h_calc_pedestal(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         close(SPAREID)
         return
      endif
*
      return
      end
