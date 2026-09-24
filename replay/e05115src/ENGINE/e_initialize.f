      Subroutine e_initialize(ABORT,err)
*--------------------------------------------------------
*     Hodoscope pedestals calculation
*     August 12, 1999        Y.Fujii        Skeleton
*     
*     This routine will be called from g_initialize
*     
*     Revision 1.3 2004/03/02 Miyoshi
*     for E01-011
*     
*     $Log: e_initialize.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:46  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.1.1.1  2004/08/30 21:21:38  miyoshi
*     new dir
*
*     Revision 1.2  1999/11/02 15:55:03  ysato
*     Upgrade for HNSS
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_initialize')
*     
      Logical ABORT
      Character*(*) err
      logical FAIL
      character*1000 why
      character*20 err1
      integer*4 istat 
      
      ABORT = .FALSE.
      err= ' '
      
*     
*     -calculate physics singles constants
      call e_init_physics(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

      call e_dc1_generate_geometry  ! Tracking routine
      call e_dc2_generate_geometry  ! Tracking routine

*     -read in Optical matrix elements
      call e_targ_trans_init(FAIL,why,istat)
      if(FAIL) then
         write(err1,'(":istat=",i2)') istat
         call G_prepend(err1,why)
      endif
      if(err.NE.' ' .and. why.NE.' ') then !keep warnings
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

      if(ABORT .or. err.NE.' ') call g_add_path(here,err)

      Return
      End

