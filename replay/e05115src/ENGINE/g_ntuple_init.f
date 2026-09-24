      SUBROUTINE g_ntuple_init(ABORT,err)
*--------------------------------------------------------
*     -       Close all ntuples
*     -
*     -
*     -   Purpose and Methods : Close ntuples.
*     -      Taken from ?_initialize so that s_initialize, k_initialize,
*     -      and c_initialize can be called 
*     -      from event display without mucking
*     -      with ntuples.
*     - 
*     -   Output: ABORT	- success or failure
*     -         : err	- reason for failure, if any
*     - 
*     -Created  6-September-1995 SAW
*     -Modified 2-Jul-1999	    Jinghua Liu for HNSS
*     Revision 2.1  2000/02/28 12:34:12  jinghua
*     Added beam montitor Ntuple, removed SOS sieve slit ntuple.
*     
*     Revision 2.0  1999/07/02 15:40:00  jinghua
*     Change HMS to HNSS
*     
*     Revision 1.1  1995/10/09 18:43:07  cdaq
*     Initial revision
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*13 here
      parameter (here= 'g_ntuple_init')
*     
      logical ABORT
      character*(*) err
*     
      character*500 why
      logical FAIL
*--------------------------------------------------------
      ABORT = .false.
      err = ' '
*     
      call e_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

      call e_dc_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

      call e_track_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

      call e_scin_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

      call e_fis_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

*     
*     call k_sv_nt_init(FAIL,why)
*     if(err.NE.' ' .and. why.NE.' ') then
*     call G_append(err,' & '//why)
*     elseif(why.NE.' ') then
*     err= why
*     endif
*     ABORT= ABORT .or. FAIL
*     

*--   for test purpose
      call h_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

      call h_dc_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

c      call h_track_ntuple_init(FAIL,why)
c      if(err.NE.' ' .and. why.NE.' ') then
c         call G_append(err,' & '//why)
c      elseif(why.NE.' ') then
c         err= why
c      endif
c      ABORT= ABORT .or. FAIL
c
      call h_scin_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

      call h_ch_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

      call h_wat_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

      call h_aer_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL

      call h_luc_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL
      
      call h_pid_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL


*     
      call g_bm_nt_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL
*     
      call c_ntuple_init(FAIL,why)
      if(err.NE.' ' .and. why.NE.' ') then
         call G_append(err,' & '//why)
      elseif(why.NE.' ') then
         err= why
      endif
      ABORT= ABORT .or. FAIL
*     
      if(ABORT .or. err.NE.' ') call g_add_path(here,err)
*     
      return
      end


