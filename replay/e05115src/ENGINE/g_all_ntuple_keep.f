      subroutine g_all_ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the all ntuple only for commissioning time
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='g_all_Ntuple_keep')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'gen_ntuple.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'gen_run_info.cmn'
*     
      logical HEXIST    !CERNLIB function
*
      integer m,i,j,k

*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(.NOT.g_all_Ntuple_exists) RETURN !nothing to do
*     
      m= 0
      m= m+1
      g_all_Ntuple_contents(m)= 0
      
*     Fill ntuple for this event
      ABORT= .NOT.HEXIST(g_all_Ntuple_ID)
      IF(ABORT) THEN
         call G_build_note(':Ntuple ID#$ does not exist',
     &        '$',g_all_Ntuple_ID,' ',0.,' ',err)
         call G_add_path(here,err)
      ELSE
         call HFN(g_all_Ntuple_ID,g_all_Ntuple_contents)
      ENDIF

      
      RETURN
      END      
