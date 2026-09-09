      subroutine g_bm_Nt_shutdown(ABORT,err)
*----------------------------------------------------------------------
*
*     Final shutdown of the GEN BEAM Ntuple
*
*     Purpose : Flushes and closes the GEN BEAM Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 28-Feb-2000 
*
* Revision 1.0  2000/02/28 11:33:18  jinghua
* Initial revision
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='g_bm_Nt_shutdown')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'g_beam_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      logical HEXIST    !CERNLIB function
*
      logical FAIL
      character*80 why,directory,name
      character*1000 pat,msg
      integer io,id,cycle,m,iv(10)
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.g_beam_Ntuple_exists) RETURN       !nothing to do
*
      call HCDIR(directory,'R')                !keep current directory

      id= g_beam_Ntuple_ID
      io= g_beam_Ntuple_IOchannel
*
      ABORT= .NOT.HEXIST(id)
      IF(ABORT) THEN
        pat= ': Ntuple ID#$ does not exist'
        call G_build_note(pat,'$',id,' ',0.,' ',err)
        call G_add_path(here,err)
        If(io.GT.0) Then
          call G_IO_control(io,'FREE',FAIL,why) !free up
          if(.NOT.FAIL) CLOSE(io)
        EndIf
        g_beam_Ntuple_exists= .FALSE.
        g_beam_Ntuple_ID= 0
        g_beam_Ntuple_name= ' '
        g_beam_Ntuple_IOchannel= 0
        g_beam_Ntuple_file= ' '
        g_beam_Ntuple_title= ' '
        g_beam_Ntuple_directory= ' '
        g_beam_Ntuple_size= 0
        do m=1,SMAX_bm_Ntuple_size
          g_beam_Ntuple_tag(m)= ' '
          g_beam_Ntuple_contents(m)= 0.
        enddo
        RETURN
      ENDIF
*

      id= g_beam_Ntuple_ID
      io= g_beam_Ntuple_IOchannel
      name= g_beam_Ntuple_name
      call HCDIR(g_beam_Ntuple_directory,' ')      !goto Ntuple directory
*
      iv(1)= id
      iv(2)= io
C      pat= 'closing ID#$ IO#$ "'//g_beam_Ntuple_file//'"'

C      call G_build_note(pat,'$',iv,' ',0.,' ',msg)

C      call G_add_path(here,msg)

C      call G_log_message('INFO: '//msg)

*
      cycle= 0                                !dummy for HROUT
      call HROUT(id,cycle,' ')                !flush CERNLIB buffers

      call HREND(name)                        !CERNLIB close file
*      call HDELET(id)                         !CERNLIB delete tuple
      call G_IO_control(io,'FREE',ABORT,err)  !free up IO channel
      CLOSE(io)                               !close IO channel
*
      call HCDIR(directory,' ')               !return to current directory
*
      g_beam_Ntuple_exists= .FALSE.
      g_beam_Ntuple_ID= 0
      g_beam_Ntuple_name= ' '
      g_beam_Ntuple_IOchannel= 0
      g_beam_Ntuple_file= ' '
      g_beam_Ntuple_title= ' '
      g_beam_Ntuple_directory= ' '
      g_beam_Ntuple_size= 0
      do m=1,SMAX_bm_Ntuple_size
        g_beam_Ntuple_tag(m)= ' '
        g_beam_Ntuple_contents(m)= 0.
      enddo
*
      IF(ABORT) call G_add_path(here,err)
*
      RETURN
      END      
