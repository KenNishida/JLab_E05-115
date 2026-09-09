      subroutine e_fis_ntuple_shutdown(ABORT,err)
*----------------------------------------------------------------------
*     
*     Final shutdown of the fission chamber ntuple
*     
*     Purpose : Flushes and closes the fission chamber ntuple
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*17 here
      parameter (here='e_fis_ntuple_shutdown')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'e_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*     
      logical HEXIST            !CERNLIB function
*     
      logical FAIL
      character*80 why,directory,name
      character*1000 msg
      integer io,id,cycle,m,iv(10)
*     
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(.NOT.e_fis_ntuple_exists) RETURN !nothing to do
*     
      call HCDIR(directory,'R') !keep current directory
*     
*
      id= e_fis_ntuple_ID
      io= e_fis_ntuple_IOchannel
*
      ABORT= .NOT.HEXIST(id)
      IF(ABORT) THEN
        write(err,'(": Ntuple ID#",i5," does not exist")') id
        call G_add_path(here,err)
        If(io.GT.0) Then
          call G_IO_control(io,'FREE',FAIL,why) !free up
          if(.NOT.FAIL) CLOSE(io)
        EndIf
        e_fis_ntuple_exists= .FALSE.
        e_fis_ntuple_ID= 0
        e_fis_ntuple_name= ' '
        e_fis_ntuple_IOchannel= 0
        e_fis_ntuple_file= ' '
        e_fis_ntuple_title= ' '
        e_fis_ntuple_directory= ' '
        e_fis_ntuple_size= 0
        do m=1,EMAX_Ntuple_size
          e_fis_ntuple_tag(m)= ' '
          e_fis_ntuple_contents(m)= 0.
        enddo
        RETURN
      ENDIF
*
      id= e_fis_ntuple_ID
      io= e_fis_ntuple_IOchannel
      name= e_fis_ntuple_name
      call HCDIR(e_fis_ntuple_directory,' ')      !goto Ntuple directory
*
      write(msg,'("closing ID#",i5," IO#",i3," ",a)') 
     &     id,io,e_fis_ntuple_file
      call G_add_path(here,msg)
c      call G_log_message('INFO: '//msg)
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
      e_fis_ntuple_exists= .FALSE.
      e_fis_ntuple_ID= 0
      e_fis_ntuple_name= ' '
      e_fis_ntuple_IOchannel= 0
      e_fis_ntuple_file= ' '
      e_fis_ntuple_title= ' '
      e_fis_ntuple_directory= ' '
      e_fis_ntuple_size= 0
      do m=1,EMAX_Ntuple_size
        e_fis_ntuple_tag(m)= ' '
        e_fis_ntuple_contents(m)= 0.
      enddo
*
      IF(ABORT) call G_add_path(here,err)
*
      RETURN
      END      
