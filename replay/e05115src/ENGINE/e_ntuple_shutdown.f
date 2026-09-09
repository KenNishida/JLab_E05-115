      subroutine e_Ntuple_shutdown(ABORT,err)
*----------------------------------------------------------------------
*
*     Final shutdown of the SOS Ntuple
*
*     Purpose : Flushes and closes the SOS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, HU: added Ntuples
* $Log: e_ntuple_shutdown.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:46  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:38  miyoshi
* new dir
*
* Revision 1.6 2004/03/02 Miyoshi
* for E01-011
*
* Revision 1.1.1.1  1999/11/01 13:54:27  ysato
* Upgrade for HNSS
*
* Revision 1.5  1998/12/01 16:02:39  saw
* (SAW) Clean out archaic g_build_note stuff
*
* Revision 1.4  1996/01/16 16:38:45  cdaq
* (SAW) Comment out an info message
*
* Revision 1.3  1994/06/29 03:30:25  cdaq
* (KBB) Remove HDELET call
*
* Revision 1.2  1994/06/17  02:57:45  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:16:53  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*17 here
      parameter (here='e_Ntuple_shutdown')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'e_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
*
      logical HEXIST      !CERNLIB function
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
      IF(.NOT.e_Ntuple_exists) RETURN       !nothing to do
*
      call HCDIR(directory,'R')                !keep current directory
*
*
      id= e_Ntuple_ID
      io= e_Ntuple_IOchannel
*
      ABORT= .NOT.HEXIST(id)
      IF(ABORT) THEN
        write(err,'(": Ntuple ID#",i5," does not exist")') id
        call G_add_path(here,err)
        If(io.GT.0) Then
          call G_IO_control(io,'FREE',FAIL,why) !free up
          if(.NOT.FAIL) CLOSE(io)
        EndIf
        e_Ntuple_exists= .FALSE.
        e_Ntuple_ID= 0
        e_Ntuple_name= ' '
        e_Ntuple_IOchannel= 0
        e_Ntuple_file= ' '
        e_Ntuple_title= ' '
        e_Ntuple_directory= ' '
        e_Ntuple_size= 0
        do m=1,EMAX_Ntuple_size
          e_Ntuple_tag(m)= ' '
          e_Ntuple_contents(m)= 0.
        enddo
        RETURN
      ENDIF
*
      id= e_Ntuple_ID
      io= e_Ntuple_IOchannel
      name= e_Ntuple_name
      call HCDIR(e_Ntuple_directory,' ')      !goto Ntuple directory
*
      write(msg,'("closing ID#",i5," IO#",i3," ",a)') 
     &     id,io,e_ntuple_file
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
      e_Ntuple_exists= .FALSE.
      e_Ntuple_ID= 0
      e_Ntuple_name= ' '
      e_Ntuple_IOchannel= 0
      e_Ntuple_file= ' '
      e_Ntuple_title= ' '
      e_Ntuple_directory= ' '
      e_Ntuple_size= 0
      do m=1,EMAX_Ntuple_size
        e_Ntuple_tag(m)= ' '
        e_Ntuple_contents(m)= 0.
      enddo
*
      IF(ABORT) call G_add_path(here,err)
*
      RETURN
      END      
