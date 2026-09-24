      subroutine e_fis_Ntuple_init(ABORT,err)
*----------------------------------------------------------------------
*
*     Creates an fission Ntuple
*
*     Purpose : Books an HES Ntuple; defines structure of it
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, Hampton Univ.
* $Log: e_fis_ntuple_init.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:46  kawama
*
* e05115 src repository for software development
*
* Revision 1.1  2005/06/10 18:56:42  cdaq
* initial version
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:38  miyoshi
* new dir
*
*
* Revision 1.3 2004/03/02 Miyoshi
* for E01-011
*
* Revision 1.2  2000/03/09 01:32:36  ysato
* Update in the production run Mar.8
*
* Revision 1.1.1.1  1999/11/01 13:54:26  ysato
* Upgrade for HNSS
*
* Revision 1.7  1996/09/04 15:18:02  saw
* (JRA) Modify ntuple contents
*
* Revision 1.6  1996/01/16 16:41:14  cdaq
* (JRA) Modify ntuple contents
*
* Revision 1.5  1995/09/01 13:38:59  cdaq
* (JRA) Add Cerenkov photoelectron count to ntuple
*
* Revision 1.4  1995/07/27  19:00:31  cdaq
* (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.3  1995/05/11  19:00:02  cdaq
* (SAW) Allow %d for run number in filenames
*
* Revision 1.2  1994/06/17  02:36:00  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:16:18  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='e_fis_Ntuple_init')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'e_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
*
      character*80 default_name
      parameter (default_name= 'HESFISntuple')
      integer default_bank,default_recL
      parameter (default_bank= 8000)    !4 bytes/word
      parameter (default_recL= 1024)    !record length
      character*80 title
      character*80 directory,name
      character*256 file
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)
*
      logical HEXIST           !CERNLIB function
*
*      INCLUDE 'e_ntuple.dte'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(e_fis_Ntuple_exists) THEN
        call e_fis_Ntuple_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*
      call NO_nulls(e_fis_Ntuple_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(e_fis_Ntuple_file.EQ.' ') RETURN   !do nothing
*
*- get any free IO channel
*
      call g_IO_control(e_fis_Ntuple_IOchannel,'ANY',ABORT,err)
      io= e_fis_Ntuple_IOchannel
      e_fis_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      e_fis_Ntuple_ID= default_e_fis_Ntuple_ID
      id= e_fis_Ntuple_ID
*
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(e_fis_Ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      CALL HCDIR(directory,'R')       !CERNLIB read current directory
*
      e_fis_Ntuple_name= default_name
*
      id= e_fis_Ntuple_ID
      name= e_fis_Ntuple_name

      file= e_fis_Ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(e_fis_Ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF

      e_fis_Ntuple_file = file
*
      m= 0
      m= m+1
      e_fis_Ntuple_tag(m)= 'esp'    ! 1

* Open ntuple.
*
      e_fis_Ntuple_size= m     !total size
*
      title= e_fis_Ntuple_title
      IF(title.EQ.' ') THEN
        msg= name//' '//e_fis_Ntuple_file
        call only_one_blank(msg)
        title= msg   
        e_fis_Ntuple_title= title
      ENDIF
*
      id= e_fis_Ntuple_ID
      title= e_fis_Ntuple_title
      size= e_fis_Ntuple_size
      file= e_fis_Ntuple_file
      bank= default_bank
      call HBOOKN(id,title,size,name,bank,e_fis_Ntuple_tag)      !create Ntuple
      Write(*,*) e_fis_ntuple_file,' is opened.'
*
      call HCDIR(e_fis_Ntuple_directory,'R')      !record Ntuple directory
*
      CALL HCDIR(directory,' ')       !reset CERNLIB directory
*
      e_fis_Ntuple_exists= HEXIST(e_fis_Ntuple_ID)
      ABORT= .NOT.e_fis_Ntuple_exists
*
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // e_fis_Ntuple_directory // '/]' // 
     &           name // ' IO#$ "' // e_fis_Ntuple_file // '"'
      call G_build_note(pat,'$',iv,' ',rv,' ',msg)
      call sub_string(msg,' /]','/]')
*
      IF(ABORT) THEN
        err= ':unable to create '//msg
        call G_add_path(here,err)
c      ELSE
c        pat= ':created '//msg
c        call G_add_path(here,pat)
c        call G_log_message('INFO: '//pat)
      ENDIF
*
      RETURN
      END  
