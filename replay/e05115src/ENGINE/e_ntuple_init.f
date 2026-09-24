      subroutine e_Ntuple_init(ABORT,err)
*----------------------------------------------------------------------
*
*     Creates an HES Ntuple
*
*     Purpose : Books an HES Ntuple; defines structure of it
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 8-Apr-1994  K.B.Beard, Hampton Univ.
* $Log: e_ntuple_init.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:46  kawama
*
* e05115 src repository for software development
*
* Revision 1.4  2005/09/20 18:00:30  sumihama
* Add ndf and remove theta/phi from ntuple
*
* Revision 1.3  2005/07/06 19:55:38  cdaq
* Mod. ntuple for rf
*
* Revision 1.2  2005/07/03 11:54:42  sumihama
* Add Ehodo info. in hes-ntuple
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
      parameter (here='e_Ntuple_init')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'e_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
*
      character*80 default_name
      parameter (default_name= 'HESntuple')
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
      INCLUDE 'e_ntuple.dte'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(e_Ntuple_exists) THEN
        call e_Ntuple_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*
      call NO_nulls(e_Ntuple_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(e_Ntuple_file.EQ.' ') RETURN   !do nothing
*
*- get any free IO channel
*
      call g_IO_control(e_Ntuple_IOchannel,'ANY',ABORT,err)
      io= e_Ntuple_IOchannel
      e_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      e_Ntuple_ID= default_e_Ntuple_ID
      id= e_Ntuple_ID
*
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(e_Ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      CALL HCDIR(directory,'R')       !CERNLIB read current directory
*
      e_Ntuple_name= default_name
*
      id= e_Ntuple_ID
      name= e_Ntuple_name

      file= e_Ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(e_Ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF

      e_Ntuple_file = file
*
      m= 0
      m= m+1
      e_Ntuple_tag(m)= 'esp'    ! 1
      m= m+1
      e_Ntuple_tag(m)= 'esdelta' ! 2  
c      m= m+1
c      e_Ntuple_tag(m)= 'estheta' ! 3     
c      m= m+1
c      e_Ntuple_tag(m)= 'esphi'  ! 4     
      m= m+1
      e_Ntuple_tag(m)= 'escnhit' ! 5
      m= m+1
      e_Ntuple_tag(m)= 'esnco1' ! 6
      m= m+1
      e_Ntuple_tag(m)= 'esnco2' ! 6
      m= m+1
      e_Ntuple_tag(m)= 'esnco3' ! 6
      m= m+1
      e_Ntuple_tag(m)= 'esnt1' ! 6
      m= m+1
      e_Ntuple_tag(m)= 'esnt2' ! 6
      m= m+1
      e_Ntuple_tag(m)= 'esnt3' ! 6
      m= m+1
      e_Ntuple_tag(m)= 'esnt1m' ! 6
      m= m+1
      e_Ntuple_tag(m)= 'esnt2m' ! 6
      m= m+1
      e_Ntuple_tag(m)= 'escdepo' ! 6
      m= m+1
      e_Ntuple_tag(m)= 'esxfp'  ! 7   
      m= m+1
      e_Ntuple_tag(m)= 'esyfp'  ! 8     
      m= m+1
      e_Ntuple_tag(m)= 'esxpfp'    ! 9
      m= m+1
      e_Ntuple_tag(m)= 'esypfp' ! 10  
      m= m+1
      e_Ntuple_tag(m)= 'estimefp' ! 11   
      m= m+1
      e_Ntuple_tag(m)= 'espathl'  ! 12     
      m= m+1
      e_Ntuple_tag(m)= 'esytar' ! 13     
      m= m+1
      e_Ntuple_tag(m)= 'esxptar' ! 14   
      m= m+1
      e_Ntuple_tag(m)= 'esyptar'  ! 15
      m= m+1
      e_Ntuple_tag(m)= 'esxsv' ! 16  
      m= m+1
      e_Ntuple_tag(m)= 'esysv'  ! 17
      m= m+1
      e_Ntuple_tag(m)= 'eventid' ! 18    
      m= m+1
      e_Ntuple_tag(m)= 'runnum' ! 19   
      m= m+1
      e_Ntuple_tag(m)= 'etrkchi2' ! 20   
      m= m+1
      e_Ntuple_tag(m)= 'etrkndf' ! 20   
      m= m+1
      e_Ntuple_tag(m)= 'rftime'  ! 21  
      m= m+1
      e_Ntuple_tag(m)= 'etimetar' ! 22
      m= m+1
      e_Ntuple_tag(m)= 'enphys' ! 23   
      m= m+1
      e_Ntuple_tag(m)= 'erf' ! 24   
      m= m+1
      e_Ntuple_tag(m)= 'erfdiff' ! 25
      m= m+1
      e_Ntuple_tag(m)= 'sngres1' ! 26
      m= m+1
      e_Ntuple_tag(m)= 'sngres2' ! 27
      m= m+1
      e_Ntuple_tag(m)= 'sngres3' ! 28
      m= m+1
      e_Ntuple_tag(m)= 'sngres4' ! 29
      m= m+1
      e_Ntuple_tag(m)= 'sngres5' ! 30
      m= m+1
      e_Ntuple_tag(m)= 'sngres6' ! 31
      m= m+1
      e_Ntuple_tag(m)= 'sngres7' ! 32
      m= m+1
      e_Ntuple_tag(m)= 'sngres8' ! 33
      m= m+1
      e_Ntuple_tag(m)= 'sngres9' ! 34
      m= m+1
      e_Ntuple_tag(m)= 'sngres10' ! 35
      m= m+1
      e_Ntuple_tag(m)= 'sngres11' ! 36
      m= m+1
      e_Ntuple_tag(m)= 'sngres12' ! 37
      m= m+1
      e_Ntuple_tag(m)= 'sngres13' ! 38
      m= m+1
      e_Ntuple_tag(m)= 'sngres14' ! 39
      m= m+1
      e_Ntuple_tag(m)= 'sngres15' ! 40
      m= m+1
      e_Ntuple_tag(m)= 'sngres16' ! 41


* Open ntuple.
*
      e_Ntuple_size= m     !total size
*
      title= e_Ntuple_title
      IF(title.EQ.' ') THEN
        msg= name//' '//e_Ntuple_file
        call only_one_blank(msg)
        title= msg   
        e_Ntuple_title= title
      ENDIF
*
      id= e_Ntuple_ID
      title= e_Ntuple_title
      size= e_Ntuple_size
      file= e_Ntuple_file
      bank= default_bank
      call HBOOKN(id,title,size,name,bank,e_Ntuple_tag)      !create Ntuple
      Write(*,*) e_ntuple_file,' is opened.'
*
      call HCDIR(e_Ntuple_directory,'R')      !record Ntuple directory
*
      CALL HCDIR(directory,' ')       !reset CERNLIB directory
*
      e_Ntuple_exists= HEXIST(e_Ntuple_ID)
      ABORT= .NOT.e_Ntuple_exists
*
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // e_Ntuple_directory // '/]' // 
     &           name // ' IO#$ "' // e_Ntuple_file // '"'
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
