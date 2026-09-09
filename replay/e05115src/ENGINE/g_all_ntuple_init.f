      SUBROUTINE g_all_ntuple_init(ABORT,err)
*--------------------------------------------------------
*     -   Purpose and Methods :
*     -   check all signals during commissioning time
*     - 
*     -   Output: ABORT	- success or failure
*     -         : err	- reason for failure, if any
*     - 
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*13 here
      parameter (here='g_all_ntuple_init')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
*
      character*80 default_name
      parameter (default_name= 'allntuple')
      integer default_bank,default_recL
      parameter (default_bank= 8000)    !4 bytes/word
      parameter (default_recL= 1024)    !record length
      character*80 title
      character*80 directory,name
      character*256 file
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)
      integer*4 ntind
      character*8 ntname

*
      logical HEXIST           !CERNLIB function
*
      INCLUDE 'gen_ntuple.dte'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(g_all_ntuple_exists) THEN
         call g_all_ntuple_shutdown(ABORT,err)
         If(ABORT) Then
            call G_add_path(here,err)
            RETURN
         EndIf
      ENDIF
*     
      call NO_nulls(g_all_ntuple_file) !replace null characters with blanks
*     
*     -if name blank, just forget it
      IF(g_all_ntuple_file.EQ.' ') RETURN !do nothing
*     
*     - get any free IO channel
*     
      call g_IO_control(g_all_ntuple_IOchannel,'ANY',ABORT,err)
      io= g_all_ntuple_IOchannel
      g_all_ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      g_all_ntuple_ID= default_g_ntuple_ID
      id= g_all_ntuple_ID
*
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(g_all_ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      CALL HCDIR(directory,'R')       !CERNLIB read current directory
*
      g_all_ntuple_name= default_name
*
      id= g_all_ntuple_ID
      name= g_all_ntuple_name

      file= g_all_ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(g_all_ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      
      m= 0
      m= m+1
      ntname='test    '
      g_all_ntuple_tag(m)= ntname
      
*     Open ntuple.
*     
      g_all_ntuple_size= m      !total size
*     
      title= g_all_ntuple_title
      IF(title.EQ.' ') THEN
         msg= name//' '//g_all_ntuple_file
         call only_one_blank(msg)
         title= msg   
         g_all_ntuple_title= title
      ENDIF
*     
      id= g_all_ntuple_ID
      title= g_all_ntuple_title
      size= g_all_ntuple_size
      file= g_all_ntuple_file
      bank= default_bank
      call HBOOKN(id,title,size,name,bank,g_all_ntuple_tag) !create Ntuple
*     
      call HCDIR(g_all_ntuple_directory,'R') !record Ntuple directory
*     
      CALL HCDIR(directory,' ') !reset CERNLIB directory
*     
      g_all_ntuple_exists= HEXIST(g_all_ntuple_ID)
      ABORT= .NOT.g_all_ntuple_exists
*     
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // g_all_ntuple_directory // '/]' // 
     &     name // ' IO#$ "' // g_all_ntuple_file // '"'
      call G_build_note(pat,'$',iv,' ',rv,' ',msg)
      call sub_string(msg,' /]','/]')
*     
      IF(ABORT) THEN
         err= ':unable to create '//msg
         call G_add_path(here,err)
         
c     ELSE
c     pat= ':created '//msg
c     call G_add_path(here,pat)
c     call G_log_message('INFO: '//pat)
         
      ENDIF
*
      RETURN
      END  


