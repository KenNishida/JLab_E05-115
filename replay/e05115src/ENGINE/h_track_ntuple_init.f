      subroutine h_track_ntuple_init(ABORT,err)
*----------------------------------------------------------------------
*     
*     Creates an HES DC ntuple for debug and calibration purpose only.
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='h_track_ntuple_init')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
*
      character*80 default_name
      parameter (default_name= 'HKSTRACKntuple')
      integer default_bank,default_recL
      parameter (default_bank= 8000)    !4 bytes/word
      parameter (default_recL= 4096)    !record length
      character*80 title
      character*80 directory,name
      character*256 file
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)
*
      logical HEXIST           !CERNLIB function
*
c      INCLUDE 'e_ntuple.dte'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(h_track_ntuple_exists) THEN
        call h_track_ntuple_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*
      call NO_nulls(h_track_ntuple_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(h_track_ntuple_file.EQ.' ') RETURN   !do nothing
*
*- get any free IO channel
*
      call g_IO_control(h_track_ntuple_IOchannel,'ANY',ABORT,err)
      io= h_track_ntuple_IOchannel
      h_track_ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      h_track_ntuple_ID= default_h_track_ntuple_ID
      id= h_track_ntuple_ID
*
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(h_track_ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      CALL HCDIR(directory,'R')       !CERNLIB read current directory
*
      h_track_ntuple_name= default_name
*
      id= h_track_ntuple_ID
      name= h_track_ntuple_name

      file= h_track_ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(h_track_ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*--   put contents here.
       m= 0
       m= m+1
       h_track_ntuple_tag(m)= 'res1'    ! 1
       m= m+1
       h_track_ntuple_tag(m)= 'res2'    ! 2
       m= m+1
       h_track_ntuple_tag(m)= 'res3'    ! 3
       m= m+1
       h_track_ntuple_tag(m)= 'res4'    ! 4
       m= m+1
       h_track_ntuple_tag(m)= 'res5'    ! 5
       m= m+1
       h_track_ntuple_tag(m)= 'res6'    ! 6
       m= m+1
       h_track_ntuple_tag(m)= 'res7'    ! 7
       m= m+1
       h_track_ntuple_tag(m)= 'res8'    ! 8
       m= m+1
       h_track_ntuple_tag(m)= 'res9'    ! 9 
       m= m+1
       h_track_ntuple_tag(m)= 'res10'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'res11'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'res12'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'tcoord1'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'tcoord2'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'tcoord3'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'tcoord4'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'tcoord5'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'tcoord6'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'tcoord7'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'tcoord8'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'tcoord9'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'tcoord10'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'tcoord11'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'tcoord12'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'ddis1'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'ddis2'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'ddis3'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'ddis4'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'ddis5'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'ddis6'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'ddis7'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'ddis8'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'ddis9'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'ddis10'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'ddis11'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'ddis12'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'dtime1'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'dtime2'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'dtime3'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'dtime4'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'dtime5'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'dtime6'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'dtime7'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'dtime8'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'dtime9'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'dtime10'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'dtime11'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'dtime12'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'wc1'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'wc2'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'wc3'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'wc4'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'wc5'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'wc6'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'wc7'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'wc8'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'wc9'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'wc10'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'wc11'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'wc12'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'chi2pdof'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'hxfp'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'hyfp'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'hxpfp'    ! 10
       m= m+1
       h_track_ntuple_tag(m)= 'hypfp'    ! 10

* Open ntuple.
*
      h_track_ntuple_size= m     !total size
*
      title= h_track_ntuple_title
      IF(title.EQ.' ') THEN
        msg= name//' '//h_track_ntuple_file
        call only_one_blank(msg)
        title= msg   
        h_track_ntuple_title= title
      ENDIF
*
      id= h_track_ntuple_ID
      title= h_track_ntuple_title
      size= h_track_ntuple_size
      file= h_track_ntuple_file
      bank= default_bank
      call HBOOKN(id,title,size,name,bank,h_track_ntuple_tag)      !create Ntuple
*
      call HCDIR(h_track_ntuple_directory,'R')      !record Ntuple directory
*
      CALL HCDIR(directory,' ')       !reset CERNLIB directory
*
      h_track_ntuple_exists= HEXIST(h_track_ntuple_ID)
      ABORT= .NOT.h_track_ntuple_exists
*
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // h_track_ntuple_directory // '/]' // 
     &           name // ' IO#$ "' // h_track_ntuple_file // '"'
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
