      subroutine h_Ntuple_init(ABORT,err)
*------------------------------------------------------------------
*     
*     Creates an HKS Ntuple
*     
*     Purpose : Books an HKS Ntuple; defines structure of it
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*     Created: 8-Apr-1994  K.B.Beard, Hampton Univ.
*     
*     Revision 2.2 2009/08/11 Z.Ye
*     for E05-115: Add Lucite info
*   
*     Revision 2.1 2004/03/03 Miyoshi
*     for E01-011 
*     
*     Revision 2.0  2000/03/03 12:11:08  jinghua
*     (JLiu) Added Lucite info
*     
*     Revision 1.7  1996/09/04 15:18:02  saw
*     (JRA) Modify ntuple contents
*     
*     Revision 1.6  1996/01/16 16:41:14  cdaq
*     (JRA) Modify ntuple contents
*     
*     Revision 1.5  1995/09/01 13:38:59  cdaq
*     (JRA) Add Cerenkov photoelectron count to ntuple
*     
*     Revision 1.4  1995/07/27  19:00:31  cdaq
*     (SAW) Relocate data statements for f2c compatibility
*     
*     Revision 1.3  1995/05/11  19:00:02  cdaq
*     (SAW) Allow %d for run number in filenames
*     
*     Revision 1.2  1994/06/17  02:36:00  cdaq
*     (KBB) Upgrade
*     
*     Revision 1.1  1994/04/12  16:16:18  cdaq
*     Initial revision
*     
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='h_Ntuple_init')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
*     
      character*80 default_name
      parameter (default_name= 'HKSntuple')
      integer default_bank,default_recL
      parameter (default_bank= 8000) !4 bytes/word
c      parameter (default_recL= 1024) !record length
      parameter (default_recL= 4096) !record length
      character*80 title
      character*80 directory,name
      character*256 file
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)
*     
      logical HEXIST            !CERNLIB function
*     
      INCLUDE 'h_ntuple.dte'
*     
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(h_Ntuple_exists) THEN
         call h_Ntuple_shutdown(ABORT,err)
         If(ABORT) Then
            call G_add_path(here,err)
            RETURN
         EndIf
      ENDIF
*     
      call NO_nulls(h_Ntuple_file) !replace null characters with blanks
*     
*     -if name blank, just forget it
      IF(h_Ntuple_file.EQ.' ') RETURN !do nothing
*     
*     - get any free IO channel
*     
      call g_IO_control(h_Ntuple_IOchannel,'ANY',ABORT,err)
      io= h_Ntuple_IOchannel
      h_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
         call G_add_path(here,err)
         RETURN
      ENDIF
*     
      h_Ntuple_ID= default_h_Ntuple_ID
      id= h_Ntuple_ID
*     
      ABORT= HEXIST(id)
      IF(ABORT) THEN
         call g_IO_control(h_Ntuple_IOchannel,'FREE',ABORT,err)
         call G_build_note(':HBOOK id#$ already in use',
     &        '$',id,' ',rv,' ',err)
         call G_add_path(here,err)
         RETURN
      ENDIF
*     
      CALL HCDIR(directory,'R') !CERNLIB read current directory
*     
      h_Ntuple_name= default_name
*     
      id= h_Ntuple_ID
      name= h_Ntuple_name
      
      file= h_Ntuple_file
      call g_sub_run_number(file,gen_run_number)
      
      recL= default_recL
*     
*     -open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status) !CERNLIB
*     !directory set to "//TUPLE"
      ABORT= status.NE.0
      IF(ABORT) THEN
         call g_IO_control(h_Ntuple_IOchannel,'FREE',ABORT,err)
         iv(1)= status
         iv(2)= io
         pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
         call G_build_note(pat,'$',iv,' ',rv,' ',err)
         call G_add_path(here,err)
         RETURN
      ENDIF
      h_Ntuple_file = file
*     
      m= 0
      m= m+1
      h_Ntuple_tag(m)= 'hsp'    ! 1
      m= m+1
      h_Ntuple_tag(m)= 'hsdelta' ! 2
c      m= m+1
c      h_Ntuple_tag(m)= 'hstheta' ! 3
c      m= m+1
c      h_Ntuple_tag(m)= 'hsphi'  ! 4
      m= m+1
      h_Ntuple_tag(m)= 'hscnhit' ! 5
      m= m+1
      h_Ntuple_tag(m)= 'hsna1' ! 6
      m= m+1
      h_Ntuple_tag(m)= 'hsna2' ! 7
      m= m+1
      h_Ntuple_tag(m)= 'hsna3' ! 8
      m= m+1
      h_Ntuple_tag(m)= 'hsnt1' ! 6
      m= m+1
      h_Ntuple_tag(m)= 'hsnt2' ! 7
      m= m+1
      h_Ntuple_tag(m)= 'hsnt3' ! 8
      m= m+1
      h_Ntuple_tag(m)= 'hsnco1' ! 6
      m= m+1
      h_Ntuple_tag(m)= 'hsnco2' ! 7
      m= m+1
      h_Ntuple_tag(m)= 'hsnco3' ! 8
      m= m+1
      h_Ntuple_tag(m)= 'hsbeta' ! 9
      m= m+1
      h_Ntuple_tag(m)= 'hstof' ! 9
      m= m+1
      h_Ntuple_tag(m)= 'hsxfp'  ! 10 
      m= m+1
      h_Ntuple_tag(m)= 'hsyfp'  ! 11
      m= m+1
      h_Ntuple_tag(m)= 'hsxpfp' ! 12
      m= m+1
      h_Ntuple_tag(m)= 'hsypfp' ! 13
      m= m+1
      h_Ntuple_tag(m)= 'hstimefp' ! 14
      m= m+1
      h_Ntuple_tag(m)= 'hspathl' ! 15
      m= m+1
      h_Ntuple_tag(m)= 'hsytar' ! 16
      m= m+1
      h_Ntuple_tag(m)= 'hsxptar' ! 17
      m= m+1
      h_Ntuple_tag(m)= 'hsyptar' ! 18
      m= m+1
      h_Ntuple_tag(m)= 'hsxsv' ! 19
      m= m+1
      h_Ntuple_tag(m)= 'hsysv' ! 20
      m= m+1
      h_Ntuple_tag(m)= 'eventid' ! 21
      m = m+1
      h_Ntuple_tag(m)= 'runnum' ! 22
      m = m+1
      h_Ntuple_tag(m)= 'haernhit' ! 23
      m = m+1
      h_Ntuple_tag(m)= 'haernum1' ! 24
      m = m+1
      h_Ntuple_tag(m)= 'haernum2' ! 25
      m = m+1
      h_Ntuple_tag(m)= 'haernum3' ! 26
      m= m+1
      h_Ntuple_tag(m)= 'haernpe1' ! 27
      m= m+1
      h_Ntuple_tag(m)= 'haernpe2' ! 28
      m= m+1
      h_Ntuple_tag(m)= 'haernpe3' ! 29
      m= m+1
      h_Ntuple_tag(m)= 'haert1' ! 27
      m= m+1
      h_Ntuple_tag(m)= 'haert2' ! 28
      m= m+1
      h_Ntuple_tag(m)= 'haert3' ! 29
       m= m+1
      h_Ntuple_tag(m)= 'hwatnhit' ! 30
       m= m+1
      h_Ntuple_tag(m)= 'hwatnum1' ! 31
       m= m+1
      h_Ntuple_tag(m)= 'hwatnum2' ! 32
      m= m+1
      h_Ntuple_tag(m)= 'hwatnpe1' ! 33
      m= m+1
      h_Ntuple_tag(m)= 'hwatnpe2' ! 34
      m= m+1
      h_Ntuple_tag(m)= 'hwatt1' ! 33
      m= m+1
      h_Ntuple_tag(m)= 'hwatt2' ! 34
       m= m+1
      h_Ntuple_tag(m)= 'hlucnhit' ! 44
      m= m+1
      h_Ntuple_tag(m)= 'hlucnum' ! 45
       m= m+1
      h_Ntuple_tag(m)= 'hlucnpe' ! 47
      m= m+1
      h_Ntuple_tag(m)= 'trkchi2' ! 35
      m= m+1
      h_Ntuple_tag(m)= 'trkndf' ! 35
      m= m+1
      h_Ntuple_tag(m)= 'betak'  ! 36
      m= m+1
      h_Ntuple_tag(m)= 'betapi'  ! 37
      m= m+1
      h_Ntuple_tag(m)= 'betapr'  ! 38
      m= m+1
      h_Ntuple_tag(m)= 'hsrftime' ! 39 
      m= m+1
      h_Ntuple_tag(m)= 'htimetar' ! 40 
      m= m+1
      h_Ntuple_tag(m)= 'hnphys' ! 41
      m= m+1
      h_Ntuple_tag(m)= 'hrf' ! 42
      m= m+1
      h_Ntuple_tag(m)= 'hrfdiff' ! 43
     
*     Experiment dependent entries start here.
      
*     Open ntuple.
*     
      h_Ntuple_size= m          !total size
*     
      title= h_Ntuple_title
      IF(title.EQ.' ') THEN
         msg= name//' '//h_Ntuple_file
         call only_one_blank(msg)
         title= msg   
         h_Ntuple_title= title
      ENDIF
*     
      id= h_Ntuple_ID
      title= h_Ntuple_title
      size= h_Ntuple_size
      file= h_Ntuple_file
      bank= default_bank

      call HBOOKN(id,title,size,name,bank,h_Ntuple_tag) !create Ntuple
      Write(*,*) h_ntuple_file,' is opened.'

*     
      call HCDIR(h_Ntuple_directory,'R') !record Ntuple directory
*     
      CALL HCDIR(directory,' ') !reset CERNLIB directory
*     
      h_Ntuple_exists= HEXIST(h_Ntuple_ID)
      ABORT= .NOT.h_Ntuple_exists
*     
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // h_Ntuple_directory // '/]' // 
     &     name // ' IO#$ "' // h_Ntuple_file // '"'
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
