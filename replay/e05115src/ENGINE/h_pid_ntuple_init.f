      subroutine h_pid_ntuple_init(ABORT,err)
*------------------------------------------------------------------
*     
*     Creates an HKS SCIN Ntuple
*     
*     Purpose : Books an HKS SCIN Ntuple; defines structure of it
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='h_pid_ntuple_init')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
      Include 'hks_data_structures.cmn'
      Include 'hks_scin_tof.cmn'
      Include 'hks_bypass_switches.cmn'
*     
      character*80 default_name
      parameter (default_name= 'HKSPIDntuple')
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
c     INCLUDE 'h_ntuple.dte'
*     
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(h_pid_ntuple_exists) THEN
         call h_pid_ntuple_shutdown(ABORT,err)
         If(ABORT) Then
            call G_add_path(here,err)
            RETURN
         EndIf
      ENDIF
*     
      call NO_nulls(h_pid_ntuple_file) !replace null characters with blanks
*     
*     -if name blank, just forget it
      IF(h_pid_ntuple_file.EQ.' ') RETURN !do nothing

*     - get any free IO channel
*     
      call g_IO_control(h_pid_ntuple_IOchannel,'ANY',ABORT,err)
      io= h_pid_ntuple_IOchannel
      h_pid_ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
         call G_add_path(here,err)
         RETURN
      ENDIF
*     
      h_pid_ntuple_ID= default_h_pid_ntuple_ID
      id= h_pid_ntuple_ID
*     
      ABORT= HEXIST(id)
      IF(ABORT) THEN
         call g_IO_control(h_pid_ntuple_IOchannel,'FREE',ABORT,err)
         call G_build_note(':HBOOK id#$ already in use',
     &        '$',id,' ',rv,' ',err)
         call G_add_path(here,err)
         RETURN
      ENDIF
*     
      CALL HCDIR(directory,'R') !CERNLIB read current directory
*     
      h_pid_ntuple_name= default_name
*     
      id= h_pid_ntuple_ID
      name= h_pid_ntuple_name
      
      file= h_pid_ntuple_file
      call g_sub_run_number(file,gen_run_number)
      
      recL= default_recL
*     
*     -open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status) !CERNLIB
*     !directory set to "//TUPLE"
      ABORT= status.NE.0
      IF(ABORT) THEN
         call g_IO_control(h_pid_ntuple_IOchannel,'FREE',ABORT,err)
         iv(1)= status
         iv(2)= io
         pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
         call G_build_note(pat,'$',iv,' ',rv,' ',err)
         call G_add_path(here,err)
         RETURN
      ENDIF

      if(h_debugnt(1).eq.0) then
*--   put contents here.
         m= 0
         m= m+1
         h_pid_ntuple_tag(m)= 'grun'    ! 1
         m= m+1
         h_pid_ntuple_tag(m)= 'gevent'  ! 2
         m= m+1
         h_pid_ntuple_tag(m)= 'co1X'    ! 4  
         m= m+1
         h_pid_ntuple_tag(m)= 'adc1Xp'  ! 5 
         m= m+1
         h_pid_ntuple_tag(m)= 'adc1Xn'  ! 6 
         m= m+1
         h_pid_ntuple_tag(m)= 'time1Xp' ! 7 
         m= m+1
         h_pid_ntuple_tag(m)= 'time1Xn' ! 8 
         m= m+1
         h_pid_ntuple_tag(m)= 'co1Y'    ! 9 
         m= m+1
         h_pid_ntuple_tag(m)= 'adc1Yp'  ! 10 
         m= m+1
         h_pid_ntuple_tag(m)= 'adc1Yn'  ! 11
         m= m+1                               
         h_pid_ntuple_tag(m)= 'time1Yp' ! 12
         m= m+1                               
         h_pid_ntuple_tag(m)= 'time1Yn' ! 13 
         m= m+1                               
         h_pid_ntuple_tag(m)= 'co2X'    ! 19 
         m= m+1                               
         h_pid_ntuple_tag(m)= 'adc2Xp'  ! 20 
         m= m+1
         h_pid_ntuple_tag(m)= 'adc2Xn'  ! 21
         m= m+1                             
         h_pid_ntuple_tag(m)= 'time2Xp' ! 22
         m= m+1                             
         h_pid_ntuple_tag(m)= 'time2Xn' ! 23
         m= m+1                               
         h_pid_ntuple_tag(m)= 'co2Y'    ! 14  
         m= m+1                               
         h_pid_ntuple_tag(m)= 'adc2Yp'  ! 15 
         m= m+1                               
         h_pid_ntuple_tag(m)= 'adc2Yn'  ! 16 
         m= m+1                               
         h_pid_ntuple_tag(m)= 'time2Yp' ! 17 
         m= m+1                               
         h_pid_ntuple_tag(m)= 'time2Yn' ! 18 
         m= m+1                             
         h_pid_ntuple_tag(m)= 'coAC1'   ! 24
         m= m+1                             
         h_pid_ntuple_tag(m)= 'adcAC1p' ! 25
         m= m+1                             
         h_pid_ntuple_tag(m)= 'adcAC1n' ! 26
         m= m+1                             
         h_pid_ntuple_tag(m)= 'timeAC1p'! 27
         m= m+1                             
         h_pid_ntuple_tag(m)= 'timeAC1n'! 28
         m= m+1                             
         h_pid_ntuple_tag(m)= 'coAC2'   ! 29
         m= m+1
         h_pid_ntuple_tag(m)= 'adcAC2p' ! 30
         m= m+1                             
         h_pid_ntuple_tag(m)= 'adcAC2n' ! 31
         m= m+1                             
         h_pid_ntuple_tag(m)= 'timeAC2p'! 32
         m= m+1                             
         h_pid_ntuple_tag(m)= 'timeAC2n'! 33
         m= m+1                             
         h_pid_ntuple_tag(m)= 'coAC3'   ! 34
         m= m+1                             
         h_pid_ntuple_tag(m)= 'adcAC3p' ! 35
         m= m+1                             
         h_pid_ntuple_tag(m)= 'adcAC3n' ! 36
         m= m+1                             
         h_pid_ntuple_tag(m)= 'timeAC3p'! 37
         m= m+1                             
         h_pid_ntuple_tag(m)= 'timeAC3n'! 38
         m= m+1                             
         h_pid_ntuple_tag(m)= 'coWC1'   ! 39
         m= m+1
         h_pid_ntuple_tag(m)= 'adcWC1p' ! 40
         m= m+1                             
         h_pid_ntuple_tag(m)= 'adcWC1n' ! 41
         m= m+1                             
         h_pid_ntuple_tag(m)= 'timeWC1'! 42
         m= m+1                             
         h_pid_ntuple_tag(m)= 'coWC2'   ! 44
         m= m+1                             
         h_pid_ntuple_tag(m)= 'adcWC2p' ! 45
         m= m+1                             
         h_pid_ntuple_tag(m)= 'adcWC2n' ! 46
         m= m+1                             
         h_pid_ntuple_tag(m)= 'timeWC2' ! 47
         m= m+1                             
         h_pid_ntuple_tag(m)= 'coLC'    ! 48
         m= m+1                             
         h_pid_ntuple_tag(m)= 'adcLCp'  ! 49
         m= m+1
         h_pid_ntuple_tag(m)= 'adcLCn'  ! 50
         m= m+1
         h_pid_ntuple_tag(m)= 'timeLCp' ! 51
         m= m+1
         h_pid_ntuple_tag(m)= 'timeLCn' ! 52
         m= m+1
         h_pid_ntuple_tag(m)= 'HES0'    ! 53
         m= m+1
         h_pid_ntuple_tag(m)= 'HKS0'    ! 54
         m= m+1
         h_pid_ntuple_tag(m)= 'HKSg1'   ! 55
         m= m+1
         h_pid_ntuple_tag(m)= 'HKSg2'   ! 56
         m= m+1
         h_pid_ntuple_tag(m)= 'HKSg3'   ! 57
         m= m+1
         h_pid_ntuple_tag(m)= 'HKSg4'   ! 58
         m= m+1
         h_pid_ntuple_tag(m)= 'HKSg5'   ! 59
         m= m+1
         h_pid_ntuple_tag(m)= 'HKSg6'   ! 60


*     Experiment dependent entries start here.
      
*     Open ntuple.
*     
         h_pid_ntuple_size= m          !total size
*     
         title= h_pid_ntuple_title
         IF(title.EQ.' ') THEN
            msg= name//' '//h_pid_ntuple_file
            call only_one_blank(msg)
            title= msg   
            h_pid_ntuple_title= title
         ENDIF
*     
         id= h_pid_ntuple_ID
         title= h_pid_ntuple_title
         size= h_pid_ntuple_size
         file= h_pid_ntuple_file
         bank= default_bank

         call HBOOKN(id,title,size,name,bank,h_pid_ntuple_tag) 
         Write(*,*) h_pid_ntuple_file,' is opened.'
         !create Ntuple

      elseif(h_debugnt(1).eq.1) then

      call HBNT(id,'HKS',' ')

c      if(h_debugnt(2).eq.1) then 
c         call HBNAME(id,'hpidraw',hpid_raw_tot_hits,
c     +   'hpid_raw_tot_hits[0,90]:i,'//
c     +   'hpid_raw_layer_num(hpid_raw_tot_hits):i,'//
c     +   'hpid_raw_counter_num(hpid_raw_tot_hits):i,'//
c     +   'hpid_rawtdc_pos_sub_trig(hpid_raw_tot_hits):i,'//
c     +   'hpid_rawtdc_neg_sub_trig(hpid_raw_tot_hits):i,'//
c     +   'hpid_raw_adc_pos(hpid_raw_tot_hits):i,'//
c     +   'hpid_raw_adc_neg(hpid_raw_tot_hits):i')
c      else if(h_debugnt(2).eq.2) then 
c         call HBNAME(id,'hpiddec',hpid_tot_hits,
c     +   'hpid_tot_hits[0,90]:i,'//
c     +   'hpid_layer_num(hpid_tot_hits):i,'//
c     +   'hpid_counter_num(hpid_tot_hits):i,'//
c     +   'hpid_adc_pos(hpid_tot_hits),'//
c     +   'hpid_adc_neg(hpid_tot_hits),'//
c     +   'hpid_tdc_pos(hpid_tot_hits):i,'//
c     +   'hpid_tdc_neg(hpid_tot_hits):i,'//
c     +   'hpid_pos_time(hpid_tot_hits),'//
c     +   'hpid_neg_time(hpid_tot_hits),'//
c     +   'hpid_time(hpid_tot_hits),'//
c     +   'hpid_sing_counter(3):i')
c      endif


      endif

      call HCDIR(h_pid_ntuple_directory,'R') !record Ntuple directory
*     
      CALL HCDIR(directory,' ') !reset CERNLIB directory
*     
      h_pid_ntuple_exists= HEXIST(h_pid_ntuple_ID)
      ABORT= .NOT.h_pid_ntuple_exists
*     
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // h_pid_ntuple_directory // '/]' // 
     &     name // ' IO#$ "' // h_pid_ntuple_file // '"'
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
