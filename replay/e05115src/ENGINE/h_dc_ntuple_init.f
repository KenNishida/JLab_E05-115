      subroutine h_dc_ntuple_init(ABORT,err)
*------------------------------------------------------------------
*     
*     Creates an HKS DC Ntuple
*     
*     Purpose : Books an HKS DC Ntuple; defines structure of it
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='h_dc_ntuple_init')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
      include 'hks_data_structures.cmn'
      include 'hks_bypass_switches.cmn'
*     
      character*80 default_name
      parameter (default_name= 'HKSDCntuple')
      integer default_bank,default_recL
      parameter (default_bank= 8000) !4 bytes/word
      parameter (default_recL= 1024) !record length
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
      IF(h_dc_ntuple_exists) THEN
         call h_dc_ntuple_shutdown(ABORT,err)
         If(ABORT) Then
            call G_add_path(here,err)
            RETURN
         EndIf
      ENDIF
*     
      call NO_nulls(h_dc_ntuple_file) !replace null characters with blanks
*     
*     -if name blank, just forget it
      IF(h_dc_ntuple_file.EQ.' ') RETURN !do nothing
*     
*     - get any free IO channel
*     
      call g_IO_control(h_dc_ntuple_IOchannel,'ANY',ABORT,err)
      io= h_dc_ntuple_IOchannel
      h_dc_ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
         call G_add_path(here,err)
         RETURN
      ENDIF
*     
      h_dc_ntuple_ID= default_h_dc_ntuple_ID
      id= h_dc_ntuple_ID
*     
      ABORT= HEXIST(id)
      IF(ABORT) THEN
         call g_IO_control(h_dc_ntuple_IOchannel,'FREE',ABORT,err)
         call G_build_note(':HBOOK id#$ already in use',
     &        '$',id,' ',rv,' ',err)
         call G_add_path(here,err)
         RETURN
      ENDIF
*     
      CALL HCDIR(directory,'R') !CERNLIB read current directory
*     
      h_dc_ntuple_name= default_name
*     
      id= h_dc_ntuple_ID
      name= h_dc_ntuple_name
      
      file= h_dc_ntuple_file
      call g_sub_run_number(file,gen_run_number)
      
      recL= default_recL
*     
*     -open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status) !CERNLIB
*     !directory set to "//TUPLE"
      ABORT= status.NE.0
      IF(ABORT) THEN
         call g_IO_control(h_dc_ntuple_IOchannel,'FREE',ABORT,err)
         iv(1)= status
         iv(2)= io
         pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
         call G_build_note(pat,'$',iv,' ',rv,' ',err)
         call G_add_path(here,err)
         RETURN
      ENDIF
*     
      if (h_debugnt(1).eq.0) then
         if (h_debugnt(3).eq.1) then
            m= 0
            m= m+1
            h_dc_ntuple_tag(m)= 'grun'! 1
            m= m+1
            h_dc_ntuple_tag(m)= 'gevent'! 2
            m= m+1
            h_dc_ntuple_tag(m)= 'tothits'! 3
            m= m+1
            h_dc_ntuple_tag(m)= 'la'     ! 4
            m= m+1
            h_dc_ntuple_tag(m)= 'wi'     ! 5
            m= m+1
            h_dc_ntuple_tag(m)= 'dtim'    ! 6
            m= m+1
            h_dc_ntuple_tag(m)= 'ddis'! 7
            m= m+1
            h_dc_ntuple_tag(m)= 'wcent'     ! 8
            m= m+1
            h_dc_ntuple_tag(m)= 'wcoor'     ! 9
            m= m+1
            h_dc_ntuple_tag(m)= 'tdc'    ! 10
      
*     Experiment dependent entries start here.
      
*     Open ntuple.
*     
            h_dc_ntuple_size= m          !total size
*     
            title= h_dc_ntuple_title
            IF(title.EQ.' ') THEN
               msg= name//' '//h_dc_ntuple_file
               call only_one_blank(msg)
               title= msg   
               h_dc_ntuple_title= title
            ENDIF
*     
            id= h_dc_ntuple_ID
            title= h_dc_ntuple_title
            size= h_dc_ntuple_size
            file= h_dc_ntuple_file
            bank= default_bank

            call HBOOKN(id,title,size,name,bank,h_dc_ntuple_tag) 
            !create Ntuple
         
         else if (h_debugnt(3).eq.2) then
            m= 0
            m= m+1
            h_dc_ntuple_tag(m)= 'grun'! 1
            m= m+1
            h_dc_ntuple_tag(m)= 'gevent'! 2
            m= m+1
            h_dc_ntuple_tag(m)= 'tothits'! 3
            m= m+1
            h_dc_ntuple_tag(m)= 'la'     ! 4
            m= m+1
            h_dc_ntuple_tag(m)= 'wi'     ! 5
            m= m+1
            h_dc_ntuple_tag(m)= 'dtime'  ! 6
            m= m+1
            h_dc_ntuple_tag(m)= 'ddis'   ! 7
            m= m+1
            h_dc_ntuple_tag(m)= 'wicen'  ! 8
            m= m+1
            h_dc_ntuple_tag(m)= 'wico'   ! 9
            m= m+1
            h_dc_ntuple_tag(m)= 'tdc'    ! 10
      
*     Experiment dependent entries start here.
      
*     Open ntuple.
*     
            h_dc_ntuple_size= m          !total size
*     
            title= h_dc_ntuple_title
            IF(title.EQ.' ') THEN
               msg= name//' '//h_dc_ntuple_file
               call only_one_blank(msg)
               title= msg   
               h_dc_ntuple_title= title
            ENDIF
*     
            id= h_dc_ntuple_ID
            title= h_dc_ntuple_title
            size= h_dc_ntuple_size
            file= h_dc_ntuple_file
            bank= default_bank

            call HBOOKN(id,title,size,name,bank,h_dc_ntuple_tag) 
            Write(*,*) h_dc_ntuple_file,' is opened.'
            !create Ntuple
         endif
      
      else if (h_debugnt(1).eq.1) then
         call HBNT(id,'HKS',' ')
         if(h_debugnt(3).eq.1) then 
            call HBNAME(id,'hdcraw',hdc_raw_tot_hits,
     +      'hdc_raw_tot_hits[0,3600]:i,'//
     +      'hdc_raw_layer_num(hdc_raw_tot_hits):i,'//
     +      'hdc_raw_wire_num(hdc_raw_tot_hits):i,'//
     +      'hdc_raw_tdc(hdc_raw_tot_hits):i')
         else if(h_debugnt(3).eq.2) then 
            call HBNAME(id,'hdcdec',hdc_tot_hits,
     +      'hdc_tot_hits[0,3600]:i,'//
     +      'hdc_drift_time(hdc_tot_hits),'//
     +      'hdc_drift_dis(hdc_tot_hits),'//
     +      'hdc_wire_center(hdc_tot_hits),'//
     +      'hdc_wire_coord(hdc_tot_hits),'//
     +      'hdc_layer_num(hdc_tot_hits):i,'//
     +      'hdc_wire_num(hdc_tot_hits):i,'//
     +      'hdc_tdc(hdc_tot_hits):i,'//
     +      'hdc_hits_per_layer(3):i')
         endif
      endif
*     
      call HCDIR(h_dc_ntuple_directory,'R') !record Ntuple directory
*     
      CALL HCDIR(directory,' ') !reset CERNLIB directory
*     
      h_dc_ntuple_exists= HEXIST(h_dc_ntuple_ID)
      ABORT= .NOT.h_dc_ntuple_exists
*     
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // h_dc_ntuple_directory // '/]' // 
     &     name // ' IO#$ "' // h_dc_ntuple_file // '"'
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
