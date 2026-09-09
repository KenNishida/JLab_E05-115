      subroutine e_dc_ntuple_init(ABORT,err)
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
      parameter (here='e_dc_ntuple_init')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'e_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
      include 'hes_bypass_swiches.cmn'
      include 'hes_data_structures.cmn'
*
      character*80 default_name
      parameter (default_name= 'HESDCntuple')
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
c      INCLUDE 'e_ntuple.dte'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(e_dc_ntuple_exists) THEN
        call e_dc_ntuple_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*
      call NO_nulls(e_dc_ntuple_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(e_dc_ntuple_file.EQ.' ') RETURN   !do nothing
*
*- get any free IO channel
*
      call g_IO_control(e_dc_ntuple_IOchannel,'ANY',ABORT,err)
      io= e_dc_ntuple_IOchannel
      e_dc_ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      e_dc_ntuple_ID= default_e_dc_ntuple_ID
      id= e_dc_ntuple_ID
*
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(e_dc_ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      CALL HCDIR(directory,'R')       !CERNLIB read current directory
*
      e_dc_ntuple_name= default_name
*
      id= e_dc_ntuple_ID
      name= e_dc_ntuple_name

      file= e_dc_ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(e_dc_ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF

      if (e_debugnt(1).eq.0) then
         m= 0
*--   put contents here.
         if (e_debugnt(3).eq.1) then
            m= m+1
            e_dc_ntuple_tag(m)= 'grun'    ! 1
            m= m+1
            e_dc_ntuple_tag(m)= 'gevent'    ! 2
            m= m+1
            e_dc_ntuple_tag(m)= 'tothits'    ! 3
            m= m+1
            e_dc_ntuple_tag(m)= 'la'    ! 3
            m= m+1
            e_dc_ntuple_tag(m)= 'wi'    ! 4
            m= m+1
            e_dc_ntuple_tag(m)= 'tdc'    ! 5
         else if (e_debugnt(3).eq.2) then
            m= m+1
            e_dc_ntuple_tag(m)= 'grun'    ! 1
            m= m+1
            e_dc_ntuple_tag(m)= 'gevent'    ! 2
            m= m+1
            e_dc_ntuple_tag(m)= 'tothits'    ! 3
            m= m+1
            e_dc_ntuple_tag(m)= 'la'    ! 4
            m= m+1
            e_dc_ntuple_tag(m)= 'wi'    ! 5
            m= m+1
            e_dc_ntuple_tag(m)= 'dtime'    ! 6
            m= m+1
            e_dc_ntuple_tag(m)= 'ddis'    ! 7
            m= m+1
            e_dc_ntuple_tag(m)= 'wicen'    ! 8
            m= m+1
            e_dc_ntuple_tag(m)= 'wico'    ! 9
            m= m+1
            e_dc_ntuple_tag(m)= 'tdc'    ! 10
         endif

* Open ntuple.
*
         e_dc_ntuple_size= m     !total size
*
         title= e_dc_ntuple_title
         IF(title.EQ.' ') THEN
            msg= name//' '//e_dc_ntuple_file
            call only_one_blank(msg)
            title= msg   
            e_dc_ntuple_title= title
         ENDIF
*
         id= e_dc_ntuple_ID
         title= e_dc_ntuple_title
         size= e_dc_ntuple_size
         file= e_dc_ntuple_file
         bank= default_bank
         call HBOOKN(id,title,size,name,bank,e_dc_ntuple_tag)      
         Write(*,*) e_dc_ntuple_file,' is opened.'
         !create Ntuple
*
      else if (e_debugnt(1).eq.1) then
         call HBNT(id, 'HKS', ' ')
         if(e_debugnt(3).eq.1) then 
            call HBNAME(id,'edc1raw',edc1_raw_tot_hits,
     +         'edc1_raw_tot_hits[0,4800]:i,'//
     +         'edc1_raw_layer_num(edc1_raw_tot_hits):i,'//
     +         'edc1_raw_wire_num(edc1_raw_tot_hits):i,'//
     +         'edc1_raw_tdc(edc1_raw_tot_hits):i,')
         else if(e_debugnt(3).eq.2) then 
            call HBNAME(id,'edcdec',edc1_tot_hits,
     +         'edc1_tot_hits[0,200]:i,'//
     +         'edc1_drift_time(edc1_tot_hits),'//
     +         'edc1_drift_dis(edc1_tot_hits),'//
     +         'edc1_wire_center(edc1_tot_hits),'//
     +         'edc1_wire_coord(edc1_tot_hits),'//
     +         'edc1_layer_num(edc1_tot_hits):i,'//
     +         'edc1_group_num(edc1_tot_hits):i,'//
     +         'edc1_wire_num(edc1_tot_hits):i,'//
     +         'edc1_slot_num(edc1_tot_hits):i,'//
     +         'edc1_cluster_size(edc1_tot_hits):i,'//
     +         'edc1_tdc(edc1_tot_hits):i,'//
     +         'edc1_hits_per_layer(16):i')
         endif
      endif

      call HCDIR(e_dc_ntuple_directory,'R')      !record Ntuple directory
*
      CALL HCDIR(directory,' ')       !reset CERNLIB directory
*
      e_dc_ntuple_exists= HEXIST(e_dc_ntuple_ID)
      ABORT= .NOT.e_dc_ntuple_exists
*
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // e_dc_ntuple_directory // '/]' // 
     &           name // ' IO#$ "' // e_dc_ntuple_file // '"'
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
