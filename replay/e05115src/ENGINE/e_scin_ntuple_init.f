      subroutine e_scin_ntuple_init(ABORT,err)
*----------------------------------------------------------------------
*     
*     Creates an ENGE SCIN ntuple for debug and calibration purpose only.
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='e_scin_ntuple_init')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'e_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
      Include 'hes_data_structures.cmn'
      Include 'hks_data_structures.cmn'
      Include 'hes_bypass_swiches.cmn'
*      
      character*80 default_name
      parameter (default_name= 'HESSCINntuple')
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

      IF(e_scin_ntuple_exists) THEN
        call e_scin_ntuple_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*
      call NO_nulls(e_scin_ntuple_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(e_scin_ntuple_file.EQ.' ') RETURN   !do nothing
*
*- get any free IO channel
*
      call g_IO_control(e_scin_ntuple_IOchannel,'ANY',ABORT,err)
      io= e_scin_ntuple_IOchannel
      e_scin_ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      e_scin_ntuple_ID= default_e_scin_ntuple_ID
      id= e_scin_ntuple_ID
*
      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(e_scin_ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*
      CALL HCDIR(directory,'R')       !CERNLIB read current directory
*
      e_scin_ntuple_name= default_name
*
      id= e_scin_ntuple_ID
      name= e_scin_ntuple_name

      file= e_scin_ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(e_scin_ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF

      if(e_debugnt(1).eq.0) then
         m= 0
*--   put contents here.
         if (e_debugnt(2).eq.1) then
            m= m+1
            e_scin_ntuple_tag(m)= 'grun' ! 1
            m= m+1
            e_scin_ntuple_tag(m)= 'gevent' ! 2
            m= m+1
            e_scin_ntuple_tag(m)= 'tothits' ! 3 total hits
            m= m+1
            e_scin_ntuple_tag(m)= 'la' ! 4 layer 
            m= m+1
            e_scin_ntuple_tag(m)= 'co' ! 5 counter
            m= m+1
            e_scin_ntuple_tag(m)= 'adcpr' ! 6 adc+
            m= m+1
            e_scin_ntuple_tag(m)= 'adcnr' ! 7 adc-
            m= m+1
            e_scin_ntuple_tag(m)= 'tdcpr' ! 8 tdc+
            m= m+1
            e_scin_ntuple_tag(m)= 'tdcnr' ! 9 tdc-
      
         else if (e_debugnt(2).eq.2) then
            m= m+1
            e_scin_ntuple_tag(m)= 'grun' ! 1
            m= m+1
            e_scin_ntuple_tag(m)= 'gevent' ! 2
            m= m+1
            e_scin_ntuple_tag(m)= 'tothits' ! 3 total hits
            m= m+1
            e_scin_ntuple_tag(m)= 'la' ! 4 layer 
            m= m+1
            e_scin_ntuple_tag(m)= 'co' ! 5 counter
            m= m+1
            e_scin_ntuple_tag(m)= 'adcpd' ! 6 adc+
            m= m+1
            e_scin_ntuple_tag(m)= 'adcnd' ! 7 adc-
            m= m+1
            e_scin_ntuple_tag(m)= 'tdcpd' ! 8 tdc+
            m= m+1
            e_scin_ntuple_tag(m)= 'tdcnd' ! 9 tdc-
            m= m+1
            e_scin_ntuple_tag(m)= 'timep' ! 10 tdc+
            m= m+1
            e_scin_ntuple_tag(m)= 'timen' ! 11 tdc-
            m= m+1
            e_scin_ntuple_tag(m)= 'time' ! 12 mean time 
         endif
      
*     Open ntuple.
*     
         e_scin_ntuple_size= m     !total size
*     
         title= e_scin_ntuple_title
         IF(title.EQ.' ') THEN
            msg= name//' '//e_scin_ntuple_file
            call only_one_blank(msg)
            title= msg   
            e_scin_ntuple_title= title
         ENDIF
*     
         id= e_scin_ntuple_ID
         title= e_scin_ntuple_title
         size= e_scin_ntuple_size
         file= e_scin_ntuple_file
         bank= default_bank
         call HBOOKN(id,title,size,name,bank,e_scin_ntuple_tag) 
         Write(*,*) e_scin_ntuple_file,' is opened.'
         !create Ntuple
*     
      elseif(e_debugnt(1).eq.1) then

      call HBNT(id,'HES',' ')

      if(e_debugnt(2).eq.1) then 
      call HBNAME(id,'escinraw',escin_raw_tot_hits,
     + 'escin_raw_tot_hits[0,900]:i,'//
     + 'escin_raw_layer_num(escin_raw_tot_hits):i,'//
     + 'escin_raw_counter_num(escin_raw_tot_hits):i,'//
     + 'escin_rawtdc_pos(escin_raw_tot_hits):i,'//
     + 'escin_rawtdc_neg(escin_raw_tot_hits):i,'//
     + 'escin_rawadc_pos(escin_raw_tot_hits):i,'//
     + 'escin_rawadc_neg(escin_raw_tot_hits):i,'//
     + 'escin_rawtdc_pos_sub_trig(escin_raw_tot_hits):i,'//
     + 'escin_rawtdc_neg_sub_trig(escin_raw_tot_hits):i')
      endif

      if(e_debugnt(2).eq.2) then 
      call HBNAME(id,'escindec',escin_tot_hits,
     + 'escin_tot_hits[0,30]:i,escin_good_hits(escin_tot_hits):i,'//
     + 'escin_layer_num(escin_tot_hits):i,'//
     + 'escin_counter_num(escin_tot_hits):i,'//
     + 'escin_tdc_pos(escin_tot_hits):i,'//
     + 'escin_tdc_neg(escin_tot_hits):i,'//
     + 'escin_adc_pos(escin_tot_hits):r,'//
     + 'escin_adc_neg(escin_tot_hits):r,'//
     + 'escin_time_pos(escin_tot_hits):r,'//
     + 'escin_time_neg(escin_tot_hits):r,'//
     + 'escin_mean_time(escin_tot_hits):r')
      endif


      if(e_debugnt(7).eq.1) then 
      call HBNAME(id,'etrack',entracks_fp,
     +     'entracks_fp[0,50]:i,'//
     +     'ex_fp(entracks_fp),'//
     +     'ey_fp(entracks_fp),'//
     +     'ez_fp(entracks_fp),'//
     +     'exp_fp(entracks_fp),'//
     +     'eyp_fp(entracks_fp),'//
     +     'echi2_fp(entracks_fp),'//
     +     'echi2perdof_fp(entracks_fp),'//
     +     'enfree_fp(entracks_fp):i,'//
     +     'entrack_hits(50,51)')
      endif

c      if(e_debugnt(8).eq.1) then 
c         call HBNAME(id,'hesrf',esrftime,
c     +        'esrftime,esrfdiff')
c         call HBNAME(id,'hksrf',hsrftime,
c     +        'hsrftime,hsrfdiff')
c      endif
      
      endif

      call HCDIR(e_scin_ntuple_directory,'R') !record Ntuple directory
*     
      CALL HCDIR(directory,' ') !reset CERNLIB directory
*     
      e_scin_ntuple_exists= HEXIST(e_scin_ntuple_ID)
      ABORT= .NOT.e_scin_ntuple_exists
*     
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // e_scin_ntuple_directory // '/]' // 
     &     name // ' IO#$ "' // e_scin_ntuple_file // '"'
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
