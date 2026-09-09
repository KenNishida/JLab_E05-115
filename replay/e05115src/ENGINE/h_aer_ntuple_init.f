      subroutine h_aer_ntuple_init(ABORT,err)
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
      parameter (here='h_aer_ntuple_init')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
      Include 'hks_data_structures.cmn'
      Include 'hks_bypass_switches.cmn'
*     
      character*80 default_name
      parameter (default_name= 'HKSAERntuple')
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
      IF(h_aer_ntuple_exists) THEN
         call h_aer_ntuple_shutdown(ABORT,err)
         If(ABORT) Then
            call G_add_path(here,err)
            RETURN
         EndIf
      ENDIF
*     
      call NO_nulls(h_aer_ntuple_file) !replace null characters with blanks
*     
*     -if name blank, just forget it
      IF(h_aer_ntuple_file.EQ.' ') RETURN !do nothing

*     - get any free IO channel
*     
      call g_IO_control(h_aer_ntuple_IOchannel,'ANY',ABORT,err)
      io= h_aer_ntuple_IOchannel
      h_aer_ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
         call G_add_path(here,err)
         RETURN
      ENDIF
*     
      h_aer_ntuple_ID= default_h_aer_ntuple_ID
      id= h_aer_ntuple_ID
*     
      ABORT= HEXIST(id)
      IF(ABORT) THEN
         call g_IO_control(h_aer_ntuple_IOchannel,'FREE',ABORT,err)
         call G_build_note(':HBOOK id#$ already in use',
     &        '$',id,' ',rv,' ',err)
         call G_add_path(here,err)
         RETURN
      ENDIF
*     
      CALL HCDIR(directory,'R') !CERNLIB read current directory
*     
      h_aer_ntuple_name= default_name
*     
      id= h_aer_ntuple_ID
      name= h_aer_ntuple_name
      
      file= h_aer_ntuple_file
      call g_sub_run_number(file,gen_run_number)
      
      recL= default_recL
*     
*     -open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status) !CERNLIB
*     !directory set to "//TUPLE"
      ABORT= status.NE.0
      IF(ABORT) THEN
         call g_IO_control(h_aer_ntuple_IOchannel,'FREE',ABORT,err)
         iv(1)= status
         iv(2)= io
         pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
         call G_build_note(pat,'$',iv,' ',rv,' ',err)
         call G_add_path(here,err)
         RETURN
      ENDIF

      if(h_debugnt(1).eq.0) then
         if(h_debugnt(4).eq.1) then
*--   put contents here.
            m= 0
            m= m+1
            h_aer_ntuple_tag(m)= 'grun' ! 1
            m= m+1
            h_aer_ntuple_tag(m)= 'gevent' ! 2
            m= m+1
            h_aer_ntuple_tag(m)= 'tothits' ! 3 total hits
            m= m+1
            h_aer_ntuple_tag(m)= 'la' ! 4 layer 
            m= m+1
            h_aer_ntuple_tag(m)= 'co' ! 5 counter
            m= m+1
            h_aer_ntuple_tag(m)= 'adcpr' ! 6 raw adc+
            m= m+1
            h_aer_ntuple_tag(m)= 'adcnr' ! 7 raw adc-
            m= m+1
            h_aer_ntuple_tag(m)= 'tdcpr' ! 8 raw tdc+
            m= m+1
            h_aer_ntuple_tag(m)= 'tdcnr' ! 9 raw tdc-

*     Experiment dependent entries start here.
      
*     Open ntuple.
*     
            h_aer_ntuple_size= m          !total size
*     
            title= h_aer_ntuple_title
            IF(title.EQ.' ') THEN
               msg= name//' '//h_aer_ntuple_file
               call only_one_blank(msg)
               title= msg   
               h_aer_ntuple_title= title
            ENDIF
*     
            id= h_aer_ntuple_ID
            title= h_aer_ntuple_title
            size= h_aer_ntuple_size
            file= h_aer_ntuple_file
            bank= default_bank

            call HBOOKN(id,title,size,name,bank,h_aer_ntuple_tag) 
            Write(*,*) h_aer_ntuple_file,' is opened.'
            !create Ntuple

         else if(h_debugnt(4).eq.2) then
*--   put contents here.
            m= 0
            m= m+1
            h_aer_ntuple_tag(m)= 'grun' ! 1
            m= m+1
            h_aer_ntuple_tag(m)= 'gevent' ! 2
            m= m+1
            h_aer_ntuple_tag(m)= 'tothits' ! 3 total hits
            m= m+1
            h_aer_ntuple_tag(m)= 'la' ! 4 layer 
            m= m+1
            h_aer_ntuple_tag(m)= 'co' ! 5 counter
            m= m+1
            h_aer_ntuple_tag(m)= 'bothhit' ! 6 bothhit
            m= m+1
            h_aer_ntuple_tag(m)= 'npep' ! 7 npe+
            m= m+1
            h_aer_ntuple_tag(m)= 'npen' ! 8 npe-
            m= m+1
            h_aer_ntuple_tag(m)= 'timep' ! 9 time+
            m= m+1
            h_aer_ntuple_tag(m)= 'timen' ! 10 time+

*     Experiment dependent entries start here.
      
*     Open ntuple.
*     
            h_aer_ntuple_size= m          !total size
*     
            title= h_aer_ntuple_title
            IF(title.EQ.' ') THEN
               msg= name//' '//h_aer_ntuple_file
               call only_one_blank(msg)
               title= msg   
               h_aer_ntuple_title= title
            ENDIF
*     
            id= h_aer_ntuple_ID
            title= h_aer_ntuple_title
            size= h_aer_ntuple_size
            file= h_aer_ntuple_file
            bank= default_bank

            call HBOOKN(id,title,size,name,bank,h_aer_ntuple_tag) 
            !create Ntuple
         endif

      elseif(h_debugnt(1).eq.1) then

         call HBNT(id,'HKS',' ')

         if(h_debugnt(4).eq.1) then 
            call HBNAME(id,'haerraw',haer_raw_tot_hits,
     &        'haer_raw_tot_hits[0,384]:i,'//
     &        'haer_raw_layer_num(haer_raw_tot_hits):i,'//
     &        'haer_raw_counter_num(haer_raw_tot_hits):i,'//
     &        'haer_rawadc_pos(haer_raw_tot_hits):i,'//
     &        'haer_rawadc_neg(haer_raw_tot_hits):i,'//
     &        'haer_rawtdc_pos_sub_trig(haer_raw_tot_hits):i,'//
     &        'haer_rawtdc_neg_sub_trig(haer_raw_tot_hits):i,'//
     &        'haer_raw_layer_hits(2):i')
         elseif(h_debugnt(4).eq.2) then 
            call HBNAME(id,'haerdec',haer_tot_hits,
     &        'haer_tot_hits[0,384]:i,'//
     &        'haer_layer_num(haer_tot_hits):i,'//
     &        'haer_counter_num(haer_tot_hits):i,'//
     &        'haer_both_hits(haer_tot_hits):i,'//
     &        'haer_pos_npe(haer_tot_hits),'//
     &        'haer_neg_npe(haer_tot_hits),'//
     &        'haer_pos_time(haer_tot_hits),'//
     &        'haer_neg_time(haer_tot_hits),'//
     &        'haer_layer_hits(2):i')
         endif
      endif

      call HCDIR(h_aer_ntuple_directory,'R') !record Ntuple directory
*     
      CALL HCDIR(directory,' ') !reset CERNLIB directory
*     
      h_aer_ntuple_exists= HEXIST(h_aer_ntuple_ID)
      ABORT= .NOT.h_aer_ntuple_exists
*     
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // h_aer_ntuple_directory // '/]' // 
     &     name // ' IO#$ "' // h_aer_ntuple_file // '"'
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
