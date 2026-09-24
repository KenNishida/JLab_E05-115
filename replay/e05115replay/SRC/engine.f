      PROGRAM Engine
*-------------------------------------------------------
*     -       Prototype C analysis routine
*     -
*     - This is the analysis shell for CEBAF hall C.
*     - It gets all of its instructions via the CTP package
*     - Loops through data until it encounters an error.
*     -
*     - Created  18-Nov-1993   Kevin B. Beard, Hampton Univ.
*     $Log: engine.f,v $
*
*     Revision 1.1.1.2  2009/06/23 14:25:40  Z. Ye
*     Added call to h_tofcal_endrun
*
*     Revision 1.1.1.1  2009/06/23 13:55:50  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.11  2005/08/04 21:56:15  cdaq
*     Remove pedestal events in keep_results
*
*     Revision 1.10  2005/06/12 03:33:41  kaneta
*
*
*     Added the following three lines in the code.
*     If you want to run engine-replay as an one-event-display program,
*     please remove 'c' of the lines.
*
*     c     call evdisplay_init(Abort,err)                                    ! initialization for the one event display. MK
*     c                 call both_detector_drawing(ABORT,err)                 !  hes and hks ditector drawing. MK
*     c     call evdisplay_end(ABORT,err)                                     ! end process for the one event display. MK
*
*     Revision 1.9  2005/05/30 18:13:33  miyoshi
*     change event type condition
*
*     Revision 1.8  2005/05/26 01:36:39  cdaq
*     add rpc routine again
*
*     Revision 1.7  2005/05/26 01:19:58  cdaq
*     update event type
*
*     Revision 1.6  2005/05/14 22:04:29  miyoshi
*     *** empty log message ***
*
*     Revision 1.5  2005/04/19 17:58:53  miyoshi
*     engine.f
*
*     Revision 1.4  2005/04/08 21:35:13  miyoshi
*     add close statement
*
*     Revision 1.3  2004/12/24 22:04:19  miyoshi
*     change minor
*
*     Revision 1.2  2004/10/14 22:21:21  miyoshi
*     correct analysis time
*     
*     Revision 1.1.1.1  2004/08/31 21:28:02  miyoshi
*     add replay
*     
*     Revision 2.3 2004/03/01 17:26 Miyoshi
*     change for E01-011
*     
*     Revision 2.2   2000/03/04 23:12:56  jinghua
*     (JLiu) Cleanup. re-organised the event processor. 
*     Run info event still being analysed 
*     but the values will not be used.
*     Removed old scaler event type 129.
*     
*     Revision 2.1   2000/02/18 15:34:16  jinghua
*     (JLiu) Removed the RPC handling
*     
*     Revision 2.0   1999/07/16 12:10:00  jinghua
*     (JLiu) Change HMS to HNSS
*     
*--------------------------------------------------------
      
      implicit none
      save
      
      character*6 here
      parameter (here= 'Engine')
      
      logical       ABORT, eof
      character*800 err, mss
      
      include 'gen_filenames.cmn'
      include 'gen_craw.cmn'
      include 'gen_run_info.cmn'
      include 'gen_event_info.cmn'
      include 'gen_run_pref.cmn'
      include 'gen_routines.dec'
      include 'gen_scalers.cmn'
      include 'gen_data_structures.cmn'
      

      logical      problems
      integer      total_event_count
      integer      physics_events
      integer      analyzed_events(0:gen_max_trigger_types)
      integer      sum_analyzed
      integer      recorded_events(0:gen_max_trigger_types)
      integer      sum_recorded
      integer      num_events_skipped
      integer      i,since_cnt,lastdump
      integer      time
c      external     time
      intrinsic    time
      integer      localy
      external     localy
      character*80 g_config_environmental_var
      parameter   (g_config_environmental_var = 'ENGINE_CONFIG_FILE')
      
c     --- rpc 
      integer rpc_pend                  

c     --- Pending asynchronous RPC requests   
      integer        ierr
      integer*4      status
      integer*4      evclose
      character*132  file
      character*20   groupname
      character*132  system_string
      character*16   hostname

      real*4    ebeam, phes, thes, phks, thks, ntarg
      
      integer   start_time,  lasttime
      integer*4 preprocessor_keep_event
      integer   ending_time,  analysis_time

*     --- track output file
      Character*30 hks_track_file
      Character*30 hes_track_file
      


c     ---1---------2---------3---------4---------5---------6---------7--


      print *
      print *,
     &     '  --- Hall C Proudly Presents  --- '
      print *,'PHYSICS Analysis Engine for HKS/HES!'
      print *
      
      total_event_count= 0                                              ! Need to register this
      lastdump=0
      do i=0,gen_max_trigger_types
        analyzed_events(i)=0
        recorded_events(i)=0
      enddo
      sum_analyzed=0
      sum_recorded=0
      num_events_skipped=0

      rpc_on      =  0                                                  !  RPC servicing off by default          
      rpc_control = -1                                                  !  If RPC on, don't block by default     

      print *, '> registering variables...'
      call g_register_variables(ABORT,err)
      if (ABORT.or.err.ne.' ') then
        call G_add_path(here,err)
        call G_rep_err(ABORT,err)
        if (ABORT) STOP
        err= ' '
      endif
      
      g_config_filename = ' '
      
      call engine_command_line(.false.)                                 !  Set CTP vars from command line

      print *, '> init filenames...'
      call G_init_filenames(ABORT,err,g_config_environmental_var)
      if (ABORT.or.err.ne.' ') then
        call G_add_path(here,err)
        call G_rep_err(ABORT,err)
        if (ABORT) STOP
        err= ' '
      endif
      
      call engine_command_line(.false.)                                 !  Set CTP vars from command line


      if (.not.ABORT.and.g_ctp_database_filename.ne.' ') then           !  If there is a g_ctp_database_filename set,
                                                                        !  pass the run number to it to set CTP variables
        print *,'> set ctp database...'
        call g_ctp_database(ABORT, err ,
     &       gen_run_number, g_ctp_database_filename)
        if (ABORT) then
          call G_add_path(here,err)
        endif
      endif
      
      call engine_command_line(.false.)                                 !  Set CTP vars from command line
      
      print *,'> initialize decode...'
      call G_decode_init(ABORT,err)
      if (ABORT.or.err.ne.' ') then
        call G_add_path(here,err)
        call G_rep_err(ABORT,err)
        if (ABORT) STOP
        err = ' '
      endif
      
      g_data_source_opened  = .false.                                    !  not opened yet
      g_data_source_in_hndl = 0                                          !  none
      print *,'> open source...'
      call G_open_source(ABORT,err)
      if (ABORT.or.err.ne.' ') then
        call G_add_path(here,err)
        call G_rep_err(ABORT,err)
        if (ABORT) STOP
        err = ' '
      endif


      if (g_preproc_on.ne.0) then                                       !  If preprocessor on, open event file
        g_preproc_opened = .false.                                      !  not opened yet
        g_preproc_in_hndl = 0                                           !  none IO opened
        call g_preproc_open(ABORT,err)
        if (ABORT.or.err.ne.' ') then
          call G_add_path(here,err)
          call G_rep_err(ABORT,err)
          if (ABORT) STOP
          err=' '
        endif
        write(6,*)'Opened CODA event file for preprocessor output'
      endif

      print *,'> initializing...'
      call G_initialize(ABORT,err)                                      !  includes a total reset
      if (ABORT.or.err.NE.' ') then
        call G_add_path(here,err)
        call G_rep_err(ABORT,err)
        if (ABORT) STOP
        err= ' '
      endif
      
      if (g_stats_blockname.ne.' '.and.                                 !  Print out the statistics report once...
     $     g_stats_output_filename.ne.' ') then
        file = g_stats_output_filename
        call g_sub_run_number(file, gen_run_number)
        ierr = threp(g_stats_blockname,file)
      endif

     
      if (hostname(1:6) .eq. 'cdaql3') then                             ! Comment out the these 6 lines if they cause trouble 
        write(system_string,*) 'runstats ',
     $       file(1:index(file,' ')-1), ' ',
     $       gen_run_number, ' > /dev/null &'
        call system(system_string)
      endif
     
     
      do i=1,length_craw                                                !  zero entire event buffer
        craw(i)= 0
      enddo
      
      since_cnt = 0
      problems  = .false.
      eof       = .false.


      if (rpc_on.ne.0) then
        print *,"*****************************************************"
        print *," "
        print *,"ENGINE is enabled to receive RPC requests"
        if (rpc_control.eq.0) then
          print *," "
          print *,"ENGINE will HANG waiting for RPC requests"
                else if (rpc_control.gt.0) then
          print *,"ENGINE will HANG to waitfor RPC requests after "
     $         ,rpc_control," events"
        endif
        if (rpc_control.ge.0) then
          print *,"If you don't want this to happen, put one of the"
          print *,"following in your CTP setup file"
          print *,"    rpc_on = 0 ; Turns off RPC handling"
          print *,"    rpc_control = -1 ; No Hanging, but RPC handled"
        endif
        print *," "
        print *,"*****************************************************"

        call thservset(0,0)                                             ! prepare for RPC requests               

      endif
      rpc_pend = 0

      
      start_time = time()
      lasttime = 0.
      

c     ---1---------2---------3---------4---------5---------6---------7--
c     print *,'> analysis start time',localy(start_time)
      
c      call evdisplay_init(Abort,err)                                    
      ! initialization for the one event display. MK

      do while ((.not. problems).and.(.NOT. ABORT).and.(.NOT. eof) )
        mss= ' '
        g_replay_time = time() - start_time
        
        call G_clear_event(ABORT,err)                                   !  clear out old data
        problems = problems .OR. ABORT
        
        if ( (mss.ne.' ').and.(err.NE.' ') ) then
          call G_append(mss,' & '//err)
        elseif (err.NE.' ') then
          mss= err
        endif
        
        if (.not.problems) Then
           call G_get_next_event(ABORT,err)                             !  get and store 1 event
         problems= problems .OR. ABORT
         if (.not. ABORT) total_event_count = total_event_count + 1
        endif
        
        if ( (mss.NE.' ').and.(err.NE.' ') ) then
          call G_append(mss,' & '//err)
        else if (err.NE.' ') then
          mss= err
        endif


        if (.not.problems) then                                         !  Check if this is a physics event or a CODA control event.
          gen_event_type = ishft(craw(2),-16)
c         write(*,*)'evtype=',gen_event_type,total_event_count

          if (gen_event_type.le.gen_MAX_trigger_types) then
            recorded_events(gen_event_type) = 
     &           recorded_events(gen_event_type) + 1
            if (gen_event_type.ne.0) sum_recorded = sum_recorded + 1
          endif

          if (gen_event_type.ge.(gen_max_trigger_types-1) .and.         !  if preprocessor is on write all events of trig type > 16
     &        g_preproc_on.ne.0                                 ) then  !  (i.e. all non-physics events)
            call g_write_event(ABORT,err)
          endif
          
          if (gen_event_type.eq.130) then                               !  run info event (get e,p,theta)
            call g_extract_kinematics(ebeam,phes,thes,phks,thks,ntarg)
            write(*,*) 'COMMENTS FROM RUN INFO EVENT'
            write(*,*) '!!! THESE VALUES ARE NOT USED BY ENGINE!'
            if (ebeam.gt.10.) ebeam=ebeam/1000.                         !  usually in MeV
            write(6,*) '  beam energy     =',abs(ebeam),' GeV'
            write(6,*) '  HKS central momentum =',abs(phks),' GeV/c'
            write(6,*) '  HKS theta in lab system =',abs(thks),' deg.'
            write(6,*) '  HES central momentum =',abs(phes),' GeV/c'
            write(6,*) '  HES theta in lab system =',abs(thes),' deg.'
            write(6,*) '  target number  =',abs(ntarg)
          endif
          
          if (iand(craw(2),INT(Z'FFFF')).EQ.INT(Z'10CC')) then                    !  Physics event

            if (gen_event_type.eq.0) then                               !  scaler event.

              call g_analyze_scalers_by_banks(craw,ABORT,err)
              analyzed_events(gen_event_type)=
     &          analyzed_events(gen_event_type)+1

c             if preprocessor is on, write trig type 0 (scaler events)

              if (gen_event_type.eq.0 .and. g_preproc_on.ne.0) then
                call g_write_event(ABORT,err)
              endif

c             dump report at first scaler event AFTER hist_dump_interval 
c             to keep hardware and software scalers roughly in sync.

              if ((physics_events-lastdump).ge.
     &             gen_run_hist_dump_interval
     &            .and.  gen_run_hist_dump_interval.gt.0) then
                lastdump = physics_events 

c               Wait for next interval of dump_int.

                call g_proper_shutdown(ABORT,err)
                print 112,
     &            "Finished dumping histograms/scalers for first",
     &            physics_events," events"
 112            format (a,i8,a)
              endif
               
            else if (gen_event_type.le.gen_MAX_trigger_types .and.
     $               gen_run_enable(gen_event_type-1).ne.0      ) then
                

              if (gen_event_ID_number .le. 3) then
                Print *,' > examining physics event...'
              endif
              call g_examine_physics_event(craw,ABORT,err)
              problems = problems .or. ABORT
              
              if (mss.NE.' ' .and. err.NE.' ') then
                call G_append(mss,' & '//err)
              elseif (err.NE.' ') then
                mss = err
              endif
                
c             --- correct to skipped+1 (Miyoshi,12/14/2004) to avoid skipping the 1st event.

              if ((num_events_skipped+1.lt.gen_run_starting_event).and.
     +            (gen_event_type.ne.4)                        ) then   ! always analyze peds.
                num_events_skipped = num_events_skipped + 1
              else
                if (gen_run_starting_event.eq.gen_event_id_number) then
                  start_time=time()                                     !  reset starttime for analysis rate
                endif
                if (.not.problems) then

c                 --- note.1210 disable below if you don't need
                  if (gen_event_ID_number .le. 3) then
                    print *,'> reconstruct data...'
                  endif
c                 --- end of note.1210
                  call G_reconstruction(craw,ABORT,err)                 !  COMMONs
                  if (gen_event_ID_number .le. 3) then
                     print *,'> gen_event_ID_number=',
     &                       gen_event_ID_number
                  endif
                  physics_events = physics_events + 1
                  analyzed_events(gen_event_type) =
     &                 analyzed_events(gen_event_type)+1
                  if (gen_event_type.ne.0) then
                    sum_analyzed = sum_analyzed + 1
                  endif
                  problems = problems .OR. ABORT
                endif
                   
                if (mss.NE.' ' .and. err.NE.' ') then
                  call G_append(mss,' & '//err)
                elseif (err.NE.' ') then
                  mss = err
                endif
                
                groupname = ' '
                if (gen_event_type .eq. 2) then
                  groupname = 'hes'
                endif
                if (gen_event_type .eq. 1) then
                  groupname = 'hks'
                endif

c               --- =1 or 2 for test
                if ( gen_event_type .eq. 3) then
                  groupname='both'
                endif
                   
                if (  gen_event_type .eq. 4) then
                  start_time = time()                                   !  reset start time for analysis rate
                  groupname  = 'ped'
                endif

                if ( gen_event_type .lt. 1 .or. 
     &               gen_event_type .gt. 8      ) then
                  write(6,*) 'gen_event_type= ',gen_event_type,
     &                       ' for call to g_keep_results'
                endif
                   
                   
                if (.NOT.problems .and. groupname.ne.' '.and.groupname.ne.'ped') Then
                  call G_keep_results(groupname,ABORT,err) 

c                  call both_detector_drawing(ABORT,err)                 
                  !  hes and hks ditector drawing. MK

c                 file away results as
                  problems = problems .OR. ABORT                        !  specified by interface
                endif
                   
                if (mss.NE.' ' .and. err.NE.' ') then
                  call G_append(mss,' & '//err)
                elseif (err.NE.' ') then
                  mss = err
                endif

c               if preprocessor is on check event for write criteria

                if (g_preproc_on.ne.0) then
                  if (.NOT.problems) then
                    call g_preproc_event(preprocessor_keep_event)
                    if (preprocessor_keep_event.eq.1) then
                       call g_write_event(ABORT,err)
                    endif
                  endif
                endif

c               - Here is where we insert a check for an Remote Proceedure Call (RPC)   
c               - from another process for CTP to interpret              

                if (rpc_on.ne.0) then                      
                  if (rpc_pend.eq.0.and.rpc_control.eq.0) then            
                     do while(rpc_pend.eq.0.and.rpc_control.eq.0)       
                        ierr = thservone(-1)                            !  block until one RPC request serviced
                        rpc_pend = thcallback()                  
                     enddo                                      
                  else                                                  
                     ierr = thservone(0)                                !  service one RPC requests             
                     rpc_pend = thcallback()                                
                  endif                                                  
                  if (rpc_pend.lt.0) rpc_pend = 0                       !  Last thcallback took care of all
                                                                        !  outstanding requests                      
                  if (rpc_control.gt.0) rpc_control = rpc_control - 1     
                endif                                                         

              endif

            else if (gen_event_type.eq.131 .or. 
     &               gen_event_type.eq.132     ) then                   !  EPICS event
              call g_examine_epics_event
            endif             

c           if REAL physics event as opposed to scaler (evtype=0)
             
          else                                                          ! not Physics event
              
            if (gen_event_type.eq.129) then

c              Old CODA 1.4 scaler events, not exist anymore
c              Got to discard this event, it will cause trouble otherwise.
c              call g_analyze_scalers(craw,ABORT,err)

            else if (gen_event_type.eq.133) then                        !  SAW's new go_info events
              call g_examine_go_info(craw,ABORT,err)
            else
              call g_examine_control_event(craw,ABORT,err)
            endif
            mss = err
          endif                                                         !  physics event or control event
           
        endif                                                           !  if no problem of reading event 

c       Now write the statistics report every 2 sec...

        if (g_replay_time-lasttime.ge.2) then                           !  dump every 2 seconds
          lasttime=g_replay_time
          if (g_stats_blockname       .ne. ' ' .and.
     $        g_stats_output_filename .ne. ' '       ) then
            file = g_stats_output_filename
            call g_sub_run_number(file, gen_run_number)
            ierr = threp(g_stats_blockname,file)
          endif
        endif
        
        since_cnt = since_cnt + 1
        if (since_cnt.GE.10000) then
          print * , ' event#',total_event_count,
     &              '      trigger#',physics_events
          since_cnt = 0
        endif
        
        if (ABORT .or. mss.NE.' ') then
          call G_add_path(here,mss)                                     !  only if problems
          call G_rep_err(ABORT,mss)
        endif
        
        eof = gen_event_type.eq.20
        
        if (gen_run_stopping_event.gt.0 .and.
     &     gen_event_ID_number.gt.0          ) then
          eof = eof .or.
     &          gen_run_stopping_event 
     &          .le. sum_analyzed-analyzed_events(4)
        endif
        
      enddo                                                             !  found a problem or end of run

cc calibrate HKS scintilattor tof  --Z. Ye 09/12/2009

c      call h_tofcal_endrun(gen_run_number)
      
      print *,'    -------------------------------------'
      
      if (ABORT .or. mss.NE.' ') then
        call G_rep_err(ABORT,mss)                                       !  report any errors or warnings
        err = ' '
      endif
      
      print *,'    -------------------------------------'

c     Print out the statistics report one last time...

      if (g_stats_blockname.ne.' '.and.
     $    g_stats_output_filename.ne.' ' ) then
        file = g_stats_output_filename
        call g_sub_run_number(file, gen_run_number)
        ierr = threp(g_stats_blockname,file)
      endif
      
      call G_proper_shutdown(ABORT,err)                                 !  save files, etc.
      if (ABORT .or. err.NE.' ') then
        call G_add_path(here,err)                                       !  report any errors or warnings
        call G_rep_err(ABORT,err)
        err = ' '
      endif
      
      call g_ntuple_shutdown(ABORT,err)
      if (ABORT .or. err.NE.' ') then
        call G_add_path(here,err)                                       !  report any errors or warnings
        call G_rep_err(ABORT,err)
        err = ' '
      endif

      if (g_charge_scaler_filename.ne.' ') then                         !  close charge scalers output file.
        close(unit=G_LUN_CHARGE_SCALER)
      endif

      if (g_epics_output_filename.ne.' ') then                          !  close epics output file.
        close(unit=G_LUN_EPICS_OUTPUT)
      endif
      
      close(88)                                                         !  close hks track output file.
                                                                        !  check h_generate_geomerry.f to refer LUN number=88.

      close(86)                                                         !  close hes track output file.
                                                                        !  check e_generate_geomerry.f to refer LUN number=86.       

      close(84)                                                         !  close hks tar output file.
                                                                        !  check h_generate_geomerry.f to refer LUN number=84.                      
      close(82)                                                         !  close hes tar output file.
                                                                        !  check e_generate_geomerry.f to refer LUN number=82.

      if (g_preproc_opened) then
        status= evclose(g_preproc_in_hndl)
        if (status.ne.0) then
          write(6,*) 'status for evclose=', status
        endif
      endif
      
      call g_dump_peds
      call e_dump_peds
      call h_dump_peds
      

c     Report TS Latched Trigger Pattern Statistic
 9898 FORMAT(a12,i10,1x,'(',f4.1,'%)')
 9899 FORMAT(a12,i10,1x)
      write(*,*) ' '
      write(*,*) 'TS Latched Trigger Stats (might not add to 100%):'
      write(*,9898) 'HKS:',gen_event_ts_counter(1),100.*gen_event_ts_counter(1)/physics_events
      write(*,9898) 'HES:',gen_event_ts_counter(2),100.*gen_event_ts_counter(2)/physics_events
      write(*,9898) 'COIN:',gen_event_ts_counter(3),100.*gen_event_ts_counter(3)/physics_events
      write(*,9898) 'LED:',gen_event_ts_counter(4),100.*gen_event_ts_counter(4)/physics_events
      write(*,9898) 'CP0:',gen_event_ts_counter(5),100.*gen_event_ts_counter(5)/physics_events
      write(*,9898) 'PED:',gen_event_ts_counter(8),100.*gen_event_ts_counter(8)/physics_events
      write(*,*) '-----------------------------'
      write(*,9898) 'FFBoff:',gen_event_ts_counter(11),100.*gen_event_ts_counter(11)/physics_events
      write(*,9898) 'DitherON:',gen_event_ts_counter(12),100.*gen_event_ts_counter(12)/physics_events
      write(*,*) '-----------------------------'
      write(*,9899) 'HKS sp_tot:',hks_event_counter(1)
      write(*,9899) 'HKS track:',hks_event_counter(2)
      write(*,9899) 'HKS physics:',hks_event_counter(3)
      write(*,*) '----------------------------------------------------------'

      print *
      print *,'Processed:'
      do i=0,gen_MAX_trigger_types
        if (recorded_events(i).GT.0) Then
          write(mss,'(4x,i12," / ",i8," events of type",i3)')
     &         analyzed_events(i),recorded_events(i),i
          call G_log_message(mss)
        endif
      enddo

      write(mss,'(i12," / ",i8," total (neglecting scalers)")') 
     &     sum_analyzed,sum_recorded
      call G_log_message(mss)
      print *,'  for run#',gen_run_number

      ending_time = time()
      analysis_time = ending_time - start_time
      print *,'Analysis time =',
     &        int(analysis_time/60), ' min. ',
     &        analysis_time-int(analysis_time/60)*60,
     &        ' sec.'

c     Comment out the following 4 lines if they cause trouble
      if (hostname(1:6).eq.'cdaql3') then
         call system(
     &        "kill `ps|grep runstats|awk '{ print $1}'` >/dev/null"
     &        )
      endif

c      call evdisplay_end(ABORT,err)                                     ! end process for the one event display. MK

      end
      
c     ---1---------2---------3---------4---------5---------6---------7--

      subroutine engine_command_line(outputflag)
      
      implicit      none
      integer       iargc
      integer       iarg
      character*132 arg
      logical       outputflag

c     Process command line args that set CTP variables

      do iarg=1,iargc()
        call getarg(iarg,arg)
        if (index(arg,'=').gt.0) then
          call thpset(arg)
          if (outputflag) then
            write(6,'(4x,a70)') arg(1:70)
          endif
        endif
      enddo
      
      return
      end
      
c     ---1---------2---------3---------4---------5---------6---------7--
