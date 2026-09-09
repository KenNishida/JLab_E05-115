      subroutine h_scin_ntuple_keep(ABORT,err)
*-----------------------------------------------------------------
*     
*     Purpose : Add entry to the HKS scin Ntuple
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='h_scin_ntuple_keep')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'hks_tracking.cmn'
      INCLUDE 'hks_physics_sing.cmn'
      INCLUDE 'hks_scin_parms.cmn'
      INCLUDE 'hks_scin_tof.cmn'
      include 'hks_track_histid.cmn'
      Include 'hks_bypass_switches.cmn'
      include 'hks_aero_parms.cmn'
      include 'gen_data_structures.cmn'
*     
      logical HEXIST            !CERNLIB function
*     
      integer m,i,j,k
      
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
c      write(*,*)"ev=",gen_event_ID_number
c      write(*,*)"hit=",hscin_raw_tot_hits
      IF(.NOT.h_scin_ntuple_exists) RETURN !nothing to do
*     
      if(hscin_raw_tot_hits .le. 0) RETURN
      
      if(h_debugnt(1).eq.0) then

         if(h_debugnt(2).eq.1) then
            Do i=1,hscin_raw_tot_hits
               m= 0
               m= m+1
               h_scin_ntuple_contents(m)= gen_run_number ! 1
               m= m+1
               h_scin_ntuple_contents(m)= gen_event_ID_number ! 2
               m= m+1
               h_scin_ntuple_contents(m)= hscin_raw_tot_hits ! 3         
               m= m+1
               h_scin_ntuple_contents(m)= hscin_raw_layer_num(i) ! 4
               m= m+1
               h_scin_ntuple_contents(m)= hscin_raw_counter_num(i) ! 5
               m= m+1
               h_scin_ntuple_contents(m)= hscin_raw_adc_pos(i) ! 6
               m= m+1
               h_scin_ntuple_contents(m)= hscin_raw_adc_neg(i) ! 7
               m= m+1
               h_scin_ntuple_contents(m)= hscin_rawtdc_pos_sub_trig(i) ! 8
               m= m+1
               h_scin_ntuple_contents(m)= hscin_rawtdc_neg_sub_trig(i) !
               
         
*     Fill ntuple for this event
               ABORT= .NOT.HEXIST(h_scin_ntuple_ID)
               IF(ABORT) THEN
                  call G_build_note(':Ntuple ID#$ does not exist',
     &               '$',h_scin_ntuple_ID,' ',0.,' ',err)
                  call G_add_path(here,err)
               ELSE
                  call HFN(h_scin_ntuple_ID,h_scin_ntuple_contents)
               ENDIF
            EndDo                     ! hscin_raw_tot_hits
         else if(h_debugnt(2).eq.2) then
            Do i=1,hscin_tot_hits
               m= 0
               m= m+1
               h_scin_ntuple_contents(m)= gen_run_number ! 1
               m= m+1
               h_scin_ntuple_contents(m)= gen_event_ID_number ! 2
               m= m+1
               h_scin_ntuple_contents(m)= hscin_tot_hits ! 3         
               m= m+1
               h_scin_ntuple_contents(m)= hscin_layer_num(i) ! 4
               m= m+1
               h_scin_ntuple_contents(m)= hscin_counter_num(i) ! 5
               m= m+1
               h_scin_ntuple_contents(m)= hscin_adc_pos(i) ! 6
               m= m+1
               h_scin_ntuple_contents(m)= hscin_adc_neg(i) ! 7
               m= m+1
               h_scin_ntuple_contents(m)= hscin_pos_time(i) !8 
               m= m+1
               h_scin_ntuple_contents(m)= hscin_neg_time(i) ! 9 
               m= m+1
               h_scin_ntuple_contents(m)= hscin_time(i) ! 10 
         
*     Fill ntuple for this event
               ABORT= .NOT.HEXIST(h_scin_ntuple_ID)
               IF(ABORT) THEN
                  call G_build_note(':Ntuple ID#$ does not exist',
     &               '$',h_scin_ntuple_ID,' ',0.,' ',err)
                  call G_add_path(here,err)
               ELSE
                  call HFN(h_scin_ntuple_ID,h_scin_ntuple_contents)
               ENDIF
            EndDo                     ! hscin_raw_tot_hits
         endif

      elseif(h_debugnt(1).eq.1) then

*     Fill ntuple for this event
         ABORT= .NOT.HEXIST(h_scin_ntuple_ID)
         IF(ABORT) THEN
            call G_build_note(':Ntuple ID#$ does not exist',
     &           '$',h_scin_ntuple_ID,' ',0.,' ',err)
            call G_add_path(here,err)
         ELSE
              call HFNT(h_scin_ntuple_ID)
         ENDIF

      endif
*
      RETURN
      END      
