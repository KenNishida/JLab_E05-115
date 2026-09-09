      subroutine h_luc_ntuple_keep(ABORT,err)
*-----------------------------------------------------------------
*     
*     Purpose : Add entry to the HKS luc Ntuple
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='h_luc_ntuple_keep')
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
      INCLUDE 'hks_lucite_parms.cmn'
      include 'hks_track_histid.cmn'
      Include 'hks_bypass_switches.cmn'
      INCLUDE 'hks_scin_parms.cmn'
      INCLUDE 'hks_scin_tof.cmn'
*      INCLUDE 'gen_data_structures.cmn'
*     
      logical HEXIST            !CERNLIB function
*     
      integer m,i,j,k, keep
      integer la,co
      real*8 scin_pos,scin_neg
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
c      write(*,*)"ev=",gen_event_ID_number
c      write(*,*)"hit=",hluc_raw_tot_hits
      IF(.NOT.h_luc_ntuple_exists) RETURN !nothing to do
*     
      if(hluc_raw_tot_hits .le. 0) RETURN
       
      if(h_debugnt(1).eq.0) then

         if(h_debugnt(8).eq.1) then
            Do i=1,hluc_raw_tot_hits
               m= 0
               m= m+1
               h_luc_ntuple_contents(m)= gen_run_number ! 1
               m= m+1
               h_luc_ntuple_contents(m)= gen_event_ID_number ! 2
               m= m+1
               h_luc_ntuple_contents(m)= hluc_raw_tot_hits ! 3         
               m= m+1
               h_luc_ntuple_contents(m)= hluc_raw_layer_num(i) ! 4
               m= m+1
               h_luc_ntuple_contents(m)= hluc_raw_counter_num(i) ! 5
               m= m+1
               h_luc_ntuple_contents(m)= hluc_rawadc_pos(i) ! 6
               m= m+1
               h_luc_ntuple_contents(m)= hluc_rawadc_neg(i) ! 7
               m= m+1
               h_luc_ntuple_contents(m)= hluc_rawadc_tot(i) ! 8
               m= m+1
               h_luc_ntuple_contents(m)= hluc_rawtdc_pos_sub_trig(i) ! 9
               m= m+1
               h_luc_ntuple_contents(m)= hluc_rawtdc_neg_sub_trig(i) ! 10
               m= m+1
               h_luc_ntuple_contents(m)= hluc_rawtdc_tot_sub_trig(i) ! 11
                           
               keep = m

*     Fill ntuple for this event
               ABORT= .NOT.HEXIST(h_luc_ntuple_ID)
               IF(ABORT) THEN
                  call G_build_note(':Ntuple ID#$ does not exist',
     &                 '$',h_luc_ntuple_ID,' ',0.,' ',err)
                  call G_add_path(here,err)
               ELSE
                  call HFN(h_luc_ntuple_ID,h_luc_ntuple_contents)
               ENDIF
            EndDo               ! hluc_raw_tot_hits

            Do i=1, hscin_raw_tot_hits

               la = hscin_raw_layer_num(i)
               co = hscin_raw_counter_num(i)
               scin_pos=-10000
               scin_neg=-10000
               if(la.eq.1) then
                  m=keep
                  m= m+1
                  h_luc_ntuple_contents(m)= hscin_raw_layer_num(i) ! 4
                  m= m+1
                  h_luc_ntuple_contents(m)= hscin_raw_counter_num(i) ! 5
                  m= m+1
                  h_luc_ntuple_contents(m)= hscin_rawtdc_pos_sub_trig(i) ! 10
                  m= m+1
                  h_luc_ntuple_contents(m)= hscin_rawtdc_neg_sub_trig(i) ! 11
                  
                  ABORT= .NOT.HEXIST(h_luc_ntuple_ID)
                  IF(ABORT) THEN
                     call G_build_note(':Ntuple ID#$ does not exist',
     &                    '$',h_luc_ntuple_ID,' ',0.,' ',err)
                     call G_add_path(here,err)
                  ELSE
                     call HFN(h_luc_ntuple_ID,h_luc_ntuple_contents)
                  ENDIF
               endif
            Enddo



         else if(h_debugnt(8).eq.2) then
            Do i=1,hluc_tot_hits
               m= 0
               m= m+1
               h_luc_ntuple_contents(m)= gen_run_number ! 1
               m= m+1
               h_luc_ntuple_contents(m)= gen_event_ID_number ! 2
               m= m+1
               h_luc_ntuple_contents(m)= hluc_tot_hits ! 3         
               m= m+1
               h_luc_ntuple_contents(m)= hluc_layer_num(i) ! 4
               m= m+1
               h_luc_ntuple_contents(m)= hluc_counter_num(i) ! 5
               m= m+1
               h_luc_ntuple_contents(m)= hluc_both_hits(i) ! 6
               m= m+1
               h_luc_ntuple_contents(m)= hluc_pos_npe(i) ! 7
               m= m+1
               h_luc_ntuple_contents(m)= hluc_neg_npe(i) ! 8
               m= m+1
               h_luc_ntuple_contents(m)= hluc_tot_npe(i) ! 9
               m= m+1
               h_luc_ntuple_contents(m)= hluc_pos_time(i) ! 10 
               m= m+1
               h_luc_ntuple_contents(m)= hluc_neg_time(i) ! 11
               m= m+1
               h_luc_ntuple_contents(m)= hluc_tot_time(i) ! 12

         
*     Fill ntuple for this event
               ABORT= .NOT.HEXIST(h_luc_ntuple_ID)
               IF(ABORT) THEN
                  call G_build_note(':Ntuple ID#$ does not exist',
     &               '$',h_luc_ntuple_ID,' ',0.,' ',err)
                  call G_add_path(here,err)
               ELSE
                  call HFN(h_luc_ntuple_ID,h_luc_ntuple_contents)
               ENDIF
            EndDo                     ! hluc_raw_tot_hits
         endif

      elseif(h_debugnt(1).eq.1) then

*     Fill ntuple for this event
         ABORT= .NOT.HEXIST(h_luc_ntuple_ID)
         IF(ABORT) THEN
            call G_build_note(':Ntuple ID#$ does not exist',
     &           '$',h_luc_ntuple_ID,' ',0.,' ',err)
            call G_add_path(here,err)
         ELSE
              call HFNT(h_luc_ntuple_ID)
         ENDIF

      endif
*
      RETURN
      END      
