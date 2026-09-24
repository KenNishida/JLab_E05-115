      subroutine h_dc_ntuple_keep(ABORT,err)
*-----------------------------------------------------------------
*     
*     Purpose : Add entry to the HKS DC Ntuple
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='h_dc_ntuple_keep')
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
      include 'hks_aero_parms.cmn'
      include 'hks_bypass_switches.cmn'
*     
      logical HEXIST            !CERNLIB function
*     
      integer m,i,j,k
      
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(.NOT.h_dc_ntuple_exists) RETURN !nothing to do
*     
      if (h_debugnt(1).eq.0) then
         if (h_debugnt(3).eq.1) then
            Do i=1,hdc_raw_tot_hits
               m= 0
               m= m+1
               h_dc_ntuple_contents(m)= gen_run_number ! 1 
               m= m+1
               h_dc_ntuple_contents(m)= gen_event_ID_number ! 2 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_raw_tot_hits ! 3 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_raw_layer_num(i) ! 4 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_raw_wire_num(i) ! 5 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_drift_time(i) ! 6 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_drift_dis(i) ! 7 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_wire_center(i) ! 8 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_wire_coord(i) ! 9 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_tdc(i) ! 10
               
*     Experiment dependent entries start here.
*     Fill ntuple for this event
               ABORT= .NOT.HEXIST(h_dc_ntuple_ID)
               IF(ABORT) THEN
                  call G_build_note(':Ntuple ID#$ does not exist',
     &               '$',h_dc_ntuple_ID,' ',0.,' ',err)
                  call G_add_path(here,err)
               ELSE
                  call HFN(h_dc_ntuple_ID,h_dc_ntuple_contents)
               ENDIF
            EndDo                     ! hnphysics loop
         else if (h_debugnt(3).eq.2) then
            Do i=1,hdc_tot_hits
               m= 0
               m= m+1
               h_dc_ntuple_contents(m)= gen_run_number ! 1 
               m= m+1
               h_dc_ntuple_contents(m)= gen_event_ID_number ! 2 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_tot_hits ! 3 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_layer_num(i) ! 4 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_wire_num(i) ! 5 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_drift_time(i) ! 6 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_drift_dis(i) ! 7 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_wire_center(i) ! 8 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_wire_coord(i) ! 9 
               m= m+1
               h_dc_ntuple_contents(m)= hdc_tdc(i) ! 10
         
*     Experiment dependent entries start here.
*     Fill ntuple for this event
               ABORT= .NOT.HEXIST(h_dc_ntuple_ID)
               IF(ABORT) THEN
                  call G_build_note(':Ntuple ID#$ does not exist',
     &               '$',h_dc_ntuple_ID,' ',0.,' ',err)
                  call G_add_path(here,err)
               ELSE
                  call HFN(h_dc_ntuple_ID,h_dc_ntuple_contents)
               ENDIF
            EndDo                     ! hnphysics loop
         endif
      else if(h_debugnt(1).eq.1) then

*     Fill ntuple for this event
         ABORT= .NOT.HEXIST(h_dc_ntuple_ID)
         IF(ABORT) THEN
            call G_build_note(':Ntuple ID#$ does not exist',
     &           '$',h_dc_ntuple_ID,' ',0.,' ',err)
            call G_add_path(here,err)
         ELSE
              call HFNT(h_dc_ntuple_ID)
         ENDIF
      endif

*
      RETURN
      END      
