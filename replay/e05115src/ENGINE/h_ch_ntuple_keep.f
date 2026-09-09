      subroutine h_ch_ntuple_keep(ABORT,err)
*-----------------------------------------------------------------
*     
*     Purpose : Add entry to the Cherenkov Ntuple
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='h_ch_ntuple_keep')
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
*     
      logical HEXIST            !CERNLIB function
*     
      integer m,i,j,k
      Integer*4 temp_vector1(3,7),temp_vector2(3,7)
      Integer*4 la,co
      
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     

      IF(.NOT.h_ch_ntuple_exists) RETURN !nothing to do
*     

      IF(haer_raw_tot_hits .le. 0) Return

c     --- reset vector
      Do i=1,3
         Do j=1,7
            temp_vector1(i,j)=0
            temp_vector2(i,j)=0
         EndDo
      EndDo


      do i=1,haer_raw_tot_hits
         la=haer_raw_layer_num(i)
         co=haer_raw_counter_num(i)
         temp_vector1(la,co)=haer_rawadc_pos(i)
         temp_vector2(la,co)=haer_rawadc_neg(i)
      endDo

      m= 0
      m= m+1
      h_ch_ntuple_contents(m)= temp_vector1(1,1) ! 1 
      m= m+1
      h_ch_ntuple_contents(m)= temp_vector2(1,1) ! 1 
      m= m+1
      h_ch_ntuple_contents(m)= temp_vector1(1,2) ! 2 
      m= m+1
      h_ch_ntuple_contents(m)= temp_vector2(1,2) ! 2 
      m= m+1
      h_ch_ntuple_contents(m)= temp_vector1(1,3) ! 3 
      m= m+1
      h_ch_ntuple_contents(m)= temp_vector2(1,3) ! 3 
      m= m+1
      h_ch_ntuple_contents(m)= temp_vector1(1,4) ! 4 
      m= m+1
      h_ch_ntuple_contents(m)= temp_vector2(1,4) ! 4 

*     Experiment dependent entries start here.
      
      
*     Fill ntuple for this event
      ABORT= .NOT.HEXIST(h_ch_ntuple_ID)
      IF(ABORT) THEN
         call G_build_note(':Ntuple ID#$ does not exist',
     &        '$',h_ch_ntuple_ID,' ',0.,' ',err)
         call G_add_path(here,err)
      ELSE
         call HFN(h_ch_ntuple_ID,h_ch_ntuple_contents)
      ENDIF
      


*
      RETURN
      END      
