      subroutine e_scin_ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the HES Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='e_scin_ntuple_keep')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'e_ntuple.cmn'
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'hks_scin_parms.cmn'
      INCLUDE 'hes_data_structures.cmn'
      INCLUDE 'hes_physics_sing.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'gen_run_info.cmn'
      Include 'hes_bypass_swiches.cmn'
*     
      logical HEXIST    !CERNLIB function
*     
      integer m,i,j,k,la,co
*     --- for test purpose
      Integer temp_vector(5,5),nlayer(3)
      Integer test_low,test_high
      Integer t1ap,t1an,t2ap,t2an
      Integer t1tp,t1tn,t2tp,t2tn
      integer eh3flag
*     --- user should specify values below.
      Parameter(test_low= -2000)
      Parameter(test_high= 100)
      
*------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(.NOT.e_scin_ntuple_exists) RETURN !nothing to do
*     
      if(escin_raw_tot_hits .le. 0) RETURN

      if(e_debugnt(1).eq.0) then
         if(e_debugnt(2).eq.1) then
            Do i=1,escin_raw_tot_hits
               m= 0
               m= m+1
               e_scin_ntuple_contents(m)= gen_run_number ! 1
               m= m+1
               e_scin_ntuple_contents(m)= gen_event_ID_number ! 2
               m= m+1
               e_scin_ntuple_contents(m)= escin_raw_tot_hits ! 3         
               m= m+1
               e_scin_ntuple_contents(m)= escin_raw_layer_num(i) ! 4
               m= m+1
               e_scin_ntuple_contents(m)= escin_raw_counter_num(i) ! 5
               m= m+1
               e_scin_ntuple_contents(m)= escin_rawadc_pos(i) ! 6
               m= m+1
               e_scin_ntuple_contents(m)= escin_rawadc_neg(i) ! 7
               m= m+1
               e_scin_ntuple_contents(m)= escin_rawtdc_pos_sub_trig(i) ! 8
               m= m+1
               e_scin_ntuple_contents(m)= escin_rawtdc_neg_sub_trig(i) ! 9

*     Fill ntuple for this event
               ABORT= .NOT.HEXIST(e_scin_ntuple_ID)
               IF(ABORT) THEN
                  call G_build_note(':Ntuple ID#$ does not exist',
     &              '$',e_scin_ntuple_ID,' ',0.,' ',err)
                  call G_add_path(here,err)
               ELSE
                  call HFN(e_scin_ntuple_ID,e_scin_ntuple_contents)
               ENDIF
            EndDo                     ! escin_tot_hits 
         else if (e_debugnt(2).eq.2) then
            eh3flag=0
            Do i=1,escin_tot_hits
               if (escin_layer_num(i).eq.3.and.
     &            abs(escin_tdc_pos(i)-escin_tdc_neg(i)).lt.50)then
                  eh3flag=1
               endif
            enddo
            if (eh3flag.eq.1) then
            Do i=1,escin_tot_hits
               m= 0
               m= m+1
               e_scin_ntuple_contents(m)= gen_run_number ! 1
               m= m+1
               e_scin_ntuple_contents(m)= gen_event_ID_number ! 2
               m= m+1
               e_scin_ntuple_contents(m)= escin_tot_hits ! 3         
               m= m+1
               e_scin_ntuple_contents(m)= escin_layer_num(i) ! 4
               m= m+1
               e_scin_ntuple_contents(m)= escin_counter_num(i) ! 5
               m= m+1
               e_scin_ntuple_contents(m)= escin_adc_pos(i) ! 6
               m= m+1
               e_scin_ntuple_contents(m)= escin_adc_neg(i) ! 7
               m= m+1
               e_scin_ntuple_contents(m)= escin_tdc_pos(i) ! 8
               m= m+1
               e_scin_ntuple_contents(m)= escin_tdc_neg(i) ! 9
               m= m+1
               e_scin_ntuple_contents(m)= escin_time_pos(i) ! 10
               m= m+1
               e_scin_ntuple_contents(m)= escin_time_neg(i) ! 11
               m= m+1
               e_scin_ntuple_contents(m)= escin_mean_time(i) ! 11

*     Fill ntuple for this event
               ABORT= .NOT.HEXIST(e_scin_ntuple_ID)
               IF(ABORT) THEN
                  call G_build_note(':Ntuple ID#$ does not exist',
     &              '$',e_scin_ntuple_ID,' ',0.,' ',err)
                  call G_add_path(here,err)
               ELSE
                  call HFN(e_scin_ntuple_ID,e_scin_ntuple_contents)
               ENDIF
            EndDo                     ! escin_tot_hits 
            endif
         endif

      
      elseif(e_debugnt(1).eq.1) then
*     Fill ntuple for this event
         ABORT= .NOT.HEXIST(e_scin_ntuple_ID)
         IF(ABORT) THEN
            call G_build_note(':Ntuple ID#$ does not exist',
     &           '$',e_scin_ntuple_ID,' ',0.,' ',err)
            call G_add_path(here,err)
         ELSE
            call HFNT(e_scin_ntuple_ID)
         ENDIF
      endif

      RETURN
      END      
