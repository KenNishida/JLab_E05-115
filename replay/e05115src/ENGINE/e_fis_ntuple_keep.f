      subroutine e_fis_ntuple_keep(ABORT,err)
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
      parameter (here='e_fis_ntuple_keep')
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
*     --- user should specify values below.
      Parameter(test_low= -2000)
      Parameter(test_high= 100)
      
*------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(.NOT.e_fis_ntuple_exists) RETURN !nothing to do
*     
c     if(escin_raw_tot_hits .le. 0) RETURN

      
      m= 0
      m= m+1
      e_fis_ntuple_contents(m)= 0 ! 1
      
*     Fill ntuple for this event
      ABORT= .NOT.HEXIST(e_fis_ntuple_ID)
      IF(ABORT) THEN
         call G_build_note(':Ntuple ID#$ does not exist',
     &        '$',e_fis_ntuple_ID,' ',0.,' ',err)
         call G_add_path(here,err)
      ELSE
         call HFN(e_fis_ntuple_ID,e_fis_ntuple_contents)
      ENDIF
      
      
*     Fill ntuple for this event
      ABORT= .NOT.HEXIST(e_fis_ntuple_ID)
      IF(ABORT) THEN
         call G_build_note(':Ntuple ID#$ does not exist',
     &        '$',e_fis_ntuple_ID,' ',0.,' ',err)
         call G_add_path(here,err)
      ELSE
         call HFNT(e_fis_ntuple_ID)
      ENDIF

      RETURN
      END      
