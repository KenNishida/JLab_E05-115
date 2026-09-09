      subroutine e_dc_ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the SOS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log: e_dc_ntuple_keep.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.2  2005/01/04 23:56:21  miyoshi
* add detector ntuple
*
* Revision 1.1  2005/01/03 19:26:30  miyoshi
* add edc1 ntuple
*
* Revision 1.1.1.1  2004/08/30 21:21:38  miyoshi
* new dir
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='e_dc_ntuple_keep')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'e_ntuple.cmn'
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'hks_scin_parms.cmn'
      INCLUDE 'hes_data_structures.cmn'
      INCLUDE 'hes_physics_sing.cmn'
      INCLUDE 'hes_tracking.cmn'
      INCLUDE 'hes_bypass_swiches.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'gen_run_info.cmn'
*     
      logical HEXIST    !CERNLIB function
*
      integer m,i,j,k

      real proton_mass
      parameter ( proton_mass = 0.93827247 ) ! [GeV/c^2]
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(.NOT.e_dc_ntuple_exists) RETURN !nothing to do
*     
      if(e_debugnt(1).eq.0) then
         if(e_debugnt(3).eq.1) then
            Do i=1,edc1_raw_tot_hits
*     --- put cut condition here 
               m= 0
               m= m+1
               e_dc_ntuple_contents(m)= gen_run_number ! 1
               m= m+1
               e_dc_ntuple_contents(m)= gen_event_ID_number ! 1
               m= m+1
               e_dc_ntuple_contents(m)= edc1_raw_tot_hits ! 1
               m= m+1
               e_dc_ntuple_contents(m)= edc1_raw_layer_num(i) ! 1
               m= m+1
               e_dc_ntuple_contents(m)= edc1_raw_wire_num(i) ! 1
               m= m+1
               e_dc_ntuple_contents(m)= edc1_tdc(i) ! 1
            
  
*     Fill ntuple for this event
               ABORT= .NOT.HEXIST(e_dc_ntuple_ID)
               IF(ABORT) THEN
                  call G_build_note(':Ntuple ID#$ does not exist',
     &              '$',e_dc_ntuple_ID,' ',0.,' ',err)
                  call G_add_path(here,err)
               ELSE
                  call HFN(e_dc_ntuple_ID,e_dc_ntuple_contents)
               ENDIF
*     
            EndDo
         else if(e_debugnt(3).eq.2) then
            Do i=1,edc1_tot_hits
*     --- put cut condition here 
               m= 0
               m= m+1
               e_dc_ntuple_contents(m)= gen_run_number ! 1
               m= m+1
               e_dc_ntuple_contents(m)= gen_event_ID_number ! 1
               m= m+1
               e_dc_ntuple_contents(m)= edc1_tot_hits ! 1
               m= m+1
               e_dc_ntuple_contents(m)= edc1_layer_num(i) ! 1
               m= m+1
               e_dc_ntuple_contents(m)= edc1_wire_num(i) ! 1
               m= m+1
               e_dc_ntuple_contents(m)= edc1_drift_time(i) ! 1
               m= m+1
               e_dc_ntuple_contents(m)= edc1_drift_dis(i) ! 1
               m= m+1
               e_dc_ntuple_contents(m)= edc1_wire_center(i) ! 1
               m= m+1
               e_dc_ntuple_contents(m)= edc1_wire_coord(i) ! 1
               m= m+1
               e_dc_ntuple_contents(m)= edc1_tdc(i) ! 1
            
  
*     Fill ntuple for this event
               ABORT= .NOT.HEXIST(e_dc_ntuple_ID)
               IF(ABORT) THEN
                  call G_build_note(':Ntuple ID#$ does not exist',
     &              '$',e_dc_ntuple_ID,' ',0.,' ',err)
                  call G_add_path(here,err)
               ELSE
                  call HFN(e_dc_ntuple_ID,e_dc_ntuple_contents)
               ENDIF
*     
            EndDo
         endif
      elseif(e_debugnt(1).eq.1) then
*     Fill ntuple for this event
         ABORT= .NOT.HEXIST(e_dc_ntuple_ID)
         IF(ABORT) THEN
            call G_build_note(':Ntuple ID#$ does not exist',
     &           '$',e_dc_ntuple_ID,' ',0.,' ',err)
            call G_add_path(here,err)
         ELSE
            call HFNT(e_dc_ntuple_ID)
         ENDIF
      endif
      
      RETURN
      END      
