      subroutine e_track_ntuple_keep(ABORT,err)
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
      parameter (here='e_track_ntuple_keep')
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
      INCLUDE 'hes_statistics.cmn'
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
      IF(.NOT.e_track_ntuple_exists) RETURN !nothing to do
*     
c      Do i=1,enphysics
c      Do i=1,emax_dc1_dec_hits
      Do i=1,entracks_pre
*      Do i=1,emax_dc_hits
*     --- put cut condition here 
c         if(
c     &        esp(i) .gt. 0
c     &        edc1_layer_num(i) .gt. 0
c     &     ) then
         if (i.eq.edc1bestchi2_pre_index(enum_fitting)) then
            
            m= 0
            
            Do j=1,emax_num_dc1_layers 
               m= m+1
               e_track_ntuple_contents(m)= 
c     &          edc1eff_residual(i,j,enum_fitting) ! 11
     &          edc1_residual_pre(i,j,enum_fitting) ! 1
            Enddo
             
            Do j=1,emax_num_dc1_layers 
               m= m+1
               e_track_ntuple_contents(m)= 
     &          edc1_track_coord_pre(i,j,enum_fitting) ! 11
            Enddo
            
            Do j=1,emax_num_dc1_layers 
               m= m+1
               e_track_ntuple_contents(m)= 
     &          edc1_drift_dis_pre(i,j,enum_fitting) ! 21
            Enddo
            
            Do j=1,emax_num_dc1_layers 
               m= m+1
               e_track_ntuple_contents(m)= 
     &          edc1_drift_time_pre(i,j,enum_fitting) ! 31
            Enddo
            
            Do j=1,emax_num_dc1_layers 
               m= m+1
               e_track_ntuple_contents(m)= 
     &          edc1_wire_center_pre(i,j,enum_fitting) ! 31
            Enddo
            
            Do j=1,emax_num_dc1_layers 
               m= m+1
               e_track_ntuple_contents(m)= edc1_z(i,j) ! 31
            Enddo
               
            m= m+1
            e_track_ntuple_contents(m)= edc1chi2perdof_pre(i,enum_fitting) ! 41
            
            m= m+1
            e_track_ntuple_contents(m)= ex_fp_pre1(i,enum_fitting) ! 42
            m= m+1
            e_track_ntuple_contents(m)= ey_fp_pre1(i,enum_fitting) ! 43
            m= m+1
            e_track_ntuple_contents(m)= exp_fp_pre1(i,enum_fitting) ! 44
            m= m+1
            e_track_ntuple_contents(m)= eyp_fp_pre1(i,enum_fitting) ! 45
            
  
*     Fill ntuple for this event
            ABORT= .NOT.HEXIST(e_track_ntuple_ID)
            IF(ABORT) THEN
               call G_build_note(':Ntuple ID#$ does not exist',
     &              '$',e_track_ntuple_ID,' ',0.,' ',err)
               call G_add_path(here,err)
            ELSE
               call HFN(e_track_ntuple_ID,e_track_ntuple_contents)
            ENDIF
*     
         EndIf
      EndDo
      
      RETURN
      END      
