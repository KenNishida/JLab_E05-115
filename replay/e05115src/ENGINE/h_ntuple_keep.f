      subroutine h_Ntuple_keep(ABORT,err)
*-----------------------------------------------------------------
*     
*     Purpose : Add entry to the SOS Ntuple
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
*     
*     Revision 2.2 2009/08/11 Z.Ye
*     for E05-115: Add Lucite info
*   
*     Revision 2.1 2004/03/03 Miyoshi
*     for E01-011
*     Revision 2.0  2000/03/03 11:50:51  jinghua
*     (JLiu) Added Lucite info
*     
*     Revision 1.7  1996/09/04 15:18:21  saw
*     (JRA) Modify ntuple contents
*     
*     Revision 1.6  1996/01/16 16:40:31  cdaq
*     (JRA) Modify ntuple contents
*     
*     Revision 1.5  1995/09/01 13:38:46  cdaq
*     (JRA) Add Cerenkov photoelectron count to ntuple
*     
*     Revision 1.4  1995/05/22  20:50:48  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, 
*     sos, and coin parts"
*     
*     Revision 1.3  1995/05/11  19:00:39  cdaq
*     (SAW) Change SSDEDXn vars to an array.
*     
*     Revision 1.2  1994/06/17  02:42:33  cdaq
*     (KBB) Upgrade
*     
*     Revision 1.1  1994/04/12  16:16:28  cdaq
*     Initial revision
*     
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='h_Ntuple_keep')
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
      integer m,i,j,k,hitn
      
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(.NOT.h_Ntuple_exists) RETURN !nothing to do
*     
      Do i=1,hnphysics
         m= 0
         m= m+1
         h_Ntuple_contents(m)= HSP(i) ! 1 
         m= m+1
         h_Ntuple_contents(m)= HSDELTA(i) ! 2 
c         m= m+1
c         h_Ntuple_contents(m)= HSTHETA(i) ! 3
c         m= m+1
c         h_Ntuple_contents(m)= HSPHI(i) ! 4
         m= m+1
         h_Ntuple_contents(m)= hphys_scin_hit(i,1) ! 5
         m= m+1
         h_Ntuple_contents(m)= hsna1(i) ! 6
         m= m+1
         h_Ntuple_contents(m)= hsna2(i) ! 7
         m= m+1
         h_Ntuple_contents(m)= hsna3(i) ! 8
         m= m+1
         h_Ntuple_contents(m)= hsnt1(i) ! 6
         m= m+1
         h_Ntuple_contents(m)= hsnt2(i) ! 7
         m= m+1
         h_Ntuple_contents(m)= hsnt3(i) ! 8
         m= m+1
         h_Ntuple_contents(m)= hsnco1(i) ! 6
         m= m+1
         h_Ntuple_contents(m)= hsnco2(i) ! 7
         m= m+1
         h_Ntuple_contents(m)= hsnco3(i) ! 8
         m= m+1
         h_Ntuple_contents(m)= HSBETA(i) ! 9
         m= m+1
         h_Ntuple_contents(m)= hstof(i) ! 9
         m= m+1
         h_Ntuple_contents(m)= HSX_FP(i) ! 10 
         m= m+1
         h_Ntuple_contents(m)= HSY_FP(i) ! 11
         m= m+1
         h_Ntuple_contents(m)= HSXP_FP(i) ! 12
         m= m+1
         h_Ntuple_contents(m)= HSYP_FP(i) ! 13
         m= m+1
         h_Ntuple_contents(m)= hstime_at_fp(i) ! 14
         m= m+1
         h_Ntuple_contents(m)= hspathlength(i) ! 15
         m= m+1
         h_Ntuple_contents(m)= HSY_TAR(i) ! 16
         m= m+1
         h_Ntuple_contents(m)= HSXP_TAR(i) ! 17
         m= m+1
         h_Ntuple_contents(m)= HSYP_TAR(i) ! 18

         m= m+1
         h_Ntuple_contents(m)= HX_SV(i) ! 19
         m= m+1
         h_Ntuple_contents(m)= HY_SV(i) ! 20

         m= m+1
         h_Ntuple_contents(m)= float(gen_event_ID_number) ! 21
         m= m+1
         h_Ntuple_contents(m)= float(gen_run_number) ! 22
         
c     --- number of hit aerogel layer
         hitn=0
         Do j=1,3
            if(htrk_naer(i,j) .gt.0) then
               hitn=hitn+1
            EndIf
         EndDo
         m= m+1
         h_Ntuple_contents(m)= float(hitn) ! 23

         m= m+1
         h_Ntuple_contents(m)= hsaer_num(i,1) ! 24
         m= m+1
         h_Ntuple_contents(m)= hsaer_num(i,2) ! 25
         m= m+1
         h_Ntuple_contents(m)= hsaer_num(i,3) ! 26

         m= m+1
         h_Ntuple_contents(m)= hsaer_npe(i,1) ! 27
         m= m+1
         h_Ntuple_contents(m)= hsaer_npe(i,2) ! 28
         m= m+1
         h_Ntuple_contents(m)= hsaer_npe(i,3) ! 29
         
         m= m+1
         h_Ntuple_contents(m)= hsaer_time(i,1) ! 27
         m= m+1
         h_Ntuple_contents(m)= hsaer_time(i,2) ! 28
         m= m+1
         h_Ntuple_contents(m)= hsaer_time(i,3) ! 29
         
c     --- number of hit water layer
         hitn=0
         Do j=1,2
            if(htrk_nwat(i,j) .gt.0) then
               hitn=hitn+1
            EndIf
         EndDo
         m= m+1
         h_Ntuple_contents(m)= float(hitn) ! 30

         m= m+1
         h_Ntuple_contents(m)= hswat_num(i,1) ! 31
         m= m+1
         h_Ntuple_contents(m)= hswat_num(i,2) ! 32

         m= m+1
         h_Ntuple_contents(m)= hswat_npe(i,1) ! 33
         m= m+1
         h_Ntuple_contents(m)= hswat_npe(i,2) ! 34
         
         m= m+1
         h_Ntuple_contents(m)= hswat_time(i,1) ! 33
         m= m+1
         h_Ntuple_contents(m)= hswat_time(i,2) ! 34
 
c     --- number of hit lucite layer
         hitn=0
         Do j=1,2
            if(htrk_nluc(i,j) .gt.0) then
               hitn=hitn+1
            EndIf
         EndDo
         m= m+1
         h_Ntuple_contents(m)= float(hitn) ! 44
         m= m+1
         h_Ntuple_contents(m)= hsluc_num(i,1) ! 45
         m= m+1
         h_Ntuple_contents(m)= hsluc_npe(i,1) ! 47
         
         m= m+1
         h_Ntuple_contents(m)= hschi2perdeg(i) ! 35
         m= m+1
         h_Ntuple_contents(m)= hsnfree_fp(i) ! 20
         m= m+1
         h_Ntuple_contents(m)= hsbeta_k(i) ! 36
         m= m+1
         h_Ntuple_contents(m)= hsbeta_pi(i) ! 37
         m= m+1
         h_Ntuple_contents(m)= hsbeta_pr(i) ! 38
         m = m+1
         h_Ntuple_contents(m)= hmisc_dec_data(65,1)*0.025 ! 39
         m = m+1
         h_Ntuple_contents(m)= hstime_at_tar(i) ! 40
         m = m+1 
         h_Ntuple_contents(m)= hnphysics ! 41
         m = m+1 
         h_Ntuple_contents(m)= hsrftime ! 42
         m = m+1 
         h_Ntuple_contents(m)= hsrfdiff ! 43

         
*     Experiment dependent entries start here.
         
         
*     Fill ntuple for this event
         ABORT= .NOT.HEXIST(h_Ntuple_ID)
         IF(ABORT) THEN
            call G_build_note(':Ntuple ID#$ does not exist',
     &           '$',h_Ntuple_ID,' ',0.,' ',err)
            call G_add_path(here,err)
         ELSE
            call HFN(h_Ntuple_ID,h_Ntuple_contents)
         ENDIF
      
      EndDo                     ! hnphysics loop

*
      RETURN
      END      
