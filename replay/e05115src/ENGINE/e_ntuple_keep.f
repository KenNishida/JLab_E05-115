      subroutine e_Ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the SOS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log: e_ntuple_keep.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:46  kawama
*
* e05115 src repository for software development
*
* Revision 1.4  2005/09/20 18:00:17  sumihama
* Add ndf and remove theta/phi from ntuple
*
* Revision 1.3  2005/07/06 19:55:48  cdaq
* Mod. ntuple for rf
*
* Revision 1.2  2005/07/03 11:54:46  sumihama
* Add Ehodo info. in hes-ntuple
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:38  miyoshi
* new dir
*
*
* Revision 1.4 2004/03/02 Miyoshi
* for E01-011
*
* Revision 1.3  2000/02/11 20:44:06  ysato
* Local update on KTRACKING
*
* Revision 1.2  1999/12/23 19:59:18  ysato
* Compiled on Redhat Linux
*
* Revision 1.1.1.1  1999/11/01 13:54:26  ysato
* Upgrade for HNSS
*
* Revision 1.7  1996/09/04 15:18:21  saw
* (JRA) Modify ntuple contents
*
* Revision 1.6  1996/01/16 16:40:31  cdaq
* (JRA) Modify ntuple contents
*
* Revision 1.5  1995/09/01 13:38:46  cdaq
* (JRA) Add Cerenkov photoelectron count to ntuple
*
* Revision 1.4  1995/05/22  20:50:48  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/11  19:00:39  cdaq
* (SAW) Change SSDEDXn vars to an array.
*
* Revision 1.2  1994/06/17  02:42:33  cdaq
* (KBB) Upgrade
*
* Revision 1.1  1994/04/12  16:16:28  cdaq
* Initial revision
*
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='e_Ntuple_keep')
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
      IF(.NOT.e_Ntuple_exists) RETURN !nothing to do
*     
      Do i=1,enphysics
*     --- put cut condition here 
         if(
     &        esp(i) .gt. 0
     &        ) then
            
            m= 0
            m= m+1
            e_Ntuple_contents(m)= esp(i) ! 1
            m= m+1
            e_Ntuple_contents(m)= esdelta(i) ! 2
c            m= m+1
c            e_Ntuple_contents(m)= estheta(i) ! 3
c            m= m+1
c            e_Ntuple_contents(m)= esphi(i) ! 4
            m= m+1
            e_Ntuple_contents(m)= ephys_scin_hits(i,1) ! 5
            m= m+1
            e_Ntuple_contents(m)= esnco1(i) ! 6
            m= m+1
            e_Ntuple_contents(m)= esnco2(i) ! 6
            m= m+1
            e_Ntuple_contents(m)= esnco3(i) ! 6
            m= m+1
            e_Ntuple_contents(m)= esnt1(i) ! 6
            m= m+1
            e_Ntuple_contents(m)= esnt2(i) ! 6
            m= m+1
            e_Ntuple_contents(m)= esnt3(i) ! 6
            m= m+1
            e_Ntuple_contents(m)= esnt1m(i) ! 6
            m= m+1
            e_Ntuple_contents(m)= esnt2m(i) ! 6
            m= m+1
            e_Ntuple_contents(m)= esscin_depo(i) ! 6
            m= m+1
            e_Ntuple_contents(m)= esx_fp(i) ! 7
            m= m+1 
            e_Ntuple_contents(m)= esy_fp(i) ! 8
            m= m+1
            e_Ntuple_contents(m)= esxp_fp(i) ! 9
            m= m+1
            e_Ntuple_contents(m)= esyp_fp(i) ! 10
            m= m+1
            e_Ntuple_contents(m)= estime_at_fp(i) ! 11
            m= m+1
            e_Ntuple_contents(m)= espathlength(i) ! 12
            m= m+1
            e_Ntuple_contents(m)= esy_tar(i) ! 13
            m= m+1
            e_Ntuple_contents(m)= esxp_tar(i) ! 14
            m= m+1
            e_Ntuple_contents(m)= esyp_tar(i) ! 15
            m= m+1
            e_Ntuple_contents(m)= esx_sv(i) ! 16
            m= m+1
            e_Ntuple_contents(m)= esy_sv(i) ! 17
            m= m+1
            e_Ntuple_contents(m)= float(gen_event_ID_number) ! 18
            m= m+1
            e_Ntuple_contents(m)= float(gen_run_number) ! 19
            m= m+1
            e_Ntuple_contents(m)= eschi2perdeg(i) ! 20
            m= m+1
            e_Ntuple_contents(m)= esnfree_fp(i) ! 20
            m = m+1
            e_Ntuple_contents(m)= hmisc_dec_data(65,1)*0.025 ! 21
            m = m+1
            e_Ntuple_contents(m)= estime_at_tar(i) ! 22 
            m = m+1 
            e_Ntuple_contents(m)= enphysics ! 23
            m = m+1 
            e_Ntuple_contents(m)= esrftime ! 24
            m = m+1 
            e_Ntuple_contents(m)= esrfdiff ! 25
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,1) ! 26
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,2) ! 27
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,3) ! 28
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,4) ! 29
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,5) ! 30
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,6) ! 31
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,7) ! 32
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,8) ! 33
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,9) ! 34
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,10) ! 35
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,11) ! 36
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,12) ! 37
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,13) ! 38
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,14) ! 39
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,15) ! 40
            m = m+1 
            e_Ntuple_contents(m)= esresidual(i,16) ! 41

            
*     Fill ntuple for this event
            ABORT= .NOT.HEXIST(e_Ntuple_ID)
            IF(ABORT) THEN
               call G_build_note(':Ntuple ID#$ does not exist',
     &              '$',e_Ntuple_ID,' ',0.,' ',err)
               call G_add_path(here,err)
            ELSE
               call HFN(e_Ntuple_ID,e_Ntuple_contents)
            ENDIF
*     
         EndIf
      EndDo
      
      RETURN
      END      
