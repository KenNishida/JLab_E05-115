      SUBROUTINE H_SELECT_BEST_TRACK(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Select the best track through the HMS
*-                              
*-
*-      Required Input BANKS
*-
*-      Output BANKS
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*- $Log: h_select_best_track.f,v $
*- Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*-
*- e05115 src repository for software development
*-
*- Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*-
*-
*- Revision 1.2  2005/01/11 00:44:10  sumihama
*- Mod HKS-TOF
*-
*- Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*- new dir
*-
*- Revision 1.4  1995/07/20 19:01:37  cdaq
*- (CC) Fix bug in best chisq finding
*-
c Revision 1.3  1995/05/22  19:45:55  cdaq
c (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
c
c Revision 1.2  1995/04/06  19:44:04  cdaq
c (JRA) Fix some latent HMS variable names
c
c Revision 1.1  1995/02/23  13:29:49  cdaq
c Initial revision
c
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'H_SELECT_BEST_TRACK')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hks_physics_sing.cmn'
      INCLUDE 'hks_scin_parms.cmn'
      INCLUDE 'hks_scin_tof.cmn'
*
*     local variables 
      integer*4 goodtrack,track
      real*4 chi2perdeg,chi2min
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*     Need to test to chose the best track
      HHNUM_FPTRACK = 0
      HHNUM_TARTRACK = 0
      if( HNTRACKS_FP.GT. 0) then
        chi2min= 1e10
        goodtrack = 0
        do track = 1, HNTRACKS_FP

          if( HNFREE_FP(track).ge. hsel_ndegreesmin) then
            chi2perdeg = HCHI2_FP(track)/FLOAT(HNFREE_FP(track))
            if(chi2perdeg .lt. chi2min) then
*     simple particle id tests
              if( ( hscin_trk_depo(track,1) .gt. hsel_dedx1min)  .and.
     &             ( hscin_trk_depo(track,1) .lt. hsel_dedx1max)  .and.
     &             ( htrk_beta(track)   .gt. hsel_betamin)   .and.
     &             ( htrk_beta(track)   .lt. hsel_betamax)   
     &          ) then
                goodtrack = track
                chi2min = chi2perdeg
              endif                     ! end test on track id
            endif                       ! end test on lower chisq
          endif                         ! end test on minimum number of degrees of freedom
        enddo                           ! end loop on track
        HHNUM_TARTRACK = goodtrack
        HHNUM_FPTRACK  = goodtrack
        if(goodtrack.eq.0) return       ! return if no valid tracks
      endif

      return
      end
