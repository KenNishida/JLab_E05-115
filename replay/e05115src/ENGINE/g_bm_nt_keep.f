      subroutine g_bm_Nt_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the SOS Sieve slit Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 28-Feb-2000 
*
* Revision 1.0  2000/02/28 12:20:04  jinghua
* Initial revision
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='g_bm_nt_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'g_beam_ntuple.cmn'
      INCLUDE 'gen_epics.cmn'
      INCLUDE 'gen_scalers.cmn'
      INCLUDE 'gen_event_info.cmn'
*
      logical HEXIST                    !CERNLIB function
*
      integer m
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.g_beam_Ntuple_exists) RETURN !nothing to do
*
************************************************
      m= 0
*  
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_pbeam
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_xc07
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_yc07
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_xc08
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_yc08
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_xc11
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_yc11
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_xc12
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_yc12
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_xc14
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_yc14
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_xc16
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_yc16
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_xc17
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_yc17
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_xc18
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_yc18
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_xc20
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_yc20
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_mag_temp1
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_mag_temp2
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_mag_temp3
      m=m+1
      g_beam_Ntuple_contents(m)= gepics_mag_temp4
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_mag_temp5
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_nmr_lock
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_nmr_field
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_otr_x
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_otr_widx
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_otr_y
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_otr_widy
      m=m+1
      g_beam_Ntuple_contents(m)= gepics_dpp
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_bcm1
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_bcm2
      m= m+1
      g_beam_Ntuple_contents(m)= FLOAT(gen_event_ID_number)
      m= m+1
      g_beam_Ntuple_contents(m)= g_run_time

*
************************************************
*
*
      ABORT= .NOT.HEXIST(g_beam_Ntuple_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &       '$',g_beam_Ntuple_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
        call HFN(g_beam_Ntuple_ID,g_beam_Ntuple_contents)
      ENDIF
*
      RETURN
      END
