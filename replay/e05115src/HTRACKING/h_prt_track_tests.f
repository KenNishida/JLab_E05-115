        subroutine h_prt_track_tests

*-------------------------------------------------------------------
* author: John Arrington
* created: 3/28/94
*
* s_prt_track_tests dumps the sos_track_tests bank.
*
* modifications:
* $Log: h_prt_track_tests.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.3  2005/04/19 17:51:45  miyoshi
* change variable name
*
* Revision 1.2  2005/01/11 00:44:10  sumihama
* Mod HKS-TOF
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.2  1995/05/22 19:45:52  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/04/13  18:22:19  cdaq
* Initial revision
*
*-------------------------------------------------------------------

        implicit none

        include 'hks_data_structures.cmn'
        include 'hks_scin_parms.cmn'
        include 'hks_scin_tof.cmn'
        include 'hks_tracking.cmn'

        logical abort
        integer*4 ihit, itrk
        character*1024 errmsg
        character*25 here
        parameter (here = 'h_prt_track_tests')

        save

       if(hntracks_fp.gt.0) then
        write(hluno,'(''        HKS_TRACK_TESTS BANK'')')
        write(hluno,'(''SCIN/CERENKOV TESTS'')')
        write(hluno,'(''  trk   beta  chisq_beta  fp_time  '',
     &        ''num_scin_hit'')')
        do itrk=1, hntracks_fp
          write(hluno,'(i4,f8.4,f10.4,f9.3,i12)') itrk,
     &          htrk_beta(itrk), htrk_tof(itrk), htrk_time_atfp(itrk),
     &          hscin_on_track(itrk,1)
        enddo

        do itrk=1, hntracks_fp
          write(hluno,'(''hits on track number'',i3,'', and dE/dx:'')') itrk
          write(hluno,'(16i6)') 
     &       (hscin_on_track(itrk,ihit),ihit=1,3)
          write(hluno,'(16f6.1)') 
     &       (hscin_trk_depo(itrk,ihit),ihit=1,3)
        enddo
       endif         ! end check on zero focal plane tracks
        return
        end
