      SUBROUTINE h_clear_event(ABORT,err)
*--------------------------------------------------------
*-       Prototype C analysis routine
*-
*-
*-   Purpose and Methods : clears all SOS quantities before event is processed.
*-
*- 
*-   Output: ABORT	- success or failure
*-         : err	- reason for failure, if any
*- 
*-   Created  2-Nov-1993   Kevin B. Beard
*-   Modified 20-Nov-1993  KBB for new errors
*
* Revision 1.13 2004/03/02 Miyoshi
* for E01-011
* Revision 1.12  1999/02/23 18:27:50  csa
* Add call to s_ntuple_clear
*
* Revision 1.11  1996/11/05 21:42:56  saw
* (WH) Add lucite counter
*
* Revision 1.10  1995/10/09 18:08:15  cdaq
* (JRA) Add clear of SCER_RAW_ADC
*
* Revision 1.9  1995/09/01 13:40:55  cdaq
* (JRA) Clear some cerenkov variables
*
* Revision 1.8  1995/05/22  20:50:48  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.7  1995/05/11  15:08:57  cdaq
* (SAW) Add clear of Aerogel hit counter
*
* Revision 1.6  1995/04/01  20:10:55  cdaq
* (SAW) Add missing SSCIN_ALL_TOT_HITS = 0
*
* Revision 1.5  1994/11/22  20:14:23  cdaq
* (SPB) Bring up to date with h_clear_event
*
* Revision 1.4  1994/06/22  20:53:59  cdaq
* (SAW) zero the miscleaneous hits counter
*
* Revision 1.3  1994/03/01  20:14:32  cdaq
* (SAW) Add zeroing of the raw total hits counter for the drift chambers
*
* Revision 1.2  1994/02/22  19:04:02  cdaq
* (SAW) SNUM_DC_LAYERS  --> SMAX_NUM_DC_LAYERS
*
* Revision 1.1  1994/02/04  22:21:07  cdaq
* Initial revision
*
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*13 here
      parameter (here= 'h_clear_event')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'hks_tracking.cmn'
      INCLUDE 'hks_statistics.cmn'
      INCLUDE 'hks_scin_tof.cmn'
      INCLUDE 'hks_scin_parms.cmn'
*     
      INTEGER i, j, k, layer
*     
*     
      HDC_RAW_TOT_HITS = 0
*
      HDC_TOT_HITS = 0
*     
      DO layer= 1,HMAX_NUM_DC_LAYERS
         HDC_HITS_PER_LAYER(layer)= 0
      ENDDO
*     
      HSCIN_RAW_TOT_HITS = 0
      HSCIN_TOT_HITS = 0
*     
      DO layer= 1,HNUM_SCIN_LAYERS
         HSCIN_HITS_PER_LAYER(layer)= 0
      ENDDO
*     
*     HKS AEROGEL HITS
*     
      HAER_TOT_HITS = 0
      HAER_RAW_TOT_HITS = 0
*     
*     HKS WATER HITS
*     
      HWAT_TOT_HITS = 0
      HWAT_RAW_TOT_HITS = 0
*     
*     HKS LUCITE HITS
*     
      HLUC_TOT_HITS = 0
      HLUC_RAW_TOT_HITS = 0
      HLUCSUM_RAW_TOT_HITS = 0
*     
*     HKS Miscleaneous hits
*     
      HMISC_TOT_HITS = 0
*     
*     HKS DETECTOR TRACK QUANTITIES
*     
      HNTRACKS_FP= 0
*     
*     HKS TARGET QUANTITIES
*     
      HNTRACKS_TAR= 0
*     
      hnphysics = 0
*
      htul_raw_tot_hits = 0
      htul_tot_hits = 0

*--------------------------------------------------------
      ABORT= .FALSE.
      err= ' '
      RETURN
      END
