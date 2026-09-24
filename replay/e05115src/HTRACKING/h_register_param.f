      SUBROUTINE h_register_param(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Initializes SOS quantities 
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created  8-Nov-1993   Kevin B. Beard
*-   Modified 20-Nov-1993  KBB for new errors
*-            14 Feb-1994  DFG Put in real variables
*-
*- All standards are from "Proposal for Hall C Analysis Software
*- Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
* $Log: h_register_param.f,v $
* Revision 1.1.1.1  2009/07/12 20:10:00 yez
* Add Lucite info    
*

* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.3  2005/04/08 20:41:32  miyoshi
* add r file
*
* Revision 1.2  2005/03/02 22:30:25  miyoshi
* remove unused file
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.11  1996/11/07 19:53:37  saw
* (WH) Add lucite parameters
*
* Revision 1.10  1996/04/30 17:15:12  saw
* (JRA) Register Aerogel variables
*
* Revision 1.9  1995/08/31 20:43:41  cdaq
* (JRA) Register Cerenkov variables
*
* Revision 1.8  1995/05/17  16:43:28  cdaq
* (JRA) Register pedestal variables
*
* Revision 1.7  1994/08/18  03:59:50  cdaq
* (SAW) Call makereg generated routines to register variables
*
* Revision 1.6  1994/06/07  03:01:22  cdaq
* (DFG) add call to register bypass switches and statistics
*
* Revision 1.5  1994/03/24  19:54:54  cdaq
* (DFG) Put actual registering of variables in subroutines
*
* Revision 1.4  1994/02/23  15:39:50  cdaq
* (SAW) ABORT now when ierr.NE.0
*
* Revision 1.3  1994/02/22  20:39:28  cdaq
* (SAW) Fix booboo
*
* Revision 1.2  1994/02/22  18:52:19  cdaq
* (SAW) Move regpar declarations to gen_routines.dec.  Make title arg null.
*
* Revision 1.1  1994/02/22  18:42:21  cdaq
* Initial revision
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*16 here
      parameter (here= 'h_register_param')
*
      logical ABORT
      character*(*) err
*
*--------------------------------------------------------
      err= ' '
      ABORT = .false.
*
*     register tracking variables
*

      call r_hks_tracking
      call r_hks_geometry
      call r_hks_recon_elements
      call r_hks_physics_sing
*
*     register cal, tof and cer variables
*

      call r_hks_scin_parms
      call r_hks_scin_tof
      call r_hks_aero_parms
      call r_hks_water_parms
      call r_hks_lucite_parms
      call r_hks_id_histid
      call r_hks_phys_histid
*
*     register bypass switches
*

      call r_hks_bypass_switches

*
*     register hks statistics
*

      call r_hks_statistics
      call r_hks_pedestals
*
      return
      end
