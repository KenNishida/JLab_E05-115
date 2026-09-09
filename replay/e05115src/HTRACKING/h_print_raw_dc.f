       SUBROUTINE  h_print_raw_dc(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump SOS_RAW_DC BANKS
*-
*-      Required Input BANKS     SOS_RAW_DC
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 29-FEB-1994   D. F. Geesaman
* $Log: h_print_raw_dc.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.2  2004/12/24 21:33:07  miyoshi
* change name plane to layer
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.2  1995/05/22 19:45:46  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/03/24  20:30:01  cdaq
* Initial revision
*
*--------------------------------------------------------
       IMPLICIT NONE
       SAVE
*
       character*50 here
       parameter (here= 'h_print_raw_dc')
*
       logical ABORT
       character*(*) err
*
       integer*4 j
       include 'hks_data_structures.cmn'
       include 'gen_constants.par'
       include 'gen_units.par'
       include 'hks_tracking.cmn'
       include 'hks_geometry.cmn'          
*
*--------------------------------------------------------
       ABORT = .FALSE.
       err = ' '
       write(hluno,'(''        HKS_RAW_DC BANKS'')')
       write(hluno,'(''    HDC _RAW_TOT_HITS='',I4)') HDC_RAW_TOT_HITS
       if(HDC_RAW_TOT_HITS.GT.0) then
         write(hluno,'('' Num  Layer     Wire          TDC Value'')')
         write(hluno,'(1x,i2,2x,i3,7x,i4,5x,i10)')
     &     (j,HDC_RAW_LAYER_NUM(j),HDC_RAW_WIRE_NUM(j),
     &        HDC_RAW_TDC(j),j=1,HDC_RAW_TOT_HITS)    
       endif
       RETURN
       END
