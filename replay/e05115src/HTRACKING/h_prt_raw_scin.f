      SUBROUTINE h_prt_raw_scin(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump SOS_RAW_SCIN BANKS
*-
*-      Required Input BANKS     SOS_RAW_SCIN
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 29-FEB-1994   D. F. Geesaman
* $Log: h_prt_raw_scin.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.3  2005/03/14 19:50:49  miyoshi
* change variables names
*
* Revision 1.2  2004/12/24 21:37:10  miyoshi
* change name plane to layer
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.5  1995/07/20 18:59:35  cdaq
* (SAW) Fix format
*
* Revision 1.4  1995/05/22  19:45:50  cdaq
* (SAW) Split gen_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/04/06  19:42:03  cdaq
* (JRA) SSCIN_TOT_HITS -> SSCIN_RAW_TOT_HITS
*
* Revision 1.2  1994/11/23  13:56:57  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/04/13  18:21:45  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'h_prt_raw_scin')
*
      logical ABORT
      character*(*) err
*
      integer*4 j
      include 'hks_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 'hks_tracking.cmn'
      include 'hks_scin_parms.cmn'
*
*--------------------------------------------------------
      ABORT = .FALSE.
      err = ' '
      write(hluno,'(''        SOS_RAW_SCIN BANKS'')')
      write(hluno,'(''     HSCIN_RAW_TOT_HITS='',I4)') HSCIN_RAW_TOT_HITS
      if(HSCIN_RAW_TOT_HITS.GT.0) then
        write(hluno,'('' Num  Layer    Counter      ADC_POS  ''
     &       '' ADC_NEG  TDC_POS  TDC_NEG'')')
        write(hluno,'(1x,i2,2x,i3,7x,i4,8x,2f8.1,2i8)')
     &       (j,HSCIN_RAW_LAYER_NUM(j),HSCIN_RAW_COUNTER_NUM(j),
     &       (HSCIN_RAW_ADC_POS(j)
     $       -HSCIN_RAW_PED_POS(hscin_raw_layer_num(j)
     $       ,hscin_raw_counter_num(j)))
     $       ,(HSCIN_RAW_ADC_NEG(j)
     $       -HSCIN_RAW_PED_NEG(hscin_raw_layer_num(j)
     $       ,hscin_raw_counter_num(j)))
     $       ,HSCIN_RAW_TDC_POS(j)
     $       ,HSCIN_RAW_TDC_NEG(j),j=1,HSCIN_RAW_TOT_HITS )
      endif
      RETURN
      END
