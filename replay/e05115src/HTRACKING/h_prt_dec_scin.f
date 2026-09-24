      SUBROUTINE  h_prt_dec_scin(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods : Dump HKS_DECODED_SCIN BANKS
*     -
*     -      Required Input BANKS     HKS_DECODED_SCIN
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     -   Created 29-FEB-1994   D. F. Geesaman
*     $Log: h_prt_dec_scin.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.3  2005/04/08 20:40:56  miyoshi
*     change names
*
*     Revision 1.2  2004/12/24 21:37:10  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.7  1996/01/17 19:00:09  cdaq
*     (JRA)
*     
*     Revision 1.6  1995/05/22 19:45:50  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.5  1995/04/06  19:40:51  cdaq
*     (SAW) Fix typo
*     
*     Revision 1.4  1995/02/10  19:57:47  cdaq
*     (JRA) Make sscin_all_adc_pos/neg floating
*     
*     Revision 1.4  1995/02/10  19:13:11  cdaq
*     (JRA) Make sscin_all_adc_pos/neg floating
*     
*     Revision 1.3  1994/11/23  13:56:18  cdaq
*     (SPB) Recopied from hms file and modified names for SOS
*     
*     Revision 1.2  1994/05/13  03:22:48  cdaq
*     (DFG) Fix logical format statement
*     
*     Revision 1.1  1994/04/13  18:21:29  cdaq
*     Initial revision
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*50 here
      parameter (here= 'h_prt_dec_scin')
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
      
      write(hluno,*) '        ***HKS_REAL_SCIN BANKS***'
      write(hluno,'(20H     HSCIN_TOT_HITS=,I4)') HSCIN_TOT_HITS
      if(HSCIN_TOT_HITS.GT.0) then
         write(hluno,*) ' Num  Layer    Counter        ADC_POS',
     &        'ADC_NEG  TDC_POS  TDC_NEG'
         Do j=1,HSCIN_TOT_HITS
            write(hluno,'(1x,i2,2x,i3,5x,i4,8x,2f8.2,2i8)')
     &           j,HSCIN_LAYER_NUM(j),HSCIN_COUNTER_NUM(j),
     &           HSCIN_ADC_POS(j),HSCIN_ADC_NEG(j),
     &           HSCIN_TDC_POS(j),HSCIN_TDC_NEG(j)
         EndDo
      endif
      
      write(hluno,*) '        SOS_DECODED_SCIN BANKS'
      if(HSCIN_TOT_HITS.GT.0) then
         write(hluno,*) ' Scintillator hits per layer'
         write(hluno,'(8H Layer  ,3i4)') (j,j=1,HNUM_SCIN_LAYERS)   
         write(hluno,'(8H Number ,3i4)') 
     &        (HSCIN_HITS_PER_LAYER(j),j=1,HNUM_SCIN_LAYERS)
         write(hluno,*) ' Num  HIT_COORD  SLOP',
     &        '   COR_TDC  TWO_GOOD'
         Do j=1,HSCIN_TOT_HITS
            write(hluno,'(1x,i2,2x,2f9.3,f10.3,4x,l2)')
     &           j,HSCIN_HIT_COORD(j),
     &           HSCIN_SLOPE(j),HSCIN_COR_TIME(j),
     &           HTWO_GOOD_TIMES(j)
         EndDo
         write(hluno,'(18H HGOOD_START_TIME=,l2)')
     &        HGOOD_START_TIME
         write(hluno,'(13H HSTART_TIME=,e10.4)') HSTART_TIME 
         write(hluno,*)
      endif

      RETURN
      END
