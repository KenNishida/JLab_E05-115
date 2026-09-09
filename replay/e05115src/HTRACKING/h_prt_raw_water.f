      SUBROUTINE h_prt_raw_water(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump SOS_RAW_WAT BANKS
*-
*-      Required Input BANKS     SOS_RAW_WAT
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* $Log: h_prt_raw_water.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
* new dir
*
* Revision 1.2  2000/02/18 16:23:02  jinghua
* (JLiu) clean up
*
* Revision 1.1  2000/01/07 01:31:34  ysato
* Lucite raw data dump/histgram added
*
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*18 here
      parameter (here= 'h_prt_raw_water')
*
      logical ABORT
      character*(*) err
*
      integer*4 j
      include 'hks_data_structures.cmn'
*     
*--------------------------------------------------------
      ABORT = .FALSE.
      err = ' '
      write(*,*) here
      write(hluno,*) '        HKS_RAW_WAT BANKS'
      write(hluno,'(23H     HWAT_RAW_TOT_HITS=,I4)') HWAT_RAW_TOT_HITS
      
      if(HWAT_RAW_TOT_HITS.le.0) Return
      
      write(hluno,*) ' Num  Plane    Counter      ADC_POS  ',
     &     ' ADC_NEG  TDC_POS  TDC_NEG'
      Do j=1,HWAT_RAW_TOT_HITS
         write(hluno,'(1x,i2,2x,i3,7x,i4,8x,2i8,2i8)')
     &        j,hwat_raw_layer_num(j)
     &        ,hwat_raw_counter_num(j)
     &        ,HWAT_RAWADC_POS(j)
     $        ,HWAT_RAWADC_NEG(j)
     $        ,HWAT_RAWTDC_POS(j)
     $        ,HWAT_RAWTDC_NEG(j)
      EndDo
      
      RETURN
      END
