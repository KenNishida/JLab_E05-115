      SUBROUTINE h_prt_raw_lucite(ABORT,err)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Dump SOS_RAW_WAT BANKS
*-
*-      Required Input BANKS     SOS_RAW_WAT
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* $Log: h_prt_raw_lucite.f,v $
* Revision 1.1.1.1  2009/06/23 17:43:12  yez
* Convert from water into Lucite for E05-115
*
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
      parameter (here= 'h_prt_raw_lucite')
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
      write(hluno,*) '        HKS_RAW_LUC BANKS'
      write(hluno,'(23H     HLUC_RAW_TOT_HITS=,I4)') HLUC_RAW_TOT_HITS  ! 23H need to be changed into 12???
      
      if(HLUC_RAW_TOT_HITS.le.0) Return
      
      write(hluno,*) ' Num  Plane    Counter      ADC_POS  ',
     &     ' ADC_NEG  TDC_POS  TDC_NEG'
      Do j=1,HLUC_RAW_TOT_HITS
         write(hluno,'(1x,i2,2x,i3,7x,i4,8x,2i8,2i8)')
     &        j,hluc_raw_layer_num(j)
     &        ,hluc_raw_counter_num(j)
     &        ,HLUC_RAWADC_POS(j)
     $        ,HLUC_RAWADC_NEG(j)
     $        ,HLUC_RAWTDC_POS(j)
     $        ,HLUC_RAWTDC_NEG(j)
      EndDo
      
      RETURN
      END
