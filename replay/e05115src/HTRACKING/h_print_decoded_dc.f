      SUBROUTINE  h_print_decoded_dc(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods : Dump HKS_DECODED_DC BANKS
*     -
*     -      Required Input BANKS     HKS_DECODED_DC
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     -   Created 29-FEB-1994   D. F. Geesaman
*     $Log: h_print_decoded_dc.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.2  2004/12/24 21:35:57  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.4  1995/10/10 16:52:50  cdaq
*     (JRA) Remove drift distance from print out
*     
*     Revision 1.3  1995/05/22 19:45:44  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, hks, and coin parts"
*     
*     Revision 1.2  1995/04/06  19:38:33  cdaq
*     (JRA) Remove SDC_WIRE_COORD
*     
*     Revision 1.1  1994/03/24  20:29:16  cdaq
*     Initial revision
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*50 here
      parameter (here= 'h_print_decoded_dc')
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
      write(hluno,*) '        HKS_DECODED_DC BANKS'
      write(hluno,'(18H     HDC_TOT_HITS=,I4)') HDC_TOT_HITS
      
      if(HDC_TOT_HITS.GT.0) then
         write(hluno,*) '     HDC_HITS_PER_LAYER'
         write(hluno,'(7H Layer=,12i4)') (j,j=1,hdc_num_layers)
         write(hluno,'(7x,12i4)')
     &        (HDC_HITS_PER_LAYER(j),j=1,hdc_num_layers)
         write(hluno,*) ' Num  Layer     Wire    Wire Center ',
     &        'TDC Value RAW DRIFT TIME'
         Do j=1,HDC_TOT_HITS
            write(hluno,'(1x,i2,2x,i3,7x,i4,5x,F10.5,i8,2x,F10.5)')       
     &           j,HDC_LAYER_NUM(j),HDC_WIRE_NUM(j),
     &           HDC_WIRE_CENTER(j),HDC_TDC(j),HDC_DRIFT_TIME(j)
         EndDo
      endif
      
      RETURN
      END
