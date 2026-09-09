      subroutine h_fill_dc_fp_hist(Abort,err)
*
*     routine to fill histograms with sos_focal_layer varibles
*
*     Author:	D. F. Geesaman
*     Date:     30 March 1994
*     Modified: 9 April 1994        DFG
*                                   Transfer ID in common block
*                                   Implement flag to turn block on
* $Log: h_fill_dc_fp_hist.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.3  2004/12/24 21:37:10  miyoshi
* change name plane to layer
*
* Revision 1.2  2004/12/24 19:47:07  miyoshi
* change minor
*
* Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
* new dir
*
* Revision 1.4  1995/05/22 19:45:38  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/04/06  19:35:40  cdaq
* (JRA) Add WC residual histograms
*
* Revision 1.2  1994/08/18  04:34:23  cdaq
* (SAW) Indentation changes
*
* Revision 1.1  1994/04/13  18:10:39  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'h_fill_dc_fp_hist')
*
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 itrk
      integer*4 layer
*
      include 'hks_data_structures.cmn'
      include 'hks_track_histid.cmn'
      include 'hks_tracking.cmn'
*
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*

* Is this histogram flag turned on
      if(hturnon_focal_plane_hist .ne. 0 ) then
* Make sure there is at least 1 track
        if(HNTRACKS_FP .gt. 0 ) then
* Loop over all hits
          do itrk=1,HNTRACKS_FP
            call hf1(hidhx_fp,HX_FP(itrk),1.)
            call hf1(hidhy_fp,HY_FP(itrk),1.)
            call hf1(hidhxp_fp,HXP_FP(itrk),1.)
            call hf1(hidhyp_fp,HYP_FP(itrk),1.)
            if(HCHI2_FP(itrk) .gt. 0 ) then
              histval=log10(HCHI2_FP(itrk))            
            else 
              histval = 10.
            endif
            call hf1(hidhlogchi2_fp,histval,1.)
            histval= HNFREE_FP(itrk)
            call hf1(hidhnfree_fp,histval,1.)
            if( HNFREE_FP(itrk) .ne.0) then
              histval= HCHI2_FP(itrk) / HNFREE_FP(itrk)
            else
              histval = -1.
            endif
            call hf1(hidhchi2perdeg_fp,histval,1.)
*
            do layer = 1,hdc_num_layers
              call hf1(hidres_fp(layer),hdc_double_residual(itrk,layer),1.)
              call hf1(hidsingres_fp(layer),hdc_single_residual(itrk,layer),1.)
            enddo

          enddo                         ! end loop over hits
        endif                           ! end test on zero hits       
      endif                             ! end test on histogramming flag
      RETURN
      END
