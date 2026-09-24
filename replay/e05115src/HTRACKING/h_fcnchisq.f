      subroutine H_FCNCHISQ(npar,grad,fval,ray,iflag,dumarg)
*     This subroutine calculates chi**2 for MINUIT. The
*     arguments are determined by MINUIT
*
*     d.f. geesaman             8 September 1993
*     modified   dfg            14 Feb 1993   Change SLAYER_PARAM to 
*                                             sdc_sigma
* $Log: h_fcnchisq.f,v $
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
* Revision 1.3  1995/05/22 19:45:37  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/11/22  21:11:17  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/02/21  16:13:20  cdaq
* Initial revision
*
*
      implicit none
      external H_DPSIFUN
      real*8 H_DPSIFUN
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'hks_geometry.cmn'
*
*     input
      real*8 ray(*),grad(*),dumarg
      integer*4 npar,iflag
*     output
      real*8 fval                              ! value of chi2
*
*     local variables
      real*8 diff
      integer*4 ihit
      integer*4 hitnum,layernum

      fval=0.0d0
      do ihit=1,HNTRACK_HITS(htrack_fit_num,1)
         hitnum=HNTRACK_HITS(htrack_fit_num,ihit+1)
         layernum=HDC_LAYER_NUM(hitnum)
         diff=(dble(HDC_WIRE_COORD(hitnum))-H_DPSIFUN(ray,layernum))
     &        /dble(hdc_sigma(layernum))
         fval=fval+diff*diff
      enddo
      return
      end
