      real*4 function h_drift_dist_calc(plane,wire,time)
*
*     function to calculate sos drift time from tdc value in sos
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log: h_drift_dist_calc.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.5  1996/04/30 17:01:21  saw
* (JRA) Add drift time correction for disc card
*
* Revision 1.4  1995/10/10 13:02:39  cdaq
* (JRA) Remove check for zero drift bin size
*
* Revision 1.3  1995/05/22 19:45:35  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/11/22  21:09:59  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/02/21  16:08:13  cdaq
* Initial revision
*
*  
      implicit none
      include 'hks_data_structures.cmn'
      include 'hks_geometry.cmn'
      include 'hks_tracking.cmn'        ! for lookup tables
*
*     input
*
      integer*4  plane      !  plane number of hit
      integer*4  wire       !  wire number  of hit
      integer*4  ilo,ihi    !  interpolate between bins ilo and ilo+1
      real*4     time       !  drift time in ns
      real*4     fractinterp        !  interpolated fraction 
*
*     output
*


* look in the appropriate drift time to distance table and perform a linear
* interpolation. minimum and maximum distance values are 0.0cm and 0.5cm. 
c      if( sdriftbinsz.eq.0.0)then
c        fractinterp = -1.0
c        s_drift_dist_calc = 0.5*fractinterp
c        return
c      endif      
c     hdriftbinsz [ns/bin]
      ilo = int((time-hdrift1stbin)/hdriftbinsz) + 1
      ihi = ilo + 1
      if( ilo.ge.1 .and. ihi.le.hdriftbins)then 
        fractinterp = hfract(ilo,plane) + 
     &    ( (hfract(ilo+1,plane)-hfract(ilo,plane))/hdriftbinsz )*
     &    (time - hdrift1stbin - (ilo-1)*hdriftbinsz)
      else
        if( ilo.lt.1 )then
cDK          fractinterp = 0.0
          fractinterp = -1000.
        else
cDK          if( ihi.gt.hdriftbins ) fractinterp = 1.0
          if( ihi.gt.hdriftbins ) fractinterp = 1000.
        endif
      endif

      h_drift_dist_calc = 0.5*fractinterp -
     $     hdc_card_delay(hdc_card_no(wire,plane))

      return
      end

