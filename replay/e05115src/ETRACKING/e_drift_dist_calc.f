      real*4 function e_drift_dist_calc(layer,slot,time)
*
*     This is a function for EDC drift distance calculation
*  
      implicit none
      include 'hes_data_structures.cmn'
      include 'hes_geometry.cmn'
      include 'hes_tracking.cmn'        ! for lookup tables
*
*     input
*
      integer*4  layer,slot     !  layer number of hit
      real*4     time           !  drift time in ns
      integer*4  ilo,ihi    !  interpolate between bins ilo and ilo+1
      real*4     fractinterp        !  interpolated fractio
*     
*     output
*     
      
*     

c     Write(*,*) 'l,s,t=',layer,slot,time
c      if(time .lt. 0.) then
c         e_drift_dist_calc = 0.
c      Else if(time .gt. 100.) then
c         e_drift_dist_calc = 0.5 * edc_pitch(layer)
c      Else
c         e_drift_dist_calc = edrift_coeff(1,slot) 
c     &        + edrift_coeff(2,slot) * time
c     &        + edrift_coeff(3,slot) * time * time 
c     &        + edrift_coeff(4,slot) * time * time * time
c      EndIF

      e_drift_dist_calc = edrift_coeff(1,slot)
     &     + edrift_coeff(2,slot) * time
     &     + edrift_coeff(3,slot) * time * time 
     &     + edrift_coeff(4,slot) * time * time * time
c      if(e_drift_dist_calc.lt.-0.03) e_drift_dist_calc=-1.
      if(e_drift_dist_calc.lt.-0.0) e_drift_dist_calc=-1.
      if(e_drift_dist_calc.gt.0.55) e_drift_dist_calc=-1.

c      ilo = int((time-edrift1stbin)/edriftbinsz) + 1
c      ihi = ilo + 1
c      if( ilo.ge.1 .and. ihi.le.edriftbins)then 
c        fractinterp = efract(ilo,layer) + 
c     &    ( (efract(ilo+1,layer)-efract(ilo,layer))/edriftbinsz )*
c     &    (time - edrift1stbin - (ilo-1)*edriftbinsz)
c      else
c        if( ilo.lt.1 )then
c          fractinterp = 0.0
c        else
c          if( ihi.gt.edriftbins ) fractinterp = 1.0
c        endif
c      endif

c      e_drift_dist_calc = 0.5*fractinterp
      
      return
      end

