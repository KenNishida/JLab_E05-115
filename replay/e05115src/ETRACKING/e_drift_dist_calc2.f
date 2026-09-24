      real*8 function e_drift_dist_calc2(layer,time)
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
      integer*4  layer          !  layer number of hit
      real*8     time           !  drift time in ns
*     
*     output
*     
      

*     

c      Write(*,*) 'layer,time=',layer,time
      if(time .lt. 0.) then
         e_drift_dist_calc2 = 0.
      Else if(time .gt. 100.) then
         e_drift_dist_calc2 = 0.5 * edc1_pitch(layer)
      Else
         e_drift_dist_calc2 = edrift_coeff(1,layer) 
     &        + edrift_coeff(2,layer) * time
     &        + edrift_coeff(3,layer) * time * time 
     &        + edrift_coeff(4,layer) * time * time * time
      EndIF
      
      return
      end

