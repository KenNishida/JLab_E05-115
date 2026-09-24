      function h_drift_time_calc(layer,wire,tdc)
*
*     function to calculate sos drift time from tdc value in sos
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log: h_drift_time_calc.f,v $
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
* Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
* new dir
*
* Revision 1.5  1995/10/09 20:16:16  cdaq
* (JRA) Remove monte carlo data option
*
* Revision 1.4  1995/05/22 19:45:36  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1994/11/22  21:10:31  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.2  1994/03/24  19:52:20  cdaq
* (DFG) Allow switch for monte carlo data
*
* Revision 1.1  1994/02/21  16:08:30  cdaq
* Initial revision
*
*  
      implicit none
      include "hks_data_structures.cmn"
      include "hks_tracking.cmn"
      include "hks_geometry.cmn"
*
*     input
*
      integer*4  layer      !  layer number of hit
      integer*4  wire       !  wire number  of hit
      integer*4  tdc        !  tdc value
*
*     output
*
      real*4     h_drift_time_calc       !  drift time in nanoseconds

*
      h_drift_time_calc = hstart_time
     &     - float(tdc)*hdc_tdc_time_per_channel
     &     + hdc_layer_time_zero(layer)
      return
      end
