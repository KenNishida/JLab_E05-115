      function h_tdc_time_per_channel(plane,wire)
*
*     routinne to return tdc slope (in ns/channel ) for a given sos plane and
*     wire
*
*     d.f. geesaman      17 feb 1994        first dummy routine
* $Log: h_tdc_time_per_channel.f,v $
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
* Revision 1.3  1995/05/22 19:45:57  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/03/24  19:56:24  cdaq
* (DFG) Add includes, return value now a registered variable
*
* Revision 1.1  1994/02/21  16:41:23  cdaq
* Initial revision
*
*
      implicit none
      include 'hks_data_structures.cmn'
      include 'hks_geometry.cmn'
*     inputs
*     
      integer*4      plane     ! sos plane number of hit
      integer*4      wire      ! sos wire number of hit
*
*     output
*    
      real*4         h_tdc_time_per_channel 
*
*      h_drift_time_calc = SSTART_TIME 
*     &      - FLOAT(tdc)*h_tdc_time_per_channel(plane,wire)
*     &      + h_tdc_zero(plane,wire)                     
*
      h_tdc_time_per_channel = hdc_tdc_time_per_channel
      return
      end
