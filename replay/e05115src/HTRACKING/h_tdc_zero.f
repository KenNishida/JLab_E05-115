      function h_tdc_zero(layer,wire)
*
*     routine to return tdc_zero offset (in ns) for a given sos layer and
*     wire
*
*     d.f. geesaman      17 feb 1994        first dummy routine
* $Log: h_tdc_zero.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.2  2004/12/24 21:35:57  miyoshi
* change name plane to layer
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.3  1995/05/22 19:45:58  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/06/14  04:38:30  cdaq
* (DFG) Make zero time a parameter
*
* Revision 1.1  1994/02/21  16:41:38  cdaq
* Initial revision
*
*
*     inputs
*     
*     integer*4      layer     sos layer number of hit
*     integer*4      wire      sos wire number of hit
*
*     output
*    
*     real*4         s_tdc_zero  offset
*
*      s_drift_time_calc = SSTART_TIME 
*     &      - FLOAT(tdc)*s_tdc_time_per_channel(layer,wire)
*     &      + s_tdc_zero(layer,wire)                     
*
      implicit none
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'hks_geometry.cmn'            
      integer*4 layer,wire
      real*4 h_tdc_zero
      h_tdc_zero=hdc_layer_time_zero(layer)
      return
      end
