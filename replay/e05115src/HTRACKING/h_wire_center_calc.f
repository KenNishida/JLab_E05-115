      function h_wire_center_calc(plane,wire)
*
*     function to calculate sos wire center positions in sos
*     wire chambers
*
*     d.f. geesaman              17 feb 1994
* $Log: h_wire_center_calc.f,v $
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
* Revision 1.5  1996/09/04 20:17:35  saw
* (??) Cosmetic
*
* Revision 1.4  1995/05/22 19:46:04  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1994/11/23  15:08:41  cdaq
* * (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.2  1994/03/24  20:04:21  cdaq
* (DFG) allow for reverse wire ordering
*
* Revision 1.1  1994/02/21  16:44:09  cdaq
* Initial revision
*
*  
      implicit none
      include "hks_data_structures.cmn"
      include "hks_geometry.cmn"
*
*     input
*
      integer*4  plane      ! plane number of hit
      integer*4  wire       ! wire number  of hit
*
*     output
*
      real*4     h_wire_center_calc       !  wire center in cm
*
*     if hdc_sire_counting(plane) is 1 then wires are number in reverse order
      if(hdc_wire_counting(plane).eq.0) then 
*          normal ordering
         h_wire_center_calc = (FLOAT(wire)-hdc_central_wire(plane))
     &        * hdc_pitch(plane) - hdc_center(plane)
      else        
           h_wire_center_calc = 
     &        ((hdc_nrwire(plane) + (1 - wire))-
     &        hdc_central_wire(plane))* hdc_pitch(plane)-hdc_center(plane)
      endif
      return
      end
