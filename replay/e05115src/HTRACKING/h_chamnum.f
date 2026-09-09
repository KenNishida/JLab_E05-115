      function h_chamnum(ispace_point)
*     This function returns the chamber number of a space point
*      d.f. geesaman              8 Sept 1993
* $Log: h_chamnum.f,v $
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
* Revision 1.2  1995/05/22 19:45:33  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/21  16:07:27  cdaq
* Initial revision
*
      implicit none
      include "hks_data_structures.cmn"
      include "hks_tracking.cmn"
      include "hks_geometry.cmn"
*     output
      integer*4 h_chamnum
*     input
      integer*4 ispace_point
*     local variables
      integer*4 layer
      h_chamnum=0
      layer=HDC_LAYER_NUM(hspace_point_hits(ispace_point,3))
      if(layer.gt.0 .and. layer.le. hdc_num_layers) then
          h_chamnum=hdc_chamber_layers(layer)
      endif
      return
      end
*
