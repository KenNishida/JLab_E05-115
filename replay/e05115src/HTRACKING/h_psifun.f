      function h_psifun(ray,ilayer)
*     this function calculates the psi coordinate of the intersection
*     of a ray (defined by ray) with a wire chamber layer. the geometry
*     of the layer is contained in the coeff array calculated in the
*     array slayer_coeff
*
*     the ray is defined by
*     x = (z-zt)*tan(xp) + xt
*     y = (z-zt)*tan(yp) + yt
*      at some fixed value of zt*
*     ray(1) = xt
*     ray(2) = yt
*     ray(3) = tan(xp)
*     ray(4) = tan(yp)
*
*     d.f. geesaman                   1 September 1993
* $Log: h_psifun.f,v $
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
* Revision 1.2  1995/05/22 19:45:53  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/21  16:40:53  cdaq
* Initial revision
*
*
      implicit none
      include "hks_data_structures.cmn"
      include "hks_geometry.cmn"
*
*     input
      real*4 ray(4)           ! xt,yt,xpt,ypt
      integer*4 ilayer        ! layer number
*     output
      real*4 H_PSIFUN         ! value of psi coordinate of hit of ray in layer
*
*     local variables   
      real*4 denom,infinity,cinfinity
      parameter (infinity = 1.0d20)
      parameter (cinfinity = 1/infinity)
*
      H_PSIFUN =  ray(3)*ray(2)*hlayer_coeff(1,ilayer) 
     &        + ray(4)*ray(1)*hlayer_coeff(2,ilayer)
     &        + ray(3)*hlayer_coeff(3,ilayer) 
     &        + ray(4)*hlayer_coeff(4,ilayer)
     &        + ray(1)*hlayer_coeff(5,ilayer) 
     &        + ray(2)*hlayer_coeff(6,ilayer)
*
      denom = ray(3)*hlayer_coeff(7,ilayer) 
     &      + ray(4)*hlayer_coeff(8,ilayer) + hlayer_coeff(9,ilayer)
*
      if(abs(denom).lt.cinfinity) then
          H_PSIFUN=infinity
      else
          H_PSIFUN = H_PSIFUN/denom
      endif
      return
      end  
