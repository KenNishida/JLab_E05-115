      function E_DC1_DPSIFUN(ray,ilayer)
*     this function calculates the psi coordinate of the intersection
*     of a ray (defined by ray) with a wire chamber plane. the geometry
*     of the plane is contained in the coeff array calculated in the
*     array splane_coeff
*     Note it is call by MINUIT via S_FCNCHISQ and so uses double precision
*     variables
*
*     the ray is defined by
*     x = (z-zt)*tan(xp) + xt
*     y = (z-zt)*tan(yp) + yt
*     at some fixed value of zt*
*     ray(1) = xt
*     ray(2) = yt
*     ray(3) = tan(xp)
*     ray(4) = tan(yp)
*     
*     d.f. geesaman                   1 September 1993
*     $Log: e_dc1_dpsifun.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.3  1995/05/22 19:45:35  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.2  1994/02/22  05:47:45  cdaq
*     (SAW) Removed dfloat calls with floating args
*     
*     Revision 1.1  1994/02/21  16:07:39  cdaq
*     Initial revision
*     
*     
      implicit none
      include "hes_data_structures.cmn"
      include "hes_geometry.cmn"
*     
*     input
      real*8 ray(4)             ! xt,yt,xpt,ypt
      integer*4 ilayer          ! plane number
*     output
      real*8 E_DC1_DPSIFUN          ! value of psi coordinate of hit of ray in layer
*     
*     local variables   
      real*8 denom,infinity,cinfinity
      parameter (infinity = 1.0d20)
      parameter (cinfinity = 1/infinity)
*     
      E_DC1_DPSIFUN =  ray(3)*ray(2)*(edc1layer_coeff(1,ilayer)) 
     &     + ray(4)*ray(1)*(edc1layer_coeff(2,ilayer))
     &     + ray(3)*(edc1layer_coeff(3,ilayer)) 
     &     + ray(4)*(edc1layer_coeff(4,ilayer))
     &     + ray(1)*(edc1layer_coeff(5,ilayer))
     &     + ray(2)*(edc1layer_coeff(6,ilayer))
*     
      denom = ray(3)*(edc1layer_coeff(7,ilayer)) 
     &      + ray(4)*(edc1layer_coeff(8,ilayer))  
     &      + (edc1layer_coeff(9,ilayer))
*
      if(abs(denom).lt.cinfinity) then
          E_DC1_DPSIFUN=infinity
      else
          E_DC1_DPSIFUN = E_DC1_DPSIFUN/denom
      endif
      return
      end  
