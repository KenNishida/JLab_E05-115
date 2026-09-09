      subroutine h_print_stubs
*     subroutine to dump output of S_LEFT_RIGHT
*     All the results are contained in sos_tracking.inc
*     d.f. geesaman          5 September 1993
*     $Log: h_print_stubs.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.2  2004/12/24 21:35:57  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.3  1995/05/22 19:45:46  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.2  1995/05/11  21:06:54  cdaq
*     (JRA) ???
*     
*     Revision 1.1  1994/02/21  16:38:06  cdaq
*     Initial revision
*     
      implicit none
      include "hks_data_structures.cmn"
      include "hks_tracking.cmn"
      include "hks_geometry.cmn"
*     local variables
      integer*4 i,j,k
      write(hluno,*) ' HKS STUB FIT RESULTS'
      if(hnspace_points_tot.ge.1) then
         write(hluno,*) 'point        x_t              y_t     ',
     &        '             xp_t          yp_t'
         write(hluno,*) '             [cm]             [cm]    ',
     &        '            [rad]          [rad]'
 1001    format(3x,i3,4x,4e15.7)
         do i=1,hnspace_points_tot
            write(hluno,1001) i,(hbeststub(i,j),j=1,4)
         enddo
         write(hluno,*) ' hit   layer    HDC_WIRE_CENTER  HDC_DRIFT_DIS',
     &        '      HDC_WIRE_COORD'
         do i=1,hnspace_points_tot
            do j=1,hspace_point_hits(i,1)
               k=hspace_point_hits(i,2+j)
               write(hluno,1002) k,hdc_layer_num(k),HDC_WIRE_CENTER(k),
     &              HDC_DRIFT_DIS(k),HDC_WIRE_COORD(k)
 1002          format(2x,i3,i4,4x,e16.8,2x,e16.8,2x,e16.8)
            enddo
            write(hluno,*) ' '
         enddo
      endif
      return
      end
