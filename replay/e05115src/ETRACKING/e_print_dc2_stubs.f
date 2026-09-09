      subroutine e_print_dc2_stubs
*     subroutine to dump output of E_LEFT_RIGHT
*     All the results are contained in sos_tracking.inc
*     d.f. geesaman          5 September 1993
*     $Log: e_print_dc2_stubs.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.1.1.1  2004/08/30 21:21:41  miyoshi
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
      include "hes_data_structures.cmn"
      include "hes_tracking.cmn"
      include "hes_geometry.cmn"
*     local variables
      integer*4 i,j,k
      
      write(eluno,*) ' EDC2 STUB FIT RESULTS'
      if(edc2nspace_points_tot.ge.1) then
         write(eluno,*) 'point        x_t              y_t     ',
     &        '             xp_t          yp_t'
         write(eluno,*) '             [cm]             [cm]    ',
     &        '            [rad]          [rad]'
 1001    format(3x,i3,4x,4e15.7)
         do i=1,edc2nspace_points_tot
            write(eluno,1001) i,(edc2beststub(i,j),j=1,4)
         enddo
         write(eluno,*) ' hit   layer  EDC2_WIRE_CENTER  EDC2_DRIFT_DIS',
     &        '      EDC2_WIRE_COORD'
         do i=1,edc2nspace_points_tot
            do j=1,edc2space_point_hits(i,1)
               k=edc2space_point_hits(i,2+j)
               write(eluno,1002) k,edc2_layer_num(k),EDC2_WIRE_CENTER(k),
     &              EDC2_DRIFT_DIS(k),EDC2_WIRE_COORD(k)
 1002          format(2x,i3,i4,4x,e16.8,2x,e16.8,2x,e16.8)
            enddo
            write(eluno,*) ' '
         enddo
      endif
      
      return
      end
