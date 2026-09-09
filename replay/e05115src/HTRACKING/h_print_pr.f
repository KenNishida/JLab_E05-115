      subroutine h_print_pr
*     subroutine to dump output of S_PATTERN_RECOGNITION
*     All the results are contained in sos_tracking.inc
*     d.f. geesaman          5 September 1993
*     $Log: h_print_pr.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:43  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.2  1995/05/22 19:45:45  cdaq
*     (SAW) Split gen_data_data_structures 
*     into gen, hms, sos, and coin parts"
*     
*     Revision 1.1  1994/02/21  16:37:52  cdaq
*     Initial revision
*     
      implicit none
      include "hks_data_structures.cmn"
      include "hks_tracking.cmn"
      include "hks_geometry.cmn"
*     local variables
      integer*4 i,j

      write(hluno,'('' HKS PATTERN RECOGNITION RESULTS'')')
      write(hluno,'('' chamber='',i3,'' number of hits='',i3)')
     &     (i,hncham_hits(i),i=1,hdc_num_chambers)
      write(hluno,'('' Total number of space points found='',i3)')
     &     hnspace_points_tot
      write(hluno,'('' chamber number'',i2,''  number of points='',i3)')
     &     (i,hnspace_points(i),i=1,hdc_num_chambers)
      write(hluno,'('' Space point requirements'')')
      Do i=1,hdc_num_chambers
         write(hluno,
     &        '(9H chamber=,i3,11H   min_hit=,i4,13H  min_combos=,i3)')
     &        i,hmin_hit(i),hmin_combos(i) 
      EndDo
      if(hnspace_points_tot.ge.1) then
         write(hluno,*) ' point       x         y     ',
     &        'number  number  hits'
         write(hluno,*) ' number                       hits   combos'
 1001    format(3x,i3,f10.4,f10.4,3x,i3,6x,i3,5x,11i3)
         do i=1,hnspace_points_tot
            write(hluno,1001) i, hspace_points(i,1),hspace_points(i,2),
     &           hspace_point_hits(i,1), hspace_point_hits(i,2),
     &           (hspace_point_hits(i,j+2),j=1,hspace_point_hits(i,1))
         enddo
      endif
      
      return
      end
