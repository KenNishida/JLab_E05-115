      subroutine h_print_links
*     prints the output of link matching
*     d.f. geesaman           7 Sept 1993
* $Log: h_print_links.f,v $
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
* Revision 1.2  1995/05/22 19:45:45  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/21  16:15:59  cdaq
* Initial revision
*
*
      implicit none
      include "hks_data_structures.cmn"
      include "hks_tracking.cmn"
      integer*4 itrack,ihit
      write(hluno,
     &  '(''  NUMBER OF TRACKS FROM SOS LINKED STUBS='',i4)') HNTRACKS_FP
        if(HNTRACKS_FP.gt.0) then
        write(hluno,'(''  Track   HITS'')')
          do itrack=1,HNTRACKS_FP
           write(hluno,1000) itrack,(HNTRACK_HITS(itrack,ihit),
     &        ihit=2,HNTRACK_HITS(itrack,1)+1) 
1000  format(2x,i3,2x,24i3)
          enddo
        endif
      return
      end
