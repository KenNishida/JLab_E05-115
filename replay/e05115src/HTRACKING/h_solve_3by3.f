      subroutine h_solve_3by3(TT,pindex,stub,ierr)
*     Explicit solution of a symmetric three by three equation TT = AA * STUB
*     Remember AA must be a symmetrix matrix
*     Used in find_best_stub.f

* $Log: h_solve_3by3.f,v $
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
* Revision 1.3  1995/10/10 17:36:30  cdaq
* (JRA) Cleanup
*
* Revision 1.2  1995/05/22 19:45:55  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
c Revision 1.1  1994/11/23  13:59:37  cdaq
c Initial revision
c
*
* djm 10/2/94
* The present version replaces solve_three_by_three(TT,AA,stub,ierr) in 
* find_best_stub. New version is entirely based on dfg's version, but matrix 
* inversion is now done only at initialization for faster event sorting.

*
      implicit none
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'


*     input quantities
      real*8 TT(3) 
      integer*4 pindex
*
*     output quantities
      real*8 stub(3)
      integer*4 ierr                       ! ierr = 0 means valid solution
*
      if(pindex.le.HDC_NUM_LAYERS+HDC_NUM_CHAMBERS)then !accept 5/6 or 6/6 good layers
        ierr=0
        stub(1)=HAAINV3(1,1,pindex)*TT(1) + HAAINV3(1,2,pindex)*TT(2) + 
     & HAAINV3(1,3,pindex)*TT(3)
        stub(2)=HAAINV3(1,2,pindex)*TT(1) + HAAINV3(2,2,pindex)*TT(2) + 
     & HAAINV3(2,3,pindex)*TT(3)
        stub(3)=HAAINV3(1,3,pindex)*TT(1) + HAAINV3(2,3,pindex)*TT(2) + 
     & HAAINV3(3,3,pindex)*TT(3)
      else
        ierr=1
      endif          !end test on layer index
 
*      write(6,*)'TT i=1,2,3',TT(1),TT(2),TT(3)
*
*      write(6,*)'aainv(1,1,) (1,2,) (1,3,)',aainv(1,1,pindex),
*     &    aainv(1,2,pindex),aainv(1,3,pindex)
*
*      write(6,*)'aainv(2,2) (2,3) (3,3)',aainv(2,2,pindex),
*     &    aainv(2,3,pindex),aainv(3,3,pindex)
*
*     write(6,*)
     
      return

      end
