      subroutine e_dc1_find_best_stub(numhits,hits,pl,pindex,plusminus,stub,chi2)
*     This subroutine does a linear least squares fit of a line to the
*     hits in an individual chamber. It assumes that the y slope is 0 
*     The wire coordinate is calculated
*     from the wire center + plusminus*(drift distance).
*     This is called in a loop over all combinations of plusminus
*     
*     d. f. geesaman
* $Log: e_dc1_find_best_stub.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:41  miyoshi
* new dir
*
* Revision 1.5  1996/01/17 19:05:23  cdaq
* (JRA)
*
* Revision 1.4  1995/10/10 13:39:56  cdaq
* (JRA) Cleanup
*
* Revision 1.3  1995/05/22 19:45:39  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/11/22  21:11:50  cdaq
* (SPB) Recopied from hms file and modified names for SOS
*
* Revision 1.1  1994/02/21  16:13:42  cdaq
* Initial revision
*
*
*     the four parameters of a stub are x_t,y_t,xp_t,yp_t
*     
*     Called by E_LEFT_RIGHT
*     
      implicit none
      include 'hes_data_structures.cmn'
      include 'hes_tracking.cmn'
      include 'hes_geometry.cmn'
*     input quantities
      integer*4 numhits
      integer*4 hits(*)
      real*4 plusminus(*)
*
*     output quantitites
      real*8 dstub(3)               ! x, xp , y of local line fit
      real*4 stub(4)
      real*4 chi2               ! chi2 of fit      
*     
*     local variables
      real*4 dpos(edc1max_hits_per_point)
      integer*4 pl(edc1max_hits_per_point)
      integer*4 pindex
      real*8 TT(3)
      integer*4 hit
      integer*4 i
      
      TT(1)=0.
      TT(2)=0.
      TT(3)=0.

* calculate trail hit position and least squares matrix coefficients.
      do hit=1,numhits
        dpos(hit)=EDC1_WIRE_CENTER(hits(hit)) +
     &       plusminus(hit)*EDC1_DRIFT_DIS(hits(hit)) -
     &       edc1psi0(pl(hit))
        do i=1,3
          TT(i)=TT(i)+(dpos(hit)*edc1stubcoef(pl(hit),i))/edc1_sigma(pl(hit))
        enddo
      enddo
*
*     solve three by three equations
ccc      call s_solve_3by3(TT,pindex,dstub,ierr)

      dstub(1)=EAAINV3(1,1,pindex)*TT(1) + EAAINV3(1,2,pindex)*TT(2) +
     &     EAAINV3(1,3,pindex)*TT(3)
      dstub(2)=EAAINV3(1,2,pindex)*TT(1) + EAAINV3(2,2,pindex)*TT(2) +
     &     EAAINV3(2,3,pindex)*TT(3)
      dstub(3)=EAAINV3(1,3,pindex)*TT(1) + EAAINV3(2,3,pindex)*TT(2) +
     &     EAAINV3(3,3,pindex)*TT(3)

*
* calculate chi2.  Remember one power of sigma is in sstubcoef
      chi2=0.
      stub(1)=dstub(1)
      stub(2)=dstub(2)
      stub(3)=dstub(3)
      stub(4)=0.
      do hit=1,numhits
        chi2=chi2+(dpos(hit)/edc1_sigma(pl(hit))
     &       -edc1stubcoef(pl(hit),1)*stub(1)
     &       -edc1stubcoef(pl(hit),2)*stub(2)
     &       -edc1stubcoef(pl(hit),3)*stub(3) )**2
      enddo
      return
      end
