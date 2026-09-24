      subroutine e_dc2_find_best_stub(numhits,hits,pl,pindex,plusminus,stub,chi2)
*     This subroutine does a linear least squares fit of a line to the
*     hits in an individual chamber. It assumes that the y slope is 0 
*     The wire coordinate is calculated
*     from the wire center + plusminus*(drift distance).
*     This is called in a loop over all combinations of plusminus
*     
*     d. f. geesaman
* $Log: e_dc2_find_best_stub.f,v $
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
      real*4 dpos(edc2max_hits_per_point)
      integer*4 pl(edc2max_hits_per_point)
      integer*4 pindex
      real*8 TT(3)
      integer*4 hit
      integer*4 i

c--   Local variable to transfer to e_dc2_best_stub
c--   L.Yuan   09/24/2009
*----------------------------------------------------------------------
* MATRICES FOR 3 PARAMETER FITS.
*
*     CTPTYPE=parm
*
* matrix AA and its inverse AAINV 
* array of determinants of AA

      real*8 edc2aa3(3,3),edc2aainv3(3,3,emax_num_dc2_layers+1)      
      real*8 edc2det3(emax_num_dc2_layers+1)             

      common/EDC2_TFIT_MATRIX/
     &     edc2aa3,
     &     edc2aainv3,
     &     edc2det3

*----------------------------------------------------------------------
      
      TT(1)=0.
      TT(2)=0.
      TT(3)=0.

* calculate trail hit position and least squares matrix coefficients.
      do hit=1,numhits
        dpos(hit)=EDC2_WIRE_CENTER(hits(hit)) +
     &       plusminus(hit)*EDC2_DRIFT_DIS(hits(hit)) -
     &       edc2psi0(pl(hit))
        do i=1,3
          TT(i)=TT(i)+(dpos(hit)*edc2stubcoef(pl(hit),i))/edc2_sigma(pl(hit))
        enddo
      enddo
*
*     solve three by three equations
ccc      call s_solve_3by3(TT,pindex,dstub,ierr)

      dstub(1)=EDC2AAINV3(1,1,pindex)*TT(1) + EDC2AAINV3(1,2,pindex)*TT(2) +
     &     EDC2AAINV3(1,3,pindex)*TT(3)
      dstub(2)=EDC2AAINV3(1,2,pindex)*TT(1) + EDC2AAINV3(2,2,pindex)*TT(2) +
     &     EDC2AAINV3(2,3,pindex)*TT(3)
      dstub(3)=EDC2AAINV3(1,3,pindex)*TT(1) + EDC2AAINV3(2,3,pindex)*TT(2) +
     &     EDC2AAINV3(3,3,pindex)*TT(3)

*
* calculate chi2.  Remember one power of sigma is in sstubcoef
      chi2=0.
      stub(1)=dstub(1)
      stub(2)=dstub(2)
      stub(3)=dstub(3)
      stub(4)=0.

cc      print *, 'AAINV',(EDC2AAINV3(1,i,pindex), i=1,3)
cc      print *, 'find_stub', TT(1),TT(2), TT(3), dpos(1), dpos(2), dstub(1), dstub(2)
      do hit=1,numhits
        chi2=chi2+(dpos(hit)/edc2_sigma(pl(hit))
     &       -edc2stubcoef(pl(hit),1)*stub(1)
     &       -edc2stubcoef(pl(hit),2)*stub(2)
     &       -edc2stubcoef(pl(hit),3)*stub(3) )**2
      enddo
      return
      end
