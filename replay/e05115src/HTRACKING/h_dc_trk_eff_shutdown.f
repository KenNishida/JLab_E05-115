      SUBROUTINE H_DC_TRK_EFF_SHUTDOWN(lunout,ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze and report drift chamber efficiencies.
*-
*-      Required Input BANKS     SOS_STATISTICS
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 9/5/95
*
* s_dc_trk_eff calculates efficiencies for the chambers (using tracking)
* s_dc_trk_eff_shutdown does some final manipulation of the numbers.
*
* $Log: h_dc_trk_eff_shutdown.f,v $
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
* Revision 1.1  1995/10/09 20:05:32  cdaq
* Initial revision
*
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*21 here
      parameter (here= 'H_DC_TRK_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hks_tracking.cmn'
      include 'hks_geometry.cmn'

      logical written_header

      integer*4 lunout
      integer*4 pln,wire
      real*4 wireeff,layereff
      real*4 num         ! real version of #/events (aviod repeated floats)
      save

      written_header = .false.

      do pln = 1 , hdc_num_layers
        hdc_didsum(pln)=0
        hdc_shouldsum(pln)=0
        do wire = 1 , hdc_nrwire(pln)
          hdc_shouldsum(pln) = hdc_shouldsum(pln) + 1
          hdc_didsum(pln) = hdc_didsum(pln) + 1
          num = float(max(1,hdc_shouldhit(pln,wire)))
          wireeff = float(hdc_didhit(pln,wire)) / num
          if (num.gt.50 .and. wireeff.lt.hdc_min_wire_eff) then
            write(lunout,111) '       HKS pln=',pln,',  wire=',wire,
     &         ',  effic=',wireeff,' = ',hdc_didhit(pln,wire),'/',
     &         hdc_shouldhit(pln,wire)

          endif
        enddo
      enddo
111   format (a,i3,a,i4,a,f4.2,a,i6,a,i6)

      do pln = 1 , hdc_num_layers
        layereff=float(hdc_didsum(pln))/float(max(1,hdc_shouldsum(pln)))
        if   (hdc_shouldsum(pln).gt.1000 .and. 
     &        layereff.gt.hdc_min_layer_eff(pln)) then
          write(lunout,112) 'ave. effic for layer',pln,' is ',
     &        layereff,' = ',hdc_didsum(pln),'/',hdc_shouldsum(pln)
        endif
      enddo
112   format (a,i3,a,f4.2,a,i7,a,i7)

      return
      end
