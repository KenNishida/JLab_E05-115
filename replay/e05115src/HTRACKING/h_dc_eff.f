      SUBROUTINE H_DC_EFF(ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze scintillator information for each track 
*-
*-      Required Input BANKS     SOS_STATISTICS
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 8/17/95
*
* s_dc_eff calculates efficiencies for the drift chambers.
*
* $Log: h_dc_eff.f,v $
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
* Revision 1.1  1995/08/31 15:07:28  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*8 here
      parameter (here= 'H_DC_EFF')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hks_statistics.cmn'
      include 'hks_tracking.cmn'

      integer*4 ind

      save

      hdc_tot_events = hdc_tot_events + 1
      do ind = 1 , hdc_num_layers
        if (hdc_hits_per_layer(ind).gt.0) hdc_events(ind)=hdc_events(ind)+1
      enddo

      if (hdc_hits_per_layer(1)+hdc_hits_per_layer(2)+hdc_hits_per_layer(3)
     &   +hdc_hits_per_layer(4)+hdc_hits_per_layer(5)+hdc_hits_per_layer(6)
     &    .ne. 0)   hdc_cham_hits(1) = hdc_cham_hits(1) + 1

      if (hdc_hits_per_layer( 7)+hdc_hits_per_layer( 8)+hdc_hits_per_layer( 9)
     &   +hdc_hits_per_layer(10)+hdc_hits_per_layer(11)+hdc_hits_per_layer(12)
     &    .ne. 0)   hdc_cham_hits(2) = hdc_cham_hits(2) + 1

      return
      end
