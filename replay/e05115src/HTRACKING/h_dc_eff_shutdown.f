      SUBROUTINE H_DC_EFF_SHUTDOWN(lunout,ABORT,errmsg)
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
* created: 2/15/95
*
* s_dc_eff calculates efficiencies for the hodoscope.
* s_dc_eff_shutdown does some final manipulation of the numbers.
*
* $Log: h_dc_eff_shutdown.f,v $
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
* Revision 1.2  1996/09/05 13:29:49  saw
* (JRA) Cosmetic
*
* Revision 1.1  1995/08/31 15:07:37  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*17 here
      parameter (here= 'H_DC_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hks_statistics.cmn'
      include 'hks_tracking.cmn'

      logical written_header

      integer*4 lunout
      integer*4 ind
      real*4 num         ! real version of #/events (aviod repeated floats)
      save

      written_header = .false.

      num = float(max(1,hdc_tot_events))
      do ind = 1 , hdc_num_layers
        hdc_layer_eff(ind) = float(hdc_events(ind))/num
        if (hdc_layer_eff(ind) .le. hdc_min_eff(ind) .and. num.ge.1000) then
          if (.not.written_header) then
            write(lunout,*)
            write(lunout,'(a,f6.3)') ' HKS DC layers with low raw hit (hits/trig)efficiencies'
            written_header = .true.
          endif
          write(lunout,'(5x,a,i2,a,f5.3,a,f5.3)') 'eff. for layer #',ind,' is ',
     &       hdc_layer_eff(ind),',   warning level is ',hdc_min_eff(ind)
        endif
      enddo

      do ind = 1 , hdc_num_chambers
        hdc_cham_eff(ind) = float(hdc_cham_hits(ind))/num
      enddo

      return
      end
