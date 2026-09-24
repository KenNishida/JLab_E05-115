      subroutine e_dc_eff_count(abort,err,ep)
c
c     // Count for EDC efficiency calculation //
c     Original file : h_dc_eff_count.f ( T.Gogami, 3/Mar/2011 )
c     Modified by Toshiyuki Gogami on 11/Jan/2012
c      

      implicit none
      save
      
      character*9 here
      parameter ( here = 'h_dc_eff_count' )
      logical abort
      character*(*) err
      
      include 'hes_data_structures.cmn'
      include 'gen_data_structures.cmn'
      include 'hks_data_structures.cmn'
      include 'gen_routines.dec'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 'hks_physics_sing.cmn'
      include 'hks_scin_parms.cmn'
      include 'hks_tracking.cmn'
      include 'hes_tracking.cmn'
      include 'hks_geometry.cmn'
      include 'gen_event_info.cmn'
      include 'hks_scin_tof.cmn'
      include 'hks_aero_parms.cmn'
      include 'hks_water_parms.cmn'
      include 'hks_lucite_parms.cmn'
      include 'hks_bypass_switches.cmn'

      integer*4 ep              ! hnphysics in "h_physiscs.f"
      integer*4 etofflag        ! KTOF flag
      integer*4 trackflag       ! Tracking flag
      real*4 accdist1
      parameter ( accdist1 = 1.0  ) ! Acceptable distance(residual) to calculate eff.
      real*4 accdist2
      parameter ( accdist2 = 1.0  ) ! Acceptable distance(residual) to calculate eff.
      integer*4 layer, layer2

c     ========= Initialize ===============
      etofflag = 0
      trackflag = 0
      
c     ======== KTOF FLAG ============
      if(esnt1(ep).gt.-200.0 
     &     .and. esnt2(ep).gt.-200.0) then
         etofflag = 1
      else
         etofflag = 0
      endif
      
c     ======== TRACKING FLAG =========
c      if(hschi2perdeg(hp).le.30.0)then
c      if(eschi2perdeg(hp).le.10.0)then
      if(eschi2perdeg(ep).le.500.0)then
         trackflag = 1
      else
         trackflag = 0
      endif

c     ======== Count for efficiency calculation ======
      if(etofflag.eq.1 .and. trackflag.eq.1)then
         edc_eff_gtrig = edc_eff_gtrig + 1.0
         do layer=1 , 10
            if( abs(edc1_single_residual(ep,layer))
     &           .le. accdist1 )then
               edc_eff_hit(layer) = edc_eff_hit(layer) + 1.0
            endif
         enddo
         do layer=11 , 16
            layer2 = layer-10
            if( abs(edc2_single_residual(ep,layer2))
     &           .le. accdist2 )then
               edc_eff_hit(layer) = edc_eff_hit(layer) + 1.0
            endif
         enddo
      endif

c     =============  END =======================
      return
      end subroutine
