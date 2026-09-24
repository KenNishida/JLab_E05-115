      subroutine h_dc_eff_count(abort,err,hp)
c
c     // Count for KDC efficiency calculation //
c
c     Toshiyuki Gogami    3/Mar/2011
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
      include 'hks_geometry.cmn'
      include 'gen_event_info.cmn'
      include 'hks_scin_tof.cmn'
      include 'hks_aero_parms.cmn'
      include 'hks_water_parms.cmn'
      include 'hks_lucite_parms.cmn'
      include 'hks_bypass_switches.cmn'

      integer*4 hp              ! hnphysics in "h_physiscs.f"
      integer*4 ktofflag        ! KTOF flag
      integer*4 trackflag       ! Tracking flag
      real*4 accdist
      parameter ( accdist = 1.0  ) ! Acceptable distance(residual) to calculate eff.
      integer*4 layer

c     ========= Initialize ===============
      ktofflag = 0
      trackflag = 0
      
c     ======== KTOF FLAG ============
      if(hsnt1(hp).gt.-200.0 
     &     .and. hsnt2(hp).gt.-200.0 
     &     .and. hsnt3(hp).gt.-200.0) then
         ktofflag = 1
      else
         ktofflag = 0
      endif
      
c     ======== TRACKING FLAG =========
c      if(hschi2perdeg(hp).le.30.0)then
c      if(hschi2perdeg(hp).le.10.0)then
      if(hschi2perdeg(hp).le.500.0)then ! For Kawama-san's study (Toshi Gogami, 7/Jan/2011)
         trackflag = 1
      else
         trackflag = 0
      endif
c      write(*,*) "TEST"
c      write(*,*) ktofflag, "," , trackflag
      
c     ======== Count for efficiency calculation ======
c      if(ktofflag.eq.1 .and. trackflag.eq.1)then ! remove for test 16/June/2011
      if(ktofflag.eq.1 .and. trackflag.eq.1)then ! put this in again (12/Oct/2011)
         hdc_eff_gtrig = hdc_eff_gtrig + 1.0
         do layer=1 , 12
            if( abs(hsdc_sing_res(hp,layer))
     &           .le. accdist )then
               hdc_eff_hit(layer) = hdc_eff_hit(layer) + 1.0
            endif
         enddo
      endif

c     =============  END =======================
      return
      end subroutine
