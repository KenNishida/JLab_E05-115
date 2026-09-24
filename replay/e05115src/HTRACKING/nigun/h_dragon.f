      subroutine h_dragon(abort,err,hp)
      
      implicit none
      save
      
      character*9 here
      parameter ( here = 'h_dragon' )
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

      integer*4 hp
      integer*4 ktofflag
c      integer*4 hittrig,lhittrig(12)
      real*4 hittrig,lhittrig(12)
      integer*4 trackflag
      real*4 accdist
      parameter ( accdist = 1.0  )
      real*4 kdc_eff(12)
      integer*4 layer



c     --- Initialize ---
      ktofflag = 0
      trackflag = 0

      
c     --- KTOF FLAG ---
      if(hsnt1(hp).gt.-200.0 
     &     .and. hsnt2(hp).gt.-200.0 
     &     .and. hsnt3(hp).gt.-200.0) then
         ktofflag = 1
      else
         ktofflag = 0
      endif
c      write(*,*)ktofflag
      
c     --- TRACKING FLAG ---
      if(hschi2perdeg(hp).le.30.0)then
         trackflag = 1
      else
         trackflag = 0
      endif
      
      if(ktofflag.eq.1 .and. trackflag.eq.1)then
         hittrig = hittrig + 1
         do layer=1 , 12
            if( abs(hsdc_sing_res(hp,layer))
     &           .le. accdist )then
c               write(*,*)abs(hsdc_sing_res(hp,layer))
               lhittrig(layer) = lhittrig(layer) + 1
            endif
         enddo
      endif
      do layer=1,12
         kdc_eff(layer) = lhittrig(layer)/hittrig
      enddo
      
      write(*,*)"        ", kdc_eff(3)
      return
      end subroutine

**************************************************************************
c     //
c     Grouping of KTOF1X and KTOF2X
c     Grouping = 1 if those two hits are in same group.
c     //
cc      integer*4 function Grouping(hit1,hit2)
cc      
cc      implicit none
cc      integer*4 group
cc      integer*4 hit1,hit2
cc
ccc     Group1
cc      if( hit1.ge.1 .and. hit1.le.3)then
cc         if( hit2.ge.1 .and. hit2.le.5 )then
cc            group = 1
cc         else 
cc            group = 0
cc         endif
ccc     Group2
cc      else if( hit1.ge.4 .and. hit1.le.6)then
cc         if( hit2.ge.3 .and. hit2.le.8 )then
cc            group = 1
cc         else 
cc            group = 0
cc         endif
ccc     Group3
cc      else if( hit1.ge.7 .and. hit1.le.9)then
cc         if( hit2.ge.6 .and. hit2.le.11 )then
cc            group = 1
cc         else 
cc            group = 0
cc         endif
ccc     Group4
cc      else if( hit1.ge.10 .and. hit1.le.12)then
cc         if( hit2.ge.9 .and. hit2.le.14 )then
cc            group = 1
cc         else 
cc            group = 0
cc         endif
ccc     Group5
cc      else if( hit1.ge.13 .and. hit1.le.15)then
cc         if( hit2.ge.12 .and. hit2.le.17 )then
cc            group = 1
cc         else 
cc            group = 0
cc         endif
ccc     Group6
cc      else if( hit1.ge.16 .and. hit1.le.17 )then
cc         if( hit2.ge.15 .and. hit2.le.18 )then
cc            group = 1
cc         else 
cc            group = 0
cc         endif
cc      else 
cc         group = 0
cc      endif
cc      
cc      Grouping = group
cc
cc      return 
cc      end function
