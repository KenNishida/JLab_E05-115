      subroutine e_dc_eff_out(abort,err)
c
c     // Calculation of EDC efficiency //
c
c     Original file : HTRACKING/h_dc_eff_out.f ( T.Gogami,3/May/2011 )
c     Modified by Toshiyuki Gogami on 12/Jan/2012
c

      implicit none
      save
      
      character*9 here
      parameter ( here = 'e_dc_eff_out' )
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
      include 'gen_run_info.cmn'
      include 'hks_scin_tof.cmn'
      include 'hks_aero_parms.cmn'
      include 'hks_water_parms.cmn'
      include 'hks_lucite_parms.cmn'
      include 'hks_bypass_switches.cmn'
      
      integer*4 layer           ! use for loop
      real*4 edc_eff1,edc_eff2,edc_eff_ ! EDC efficiencies
      integer dispflag
      parameter (dispflag = 0)  ! Efficiencies will be displayed, if dispflag==1
      integer*4 runnumber       ! Run number (int)
      character Keff_filename*40 ! Output filename
      character run*5           ! Run number (char)
      integer*4 anaEV           ! gstop (analyzed event)
      real*4 edc_rate_acc(16),edc_rate_sig(16) ! Rate
      real*4 twidth_acc,twidth_sig
      parameter ( twidth_acc = 300.0 ) ! 2400-2100[ch]
      parameter ( twidth_sig = 200.0 ) ! 2800-2600[ch]
      real*4 calc_etrackingEff
      real*4 edc_tracking_eff
      
c     ========= Initialize ===============
      edc_eff_ = 1.0
      edc_eff1 = 1.0
      edc_eff2 = 1.0
      anaEV    = gen_run_stopping_event
      do layer=1,16
         edc_rate_acc(layer) = -22222.0
         edc_rate_sig(layer) = -22222.0         
      enddo
      edc_tracking_eff = 2.0

c     ========= Calculate efficiencies =============
c     ---- EDC1 ----
      do layer=1,10
         edc_eff(layer) = edc_eff_hit(layer) / edc_eff_gtrig
         edc_eff1 = edc_eff1 * edc_eff(layer)
      enddo
c     ---- EDC2 ----
      do layer=11,16
         edc_eff(layer) = edc_eff_hit(layer) / edc_eff_gtrig
         edc_eff2 = edc_eff2 * edc_eff(layer)
      enddo
c     ---- EDC ----
      edc_eff_ = edc_eff1 * edc_eff2 ! ( 16/16 hits required )

c     ---- EDC tracking efficiency----
      edc_tracking_eff = calc_etrackingEff(edc_eff)

c     ========= Calculate Rates =============  (Switched off now, T.Gogami, 12/Jan/2012)
c      do layer=1,16
c         edc_rate_acc(layer) 
c     &        = hdc_rate_hit_acc(layer)/anaEV
c     &        /(twidth_acc*edc_tdc_time_per_channel)*1000000000.
c         edc_rate_sig(layer) 
c     &        = hdc_rate_hit_sig(layer)/anaEV
c     &        /(twidth_sig*edc_tdc_time_per_channel)*1000000000.
c      enddo

c     ======== Display efficiencies ==============
      if( dispflag.eq.1 )then
         write(*,*) "=========== EDC Plane Efficiencies ============="
         do layer=1 , 12
            write(*,*) "    ",layer," : ",hdc_eff(layer)
         enddo
         write(*,*) "  EDC1  : ",edc_eff1
         write(*,*) "  EDC2  : ",edc_eff2
         write(*,*) "  Tracking Eff. : ",edc_tracking_eff
c         write(*,*) "    "
      endif

c     ======== Output efficiencies to file ==========
      write(run,'(I5)')gen_run_number
      Keff_filename = './scalers/EDCeff/edc_eff_'//run//'.dat'
      open(23,file=Keff_filename,status='replace')
      write(23,*)"RUN: ",gen_run_number
      write(23,*)"START: ",gen_run_starting_event
      write(23,*)"ANALnum: ",gen_run_stopping_event
      write(23,*)"DENO: ",edc_eff_gtrig ! Denominaor of efficiency calc.
      write(23,*)"====EDC_efficiency====="
      do layer=1,16
         write(23,*)layer,": ",edc_eff(layer)
      enddo
      write(23,*) "EDC1: ",edc_eff1
      write(23,*) "EDC2: ",edc_eff2
      write(23,*) "Tracking Eff: ",edc_tracking_eff
c      write(23,*) "Total: ",edc_eff
      write(23,*)"====EDC_Rates(accicental,signal)====="
      do layer=1,16
         write(23,*)layer,": ",edc_rate_acc(layer)
     &        ," ",edc_rate_sig(layer)
      enddo
      close(23)

c     ==========  END  =================
      return
      end subroutine

**********************************************************************

c      real*4 function calc_trackingEff(ef1,ef2,ef3,ef4,ef5,ef6
c     &     ef7,ef8,ef9,ef10,ef11,ef12)
      real*4 function calc_etrackingEff(eff)
      
      implicit none
      real*4 tttt
      real*4 eff(10)
      real*4 eff66
      real*4 eff65
      real*4 eff65_cal(10)
      real*4 eff55
      real*4 eff55_1(10)
      real*4 eff55_2(10)
      
      integer*4 i,j,k
      integer*4 a(10)
      integer*4 flag        ! Flag
      integer*4 combo10     ! The number of combination
      parameter(combo10=45) ! The number of combination( {10}_C_{2} )
      real*4 eff55_3(combo10)
      integer*4 count
      
      eff66 = 1.0
      eff65 = 0.0
      eff55 = 0.0
      do i=1,10
         eff65_cal(i) = 1.0
         eff55_1(i) = 1.0
         eff55_2(i) = 1.0
      enddo
c     ================ Probabirity of 10/10 ==============
      do i=1,10
         eff66 = eff66 * eff(i)
      enddo


c     ================= Probabirity of 9/10 ===============
      do i=1,10
         do j=1,10
            if( j.eq.i )then
               eff65_cal(i) = eff65_cal(i) * ( 1.0-eff(j) )
            else
               eff65_cal(i) = eff65_cal(i) * eff(j)   
            endif
         enddo
c         write(*,*)"EFF(",i,")===",eff65_cal(i)
         eff65 = eff65 + eff65_cal(i)
      enddo
c      write(*,*) "EFF65 = ",eff65


c     ============== Probabirity of 8/10 ===================
c     --EDC1:(9/10)--  (no need, T.Gogami, 12/Jan/2012)----
c      do i=1,10
c         do j=1,10
c            if( j.eq.i)then
c               eff55_1(i) = eff55_1(i) * (1.0 - eff(j))
c            else
c               eff55_1(i) = eff55_1(i) * eff(j)
c            endif
c         enddo
c      enddo
c     --EDC2:(5/6)--  (no need, T.Gogami, 12/Jan/2012)----
c      do i=11,16
c         do j=1,6
c            if( j+10.eq.i )then
c               eff55_2(i-10) = eff55_2(i-10) * (1.0 - eff(j+10))
c            else
c               eff55_2(i-10) = eff55_2(i-10) * eff(j+10)
c            endif
c         enddo
c      enddo
c     ----------------------------------------------------
c     --Initialize--
      do i=1,10
         a(i) = -2222
      enddo
      flag = 0
      do i=1,combo10
         eff55_3(i) = 1.0
      enddo
      count=0
      
c      ---- Calculation for 8/10 ----
      do i=1,10
         a(i) = i
         do j=1,10
            if(i.ne.j)then
               flag = 1
               do k=1,10
                  if( a(k).eq.j )then
                     flag=0
                  endif
               enddo
               if( flag.eq.1 )then
c                  write(*,*) "=======",count,i,",",j
                  count = count+1
                  eff55_3(count) = eff55_3(count) *
     &                 ( 1.0 - eff(i) ) *
     &                 ( 1.0 - eff(j) )
                  do k=1,10
                     if(k.ne.i .and. k.ne.j)then
c                        write(*,*) k
                        eff55_3(count) = eff55_3(count)*eff(k)
                     endif
                  enddo
c                  write(*,*) eff55_3(count)
               endif
            endif
         enddo
      enddo
      

c     ==============================================================
c     ---- commented out (T.Gogami,12/Jan/2012)----
c      do i=1,10
c         do j=1,6
c            eff55 = eff55 + eff55_1(i) * eff55_2(j)
c         enddo
c      enddo

c     ---- commented out (T.Gogami,13/Jan/2012)----
c      do i=1,10
c         do j=1,count
c            eff55 = eff55_1(i) + eff55_3(j) ! wrong!!!
c         enddo
c      enddo
     
      do j=1,count
         eff55 = eff55 + eff55_3(j)
      enddo

      write(*,*) " ======== EFF55 = ",eff55
      write(*,*) " ======== EFF65 = ",eff65
      write(*,*) " ======== EFF66 = ",eff66
      write(*,*) " ======== EFF   = ",eff66 + eff65 + eff55

c     --Final calculaion of tracking efficiency--
      calc_etrackingEff = eff66 + eff65 + eff55
      
      return
      end function
