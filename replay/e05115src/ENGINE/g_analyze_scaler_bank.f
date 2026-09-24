      subroutine g_analyze_scaler_bank(event,ABORT,err)
*     
*     Revision 2.0  1999/07/15 10:26:00  jinghua
*     (JLiu) Change HMS to HNSS
*
*     Revision 1.1  1999/02/24 15:19:14  saw
*     Bring into CVS tree
*

      implicit none
      save
      integer*4 event(*)
*     
      character*17 here
      parameter (here='g_analyze_scaler_bank')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'gen_scalers.cmn'
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_filenames.cmn'
*     
      integer ind
      integer*4 cratenum        ! 1=TS, vmec9, hnss,2=sos
      real*8 realscal
      logical update_bcms
      logical update_helicity_bcms
*     
      integer*4 jiand, jishft, jieor   ! Declare to help f2c
*     
*     Scaler events have a header in from of each scaler.  High 16 bits
*     will contain the address (the switch settings).  Address for hall C
*     will be of the form DANN, where NN is the scaler number.  The low 16
*     bits will contain the number of scaler values to follow (this should
*     be no larger than 16, but we will allow more.)
*     
*     
*     NOTE that the variables gscaler(i) is REAL!!!!!
*     this is so that we can record the correct value when the 
*     hardware scalers (32 bit <> I*4) overflow.
*     

      integer evtype, evnum, evlen, pointer
      integer scalid, countinmod, address, counter
      integer roc
*      integer ROCoffset(3)/0, 1024, 2048/
      integer ROCoffset(3)/0, 0, 1024/
*     
*     Temporary variables for beam current and charge calculations
*     
      real*8 ave_current_bcm1, ave_current_bcm2, ave_current_bcm3
      real*8 ave_current_unser
      real*8 delta_time
*     
*     Find if hnss or sos scaler event (assumes first HNSS scaler is DA01).
*     write(6,'("Scaler event: event(3)=",9z)') event(3)
      roc = iand(ishft(event(2),-16),INT(Z'1F'))
      if (roc.eq.20) then
        cratenum=3              !LeCroy - TS Crate
      else if (roc.eq.14) then
        cratenum=2              ! VMEC14
      else
        cratenum=1              ! VMEC13 
      endif
*     
      evtype = jishft(event(2),-16)
      evnum = jiand(event(2),INT(Z'FF')) ! last 2 bytes give event number (mod 256)
*     
      gscal_lastevnum(cratenum)=evnum
*     
*     Should check against list of known scaler events
*     
      evlen = event(1) + 1
      update_bcms = .false.
      update_helicity_bcms = .false.

      if(evlen.gt.3) then       ! We have a scaler bank
        pointer = 3
*     
        do while(pointer.lt.evlen)
*     
          scalid = jiand(jishft(event(pointer),-16),INT(Z'FF'))
          countinmod = jiand(event(pointer),INT(Z'FFFF'))
          
*          Get starting address from header

          address = ishft(event(pointer),-16)+ROCoffset(cratenum)
*     
*     Might want to check that count is not to big.
*     
          if(countinmod.gt.32) then
            err = 'Scaler module header word has count >32'

CC MIZUKI change tempolary
            write(*,*) err
ccc            ABORT = .true.

            call g_add_path(here,err)
            return              ! Safest action
          endif
*     
          do counter = 1,countinmod
            ind=address+counter
            realscal=dfloat(event(pointer+counter))
            if (ind.eq.gbcm1_index) update_bcms=.true. !assume bcms in same crate
c     
c     For Gen (7/1998) it is noticed that some scaler channels are randomly
c     clearing themselves.  The following is a lame hack to detect and correct
c     this random clearing.
c     
            if(gscalweirdcorrect_flag.eq.1) then ! Look for random clears
              if(event(pointer+counter).gt.0) then ! Really saying < 2**31
                if(gscalweird_lastval(ind).gt.event(pointer+counter)) then
                  gscalweird_nclears(ind) = gscalweird_nclears(ind) + 1
                  gscalweird_lostcounts(ind) = gscalweird_lostcounts(ind)
     $                 +gscalweird_lastval(ind)
c     
c     If the longint scaler value is negative, but close to 0 (which means that
c     it is really >> 2**31), then we will get to a point where we can't tell the
c     difference between an overflow and a random clear. But if a channel is
c     getting random clears too much, then the channel will never get to 2**31
c     anyway.  For now, let's assume that nothing counts faster than 5Mhz and that
c     scalers are read out every 5 seconds, and then round that up to
c     2^25=33554432.  So if a scaler apparantly overflows, but jumps by more than
c     2^25, we assume it was a false clear.  We could be cleverer and use some
c     intellegence to characterize the typical rate of each channel, but let's
c     not get carried away.
c     
                else if (gscalweird_lastval(ind).lt.0 .and.
     $                 (event(pointer+counter)-gscalweird_lastval(ind))
     $                 .gt.33554432) then
                  gscalweird_nclears(ind) = gscalweird_nclears(ind) + 1
                  gscalweird_lostcounts(ind) = gscalweird_lostcounts(ind)
     $                 + gscalweird_lastval(ind)
                endif
              endif
              gscalweird_lastval(ind) = event(pointer+counter)
            endif
*     Save scaler value from previous scaler event:

*     write(101,*) 'scaler index=',ind
            gscaler_old(ind) = gscaler(ind)

            if (realscal.lt.-0.5) then
              realscal=realscal+4294967296.
            endif
            if ( (realscal+dfloat(gscaler_nroll(ind))*4294967296.
     $           +gscalweird_lostcounts(ind)) .lt. gscaler(ind) ) then
                                ! 2**32 = 4.295e+9
                                !32 bit scaler rolled over.
              gscaler_nroll(ind)=gscaler_nroll(ind)+1
            endif
            gscaler(ind) = realscal + gscaler_nroll(ind)*4294967296.
     $           + gscalweird_lostcounts(ind)
*     Calculate difference between current scaler value and previous value:
            gscaler_change(ind) = gscaler(ind) - gscaler_old(ind)
          enddo
          pointer = pointer + countinmod + 1 ! Add 17 to pointer
        enddo
      else
c     err = 'Event not big enough to contain scalers'
c     ABORT = .true.
c     call g_add_path(here,err)
c     
c     Not all banks will have scaler data every event.  Don't generate the
c     error any more.  (saw 20.6.1998)
c     
        return
      endif
*     
*     calculate time of run (must not be zero to avoid div. by zero).
      g_run_time = max(0.001D00,gscaler(gclock_index)/gclock_rate)

*     Calculate beam current and charge between scaler events

      if (update_bcms) then

        delta_time = max(gscaler_change(gclock_index)/gclock_rate,.0001D00)

c     djm        ave_current_bcm1 = gbcm1_gain*sqrt(max(0.0D00,
c     &       (gscaler_change(gbcm1_index)/delta_time)-gbcm1_offset))
        ave_current_bcm1 = gbcm1_gain*((gscaler_change(gbcm1_index)
     &       /delta_time) - gbcm1_offset)
        ave_current_bcm3 = gbcm3_gain*((gscaler_change(gbcm3_index)
     &       /delta_time) - gbcm3_offset)
        ave_current_unser = gunser_gain*((gscaler_change(gunser_index)
     &       /delta_time) - gunser_offset)
        ave_current_bcm2 = gbcm2_gain*((gscaler_change(gbcm2_index)
     &       /delta_time) - gbcm2_offset)

        if (delta_time.gt.0.0001 .and. delta_time.lt.3.) then
           if(ave_current_bcm1.ge.0.0) then
              gbcm1_charge = gbcm1_charge + ave_current_bcm1*delta_time
           endif
           if(ave_current_bcm2.ge.0.0) then
              gbcm2_charge = gbcm2_charge + ave_current_bcm2*delta_time
           endif
           if(ave_current_bcm3.ge.0.0) then
              gbcm3_charge = gbcm3_charge + ave_current_bcm3*delta_time
           endif
           if(ave_current_unser.ge.0.0) then
              gunser_charge = gunser_charge + ave_current_unser*delta_time
           endif
c           if(ave_current_bcm1.ge.0.0 .and. ave_current_bcm2.ge.0.0) then
c              gbcm_mean_charge = gbcm_mean_charge 
c     &             + (ave_current_bcm1+ave_current_bcm2)/2.*delta_time
c           endif
           gbcm_mean_charge = (gbcm1_charge+gbcm2_charge)/2.

*     
*     Check for the "beam on" condition, and update "beam on" variables if needed.
*     
*     We'll use bcm1 for now as it's zero seems more stable.  This could change.
*     
*     write(6,*) "Checking threshold..."
          if (ave_current_bcm1 .ge. g_beam_on_thresh_cur) then
            g_beam_on_run_time = g_beam_on_run_time + delta_time
            g_beam_on_bcm_charge = g_beam_on_bcm_charge
     $           + ave_current_bcm1*delta_time
*     write(6,*) "above threshold (",ave_current_bcm1,")"
          endif
*     
          gscaler_event_num = gscaler_event_num + 1

*     Write out pertinent charge scaler rates for each scaler event.

          if (g_charge_scaler_filename.ne.' ') then
            write(G_LUN_CHARGE_SCALER,1001) 
     &            gscaler_event_num, !scaler event num
     &            g_beam_on_run_time,
     &            gunser_charge,
     &            gbcm1_charge,
     &            gbcm2_charge,
     &            gbcm_mean_charge
c     &           gscaler_change(gunser_index)/delta_time, !scaler rate(Hz)
c     &           gscaler_change(gbcm1_index)/delta_time, !scaler rate(Hz)
c     &           gscaler_change(gbcm2_index)/delta_time, !scaler rate(Hz)
c     &           gscaler_change(gbcm3_index)/delta_time, !scaler rate(Hz)
c     &           delta_time     !time since last scaler event (sec)
          endif
          
        endif

      endif


*     
 1001 format(i6,5f13.3)

      return
      end
