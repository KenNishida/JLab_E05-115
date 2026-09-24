      INTEGER*4 FUNCTION g_decode_fb_detector(oslot,roc,evfrag,length,did,
     $     maxhits,hitcount,planelist,counterlist,signalcount,signal0,
     $     signal1,signal2,signal3)
*----------------------------------------------------------------------
*     - Created ?   Steve Wood, CEBAF
*     - Corrected  3-Dec-1993 Kevin Beard, Hampton U.
*     
*     Revision 2.0	 1999/07/16 11:00:00  jinghua
*     
*     Revision 1.20  1998/12/17 21:50:31  saw
*     Support extra set of tubes on HMS shower counter
*     
*     Revision 1.19  1998/12/01 15:54:57  saw
*     (SAW) Slight change in debugging output
*     
*     Revision 1.18  1997/04/03 10:56:05  saw
*     (SAW) Better report of DCFE code words.  Prints out roc, slot, event
*     number and how many extra events are in the module.
*     
*     Revision 1.17  96/09/04  14:34:19  14:34:19  saw (Stephen A. Wood)
*     (JRA) More error reporting of error codes in FB data stream
*     
*     Revision 1.16  1996/04/29 19:46:19  saw
*     (JRA) Tweak diagnostic messages
*     
*     Revision 1.15  1996/01/16 20:51:55  cdaq
*     (SAW) Fixes:  Forgot why
*     
*     Revision 1.14  1995/11/28 18:59:24  cdaq
*     (SAW) Change arrays that use roc as index to start with zero.
*     
*     Revision 1.13  1995/10/09 18:23:29  cdaq
*     (JRA) Comment out some debugging statements
*     
*     Revision 1.12  1995/07/27 19:10:02  cdaq
*     (SAW) Use specific bit manipulation routines for f2c compatibility
*     
*     Revision 1.11  1995/01/31  15:55:52  cdaq
*     (SAW) Make sure mappointer and subaddbit are set on program entry.
*     
*     Revision 1.10  1995/01/27  20:14:04  cdaq
*     (SAW) Add assorted diagnostic printouts.  Add hack to look for the headers
*       on new 1881M/1877 modules while maintaining backward compatibility.
*     
*     Revision 1.9  1994/10/20  12:34:55  cdaq
*     (SAW) Only print out "Max exceeded, did=" meesage once
*     
*     Revision 1.8  1994/06/27  02:14:18  cdaq
*     (SAW) Ignore all words that start with DC
*     
*     Revision 1.7  1994/06/22  20:21:24  cdaq
*     (SAW) Put -1 in hodoscope signals that don't get any data
*     
*     Revision 1.6  1994/06/22  20:07:37  cdaq
*     (SAW) Fix problems with filling of hodoscope type hit lists (multiple signal)
*     
*     Revision 1.5  1994/06/21  16:02:54  cdaq
*     (SAW) Ignore DCFF0000 headers from Arrington's CRL's
*     
*     Revision 1.4  1994/06/18  02:48:04  cdaq
*     (SAW) Add code for miscleaneous data and uninstrumented channels
*     
*     Revision 1.3  1994/04/06  18:03:38  cdaq
*     (SAW) # of bits to get channel number is now configurable (g_decode_subaddbit).
*     Changed range of signal types from 1:4 to 0:3 to agree with documentation.
*
*     Revision 1.2  1994/03/24  22:00:15  cdaq
*     Temporarily change shift to get subaddress from 17 to 16
*     
*     Revision 1.1  1994/02/04  21:50:03  cdaq
*     Initial revision
*     
*----------------------------------------------------------------------
      implicit none
      SAVE
*     
*     The following arguments don't get modified.
      integer*4 roc,evfrag(*),length,did,maxhits,signalcount
      
*     The following arguments get modified.
      integer*4 oslot
      integer*4 buffer
      integer*4 hitcount,planelist(*),counterlist(*)
      integer*4 signal0(*),signal1(*),signal2(*),signal3(*)
      integer pointer,newdid,subadd,slot,mappointer,plane
      integer counter,signal,sigtyp
*     
      include 'gen_decode_common.cmn'
      include 'gen_detectorids.par'
      include 'gen_scalers.cmn'
      include 'gen_event_info.cmn'
      include 'gen_rocid.cmn'

      integer iscaler,nscalers
*     
      integer h,hshift
      integer subaddbit
      logical printerr          !flag to turn off printing of error after 1 time.
      logical firsttime
*     
      printerr = .true.
      pointer = 1
      newdid = did

      firsttime = .true.
      do while(pointer.le.length .and. did.eq.newdid)
*     
*     VME/FB event length mismatch
        if(iand(evfrag(pointer),INT(Z'FFFFFFFF')).EQ.INT(Z'DCAA0000')) then 
          write(6,'(a,i10)') 
     &         'ERROR: VME/Fastbus event length mismatch for event #',
     &         gen_event_id_number
          write(6,'(a,z9,a,z9,a)') '   Fastbus event length:',
     &         evfrag(pointer+1),
     &         ' VME event length:',evfrag(pointer+2),
     &         ' (or vice-versa).'
          pointer = pointer + 3
          goto 987

!     Check for extra events in FB modules on sync events
        else if(iand(evfrag(pointer),INT(Z'FFFF0000')).EQ.INT(Z'DCFE0000')) then
          write(6,'(a,i2,a,i3,a,i3,a,i10)') 'ROC',roc,': Slot'
     $         ,iand(ishft(evfrag(pointer),-11),INT(Z'1F')),': '
     $         ,iand(evfrag(pointer),INT(Z'7FF')),' extra events, event=',
     &         gen_event_id_number
          pointer = pointer + 1
          goto 987
*     Catch arrington's headers
        else if(iand(evfrag(pointer),INT(Z'FF000000')).EQ.INT(Z'DC000000')) then
          write(6,'(a,i2,a,i10,a,z10)') 'ROC',roc,
     &         ': no gate or too much data, event=',
     &         gen_event_id_number,' error dataword=',evfrag(pointer)
          pointer = pointer + 1
          goto 987
        endif
        
*     
*     Check for event by event scalers thrown in by the scaler hack.
*     
*     if(iand(evfrag(pointer),INT(Z'FF000000')).eq.INT(Z'DA000000')) then ! Magic header
*     nscalers = iand(evfrag(pointer),INT(Z'FF'))
*     do iscaler=1,nscalers
*     evscalers(iscaler) = evfrag(pointer+iscaler)
*     enddo
*     pointer = pointer + nscalers + 1
*     goto 987
*     endif
        
        if(evfrag(pointer).le.1.and.evfrag(pointer).ge.0) then
          
!     on sync events, get zeros at end of event.
          if (gen_event_id_number .eq. 
     &         1000*int(gen_event_id_number/1000)) then
            if (evfrag(pointer).ne.0.and.roc.ne.5) then
              
              Write(6,*) 'Error: Bad FB value. ROC=',roc,' Event=',
     &             gen_event_id_number
              Write(6,'(7Hevflag(,i4,2H)=,z10)') pointer,
     &             evfrag(pointer)
c     write(6,*) 
c     &             '(22HERR.:BAD FB val. evf.(,i4,2H)=,z10,5H ROC=,i2,6Hevent=,i7)')
c     $             pointer,evfrag(pointer),roc,gen_event_id_number
            endif
          endif
          pointer = pointer + 1
          goto 987
        endif
        slot = iand(ishft(evfrag(pointer),-27),INT(Z'1F'))
c        write(*,*)"pointer=",pointer
c        write(*,*)"slot=",slot
        if(roc.eq.11) then 
            slot=slot+1 ! for roc11
        endif

        if(slot.ne.oslot.or.firsttime) then
          if (slot.lt.0 .or. slot.gt.G_DECODE_MAXSLOTS 
     &         .or. roc.le.0 .or. roc.ge.G_DECODE_MAXROCS) then
            write (6,'(a,i2,i3,z10,a,i5,a,i8)') 'roc,slot,evfrag=',roc,
     &           slot,evfrag(pointer),
     $           '(p=',pointer,') for event #',gen_event_id_number
            write (6,'(a,i3)') '  Probably after slot',
     $           iand(ishft(evfrag(pointer-1),-27),INT(Z'1F'))
            pointer = pointer + 1
            goto 987
          else
            mappointer = g_decode_slotpointer(roc,slot)
            subaddbit = g_decode_subaddbit(roc,slot) ! Usually 16 or 17
          endif
        endif

c     Write(*,*) 'in fb detector, slot=',slot, 
c     &       ' mappointer=',mappointer,
c     &       ' subaddbit=',subaddbit
        
        if(slot.ne.oslot) then
          oslot = slot
          
c     
c     On 1881M's and 1877, a subaddress of zero could be a header word, so
c     we need to put in some hackery to catch these.  We need to make sure
c     that 1881's and 1876's will still work.
c     
c     A real ugly hack that looks to see if the first word of an 1881M or
c     1877 has a subaddress of zero, in which case it is the header word and must
c     be discarded.  If it is an 1881 or 1876, then the the first word of a
c     new slot will have a subaddress of '7F' and later be discarded.
c     
          if(subaddbit.eq.17) then ! Is not an 1872A (which has not headers)
            if(iand(evfrag(pointer),INT(Z'00FE0000')).eq.0) then ! probably a header
              if(iand(evfrag(pointer),INT(Z'07FF0000')).ne.0) then
                write(*,'(z10)') iand(evfrag(pointer),INT(Z'07FF0000'))
                print *,"SHIT:misidentified real data word as a header"
                print *,"DID=",did,", SLOT=",slot,", POINTER=",pointer
                write(*,'(z10)') evfrag(pointer)
              else
                pointer = pointer + 1
                goto 987
              endif
            endif
          endif
        endif
*     
***********************
c     c     write(6,*) buffer
c     buffer = jiand(JISHFT`(evfrag(pointer),-24),'03'X)
c     if (g_decode_bufnum .ne. buffer) then
c     if (g_decode_bufnum.eq.-1) then 
c     g_decode_bufnum=buffer
c     else
c         if (roc.eq.11) then
c         write (6,*) 'g_decode_fb_detector: roc,slot='
c     &             ,roc,slot
c         endif
c     write (6,*) 'gen_event_id_number=',gen_event_id_number
c     c              stop
c     endif
c     endif
*************************
        subadd = iand(ishft(evfrag(pointer),-subaddbit),INT(Z'7F'))
c        if(roc.eq.5) then ! for HAPPEX
c          subadd = iand(ishft(evfrag(pointer),-subaddbit),'1'X)
c        endif
*     
*     If a module that uses a shift of 17 for the subaddress is in a slot
*     that we havn't told the map file about, it's data will end up in the
*     unstrimented channel "detector" hit list.  However, the decoder will
*     think that the subaddress starts in channel 16 (since some Lecroy
*     modules do so), The next statement will mean that only the first 64
*     channels will end up in the uninstrumented hit list.  The rest will
*     be lost.  If you don't want to put this module in the map file, put
*     in a single entry for it with a detector id of UNINST_ID (zero) and
*     the proper BSUB value.
*     
        if (subadd .lt. 255) then ! Only valid subaddresses
                                ! Skips headers for 1881 and 1876
          if(mappointer.gt.0) then
            newdid = g_decode_didmap(mappointer+subadd)
          else
            newdid = UNINST_ID
          endif
          if(newdid.eq.did) then
            if(did.ne.UNINST_ID) then
              plane = g_decode_planemap(mappointer+subadd)
              counter = g_decode_countermap(mappointer+subadd)
              signal =iand(evfrag(pointer),g_decode_slotmask(roc,slot))
c              if(roc.eq.5) then ! HAPPEX
c                signal = iand(ishft(evfrag(pointer),-12),
c     >               g_decode_slotmask(roc,slot))
c              endif
            else
              plane = ishft(roc,16) + slot
              counter = subadd
              signal = evfrag(pointer)
            endif
            if(hitcount .lt. maxhits 
     $           .or.(hitcount .eq. maxhits .and. 
     $           signalcount .gt. 1)) then ! Don't overwrite arrays
              
              if(signalcount .le. 1
     &           .and.((did.eq.1.and.signal.gt.2000.and.signal.lt.3000).or.
c     &           .and.((did.eq.1.and.signal.gt.2300.and.signal.lt.2900).or.
     &            (did.eq.11.and.signal.gt.3000.and.signal.lt.4000).or.
     &            (did.eq.12.and.signal.gt.3000.and.signal.lt.4000))
     &           ) then ! single signal counter
*     
*     Starting at end of hit list, search back until a hit earlier in
*     the sort order is found.
*     
                h = hitcount
                do while(h .gt. 0 .and. (plane .lt. planelist(h)
     $               .or.(plane .eq. planelist(h).and. counter .lt.
     $               counterlist(h))))
*     
*     Shift hit to next place in list
*     
                  planelist(h+1) = planelist(h)
                  counterlist(h+1) = counterlist(h)
                  signal0(h+1) = signal0(h)
                  h = h - 1
                enddo
                h = h + 1       ! Put hit pointer to blank
                planelist(h) = plane
                counterlist(h) = counter
                signal0(h) = signal
                hitcount = hitcount + 1
              elseif (signalcount.gt.1) then        
! Multiple signal counter sigcount= 2 or 3 or 4 allowed
*     Note:
*     
*     sigcount = 2 : 2 ADC data (SCER etc.)
*     sigcount = 3 : one ADC and one TDC (KSCIN)
*     sigcount = 4 : 2 ADC and 2 TDC
                
*     
*     Starting at the end of the hist list, search back until a hit on
*     the same counter or earlier in the sort order is found.
*     
                h = hitcount
                do while(h .gt. 0 .and. (plane .lt. planelist(h)
     $               .or.(plane .eq. planelist(h).and. counter .lt.
     $               counterlist(h))))
                  h = h - 1
                enddo
*     
*     If plane/counter match is not found, then need to shift up the array
*     to make room for the new hit.
*     
                if(h.le.0 .or. plane.ne.planelist(h) ! Plane and counter
     $               .or.counter.ne.counterlist(h)) then ! not found
                  if(hitcount.lt.maxhits) then
                    h = h + 1
                    do hshift=hitcount,h,-1 ! Shift up to make room
                      planelist(hshift+1) = planelist(hshift)
                      counterlist(hshift+1) = counterlist(hshift)
                      if(signalcount.eq.2) then
                        signal0(hshift+1) = signal0(hshift)
                        signal1(hshift+1) = signal1(hshift)
                      else if(signalcount.eq.3) then
                        signal0(hshift+1) = signal0(hshift)
                        signal2(hshift+1) = signal2(hshift)
                      else if(signalcount.eq.4) then
                        signal0(hshift+1) = signal0(hshift)
                        signal1(hshift+1) = signal1(hshift)
                        signal2(hshift+1) = signal2(hshift)
                        signal3(hshift+1) = signal3(hshift)
                      endif
                    enddo
                    planelist(h) = plane
                    counterlist(h) = counter

                    if(signalcount.eq.2) then
                      signal0(h) = -1
                      signal1(h) = -1
                    else if(signalcount.eq.3) then
                      signal0(h) = -1
                      signal2(h) = -1
                    else if(signalcount.eq.4) then                  
                      signal0(h) = -1
                      signal1(h) = -1
                      signal2(h) = -1
                      signal3(h) = -1
                    endif
                    hitcount = hitcount + 1
                  else          ! Too many hits
                    if(printerr) then
                      print *,'g_decode_fb_detector: Max exceeded, did=',
     $                     did,', max=',maxhits,': event',gen_event_id_number
                      print *,'   roc,slot,cntr,sig,subadd=',roc,slot,counter,sigtyp,subadd
                      printerr = .false.
                    endif
                  endif
                endif
*     
                sigtyp = g_decode_sigtypmap(mappointer+subadd)
*     
                if(sigtyp.eq.0) then
                  signal0(h) = signal
                else if (sigtyp.eq.1) then
                  signal1(h) = signal
                else if (sigtyp.eq.2) then
                  signal2(h) = signal
                else if (sigtyp.eq.3) then
                  signal3(h) = signal
                endif
              endif
            else if(hitcount.eq.maxhits .and. printerr) then ! Only print this message once
              print *,'g_decode_fb_detector: Max exceeded, did=',
     $             did,', max=',maxhits,': event',gen_event_id_number
              print *,'   roc,slot,cntr=',roc,slot,counter
              printerr = .false.
*     
*     Print/generate some kind of error that the hit array has been
*     exceeded.
*     
            endif
            pointer = pointer + 1
*         else
*           exit and get called back with the correct arrays for the new did
          endif
        else
          pointer = pointer + 1
        endif
 987    continue
      enddo

      g_decode_fb_detector = pointer - 1 ! Number of words processed
      
      return
      end
**************
*     Local Variables:
*     mode: fortran
*     fortran-if-indent: 2
*     fortran-do-indent: 2
*     End:
