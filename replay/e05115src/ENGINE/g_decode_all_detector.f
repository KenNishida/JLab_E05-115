      INTEGER*4 FUNCTION g_decode_all_detector(oslot,roc,evfrag,length,
     $     did,vmemask1,vmemask2,
     $     maxhits,hitcount,planelist,counterlist,signalcount,signal0,
     $     signal1,signal2,signal3)
*----------------------------------------------------------------------
*     - Created ?   Steve Wood, CEBAF
*     - Corrected  3-Dec-1993 Kevin Beard, Hampton U.
*     
*     add vmemask1=DA,vmemask2=ADC,DDC,or,F1C to identify module type.
*     T. Miyoshi, 08/09/2004
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
      integer*4 vmemask1,vmemask2
      
*     The following arguments get modified.
      integer*4 oslot
      integer*4 buffer
      integer*4 hitcount,planelist(*),counterlist(*)
      integer*4 signal0(*),signal1(*),signal2(*),signal3(*)
      integer pointer,newdid,subadd,slot,mappointer,plane
      integer counter,signal,sigtyp
      integer*4 mask1,mask2,mask3,mask4,mask5,mask6
      Integer*4 i,j,multi,old_multi,dup
      real*4 ttime
*     
      include 'gen_decode_common.cmn'
      include 'gen_detectorids.par'
      include 'gen_scalers.cmn'
      include 'gen_event_info.cmn'
      include 'gen_rocid.cmn'
      include 'gen_f1tdc.cmn'
      integer iscaler,nscalers,exitflag
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

*     --- rocID check. if the roc is VME, vmemasks are set.
      
      if(roc .eq. HR_VME_ROCID) then
        vmemask1 = INT(Z'DA')
        vmemask2 = INT(Z'F1C')
      EndIf
      if(roc .eq. LR_VME_ROCID) then
        vmemask1 = INT(Z'DA')
        vmemask2 = INT(Z'F1C')
      EndIf
      
*     -----------------------------------------------
*     loop with the same did=detector id
*     -----------------------------------------------
      do while(pointer.le.length .and. did.eq.newdid)

        if(g_decode_debug .eq. 1) then
          Write(*,'(16H(1)ev,p,val,len=,I6,X,I6,X,Z9,I7)') 
     &         gen_event_ID_number,
     &         pointer,evfrag(pointer),length
        EndIf
        
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
          write(*,*)"did=",did
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
        
c     ---- end of detector test routine

        if(slot.ne.oslot.or.firsttime) then
          if (slot.le.0 .or. slot .gt. G_DECODE_MAXSLOTS 
     &         .or. roc.le.0 .or. roc .gt. G_DECODE_MAXROCS) then
            
            if(.NOT.(vmemask1 .eq. INT(Z'DA') .and. vmemask2 .eq. INT(Z'F1C')) 
     &           .and. 
     &           .NOT.(vmemask1 .eq. INT(Z'DA') .and. vmemask2 .eq. INT(Z'ADC'))) 
     &           then
              write (6,'(a,i2,i3,z10,a,i5,a,i8,a,i3)') 'roc,sl,evfrag=',
     &             roc,slot,evfrag(pointer),
     $             '(p=',pointer,') for ev#',gen_event_id_number,
     &             ' probably after sl ',
     $             iand(ishft(evfrag(pointer-1),-27),INT(Z'1F'))
              write(*,*) "roc,did=",roc,did
            EndIF
c     Write(*,*) 's=0 len=',length, ' p=',pointer
            pointer = pointer + 1
            goto 987
          else
            mappointer = g_decode_slotpointer(roc,slot)
            subaddbit = g_decode_subaddbit(roc,slot) ! Usually 16 or 17
          endif
        endif
        
        if(g_decode_debug .eq. 1) then
          Write(*,*) '(2)did,roc,sl,mp,sa= ',did,roc,slot, 
     &         mappointer,
     &         subaddbit
        EndIf
        
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
c     write (6,*) 'g_decode_fb_detector: roc,slot,buffer='
c     &             ,roc,slot,buffer,'but previous data was buffer=',
c     &             g_decode_bufnum
c     write (6,*) 'gen_event_id_number=',gen_event_id_number
c     c              stop
c     endif
c     endif
*************************
        
        if(vmemask1 .ne. INT(Z'DA')) then ! not VME
          subadd = iand(ishft(evfrag(pointer),-subaddbit),INT(Z'7F'))
        Else                    ! VME
*     --- header check ---
*     if this is header, subadd = 1000 to do pointer+1.
*     For f1TDC, the 24th bit is 0 for header/trailer word
*     and 1 for data.
          mask1 = ishft(iand(evfrag(pointer),INT(Z'FF000000')),-24)
          mask2 = ishft(iand(evfrag(pointer),INT(Z'0000FFF0')),-4)
          mask3 = ishft(iand(evfrag(pointer),INT(Z'06000000')),-25)
          mask4 = ishft(iand(evfrag(pointer),INT(Z'00800000')),-23)
*     if module type is changed, update vmemask2.
          if(mask1 .eq. INT(Z'DA') .and. 
     &         (mask2 .eq. INT(Z'DDC') .or. mask2 .eq. INT(Z'ADC')
     &         .or. mask2 .eq. INT(Z'F1C'))) vmemask2 = mask2
          if(mask1 .eq. INT(Z'DA') .or. 
     &         ( (vmemask2 .eq. INT(Z'DDC') .or. vmemask2 .eq. INT(Z'ADC')) 
     &         .and. 
     &         (mask3 .eq. INT(Z'1') .or. mask3 .eq. INT(Z'2')) ) 
     &         .or.
     &         ( vmemask2 .eq. INT(Z'F1C')
     &         .and. mask4 .eq. 0 )  ) 
     &         then             ! skip this header
            !trigger time definition DK
            ttime = iand(ishft(evfrag(pointer),-7),INT(Z'1FF'))
c            ttime = ttime*128+127
            ttime = ttime*128-127 ! Safety mergin for rollover analysis
            g_f1_trigger_time(roc) = ttime 
            subadd = 1000         
          Else                  ! not header
            if(vmemask2 .eq. INT(Z'F1C')) then
              subadd = ishft(iand(evfrag(pointer),INT(Z'380000')),-19) * 8
     &             + ishft(iand(evfrag(pointer),INT(Z'70000')),-16)
              
c     if(did .eq. HSCIN_ID .or. did .eq. ESCIN_ID .and.
c     &             ESCIN_F1TDC_MODE .eq. 1) then ! high resolution mode
c     subadd = int(subadd/2)
c     EndIf
              
            Else                ! VME ADC/TDC subaddbit=16
              subadd = iand(ishft(evfrag(pointer),-subaddbit),INT(Z'3F'))
            EndIf
          EndIF
        EndIF                   ! VME or not.
        
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
        
c     Write(*,'(6Z8)') mask1,mask2,mask3,mask4,vmemask1,vmemask2
c           Write(*,*) "ev,did,slot,subadd",gen_event_ID_number,did,slot,subadd
        
        if (subadd .lt. 255) then ! Only valid subaddresses
                                ! Skips headers for 1881 and 1876
          if(mappointer.gt.0) then
            newdid = g_decode_didmap(mappointer+subadd)
c            write(*,*) "mappointer,subadd",mappointer,subadd
c            write(*,*) "didmap",g_decode_didmap(mappointer+subadd)
          else
            newdid = UNINST_ID
          endif
          
          if(g_decode_debug .eq. 1) then
            Write(*,*) '(3) newdid,did,mp,sa=',newdid,did,
     &           mappointer, 
     &           subadd
          EndIf
          

          if(newdid.eq.did) then
            if(did.ne.UNINST_ID) then
              plane = g_decode_planemap(mappointer+subadd)
              counter = g_decode_countermap(mappointer+subadd)
              signal =iand(evfrag(pointer),g_decode_slotmask(roc,slot))
            else
              plane = ishft(roc,16) + slot
              counter = subadd
              signal = evfrag(pointer)
            endif
            
            if(g_decode_debug .eq. 1) then
              Write(*,*)
     &         '(3)ev,did,pl,co,sig=',gen_event_id_number,did,plane,counter,signal
            EndIf


c            write(*,*)"roc,slot,subadd",roc,slot,subadd
c            write(*,*)"did,plane,counter,sigtyp",did,plane,counter,sigtyp
*     ---- consider F1TDC.
            if(did.eq.6 .and. (plane.eq.3.or.plane.eq.4)) then
              old_multi = 0
              multi = 1
              Do while(old_multi .ne. multi 
     &             .and. multi .le. G_F1_MULTIHIT)
               old_multi = multi
c                if(g_f1_refer_time(roc,counter,multi) .ge. 0) then
c                  multi = multi + 1
c                Else
                  g_f1_refer_time(roc,1,multi) = signal
c                EndIf
              EndDo
             
              if(g_decode_debug .eq. 1) then
c                Write(*,*) '(4) roc, trig1=',roc,
c     &               g_f1_refer_time(roc,1,1)
                Write(*,*) '(4) roc, trig, counter=',roc,
     &               g_f1_refer_time(roc,counter,1),counter
              EndIf

              
c            Else if(
c     &             g_decode_filter_on .eq. 1 .and.
c     &             (did .eq. HAER_ID .or. did .eq. HWAT_ID) .and.
c     &             signal .gt. 8192
c     &             ) then
c     --- adc should not exceed 10000. So, if it is exceed 10000,
c     --- then it is MHTDC. However, in e01011 WC/AC tdc never exceed
c     --- 5us range. So, before setting MHTDC window correctly 
c     --- we don't need such events.
c     --- !!! do nothing !!! ---
            Else                ! not trigger time.              
              if(hitcount .lt. maxhits .or.
     $             (hitcount .eq. maxhits .and. 
cDK     $             signalcount .gt. 1)
     $             signalcount .ge. 1)
     $             ) then       ! Don't overwrite arrays
                
                if(signalcount .le. 1) then ! single signal counter
*     
*     Starting at end of hit list, search back until a hit earlier in
*     the sort order is found.
*     
                  h = hitcount
                  do while(h .gt. 0 .and. (plane .lt. planelist(h)
     $                 .or.(plane .eq. planelist(h) .and. counter .lt.
     $                 counterlist(h))))
*     
*     Shift hit to next place in list
*     
*     E01-011. HDC uses two crates. So, we need roc no for each hits.
                    
                    planelist(h+1) = planelist(h)
                    counterlist(h+1) = counterlist(h)
                    signal0(h+1) = signal0(h)
                    
*                    if(did .eq. 1 .and. 
*     &                   (roc .eq. 13 .or. roc .eq. 14))
*     &                   signal1(h+1) = signal1(h)
                    
                    h = h - 1
                  enddo
                  h = h + 1     ! Put hit pointer to blank
c                  if (plane.ge.7) print *, "pl,co",plane,counter
                  planelist(h) = plane
                  counterlist(h) = counter
                  signal0(h) = signal
*                  if(did .eq. 1 .and. 
*     &                 (roc .eq. 13 .or. roc .eq. 14))
*     &                 signal1(h) = roc
                  hitcount = hitcount + 1

                else            ! Multiple signal counter sigcount= 2 or 3 or 4 allowed
*     Note:
*     E89009 Hodoscope data contain one ADC 
*     and one TDC data. So special readout
*     sigcount = 3 is applied... (Y. Sato)
*     
*     sigcount = 2 : 2 ADC data (SCER etc.)
*     sigcount = 3 : one ADC and one TDC (KSCIN)
*     sigcount = 4 : 2 ADC and 2 TDC
                  
*     
*     Starting at the end of the hist list, search back until a hit on
*     the same counter or earlier in the sort order is found.
*     
c                  write(*,*)"multiple signal counter;hitcount=",hitcount
                  h = hitcount
                  do while(h .gt. 0 .and. (plane .lt. planelist(h)
     $                 .or.(plane .eq. planelist(h).and. counter .lt.
     $                 counterlist(h))))
                    h = h - 1
                  enddo
                  
                  if(g_decode_debug .eq. 1) then
                    Write(*,*) '(5) ev hit h s did',
     &                   gen_event_id_number,hitcount,h,signal,did
                  EndIf
                  
*     
*     If plane/counter match is not found, then need to shift up the array
*     to make room for the new hit.
*     
                  if(h.le.0 .or. plane.ne.planelist(h) ! Plane and counter
     $                 .or.counter.ne.counterlist(h)) then ! not found
                    
                    if(hitcount.le.maxhits) then
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
                      enddo     ! shift up to make room
                      planelist(h) = plane
                      counterlist(h) = counter
                      
                      if(signalcount.eq.2) then
                        signal0(h) = -1
                        signal1(h) = -1
                      else if(signalcount.eq.3) then
                        signal0(h) = -1
                        if(vmemask2 .eq. INT(Z'F1C') ) then !F1TDC
                          signal2(h) = -70000
                        Else
                          signal2(h) = -1
                        EndIf
                      else if(signalcount.eq.4) then                  
                        signal0(h) = -1
                        signal1(h) = -1
                        if(vmemask2 .eq. INT(Z'F1C') ) then !F1TDC
                          signal2(h) = -70000
                          signal3(h) = -70000
                        Else
                          signal2(h) = -1
                          signal3(h) = -1
                        EndIf
                      endif
                      hitcount = hitcount + 1

                      if(g_decode_debug .eq. 1) then
                        Write(*,*) '(6)',hitcount,h,planelist(h),
     &                       counterlist(h),signal0(h),
     &                       signal1(h),
     &                       signal2(h),signal3(h)
                      EndIF

                    else        ! Too many hits
                      if(printerr) then
                        Write(*,*) 'g_decode_all_detector',
     &                       ': Max exceeded, did=',
     &                       did,', max=',maxhits,': event',
     &                       gen_event_id_number
                        Write(*,*) '   hitcount=',hitcount
                        Write(*,*) '   roc,slot,cntr,sig,subadd=',
     &                       roc,slot,counter,sigtyp,subadd
                        printerr = .false.
                      endif
                    endif       ! hits < max_hits 
                  endif         ! not listed
*     
                  sigtyp = g_decode_sigtypmap(mappointer+subadd)
                  
              if(g_decode_debug .eq. 1) then
                 Write(*,*) '(4) sigtyp=',sigtyp
              endif
*     
*     we use charge ADC, no multiplicity for ADC.
c     
c     case 1 : ADC&TDC 
c     case 2 : ADC&MHTDC
c     case 3 : ADC&F1TDC
c     
c     e05115, 
c     AC, WC, LC, HSCIN and ESCIN use F1TDC.
c     MHTDC(1877) is used only for chambers.
                  if((roc .eq. HR_VME_ROCID .or. 
     &                roc .eq. LR_VME_ROCID .or.
     &                roc .eq. CH_FB_ROCID) .and. 
     &               (did.eq.HAER_ID .or. did.eq.HWAT_ID .or. 
     &                did.eq.HLUC_ID .or. did.eq.HSCIN_ID.or. 
     &                did.eq.ESCIN_ID )) then
                    
                    if(sigtyp .eq. 0 .or. sigtyp .eq. 1) then ! ADC
                      Do i=1,h
                        if( plane .eq. planelist(i) 
     &                       .and. counter .eq. counterlist(i) ) then
                          if(sigtyp.eq.0) then
                            signal0(i) = signal
                          else if (sigtyp.eq.1) then
                            signal1(i) = signal
                          EndIf
                        EndIf
                      EndDo
                    Else        !F1TDC/MHTDC
                      multi = 0
                      exitflag = 0
                      Do i=1,h
                        if( plane .eq. planelist(i) 
     &                       .and. counter .eq. counterlist(i) ) then
****************************
*     + signal
****************************
                          if(sigtyp .eq. 2) then
                            if(signal2(i) .lt. 0) then
                              signal2(i) = signal
                            Else ! multihit
                              if(hitcount .lt. maxhits) then
C     ---- first, check the partner. review hits.
                                dup = 1
                                j = i-1
                                If(multi .gt. 0) then
                                  Do while(j .gt. 0 .and. 
     &                                 j .ge. i-multi)
                                    if(signal3(i) .eq. 
     &                                   signal3(j)) then
                                      dup = 0
                                    EndIF
                                    j = j - 1
                                  EndDo
                                EndIF ! multi>0
c     ---- (ex) if the same (-) is found, (+) is not duplicated.  
                                if(dup .eq. 1) then
                                  multi = multi + 1
                                  if(h .lt. hitcount) then
                                    Do j=hitcount,h+multi,-1
                                      planelist(j+1) = planelist(j)
                                      counterlist(j+1)=counterlist(j)
                                      signal0(j+1) = signal0(j)
                                      signal1(j+1) = signal1(j)
                                      signal2(j+1) = signal2(j)
                                      signal3(j+1) = signal3(j)
                                    EndDo
                                  EndIF
c                                  if (abs(signal-signal3(i)).le.1000) then
                                    planelist(h+multi) = planelist(i)
                                    counterlist(h+multi)=counterlist(i)
                                    signal0(h+multi) = signal0(i)
                                    signal1(h+multi) = signal1(i)
                                    signal2(h+multi) = signal
                                    signal3(h+multi) = signal3(i)
                                    hitcount = hitcount + 1
c                                  endif
c          write(*,*)i,hitcount,signal,signal3(i)

                                EndIf ! dup=1
                              Else ! hitcount >maxhits
                                if(exitflag .eq. 0) then
                                  Write(*,*) '#! ',
     &                                 'F1/WC/AC/LChits for sigtyp2 ',
     &                                 'exceed the max. ev=',
     &                                 gen_event_id_number,
     &                                 ' did=',did,
     &                                 ' roc=',roc,
     &                                 ' slot=',slot,
     &                                 ' subadd=',subadd,
     &                                 ' hitcount=',hitcount,
     &                                 ' maxhits=',maxhits
                                  exitflag = 1
                                EndIf
                              EndIF ! hitcount < maxhits
                            EndIf ! multihit
****************************
*     - signal
****************************
                          Else if(sigtyp .eq. 3) then
                            if(signal3(i) .lt. 0) then
                              signal3(i) = signal
c     if(slot .eq. 4) then
c     Write(*,*) '(OK!) hit h i mul 2 3 ',
c     &                               hitcount,h,i,multi,
c     &                               signal2(i),signal3(i)
c     EndIf
                            Else ! multihit
                              if(hitcount .lt. maxhits) then
C     ---- first, check the partner. review hits.
                                dup = 1
                                j = i-1
                                Do while(j .gt. 0)
                                  if(signal2(i) .ge. 0 .and. 
     &                                 signal2(i) .eq. signal2(j)) 
     &                                 then
                                    dup = 0
                                  EndIF
                                  j = j - 1
                                EndDo
c     ---- (ex) if the same (-) is found, (+) is not duplicated.  
                                if(dup .eq. 1) then
                                  multi = multi + 1
                                  if(h .lt. hitcount) then
                                    Do j=hitcount,h+multi,-1
                                      planelist(j+1) = planelist(j)
                                      counterlist(j+1)=counterlist(j)
                                      signal0(j+1) = signal0(j)
                                      signal1(j+1) = signal1(j)
                                      signal2(j+1) = signal2(j)
                                      signal3(j+1) = signal3(j)
                                    EndDo
                                  EndIF
c                                  if (abs(signal-signal2(i)).le.1000) then
                                    planelist(h+multi) = planelist(i)
                                    counterlist(h+multi)=counterlist(i)
                                    signal0(h+multi) = signal0(i)
                                    signal1(h+multi) = signal1(i)
                                    signal2(h+multi) = signal2(i)
                                    signal3(h+multi) = signal
                                    hitcount = hitcount + 1
c                                  endif
c                                  write(*,*)did,hitcount,signal2(h),signal3(h)
                                  
c     if(slot .eq. 4) then
c     Write(*,*) '(!!!)',hitcount,h,i,multi,
c     &                                   signal2(h+multi),signal3(h+multi)
c     EndIF
                                  
                                EndIf ! dup=1
                              Else 
                                if(exitflag .eq. 0) then
                                  Write(*,*) '#! ',
     &                                 'F1/AC/WC/LChits for sigtyp3 ',
     &                                 'exceed the max. ev=',
     &                                 gen_event_id_number,
     &                                 ' did=',did,
     &                                 ' roc=',roc,
     &                                 ' slot=',slot,
     &                                 ' subadd=',subadd,
     &                                 ' hitcount=',hitcount,
     &                                 ' maxhits=',maxhits
                                  exitflag = 1
                                EndIf !exitflag =0/1
                              EndIF ! hitcount < maxhits
                            EndIf ! signal3<0
                          EndIF ! sigtyp=2,3
                        EndIf   ! match plane and counter
                      EndDo     ! hitcount loop
                    EndIF       ! MHTDC
********************************
*     END OF FASTBUS CASE (ADC&MHTDC)
********************************
                  Else          !TDC/ADC
                    if(sigtyp.eq.0) then
                      signal0(h) = signal
                    else if (sigtyp.eq.1) then
                      signal1(h) = signal
                    else if (sigtyp.eq.2) then
                      signal2(h) = signal
                    else if (sigtyp.eq.3) then
                      signal3(h) = signal
                    endif
                  EndIF
                  
c     Write(*,*) '(in all_detector.f)',signal1(h),signal2(h)
                  
                endif           ! single/multiple signal counter
                
              else if(hitcount.eq.maxhits .and. printerr) then ! Only print this message once
                print *,'g_decode_all_detector: Max exceeded, did=',
     $               did,', max=',maxhits,': event',gen_event_id_number
                print *,'   hitcount=',hitcount
                print *,'   roc,slot,cntr=',roc,slot,counter
                printerr = .false.
*     
*     Print/generate some kind of error that the hit array has been
*     exceeded.
*     
              endif             ! hitcount < maxhits
            EndIF               ! plane = 99 or not. (trigger time or not)
            
            pointer = pointer + 1
*     else
*     exit and get called back with the correct arrays for the new did
          endif                 ! newdid=did or not
        else                    ! subadd >= 255
c     if(pointer .eq. length) goto 986
          pointer = pointer + 1
c     Write(*,*) 'p=',pointer, ' length=',length
c     if(pointer .eq. length) goto 986
        endif                   ! subadd > 255 or not.
 987    continue
        
      enddo                     ! the same did=detector id
      
 986  continue
      
      g_decode_all_detector = pointer - 1 ! Number of words processed
      
      return
      end
**************
*     Local Variables:
*     mode: fortran
*     fortran-if-indent: 2
*     fortran-do-indent: 2
*     End:
