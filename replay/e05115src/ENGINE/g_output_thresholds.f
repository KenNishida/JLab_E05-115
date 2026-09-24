      subroutine g_output_thresholds(lunout,roc,slot,signalcount,
     &               elements_per_plane,signal0,signal1,sigma0,sigma1)

* Revision 2.0  1999/07/13 10:45:00  jinghua
* (JLiu) Change HMS to HNSS. It needs more work later. 
* The thresholds depend on the hardware setting.
*
* Revision 1.6  1999/02/23 18:23:01  csa
* (JRA) Move temps to signalcount 2 and make SunOS fixes
*
* Revision 1.5  1996/09/04 14:39:01  saw
* (JRA) Modify write statements
*
* Revision 1.4  1996/01/22 15:22:57  saw
* (JRA) Add/Modify some commented out diagnostics
*
* Revision 1.3  1996/01/17 20:25:27  saw
* (SAW) Add back missing sigma0 and sigma1 arguments that got lost
*
* Revision 1.2  1996/01/16 18:13:50  cdaq
* (JRA) Warn if thresholds change by too much
*
* Revision 1.1  1995/11/28 19:12:22  cdaq
* Initial revision
*
      implicit none
      save
*
      character*21 here
      parameter (here='g_output_thresholds')
*
      integer*4 lunout
      integer*4 roc,slot
      integer*4 signalcount,elements_per_plane
      real*4 signal0(*),signal1(*)
      real*4 sigma0(*),sigma1(*)
      real*4 delta_ped
*
      Integer*4 gdebug_threshold
*
      integer*4 pln,cnt,element,sigtyp
      integer*4 ich,ind,istart,jj,kk
      logical annoying_message
*
      INCLUDE 'gen_decode_common.cmn'
      INCLUDE 'gen_detectorids.par'

c     Include 'hes_data_structures.cmn'
c     Include 'hes_pedestals.cmn'
      
c     Do jj=1,3
c     Do kk=1,25
c     Write(*,*) jj,kk,escin_new_threshold_pos(jj,kk)
c     EndDo
c     EndDo
      
      gdebug_threshold = 0

      annoying_message=.true.
      
      istart=g_decode_slotpointer(roc,slot)
      if (istart.eq.-1) then    !uninstrumented slot.
         write(lunout,*) 'roc#',roc,', slot#',slot,' is not in the map'
         return
      endif
      
!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (signalcount.eq.1) then !cerenkov. or hes hodoscope
         do ich=1,g_decode_subaddcnt(roc,slot)
            ind=istart+ich-1
            pln=g_decode_planemap(ind)
            cnt=g_decode_countermap(ind)
c     ---- 
            if(gdebug_threshold .eq. 1) then
               Write(*,*) '(goutth)map,ind,pln,cnt,didmap=',
     &              ind,pln,cnt,g_decode_didmap(ind)
            EndIf

            if (g_decode_didmap(ind) .eq. UNINST_ID) then
               write(lunout,'(a6)') '  4000'  
c     ! set threshold very high if there is no signal
            else
c               if (roc .eq. 7) then
c                  element=(cnt-1)*elements_per_plane+pln
c               else
c                  element=(pln-1)*elements_per_plane+cnt
c               endif
c               write(lunout,'(i6)') nint(signal0(element))
c               delta_ped=signal0(element)-
c     &              float(g_threshold_readback(ich,roc,slot))
c               if ( (abs(delta_ped) .gt. min(20.,2.*sigma0(element)))  
c     &              .and.
c     &              g_threshold_readback(ich,roc,slot).ne.0) then
c                  if (annoying_message) then
c                     write(6,*) 'Warning! Danger Will Robinson!',
c     &                    '  Inconsistant Thresholds approaching!'
c                     write(6,*) 'May require updating ',
c     &                    'hes(hks)_thresholds.dat',
c     &                    ' in ~cdaq/coda to avoid losing data'
c                     write(6,*) '  roc slot channel threshold ',
c     &                    'calc.thresh. ',
c     &                    'delta  #sigma(pos. is OK).'
c                     annoying_message=.false.
c                  endif
c                  write(6,'(2x,i3,i5,i6,i11,2f11.1,f9.1)') roc,slot,ich,
c     &                 g_threshold_readback(ich,roc,slot),
c     &                 signal0(element),
c     &                 delta_ped,delta_ped/(sigma0(element)+.001)
c               endif
            endif
         enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!
      else if (signalcount.eq.2) then ! hodoscopes,cherenkov
         do ich=1,g_decode_subaddcnt(roc,slot)
            ind=istart+ich-1
            pln=g_decode_planemap(ind)
            cnt=g_decode_countermap(ind)
            sigtyp=g_decode_sigtypmap(ind)
            
            if ( (roc.eq.1.and.slot.eq.1) .or. (roc.eq.1.and.slot.eq.5) 
     &           .or.
     &           (roc.eq.3.and.slot.eq.1) .or. (roc.eq.3.and.slot.eq.5) 
     &           ) then
               element=cnt+(pln-1)*elements_per_plane !calorimeter
            else if(roc .eq. 6) then
               element=pln+(cnt-1)*elements_per_plane
            else		!hodoscope.  convert 2d pln,cnt to 1d array
               element=pln+(cnt-1)*elements_per_plane
            endif
            
            if(gdebug_threshold .eq. 1) then
               Do jj=1,75
                  write(*,*) 'signal0=',jj,signal0(jj)
               EndDo
               Write(*,*) '(goutth)roc=',roc,' sl=',slot,' pl=',pln,
     &              ' cn=',cnt,' sig=',sigtyp,' el/ep=',
     &              element,elements_per_plane,' th=',
     &              nint(signal0(element))
            EndIF
            
c            if (roc.eq.1 .and. slot.eq.1 .and. 
c     &           (ich.eq.63 .or. ich.eq.64)) then
c               write(lunout,'(a6)') '     0' ! no threshold for muon hodoscope
c               goto 999
c            endif
            if (g_decode_didmap(ind).eq.UNINST_ID) then
               write(lunout,'(a6)') '  4000' 
*     ! set threshold very high if there is no signal
            else
               if (sigtyp.eq.0) then

                  write(lunout,'(i6)') nint(signal0(element))

                  delta_ped=signal0(element)-
     &                 float(g_threshold_readback(ich,roc,slot))
                  if ( (abs(delta_ped) .gt. min(20.,2.*sigma0(element))) 
     &                 .and.
     &                 g_threshold_readback(ich,roc,slot).ne.0) then
                     if (annoying_message) then
                        write(6,*) 'Warning! Danger Will Robinson!',
     &                       '  Inconsistant Thresholds approaching!'
                        write(6,*) '  roc slot channel threshold  ',
     &                       'calc.thresh. delta  #sigma(pos. is OK).'
                        annoying_message=.false.
                     endif
                     write(6,'(2x,i3,i5,i6,i11,2f11.1,f9.1)') roc,slot,
     &                    ich,
     &                    g_threshold_readback(ich,roc,slot),
     &                    signal0(element),
     &                    delta_ped,delta_ped/(sigma0(element)+.001)
                  endif
               else if (sigtyp.eq.1) then

                  write(lunout,'(i6)') nint(signal1(element))

                  delta_ped=signal1(element)-
     &                 float(g_threshold_readback(ich,roc,slot))
                  if ( (abs(delta_ped) .gt. min(20.,2.*sigma1(element)))
     &                 .and.
     &                 g_threshold_readback(ich,roc,slot).ne.0) then
                     if (annoying_message) then
                        write(6,*) 'Warning! Danger Will Robinson! ',
     &                       ' Inconsistant Thresholds approaching!'
                        write(6,*) '  roc slot channel threshold  ',
     &                       'calc.thresh.',
     &                       ' delta  #sigma(pos. is OK).'
                        annoying_message=.false.
                     endif
                     write(6,'(2x,i3,i5,i6,i11,2f11.1,f9.1)') roc,
     &                    slot,ich,
     &                    g_threshold_readback(ich,roc,slot),
     &                    signal1(element),
     &                    delta_ped,delta_ped/(sigma1(element)+.001)
                  endif
               else             !sigtyp not (0.or.1)
c     ---     
                  if(gdebug_threshold .eq. 1) then
                     Write(*,*) '(output threshold=)roc,slot=',roc,slot
                  endif
                  write(6,*) 'sigtyp=',sigtyp,
     &                 ' in g_output_thresholds (should be 0 or 1)'
               endif
            endif
 999        continue
         enddo
      else                      ! signalcount not (1.or.2)
         write(6,*) 'signalcount=',signalcount,
     &        ' in g_output_thresholds (1=cal/cer, 2=hodoscopes)'
      endif

      return
      end
