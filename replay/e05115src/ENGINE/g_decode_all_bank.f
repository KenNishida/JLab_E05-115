      subroutine g_decode_all_bank(bank, ABORT, error)
*
*     Purpose and Methods: Decode a Fastbus and VME bank.
*
*     Looks at detector ID for a word in a data bank and passes the
*     appopriate data structure pointers to the g_decode_fb_detector routine.
*     That routine will return when it gets to another detector in which
*     case the the present routine will dispatch g_decode_fb_detector with a
*     new set of pointers.
*
*     This routine must be modified when new detectors are added.  It may
*     also may need to modified if fastbus modules other than from LeCroy
*     are used.
*
*     It is the responsibility of the calling routine to call
*     g_decode_fb_bank only for banks of fastbus data.
*
*     Inputs:
*
*     bank       Pointer to the first word (length) of a data bank.
*
*     Outputs:
*
*     ABORT
*     error
*
*     Created  16-NOV-1993   Stephen Wood, CEBAF
*     Modified  3-Dec-1993   Kevin Beard, Hampton U.
*     Modified  6-Jul-1999   Jinghua Liu for HNSS

*     Revision 2.2 2009/08/17 Z. Ye
*     Add TDC ADC channels for lucite counter
*     Revision 2.1 2004/03/02 Miyoshi
*     for E01-011. renamed because we use vme and fb.
*     
*     Revision 2.0   1999/07/20 11:07:00  jinghua
*     (JLiu) Change HMS to HNSS
*     
*     Revision 1.29  1999/02/23 16:58:58  csa
*     (JRA) Add roc 20 handling
*     
*     Revision 1.28  1999/01/29 17:47:44  saw
*     Fix Typo
*     
*     Revision 1.27  1999/01/29 17:23:03  saw
*     Add second tubes to SOS shower counter
*     
*     Revision 1.26  1998/12/17 21:50:31  saw
*     Support extra set of tubes on HMS shower counter
*     
*     Revision 1.25  1998/12/01 15:54:26  saw
*     (SAW) Slight change in debugging output
*     
*     Revision 1.24  1996/11/08 15:48:01  saw
*     (WH) Add decoding for lucite counter
*     
*     Revision 1.23  1996/04/29 19:45:37  saw
*     (JRA) Update Aerogel variable names
*     
*     Revision 1.22  1996/01/22 15:13:56  saw
*     (JRA) Put BPM/Raster data into MISC data structures
*     
*     Revision 1.21  1996/01/16 20:49:40  cdaq
*     (SAW) Handle banks containing two parallel link ROC banks
*     
*     Revision 1.20  1995/12/06 19:04:24  cdaq
*     (SAW) What is this version?  Two bank banks processing lost.
*     
*     Revision 1.19  1995/11/28 18:50:03  cdaq
*     (SAW) Quick hack to accept banks with 2 rocs (from parallel link)
*     
*     Revision 1.18  1995/10/09 18:20:51  cdaq
*     (JRA) Change HCER_ADC to HCER_RAW_ADC
*     Replace g_decode_getdid call with explicit calculation (for speed)
*     
*     Revision 1.17  1995/07/27 19:06:02  cdaq
*     (SAW) Use specific bit manipulation routines for f2c compatibility
*     Get FB roc from header on parallel link banks
*     
*     Revision 1.16  1995/05/22  20:50:45  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.15  1995/05/22  13:35:40  cdaq
*     (SAW) Fix up some problems with decoding of parallel link wrappers around
*     fastbus events.  Still doesn't hadle two fb rocs wrapped into one bank.
*     
*     Revision 1.14  1995/05/11  17:17:00  cdaq
*     (SAW) Extend || link hack for SOS.  Add Aerogel detector.
*     
*     Revision 1.13  1995/04/01  19:44:50  cdaq
*     (SAW) Add BPM hitlist
*     
*     Revision 1.12  1995/01/27  20:12:48  cdaq
*     (SAW) Add hacks to deal with parallel link data.  Pass lastslot variable to
*     g_decode_fb_detector so it can find 1881M/1877 headers.
*     
*     Revision 1.11  1994/11/22  20:13:02  cdaq
*     (SPB) Update array names for raw SOS Scintillator bank
*     
*     Revision 1.10  1994/06/28  20:01:23  cdaq
*     (SAW) Change arrays that HMS scintillators go into
*     
*     Revision 1.9  1994/06/18  02:45:49  cdaq
*     (SAW) Add code for miscleaneous data and uninstrumented channels
*     
*     Revision 1.8  1994/06/09  04:48:28  cdaq
*     (SAW) Fix length argument on gmc_mc_decode call again
*     
*     Revision 1.7  1994/04/13  18:49:10  cdaq
*     (KBB Fix length argument on gmc_mc_deocde call
*     
*     
      implicit none
      SAVE
*     
      character*16 here
      parameter (here='g_decode_all_bank')
*
      logical ABORT
      character*(*) error
      integer*4 bank(*)

*     This routine unpacks a ROC bank.  It looks a fastbus word to
*     determine which detector it belongs to.  It then passes the
*     appropriate arrays for that detector to detector independent unpacker
*     G_DECODE_FB_DETECTOR which will unpack words from the bank into the
*     hit arrays until the detector changes or it runs out of data.
*     G_DECODE_FB_DETECTOR returns a pointer to the next data word to be
*     processed.
*
      include 'gen_detectorids.par'
      include 'gen_data_structures.cmn'
      include 'hes_data_structures.cmn'
      include 'hks_data_structures.cmn'
      include 'gen_decode_common.cmn'
      include 'mc_structures.cmn'
      include 'gen_event_info.cmn'
      include 'gen_rocid.cmn'

      integer*4 pointer                 ! Pointer FB data word
      integer*4 banklength,maxwords
      integer*4 roc,subadd,slot,lastslot
      integer*4 stat_roc
      integer*4 slotp           ! temp variable
      integer*4 did             ! Detector ID
      integer*4 g_decode_fb_detector ! Detector unpacking routine
      integer*4 g_decode_all_detector ! detector unpacking routine for VME/FB
    
      integer*4 last_first      ! Last word of first bank in || bank
      integer*4 i
      integer*4 mask_vme1,mask_vme2,mask_vme3,mask_vme4

      integer*4 decode_lunno
      Parameter(decode_lunno=52)

*

      banklength = bank(1) + 1  ! Bank length including count
      last_first = banklength
      stat_roc = ishft(bank(2),-16)
      roc = iand(stat_roc,INT(Z'1F')) ! Get ROC from header

      mask_vme1 = 0
      
      if(roc.eq.20) then
        gen_event_ts_trigger_data=bank(3) ! Latched TS input pattern
        return                  ! scaler ROC
      endif
      
c      Write(*,*) '(all bank) ev=',gen_event_ID_number,
c     &     ' len=',banklength,' roc=',roc
c      if(banklength .ge. 10) then
c        Write(*,'(5(Z8,X))') (bank(i),i=1,5)
c        Write(*,'(5(Z8,X))') (bank(i),i=6,10) 
c      endif
*     
*     First look for special Monte Carlo Banks
*     
      if(stat_roc.eq.mc_status_and_ROC) then
*     call gmc_mc_decode(banklength-2,bank(3),ABORT,error)
        ABORT = .TRUE.
        error = 'Monte Carlo Event analysis disabled'
        if(ABORT) then
          call g_add_path(here,error)
        endif
        return
      endif
*     
      if(roc.gt.G_DECODE_MAXROCS .and. roc.ne.9) then
        ABORT = .false.         ! Just warn
        write(error,*) ':ROC out of range, ROC#=',roc
        call g_add_path(here,error)
        return
      endif
*     
      pointer = 3               ! First word of bank
*     
      if (roc.eq.8 .or. roc.eq.9) then
*     
*     These 3 rocs are VME front ends for fastbus crates.  At present
*     we assume that each VME front end is only taking data from one
*     FB roc and that this FB roc # is in 4 word of the bank.  This
*     hack will not work when we have roc 8 taking data from both
*     fbch1 and fbch2.  But it should work for runs up through
*     at least 5/31/95.
*     
        last_first = pointer + bank(pointer) ! Last word in sub bank
        stat_roc = ishft(bank(pointer+1),-16) !2 words are fb roc header.
        roc = iand(stat_roc,INT(Z'1F'))
        pointer=pointer+2       !using parallel link, so next
      endif
      
*      Write(*,*) 'roc=',roc
      
c     ******************************************************
c     --- start loop ---
c     ******************************************************
      
      lastslot = -1
      do while (pointer .le. banklength)
        
        if(pointer.eq.(last_first+1)) then ! Second bank in a two bank
          last_first = banklength ! Reset to end of second bank
          stat_roc = ishft(bank(pointer+1),-16) ! 2 words are fb roc header
          roc = iand(stat_roc,INT(Z'1F')) ! New roc
        endif
*     
*     Look for and report empty ROCs.
*     
        if(g_decode_debug .eq. 1) then
          Write(*,'(15H(allbank)b.l.p=,z8,X,2I7)') 
     &         bank(pointer),banklength,pointer
        EndIf

*     check where missing data
        
        if (bank(pointer).EQ.INT(Z'DCFF0000')) then
          if (roc.eq.1 .or. roc.eq.2) then !missing hnss data
            if (gen_event_type.ne.2) then !event type 2 is sos only event.
              write(6,'(a,i3,a,i8,a,z8,a,i2)') 'roc',roc,
     &             ' has no data for event'
     &             ,gen_event_id_number,' scanmask=',bank(pointer+1)
     $             ,', evtype=',gen_event_type
            endif
          else                  !missing sos data
            if (gen_event_type.ne.1) then !event type 1 is hnss only data.
              write(6,'(a,i3,a,i8,a,z8,a,i2)') 'roc',roc,
     &             ' has no data for event'
     &             ,gen_event_id_number,' scanmask=',bank(pointer+1)
     $             ,', evtype=',gen_event_type
            endif
          endif
        endif
*     
*     the 1st data.
*     VME?
*     For VME ADC/TDC, header is DA00ADC0 or DA00DDC0 or BA000000.
*     DA --> slot 27 BA --> slot 23. 
*     Event vector including 'BA' may not reach here. 
*     
        
*     if you have DA at least once the ROC, mask_vme1 = DA
        if(ishft(iand(bank(pointer),INT(Z'FF000000')),-24) .eq. INT(Z'DA')) then
          mask_vme1= ishft(iand(bank(pointer),INT(Z'FF000000')),-24)
        EndIf
        
*     if you have 'ADC','DDC' or 'F1C' then update module type.
        if(ishft(iand(bank(pointer),INT(Z'0000FFC0')),-4) .eq. INT(Z'ADC').or.
     &       ishft(iand(bank(pointer),INT(Z'0000FFC0')),-4) .eq. INT(Z'DDC') .or.
     &       ishft(iand(bank(pointer),INT(Z'0000FFC0')),-4) .eq. INT(Z'F1C')) then
          mask_vme2 = ishft(iand(bank(pointer),INT(Z'0000FFF0')),-4)
        EndIF
*     --- if 'F1C' tag is not found in VME crate, do roc scan.
        if(roc .eq. HR_VME_ROCID) then
          mask_vme1 = INT(Z'DA')
          mask_vme2 = INT(Z'F1C')
c     --- we care this is f1 or c794 in g_decode_all_detector.
        EndIf
        if(roc .eq. LR_VME_ROCID) then
          mask_vme1 = INT(Z'DA')
          mask_vme2 = INT(Z'F1C')
c     --- we care this is f1 or c794 in g_decode_all_detector.
        EndIf
        
        if(g_decode_debug .eq. 1) then
          Write(*,'(20H(allbank) p,b,m1,m2=,2X,I7,4X,3Z9)') pointer,
     &         bank(pointer),mask_vme1,mask_vme2
        EndIf
        
*--   looking for header/trailer data.  
        if(mask_vme1 .eq. INT(Z'DA')) then
          mask_vme3 = ishft(iand(bank(pointer),INT(Z'FF000000')),-24)
          if(mask_vme3 .eq. INT(Z'DA')) then
            pointer=pointer+1
          EndIF
*     VME-c775/c792, 1 at 25th bit for header and 1 at 26th bit for end of words 
c     --- c775/792, 0000 0011 0000 0000 0000 0000 0000 0000
          mask_vme4 = ishft(iand(bank(pointer),INT(Z'01000000')),-24)
          if(mask_vme4 .eq. INT(Z'1')) pointer = pointer + 1
          mask_vme4 = ishft(iand(bank(pointer),INT(Z'03000000')),-25)
          if(mask_vme4 .eq. INT(Z'1')) pointer = pointer + 1
c     --- f1c, 0000 0000 1000 0000 0000 0000 0000 0000 1:data 0:header
          mask_vme4 = ishft(iand(bank(pointer),INT(Z'00800000')),-23)            
          if(mask_vme4 .eq. INT(Z'0')) pointer = pointer + 1
        EndIF
        
        slot = iand(ishft(bank(pointer),-27),INT(Z'1F'))
c        write(*,*) "roc,slot=",roc,slot
        if(roc.eq.11) then ! for EDC1 
            slot=slot+1
        endif
        
        if(g_decode_debug .eq. 1) then
          Write(*,'(26H(all bank) sl,p,ba,v1,v2=,I3,I6,3Z9)') 
     &         slot,pointer,bank(pointer),mask_vme1,mask_vme2
        EndIf

*     -- in case you use only FASTBUS, set mask1= 0
c     -- to analyze one data including bug slot=28, increase max slots
c     -- but do not include X'BA'=23
        
        if( slot .gt. 0 .and. slot .le. G_DECODE_MAXSLOTS
     $       .and.
     $       mask_vme1 .ne. INT(Z'BA') .and.
     $       roc .gt. 0 .and. roc .le. g_decode_maxrocs ) then
          
          if(mask_vme1 .ne. INT(Z'DA')) then ! not VME
            subadd = iand(ishft(bank(pointer),
     $           -g_decode_subaddbit(roc,slot)),INT(Z'7F'))
          Else                  ! VME
            if(mask_vme2 .eq. INT(Z'F1C')) then
              subadd = ishft(iand(bank(pointer),INT(Z'380000')),-19) * 8
     &             + ishft(iand(bank(pointer),INT(Z'70000')),-16)
            Else
              subadd = iand(ishft(bank(pointer),
     $             -g_decode_subaddbit(roc,slot)),INT(Z'3F'))
            EndIf
          EndIF
          
c     check roc,slot and did
          if(g_decode_debug .eq. 1) then
            Write(*,*) '(allbank)e,r,s,a=',gen_event_id_number,
     &           roc,slot,subadd
          EndIf
          
          if (subadd .lt. INT(Z'7F')) then ! Only valid subaddress
            
            slotp = g_decode_slotpointer(roc,slot)
            
            if (slotp.gt.0) then
              did = g_decode_didmap(slotp+subadd)
c     Write(*,*) 'did?=',did
            else
              did = UNINST_ID
            endif
            maxwords = last_first - pointer + 1
            
*            if(roc.eq.5.and.
*     &           subadd.ne.0.and.subadd.ne.1) then ! for HAPPEX
*              write(*,*) "Bad data in ROC5. Skipped. event=",
*     &             gen_event_id_number
*              did=0
*            endif
            if(g_decode_debug .eq. 1) then
              Write(*,*) '(allbank) sa=',subadd,' roc=',roc,
     &             ' s=',slot,' sp=',slotp,
     &             ' did=',did,' max=',maxwords,' p=',pointer
            EndIf
*     
*     1         2         3         4         5         6         7
*     23456789012345678901234567890123456789012345678901234567890123456789012
*     
*     lastslot = -1 (a initial value)
            
*     if you use fastbus, you may use g_decode_fb_detector.
*     the difference is only mask_vme1 and mask_vme2.

            if(did.eq.HDC_ID) then
              pointer = pointer +
     $            g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &            maxwords, did, 
     $            HMAX_DC_HITS, HDC_RAW_TOT_HITS, HDC_RAW_LAYER_NUM,
     $            HDC_RAW_WIRE_NUM, 1,HDC_RAW_TDC,0, 0, 0)
              
              if(g_decode_debug .eq. 2) then
                Do i=1,hdc_raw_tot_hits
                  Write(*,*) 'ev,i,la,wi,tdc,roc=',gen_event_ID_number,
     &                 i,hdc_raw_layer_num(i),hdc_raw_wire_num(i),
     &                 hdc_raw_tdc(i),hdc_raw_rocno(i)
                EndDo
              EndIf

            else if (did.eq.HSCIN_ID) then
              pointer = pointer +
     $             g_decode_all_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, mask_vme1,mask_vme2,
     $             HMAX_RAW_SCIN_HITS, HSCIN_RAW_TOT_HITS,
     $             HSCIN_RAW_LAYER_NUM, HSCIN_RAW_COUNTER_NUM, 4,
     $             HSCIN_RAW_ADC_POS, HSCIN_RAW_ADC_NEG,
     $             HSCIN_RAW_TDC_POS, HSCIN_RAW_TDC_NEG)
              
c            Write(*,*) "(all bank) SCIN ev la, co, ap, an, tp,tn, hits="
c            write(*,*) gen_event_id_number,
c     $             HSCIN_RAW_LAYER_NUM(hscin_raw_tot_hits),
c     &             HSCIN_RAW_COUNTER_NUM(hscin_raw_tot_hits), 
c     &             hscin_raw_adc_pos(hscin_raw_tot_hits),
c     &             hscin_raw_adc_neg(hscin_raw_tot_hits),
c     &             hscin_raw_tdc_pos(hscin_raw_tot_hits),
c     &             hscin_raw_tdc_neg(hscin_raw_tot_hits),
c     &             hscin_raw_tot_hits
              
            else if (did.eq.HAER_ID) then
*     
*     Aerogel has two tubes for each counter.  The detector
*     has both ADC's and TDC's signals.
*     
              pointer = pointer +
     $             g_decode_all_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, mask_vme1,mask_vme2,
     $             HMAX_AER_HITS, HAER_RAW_TOT_HITS, HAER_RAW_LAYER_NUM,
     $             HAER_RAW_COUNTER_NUM, 4, HAER_RAWADC_POS, 
     $             HAER_RAWADC_NEG,
     $             HAER_RAWTDC_POS, HAER_RAWTDC_NEG)
              
              if(g_decode_debug .eq. 1) then
                Write(*,*) '(all bank) AC raw ev tot =',
     &               gen_event_id_number,
     &               haer_raw_tot_hits
              EndIF

              if(g_decode_debug .eq. 2) then
                Do i=1,haer_raw_tot_hits
                  Write(*,*) '(AC)ev,i,la,co,ap,an,tp,tn=',
     &                 gen_event_ID_number,i,haer_raw_layer_num(i),
     &                 haer_raw_counter_num(i),
     &                 haer_rawadc_pos(i),haer_rawadc_neg(i),
     &                 haer_rawtdc_pos(i),haer_rawtdc_neg(i)
                EndDo
              EndIf         


              
            else if (did.eq.HWAT_ID) then
*     
*     Water has two tubes for each counter.  The detector
*     has both ADC's and summed TDC's signals.
*     
              pointer = pointer +
     $             g_decode_all_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, mask_vme1,mask_vme2,
     $             HMAX_WAT_HITS, HWAT_RAW_TOT_HITS, HWAT_RAW_LAYER_NUM,
     $             HWAT_RAW_COUNTER_NUM, 4, HWAT_RAWADC_POS, 
     $             HWAT_RAWADC_NEG,
     $             HWAT_RAWTDC_POS, HWAT_RAWTDC_NEG)
              
              if(g_decode_debug .eq. 1) then
                Write(*,*) '(all bank) WC raw ev tot =',
     &               gen_event_id_number,
     &               hwat_raw_tot_hits,
     $               HWAT_RAWTDC_POS
              EndIF

            else if (did.eq.HLUC_ID) then
*     
*     LC has two tubes for each counter.  The detector
*     has both ADC's and TDC's signals.

              pointer = pointer +
     $             g_decode_all_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, mask_vme1,mask_vme2,
     $             HMAX_LUC_HITS, HLUC_RAW_TOT_HITS, 
     &             HLUC_RAW_LAYER_NUM,
     $             HLUC_RAW_COUNTER_NUM, 4, HLUC_RAWADC_POS, 
     $             HLUC_RAWADC_NEG,
     $             HLUC_RAWTDC_POS, HLUC_RAWTDC_NEG)
              
              if(g_decode_debug .eq. 1) then
                Write(*,*) '(all bank) LUC raw ev tot =',
     &               gen_event_id_number,
     &               hluc_raw_tot_hits,
     $               hluc_rawtdc_pos
              EndIF

            else if (did.eq.HLUCSUM_ID) then
*     
*     LC has two tubes for each counter.  The detector
*     has both ADC's and TDC's signals.

              pointer = pointer +
     $             g_decode_all_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, mask_vme1,mask_vme2,
     $             HMAX_LUCSUM_HITS, HLUCSUM_RAW_TOT_HITS,
     &             HLUCSUM_RAW_LAYER_NUM,
     $             HLUCSUM_RAW_COUNTER_NUM, 3, 
     &             HLUC_RAWADC_TOT, 0,
     $             HLUC_RAWTDC_TOT, 0)
              
c              if(g_decode_debug .eq. 1) then
c                Write(*,*) '(all bank) LUC raw ev tot =',
c     &               gen_event_id_number,
c     &               hluc_raw_tot_hits,
c     $               hluc_rawtdc_tot
c              EndIF

            else if (did.eq.HMISC_ID) then
*     
*     This array is for data words that don't belong to a specific
*     detector counter.  Things like energy sums, and TDC's from various
*     points in the logic will go here.  Most likely we will set ADDR1
*     always to 1, and ADDR2 will start at 1.
*     
              pointer = pointer +
     $             g_decode_all_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, mask_vme1,mask_vme2, 
     $             HMAX_MISC_HITS, HMISC_TOT_HITS, HMISC_RAW_ADDR1,
     $             HMISC_RAW_ADDR2, 1, HMISC_RAW_DATA, 0, 0, 0)
*     
*     1         2         3         4         5         6         7
*     23456789012345678901234567890123456789012345678901234567890123456789012
*     
              
*     
*     HES EDC1
*     
            else if(did.eq.EDC1_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             EMAX_DC1_HITS, EDC1_RAW_TOT_HITS, EDC1_RAW_LAYER_NUM,
     $             EDC1_RAW_WIRE_NUM,1 ,EDC1_RAW_TDC,0, 0, 0)
             
              if(g_decode_debug .eq. 1) then
                Do i=1,edc1_raw_tot_hits
                  if (edc1_raw_layer_num(i).eq.5.and.
     &                 edc1_raw_wire_num(i).gt.82    ) then
                     Write(*,*) 'ev,i,la,wi,tdcp,tdcn=',
     &                 gen_event_ID_number,
     &                 i,edc1_raw_layer_num(i),
     &                 edc1_raw_wire_num(i)
                  endif
                EndDo
              EndIf
            
            else if(did.eq.EDC2_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             EMAX_DC2_HITS, EDC2_RAW_TOT_HITS, EDC2_RAW_LAYER_NUM,
     $             EDC2_RAW_WIRE_NUM,1 ,EDC2_RAW_TDC,0, 0, 0)
             
              if(g_decode_debug .eq. 1) then
                Write(*,*) 'nedc1=',edc2_raw_tot_hits
              EndIf
*     
*     HES HODOSCOPE
*     
            else if(did.eq.ESCIN_ID) then
              pointer = pointer +
     $             g_decode_all_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,mask_vme1,mask_vme2, 
     $             EMAX_SCIN_HITS, ESCIN_RAW_TOT_HITS, 
     $             ESCIN_RAW_LAYER_NUM,
     $             ESCIN_RAW_COUNTER_NUM,4 ,ESCIN_RAWADC_POS, 
     $             ESCIN_RAWADC_NEG,
     $             ESCIN_RAWTDC_POS, ESCIN_RAWTDC_NEG)
              
              if(g_decode_debug .eq. 2) then
                Do i=1,escin_raw_tot_hits
                  Write(*,*) 'ev,i,la,wi,tdcp,tdcn=',
     &                 gen_event_ID_number,
     &                 i,escin_raw_layer_num(i),
     &                 escin_raw_counter_num(i),
     &                 escin_rawtdc_pos(i),escin_rawtdc_neg(i)
                EndDo
              EndIf

c     Write(*,*) 'nescin.p=',escin_raw_tot_hits,pointer
*     
*     HNSS MISC (Target Monitor, Vth parameter)
*     
            else if(did.eq.EMISC_ID) then
              pointer = pointer +
     $             g_decode_all_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, mask_vme1,mask_vme2, 
     $             EMAX_MISC_HITS, EMISC_RAW_TOT_HITS, 
     $             EMISC_RAW_LAYER_NUM,
     $             EMISC_RAW_COUNTER_NUM,3 ,EMISC_RAW_ADC, 
     $             EMISC_RAW_DUMMY,
     $             EMISC_RAW_TDC, EMISC_RAW_DUMMY)

*     
*     efis_raw_dummy(1024) is used for fill it as dummy.
*     
*     FISSION CHAMBER timing FIS1_ID
*     
c            else if(did.eq.FIS1_ID) then
c              pointer = pointer +
c     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
c     &             maxwords, did,
c     $             EMAX_FIS_TIME_HITS, EFIS_TIME_RAW_HITS, 
c     $             EFIS_TIME_RAW_LAYER,
c     $             Efis_RAW_DUMMY,4 , EFIS_TIME_RAW_ADC, 
c     $             Efis_RAW_DUMMY,EFIS_TIME_RAW_TDC,
c     $             Efis_RAW_DUMMY)

*
*     FISSION CHAMBER position FIS2_ID
*
c            else if(did.eq.FIS2_ID) then
c              pointer = pointer +
c     $             g_decode_all_detector(lastslot, roc, bank(pointer), 
c     &             maxwords, did, mask_vme1,mask_vme2, 
c     $             EMAX_FIS_POS_HITS, EFIS_POS_RAW_HITS, 
c     $             EFIS_POS_RAW_LAYER,
c     $             Efis_RAW_DUMMY,1 , EFIS_POS_RAW_TDC, 
c     $             0, 0, 0)

*     
*     BPM/Raster ADC values. 
*     
            else if (did.eq.GMISC_ID) then
              pointer = pointer +
     $             g_decode_all_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, mask_vme1,mask_vme2, 
     $             GMAX_MISC_HITS, GMISC_TOT_HITS, GMISC_RAW_ADDR1,
     $             GMISC_RAW_ADDR2, 1, GMISC_RAW_DATA, 0, 0, 0)
              
              if(g_decode_debug .eq. 2) then
                Do i=1,gmisc_tot_hits
                  Write(*,*) '(allbank)GMISC tot=',i,
     &                 gmisc_raw_addr1(i),
     &                 gmisc_raw_addr2(i),
     &                 gmisc_raw_data(i)
                EndDo
              EndIF
c
c     HTUL
c     tul ecl output goes to mhtdc
c     efis_raw_dummy(1024) is used for just filling dummy.
c     
            else if (did.eq.htul_ID) then
              pointer = pointer +
     $             g_decode_all_detector(lastslot, roc, 
     $             bank(pointer), maxwords, did, mask_vme1,mask_vme2,
     $             HMAX_TUL_HITS, HTUL_RAW_TOT_HITS,
     $             htul_raw_module_num,
     $             htul_raw_channel_num, 1, htul_raw_tdc,
     $             0, 0, 0)
              
              if(g_decode_debug .eq. 2) then
                Do i=1,htul_raw_tot_hits
                  Write(*,*) '(allbank)HTUL tot=',i,
     &                 htul_raw_module_num(i),
     &                 htul_raw_channel_num(i),
     &                 htul_raw_tdc(i)
                EndDo
              EndIF

*
*     HES TUL
*

            else if (did.eq.etul_ID) then
              pointer = pointer +
     $             g_decode_all_detector(lastslot, roc, 
     $             bank(pointer), maxwords, did, mask_vme1,mask_vme2,
     $             EMAX_TUL_HITS, ETUL_RAW_TOT_HITS,
     $             etul_raw_layer_num,
     $             etul_raw_channel_num, 1, etul_raw_tdc,
     $             0, 0, 0)
              
              if(g_decode_debug .eq. 2) then
                Do i=1,etul_raw_tot_hits
                  Write(*,*) '(allbank)ETUL tot=',i,
     &                 etul_raw_channel_num(i),
     &                 etul_raw_tdc(i)
                EndDo
              EndIF

*     
*     GTRIG
*     
              
            else if (did.eq.gtrig_ID) then
              pointer = pointer +
     $             g_decode_all_detector(lastslot, roc, 
     $             bank(pointer), maxwords, did, mask_vme1,mask_vme2,
     $             GMAX_TRIG_HITS, GTRIG_RAW_TOT_HITS,
     $             gtrig_raw_layer_num,
     $             gtrig_raw_channel_num, 1, gtrig_raw_tdc,
     $             0, 0, 0)
              
              if(g_decode_debug .eq. 2) then
                Do i=1,gtrig_raw_tot_hits
                  Write(*,*) '(allbank)GTRIG tot=',i,
     &                 gtrig_raw_channel_num(i),
     &                 gtrig_raw_tdc(i)
                EndDo
              EndIF

*     
*     Data from Uninstrumented channels and slots go into a special array
*     
c            else if (did.eq.UNINST_ID) then
c              pointer = pointer +
c     $             g_decode_all_detector(lastslot, roc, bank(pointer), 
c     &             maxwords, did, mask_vme1, mask_vme2,
c     $             GMAX_UNINST_HITS, GUNINST_TOT_HITS, 
c     $             GUNINST_RAW_ROCSLOT,
c     $             GUNINST_RAW_SUBADD, 
c     $             1, GUNINST_RAW_DATAWORD, 0, 0, 0)
              
c     Write(*,*) ' ev=',gen_event_ID_number, ' g uninst hits=',
c     &             gmax_uninst_hits,' p=',pointer
              
            else
*     Should never get here.  Unknown detector ID's or did=-1 for bad ROC#
*     or SLOT# will come here.
*     
*              Write(decode_lunno,*) 'bad did, unknown ROC, slot, did=',
*     &             roc,slot,did, ' ev=',gen_event_ID_number
              
              pointer = pointer + 1 ! Skip unknown detector id's
            endif
          else
            lastslot = slot
            pointer = pointer + 1 ! Skip Bad subaddresses (module header)
          endif
*     
        else if(mask_vme1 .eq. INT(Z'BA')) then ! VME bad data
          Write(*,*) 'This is VME bad data!'
          pointer = pointer + 1 ! Skip bad slots
        else                    ! slot,roc>max values
          pointer = pointer + 1 ! Skip bad slots
        endif
*     
      enddo                     ! pointer<=banklength
      
      ABORT= .FALSE.
      error= ' '
      
      return
      end
**************
*     Local Variables:
*     mode: fortran
*     fortran-if-indent: 2
*     fortran-do-indent: 2
*     End:
