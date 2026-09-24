      subroutine g_decode_fb_bank(bank, ABORT, error)
*
*     Purpose and Methods: Decode a Fastbus bank.
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
*
* Revision 2.1 2004/03/02 Miyoshi
* for E01-011
* Revision 2.2 2009/08/11 Z.Ye
* for E05-115 Add Lucite counter
*
* Revision 2.0   1999/07/20 11:07:00  jinghua
* (JLiu) Change HMS to HNSS
*
* Revision 1.29  1999/02/23 16:58:58  csa
* (JRA) Add roc 20 handling
*
* Revision 1.28  1999/01/29 17:47:44  saw
* Fix Typo
*
* Revision 1.27  1999/01/29 17:23:03  saw
* Add second tubes to SOS shower counter
*
* Revision 1.26  1998/12/17 21:50:31  saw
* Support extra set of tubes on HMS shower counter
*
* Revision 1.25  1998/12/01 15:54:26  saw
* (SAW) Slight change in debugging output
*
* Revision 1.24  1996/11/08 15:48:01  saw
* (WH) Add decoding for lucite counter
*
* Revision 1.23  1996/04/29 19:45:37  saw
* (JRA) Update Aerogel variable names
*
* Revision 1.22  1996/01/22 15:13:56  saw
* (JRA) Put BPM/Raster data into MISC data structures
*
* Revision 1.21  1996/01/16 20:49:40  cdaq
* (SAW) Handle banks containing two parallel link ROC banks
*
* Revision 1.20  1995/12/06 19:04:24  cdaq
* (SAW) What is this version?  Two bank banks processing lost.
*
* Revision 1.19  1995/11/28 18:50:03  cdaq
* (SAW) Quick hack to accept banks with 2 rocs (from parallel link)
*
* Revision 1.18  1995/10/09 18:20:51  cdaq
* (JRA) Change HCER_ADC to HCER_RAW_ADC
*       Replace g_decode_getdid call with explicit calculation (for speed)
*
* Revision 1.17  1995/07/27 19:06:02  cdaq
* (SAW) Use specific bit manipulation routines for f2c compatibility
*       Get FB roc from header on parallel link banks
*
* Revision 1.16  1995/05/22  20:50:45  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.15  1995/05/22  13:35:40  cdaq
* (SAW) Fix up some problems with decoding of parallel link wrappers around
* fastbus events.  Still doesn't hadle two fb rocs wrapped into one bank.
*
* Revision 1.14  1995/05/11  17:17:00  cdaq
* (SAW) Extend || link hack for SOS.  Add Aerogel detector.
*
* Revision 1.13  1995/04/01  19:44:50  cdaq
* (SAW) Add BPM hitlist
*
* Revision 1.12  1995/01/27  20:12:48  cdaq
* (SAW) Add hacks to deal with parallel link data.  Pass lastslot variable to
*       g_decode_fb_detector so it can find 1881M/1877 headers.
*
* Revision 1.11  1994/11/22  20:13:02  cdaq
* (SPB) Update array names for raw SOS Scintillator bank
*
* Revision 1.10  1994/06/28  20:01:23  cdaq
* (SAW) Change arrays that HMS scintillators go into
*
* Revision 1.9  1994/06/18  02:45:49  cdaq
* (SAW) Add code for miscleaneous data and uninstrumented channels
*
* Revision 1.8  1994/06/09  04:48:28  cdaq
* (SAW) Fix length argument on gmc_mc_decode call again
*
* Revision 1.7  1994/04/13  18:49:10  cdaq
* (KBB Fix length argument on gmc_mc_deocde call
*
*
      implicit none
      SAVE
*
      character*16 here
      parameter (here='g_decode_fb_bank')
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
      include 'gen_run_info.cmn'

      integer*4 pointer                 ! Pointer FB data word
      integer*4 banklength,maxwords
      integer*4 roc,subadd,slot,lastslot
      integer*4 stat_roc
      integer*4 slotp                   ! temp variable
      integer*4 did                     ! Detector ID
      integer*4 g_decode_fb_detector    ! Detector unpacking routine
      integer*4 last_first              ! Last word of first bank in || bank
      integer*4 i
*
      banklength = bank(1) + 1          ! Bank length including count
      last_first = banklength

      stat_roc = ishft(bank(2),-16)
      if (gen_run_number.lt.50000) then
         roc = 1 !for EDC test bench DK
      else
         roc = iand(stat_roc,INT(Z'1F'))                ! Get ROC from header
      endif

      if(roc.eq.20) then
        return                  ! scaler ROC
      endif
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
      
      lastslot = -1
      do while (pointer .le. banklength)
        if(pointer.eq.(last_first+1)) then ! Second bank in a two bank bank
          last_first = banklength ! Reset to end of second bank
          stat_roc = ishft(bank(pointer+1),-16) !2 words are fb roc header
          roc = iand(stat_roc,INT(Z'1F')) ! New roc
        endif
*     
*     Look for and report empty ROCs.
*     
        if (bank(pointer).EQ.INT(Z'DCFF0000')) then
          if (roc.eq.1 .or. roc.eq.2) then !missing hnss data
            if (gen_event_type.ne.2) then !event type 2 is sos only event.
              write(6,'(a,i3,a,i8,a,z8,a,i2)') 'roc',roc,' has no data for event'
     &             ,gen_event_id_number,' scanmask=',bank(pointer+1)
     $             ,', evtype=',gen_event_type
            endif
          else                  !missing sos data
            if (gen_event_type.ne.1) then !event type 1 is hnss only data.
              write(6,'(a,i3,a,i8,a,z8,a,i2)') 'roc',roc,' has no data for event'
     &             ,gen_event_id_number,' scanmask=',bank(pointer+1)
     $             ,', evtype=',gen_event_type
            endif
          endif
        endif
*     
        slot = iand(ishft(bank(pointer),-27),INT(Z'1F'))
        if(slot.gt.0.and.slot.le.G_DECODE_MAXSLOTS .and.
     $       roc.gt.0 .and. roc.le.g_decode_maxrocs) then
          subadd = iand(ishft(bank(pointer),
     $         -g_decode_subaddbit(roc,slot)),INT(Z'7F'))
          
          if (subadd .lt. INT(Z'7F')) then ! Only valid subaddress
                                ! This skips module headers
            
            slotp = g_decode_slotpointer(roc,slot)
            if (slotp.gt.0) then
              did = g_decode_didmap(slotp+subadd)
            else
              did = UNINST_ID
            endif
            maxwords = last_first - pointer + 1
*     
*     1         2         3         4         5         6         7
*     23456789012345678901234567890123456789012345678901234567890123456789012
*     
            if(did.eq.HDC_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,
     $             HMAX_DC_HITS, HDC_RAW_TOT_HITS, HDC_RAW_LAYER_NUM,
     $             HDC_RAW_WIRE_NUM, 1,HDC_RAW_TDC,0, 0, 0)
            else if (did.eq.HSCIN_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,
     $             HMAX_RAW_SCIN_HITS, HSCIN_RAW_TOT_HITS,
     $             HSCIN_RAW_LAYER_NUM, HSCIN_RAW_COUNTER_NUM, 4,
     $             HSCIN_RAW_ADC_POS, HSCIN_RAW_ADC_NEG,
     $             HSCIN_RAW_TDC_POS, HSCIN_RAW_TDC_NEG)
              
            else if (did.eq.HAER_ID) then
*     
*     Aerogel has two tubes for each counter.  The detector
*     has both ADC's and TDC's signals.
*     
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,
     $             HMAX_AER_HITS, HAER_TOT_HITS, HAER_RAW_LAYER_NUM,
     $             HAER_RAW_COUNTER_NUM, 4, HAER_RAWADC_POS, 
     $             HAER_RAWADC_NEG,
     $             HAER_RAWTDC_POS, HAER_RAWTDC_NEG)
              
            else if (did.eq.HWAT_ID) then
*     
*     Water has two tubes for each counter.  The detector
*     has both ADC's and TDC's signals.
*     
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,
     $             HMAX_WAT_HITS, HWAT_TOT_HITS, HWAT_RAW_LAYER_NUM,
     $             HWAT_RAW_COUNTER_NUM, 4, HWAT_RAWADC_POS, 
     $             HWAT_RAWADC_NEG,
     $             HWAT_RAWTDC_POS, HWAT_RAWTDC_NEG)
              
            else if (did.eq.HLUC_ID) then
*     
*     Lucite has two tubes for each counter.  The detector
*     has both ADC's and TDC's signals.
*     
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,
     $             HMAX_LUC_HITS, HLUC_TOT_HITS, HLUC_RAW_LAYER_NUM,
     $             HLUC_RAW_COUNTER_NUM, 6, HLUC_RAWADC_POS, 
     $             HLUC_RAWADC_NEG,HLUC_RAWADC_TOT,
     $             HLUC_RAWTDC_POS, HLUC_RAWTDC_NEG,HLUC_RAWTDC_TOT)

            else if (did.eq.HMISC_ID) then
*     
*     This array is for data words that don't belong to a specific
*     detector counter.  Things like energy sums, and TDC's from various
*     points in the logic will go here.  Most likely we will set ADDR1
*     always to 1, and ADDR2 will start at 1.
*     
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             HMAX_MISC_HITS, HMISC_TOT_HITS, HMISC_RAW_ADDR1,
     $             HMISC_RAW_ADDR2, 1, HMISC_RAW_DATA, 0, 0, 0)
*     
*     1         2         3         4         5         6         7
*     23456789012345678901234567890123456789012345678901234567890123456789012
*     
              
*     
*     HNSS SSD 
*     
            else if(did.eq.EDC1_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             EMAX_DC1_HITS, EDC1_RAW_TOT_HITS, EDC1_RAW_LAYER_NUM,
     $             EDC1_RAW_WIRE_NUM,1 ,EDC1_RAW_TDC,0, 0, 0)
              
*     
*     HNSS HODOSCOPE
*     
            else if(did.eq.ESCIN_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             EMAX_SCIN_HITS, ESCIN_RAW_TOT_HITS, 
     $             ESCIN_RAW_LAYER_NUM,
     $             ESCIN_RAW_COUNTER_NUM,4 ,ESCIN_RAWADC_POS, 
     $             ESCIN_RAWADC_NEG,
     $             ESCIN_RAWTDC_POS, ESCIN_RAWTDC_NEG)
*     
*     HNSS MISC (Target Monitor, Vth parameter)
*
****Following part should be comment out in test bench DK     
            else if(did.eq.EMISC_ID.and.gen_run_number.ge.50000) then
               pointer = pointer +
     $          g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &          maxwords, did, 
     $          EMAX_MISC_HITS, EMISC_RAW_TOT_HITS, 
     $          EMISC_RAW_LAYER_NUM,
     $          EMISC_RAW_COUNTER_NUM,3 ,EMISC_RAW_ADC, 
     $          EMISC_RAW_DUMMY,
     $          EMISC_RAW_TDC, EMISC_RAW_DUMMY)
*     
*     BPM/Raster ADC values. 
*     
            else if (did.eq.GMISC_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did,
     $             GMAX_MISC_HITS, GMISC_TOT_HITS, GMISC_RAW_ADDR1,
     $             GMISC_RAW_ADDR2, 1, GMISC_RAW_DATA, 0, 0, 0)
              
*     
*     Data from Uninstrumented channels and slots go into a special array
*     
            else if (did.eq.UNINST_ID) then
              pointer = pointer +
     $             g_decode_fb_detector(lastslot, roc, bank(pointer), 
     &             maxwords, did, 
     $             GMAX_UNINST_HITS, GUNINST_TOT_HITS, GUNINST_RAW_ROCSLOT,
     $             GUNINST_RAW_SUBADD, 1, GUNINST_RAW_DATAWORD, 0, 0, 0)
              
            else
*     Should never get here.  Unknown detector ID's or did=-1 for bad ROC#
*     or SLOT# will come here.
*     
              print *,"BAD DID, unknown ROC,SLOT",roc,slot,did
              pointer = pointer + 1 ! Skip unknown detector id's
            endif
          else
            lastslot = slot
            pointer = pointer + 1 ! Skip Bad subaddresses (module header)
          endif
*     
        else
          pointer = pointer + 1 ! Skip bad slots
        endif
*     
      enddo
      
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
