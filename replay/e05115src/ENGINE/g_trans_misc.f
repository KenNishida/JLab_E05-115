      subroutine g_trans_misc(abort,errmsg)
*-------------------------------------------------------------------
* author: John Arrington
* created: 1/16/96
*
* g_trans_misc fills the gen_decoded_misc common block
*
* Revision 1.1  1996/01/22 15:14:10  saw
* Initial revision
*
*--------------------------------------------------------

      implicit none

      include 'gen_data_structures.cmn'
      Include 'gen_f1tdc.cmn'
      Include 'gen_rocid.cmn'
      INCLUDE 'gen_run_info.cmn'
      include 'hks_data_structures.cmn'
      include 'hks_scin_parms.cmn'
      include 'hks_scin_tof.cmn'

      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'g_trans_misc')

      integer*4 ihit,f1time,ttime,tdc,ich,isig

      save

      abort = .false.
      errmsg = ' '

      do ihit = 1 , gmisc_tot_hits
         f1time = g_f1_refer_time(HR_VME_ROCID,1,1)
         ttime = g_f1_trigger_time(HR_VME_ROCID)
         ich=gmisc_raw_addr1(ihit) !=layer
         isig=gmisc_raw_addr2(ihit) !=counter
c         gmisc_dec_data(ich,isig) = gmisc_raw_data(ihit)
         if (ich .eq. 5) then
            gmisc_dec_data(ich,isig) = gmisc_raw_data(ihit)
            call hf1(gidbpmadcs(isig),float(gmisc_dec_data(ich,isig)),1.)
         else
            tdc = gmisc_raw_data(ihit) ! why not work?
c         hmisc_scaler(ich,isig) = hmisc_scaler(ich,isig) + 1
            if(tdc.gt.0.and.tdc.lt.f1tdc_HR_gate_width) then
               if(tdc .le. -70000) then
                  gmisc_dec_data(ich,isig) = -70000
               Else
                  if(tdc.lt.ttime) then
                     tdc=tdc+f1tdc_HR_gate_width 
                  endif
                  if(f1time.lt.ttime) then
                     f1time=f1time+f1tdc_HR_gate_width 
                  endif
                  gmisc_dec_data(ich,isig) = tdc
     &              -f1time+1444/f1tdc_HR_tdc_to_time
               EndIf
            else 
               gmisc_dec_data(ich,isig) = -70000
            endif  
            if (isig.eq.1.and.gidmisctdcs(1).gt.0.and.ich.eq.1) then !RF1 TDC
               call hf1(gidmisctdcs(1),float(gmisc_dec_data(ich,isig)),1.)
c               call hf1(gidmisctdcs(1),float(gmisc_raw_data(ihit)),1.)
            endif
            if (isig.eq.1.and.gidmisctdcs(2).gt.0.and.ich.eq.2) then !RF2 TDC
               call hf1(gidmisctdcs(2),float(gmisc_dec_data(ich,isig)),1.)
c                call hf1(gidmisctdcs(2),float(gmisc_raw_data(ihit)),1.)
            endif
            if (gen_run_number.lt.75182) then
               if (isig.eq.1.and.gidmisctdcs(3).gt.0.and.ich.eq.5) then !HKS0 TDC
                  call hf1(gidmisctdcs(3),float(gmisc_dec_data(ich,isig)),1.)
               endif
               if (isig.eq.1.and.gidmisctdcs(4).gt.0.and.ich.eq.6) then !HES0 TDC
                  call hf1(gidmisctdcs(4),float(gmisc_dec_data(ich,isig)),1.)
               endif
            endif
            if (isig.eq.1.and.gidmisctdcs(5).gt.0.and.ich.eq.7) then !TDC
               call hf1(gidmisctdcs(5),float(gmisc_dec_data(ich,isig)),1.)
            endif
            if (isig.eq.1.and.gidmisctdcs(6).gt.0.and.ich.eq.8) then !TDC
               call hf1(gidmisctdcs(6),float(gmisc_dec_data(ich,isig)),1.)
            endif
         endif 
      enddo

      return
      end
