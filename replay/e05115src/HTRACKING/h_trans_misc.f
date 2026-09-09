      subroutine h_trans_misc(abort,errmsg)
*-------------------------------------------------------------------
*     author: John Arrington
*     created: 4/8/95
*     
*     s_trans_misc fills the sos_decoded_misc common block
*     
*     $Log: h_trans_misc.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.4  2005/07/09 02:12:05  cdaq
*     Mod. rf calc.
*
*     Revision 1.3  2005/07/06 19:21:11  sumihama
*     Mod.hrf/erf
*
*     Revision 1.2  2005/06/14 04:31:09  cdaq
*     Mod. rf histgram
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.6  1999/01/27 16:02:45  saw
*     Check if some hists are defined before filling
*     
*     Revision 1.5  1996/09/04 20:18:07  saw
*     (JRA) Add misc. tdc's
*     
*     Revision 1.4  1996/01/24 16:08:38  saw
*     (JRA) Replace 48 with smax_misc_hits
*     
*     Revision 1.3  1996/01/17 18:12:35  cdaq
*     (JRA) Misc. fixes.
*     
*     Revision 1.2  1995/05/22 19:46:03  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.1  1995/04/12  03:59:23  cdaq
*     Initial revision
*     
*     
*--------------------------------------------------------
      
      implicit none
      
      include 'gen_data_structures.cmn'
      include 'hks_data_structures.cmn'
      include 'hks_id_histid.cmn'
      include 'hks_scin_tof.cmn'
      include 'hks_scin_parms.cmn'
      Include 'gen_f1tdc.cmn'
      Include 'gen_rocid.cmn'
      
      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'h_trans_misc')
      
      integer*4 ihit,ich,isig,tdc,f1time,ttime
      
      save
      
      
c      do ihit = 1 , gmax_misc_hits
c         gmisc_dec_data(ihit,1) = 0 ! Clear TDC's
c         gmisc_dec_data(ihit,2) = -1 ! Clear ADC's
c      enddo
      
c      do ihit = 1 , gmisc_tot_hits
c         f1time = g_f1_refer_time(HR_VME_ROCID,1,1)
c         ttime = g_f1_trigger_time(HR_VME_ROCID)
c         ich=gmisc_raw_addr1(ihit)
c         isig=gmisc_raw_addr2(ihit)
c         gmisc_dec_data(ich,isig) = gmisc_raw_data(ihit)
c         tdc = gmisc_raw_data(ihit) ! why not work?
c         hmisc_scaler(ich,isig) = hmisc_scaler(ich,isig) + 1
c         if(tdc .gt. 0) then
c            if(tdc .le. -70000) then
c               gmisc_dec_data(ihit,isig) = -70000
c            Else
c               if(tdc.lt.ttime) then
c                  tdc=tdc+hscin_f1tdc_gate_width 
c               endif
c               if(f1time.lt.ttime) then
c                  f1time=f1time+hscin_f1tdc_gate_width 
c               endif
c               gmisc_dec_data(ihit,isig) = tdc
c     &              -f1time+1444/hscin_tdc_to_time
c            EndIf         
c         endif  
c         if (isig.eq.1.and.hidmisctdcs(1).gt.0.and.ich.eq.1) then !TDC
c            call hf1(hidmisctdcs(1),float(gmisc_dec_data(ich,isig)),1.)
c         endif
c         if (isig.eq.1.and.hidmisctdcs(2).gt.0.and.ich.eq.2) then !TDC
c            call hf1(hidmisctdcs(2),float(gmisc_dec_data(ich,isig)),1.)
c         endif
c         if (isig.eq.1.and.hidmisctdcs(3).gt.0.and.ich.eq.5) then !TDC
c            call hf1(hidmisctdcs(3),float(gmisc_dec_data(ich,isig)),1.)
c         endif
c         if (isig.eq.1.and.hidmisctdcs(4).gt.0.and.ich.eq.6) then !TDC
c            call hf1(hidmisctdcs(4),float(gmisc_dec_data(ich,isig)),1.)
c         endif
c         if (isig.eq.1.and.hidmisctdcs(5).gt.0.and.ich.eq.7) then !TDC
c            call hf1(hidmisctdcs(5),float(gmisc_dec_data(ich,isig)),1.)
c         endif
c         if (isig.eq.1.and.hidmisctdcs(6).gt.0.and.ich.eq.8) then !TDC
c            call hf1(hidmisctdcs(6),float(gmisc_dec_data(ich,isig)),1.)
c         endif
c      enddo

c-- 2 16ns RF signals at ROC13, use the average of these 
c--   two signals                    06/10/2005
c--   The two signals have shift, only choose the stable pass 
      
      hsrfdiff = -100
      hsrftime = -100

      if(gmisc_dec_data(1,1).gt.0.and.hsrftime.eq.-100) then
         hsrftime = gmisc_dec_data(1,1)*hscin_tdc_to_time
         if(gmisc_dec_data(2,1).gt.0) then
            hsrfdiff = (gmisc_dec_data(1,1)-gmisc_dec_data(2,1))*
     &       hscin_tdc_to_time
         else
            hsrfdiff = -100
         endif
      elseif(gmisc_dec_data(2,1).gt.0.and.hsrftime.eq.-100) then
         hsrftime = gmisc_dec_data(2,1)*hscin_tdc_to_time
         hsrfdiff = -100
      elseif(hsrftime.eq.-100) then
         hsrftime = -100
         hsrfdiff = -100
      endif

      Call HF1(hidsrfdiff,hsrfdiff,1.)
      Call HF1(hidsrftime,hsrftime,1.)

      
      return
      end
