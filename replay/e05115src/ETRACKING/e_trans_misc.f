      subroutine e_trans_misc(abort,errmsg)
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
      include 'hes_data_structures.cmn'
      include 'hes_id_histid.cmn'
      include 'hes_scin_parms.cmn'
      Include 'gen_f1tdc.cmn'
      Include 'gen_rocid.cmn'
      
      
      logical abort
      character*1024 errmsg
      character*20 here
      parameter (here = 'e_trans_misc')
      
      integer*4 ihit,ich,isig,tdc,f1time
      
      save
      
c      f1time = g_f1_refer_time(HR_VME_ROCID,1,1)
c      
c      do ihit = 1 , gmax_misc_hits
c         gmisc_dec_data(ihit,1) = 0 ! Clear TDC's
c         gmisc_dec_data(ihit,2) = -1 ! Clear ADC's
c      enddo
      
c      do ihit = 1 , gmisc_tot_hits
c         ich=gmisc_raw_addr2(ihit)
c         isig=gmisc_raw_addr1(ihit)
c         tdc = gmisc_raw_data(ihit)
c         hmisc_scaler(ich,isig) = hmisc_scaler(ich,isig) + 1
c         if(tdc .le. -70000) then
c            gmisc_dec_data(ihit,isig) = -70000
c         Else
c            if(tdc.lt.f1time) then
c               gmisc_dec_data(ihit,isig) = tdc - f1time
c            else
c               gmisc_dec_data(ihit,isig) = tdc
c     &              - (f1time+escin_f1tdc_gate_width)
c            endif
c         EndIf           
c         if (isig.eq.1.and.eidmisctdcs(1).gt.0.and.ich.eq.97) then !TDC
c            call hf1(eidmisctdcs(1),float(gmisc_dec_data(ich,isig)),1.)
c         endif
c         if (isig.eq.1.and.eidmisctdcs(2).gt.0.and.ich.eq.98) then !TDC
c            call hf1(eidmisctdcs(2),float(gmisc_dec_data(ich,isig)),1.)
c         endif
c      enddo

c-- 2 16ns RF signals at ROC13, use the average of these 
c--   two signals                    06/10/2005
c--   The two signals have shift, only choose the stable pass 
      
      esrfdiff = -100
      esrftime = -100

      if(gmisc_dec_data(1,1).gt.0.and.esrftime.eq.-100) then
         esrftime = gmisc_dec_data(1,1)*escin_tdc_to_time
         if(gmisc_dec_data(2,1).gt.0) then
            esrfdiff = (gmisc_dec_data(1,1)-gmisc_dec_data(2,1))*
     &       escin_tdc_to_time       
         else
            esrfdiff = -100
         endif
      elseif(gmisc_dec_data(2,1).gt.0.and.esrftime.eq.-100) then
         esrftime = gmisc_dec_data(2,1)*escin_tdc_to_time
         esrfdiff = -100
      elseif(esrftime.eq.-100) then
         esrftime = -100
         esrfdiff = -100
      endif

      Call HF1(eidsrfdiff,esrfdiff,1.)
      Call HF1(eidsrftime,esrftime,1.)

      
      return
      end
