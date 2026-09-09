      subroutine h_strip_scin(abort,err)
      
*-------------------------------------------------------------------
*     author: John Arrington
*     created: 6/25/94
*     
*     h_strip_scin converts the raw hits to arrays over hits
*     with good TDC values.
*     $Log: h_strip_scin.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.4  2005/01/05 20:11:05  sumihama
*     h_trans updata
*
*     Revision 1.3  2004/12/24 21:37:10  miyoshi
*     change name plane to layer
*
*     Revision 1.2  2004/12/24 19:47:07  miyoshi
*     change minor
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.8 2004/04 Miyoshi
*     for E01-011
*     
*     Revision 1.7  1999/02/23 19:00:39  csa
*     (JRA) Remove sdebugcalcpeds stuff
*     
*     Revision 1.6  1996/01/17 18:57:36  cdaq
*     (JRA) Add sdebugcalcpeds flag
*     
*     Revision 1.5  1995/08/31 20:44:25  cdaq
*     (JRA) Accumulate pedestals from pedestal events.
*     
*     Revision 1.4  1995/05/22  19:45:56  cdaq
*     (SAW) Split gen_data_data_structures 
*     into gen, hms, sos, and coin parts"
*     
*     Revision 1.3  1995/05/11  15:02:18  cdaq
*     (JRA) Cosmetic changes
*     
*     Revision 1.2  1995/02/10  19:14:37  cdaq
*     JRA) Make sscin_all_adc_pos/neg floating
*     
*     Revision 1.1  1994/11/23  14:01:45  cdaq
*     Initial revision
*     
*-------------------------------------------------------------------
      
      implicit none
      
      include 'hks_data_structures.cmn'
      include 'hks_scin_parms.cmn'
      include 'hks_scin_tof.cmn'
      include 'hks_tracking.cmn'
      include 'gen_decode_common.cmn'
      include 'gen_rocid.cmn'
      include 'gen_f1tdc.cmn'
      
      logical abort
      character*(*) err
      character*12 here
      parameter (here = 'h_strip_scin')
      
      integer*4 ihit,igoodhit,ind,layer,counter
      integer*4 la,co,i,f1_on
      Integer*4 tmin,tmax,amin,amax
      Real*4    adcpos, adcneg
      Integer*4 tdcpos, tdcneg
      
      save
      abort = .false.
      
      igoodhit = 0
      hscin_tot_hits = 0
      do ind = 1, hnum_scin_layers
         hscin_hits_per_layer(ind) = 0
         hscin_sing_counter(ind) = -1
      enddo
      
c     --- for test ---
      
      f1_on = 0
      Do i=1,5
c     Write(*,*) 'trigger(1,2)=',g_f1_refer_time(1,1,i),
c     &        g_f1_refer_time(1,2,i)
         if(g_f1_refer_time(HKS_FB_ROCID,1,i) .ge. 0) then
            f1_on = 1
         EndIf
      EndDo
      if(f1_on .ne. 0) then
         
         
      EndIf

c     Write(*,*) '(h_strip_scin.f) total=',hscin_all_tot_hits
c     Do ihit=1,hscin_all_tot_hits
c     Write(*,*) 'pl,co,adc+-,tdc+-=',hscin_all_layer_num(ihit),
c     &        hscin_all_counter_num(ihit),
c     &        hscin_all_adc_pos(ihit),hscin_all_adc_neg(ihit),
c     &        hscin_all_tdc_pos(ihit),hscin_all_tdc_neg(ihit)
c     EndDo
      
c     --- end of test ---
      
      amin = 0.0
      amax = 8000.
      tmin = -10000
      tmax = 0.
c      amin = 0.0
c      amax = 8000.
c      tmin = hscin_tdc_min
c      tmax = hscin_tdc_max
      
      if(amin .le. 0) amin = 0. 
      if(amax .le. 0) amax = 8000. 
      if(tmin .le. 0) tmin = 0. 
      if(tmax .le. 0) tmax = 4000. 
      
      do ihit = 1 , hscin_all_tot_hits ! pick out 'good' hits.
         la = hscin_all_layer_num(ihit)
         co = hscin_all_counter_num(ihit)
         adcpos = real(hscin_all_adc_pos(ihit)) - hscin_all_ped_pos(la,co)
         adcneg = real(hscin_all_adc_neg(ihit)) - hscin_all_ped_neg(la,co)
         tdcpos = hscin_all_tdc_pos(ihit)
         tdcneg = hscin_all_tdc_neg(ihit)
         
*     *    Criteria for good hit is at least one valid tdc value.
         if(
     &        (adcpos .gt. amin .and. adcpos .lt. amax .and.
     &         tdcpos .gt. tmin .and. tdcpos .lt. tmax) 
     &        .OR.
     &        (adcneg .gt. amin .and. adcneg .lt. amax .and.
     &         tdcneg .gt. tmin .and. tdcneg .lt. tmax) 
     &        ) then
            
            igoodhit = igoodhit + 1
            hscin_tot_hits = hscin_tot_hits + 1
            hscin_layer_num(igoodhit) = la
            hscin_counter_num(igoodhit) = co
            hscin_adc_pos(igoodhit) = adcpos
            hscin_adc_neg(igoodhit) = adcneg
            hscin_tdc_pos(igoodhit) = tdcpos
            hscin_tdc_neg(igoodhit) = tdcneg

            hscin_hits_per_layer(hscin_layer_num(igoodhit)) = 
     $           hscin_hits_per_layer(hscin_layer_num(igoodhit)) + 1
*     djm register counter which is hit. if more than one counter is hit per event,
*     only the last one will be histogrammed. this will bias events which have more
*     than one hit per layer, so it's only really useful for looking at single hits.
*     if you need to see all the hits, then hardwire it. 
            if(la.ge.1.and.la.le.3) 
     &           hscin_sing_counter(la) = co
         endif
      enddo
      
      abort = .false.
      return
      end

