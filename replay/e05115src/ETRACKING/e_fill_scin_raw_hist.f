      Subroutine e_fill_scin_raw_hist(ABORT,err)
*--------------------------------------------------------
*     Routine to fill histograms with hnss_raw_scin varibles
*     
*     $Log: e_fill_scin_raw_hist.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.3  2005/05/30 22:29:18  miyoshi
*     add/delete histogram
*
*     Revision 1.2  2005/05/28 00:43:03  cdaq
*     change/add f1trigtime hist
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.5  2005/04/08 18:49:13  miyoshi
*     add condition
*
*     Revision 1.4  2005/01/24 19:54:26  miyoshi
*     correct/add histogram
*
*     Revision 1.3  2005/01/14 22:42:58  miyoshi
*     add histogram
*
*     Revision 1.2  2004/12/24 21:27:03  miyoshi
*     add some histogram
*     
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*     
*     Revision 1.2  2000/03/09 01:32:40  ysato
*     Update in the production run Mar.8
*     
*     Revision 1.1  1999/12/23 19:59:24  ysato
*     Compiled on Redhat Linux
*     
*     
*     Novermber 4, 1999        Y.Sato       A first draft
*     
*     Input Banks    hnss_raw_scin
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_fill_scin_raw_hist')
*     
      Logical ABORT
      Character*(*) err
      
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"
      Include "hes_scin_parms.cmn"
      Include "gen_event_info.cmn"
      Include "gen_f1tdc.cmn"
      Include 'gen_rocid.cmn'
      
      Integer*4 i,la,co,tdcpos,tdcneg
      Integer*4 la2,co2,t1,t2,j
      Integer*4 ctothits(6)     
c     pos1,neg1,pos2,neg2,both1,both2
      Integer*4 tmin,tmax,adcpos,adcneg
      Integer*4 amin,amax
c     --- user need to change below.
      
      ABORT= .FALSE.
      err= ' '
      
c     Write(*,*) ' in e fill scin raw hist .f'
c     Write(*,*) eidscinrawtothit,escin_raw_tot_hits
c     --- Since this is initial check,
c     adcmin-max is FASTBUS adc range.
c     tdcmin-max is F1TDC tdc range.
      amin = 0
      amax = 8192
      tmin = -60000
      tmax = 60000
      
c     --- fill reference signal ---
      t1 = g_f1_refer_time(HR_VME_ROCID,1,1)
      Call Hf1(gidf1trigtimehist(1,1,1),float(t1),1.)
      
      Do i=1,6
         ctothits(i) = 0
      EndDo
      
      Call Hf1(eidscinrawtothit,Float(escin_raw_tot_hits),1.)
      Call Hf1(eidscinrawtothitzoom,Float(escin_raw_tot_hits),1.)
      
      if(escin_raw_tot_hits .le. 0) Return
      
      Do i=1,escin_raw_tot_hits
         la = escin_raw_layer_num(i)
         co = escin_raw_counter_num(i)
         tdcpos = escin_rawtdc_pos_sub_trig(i)
         tdcneg = escin_rawtdc_neg_sub_trig(i)
         adcpos = escin_rawadc_pos(i)
         adcneg = escin_rawadc_neg(i)
c     if(tdcpos .gt. -70000 .and. tdcneg .gt. -70000) then
c      Write(*,*) gen_event_ID_number,i,la,co,tdcpos,tdcneg
c     endif
         if(tdcpos.gt.tmin .and. tdcpos.lt.tmax) then
            Call Hf1(eidscinrawtdchitpatpos(la),Float(co),1.)
            Call Hf1(eidscinrawsumpostdc(la),Float(tdcpos),1.)
            if(la .eq. 1) then
               ctothits(1) = ctothits(1) + 1
            Else if(la .eq. 2) then
               ctothits(3) = ctothits(3) + 1
            EndIf
         endif
         
         if(adcpos .gt. amin .and. adcpos .lt. amax) then
            Call Hf1(eidscinrawadchitpatpos(la),Float(co),1.)
            Call Hf1(eidscinrawsumposadc(la),Float(adcpos),1.)
         endif
         
         if(tdcneg .gt. tmin .and. tdcneg .lt. tmax) then
            Call Hf1(eidscinrawtdchitpatneg(la),Float(co),1.)
            Call Hf1(eidscinrawsumnegtdc(la),Float(tdcneg),1.)

            if(la .eq. 1) then
               ctothits(2) = ctothits(2) + 1
            Else if(la .eq. 2) then
               ctothits(4) = ctothits(4) + 1
            EndIf
         endif
         
         if(adcneg .gt. amin .and. adcneg .lt. amax) then
            Call Hf1(eidscinrawadchitpatneg(la),Float(co),1.)
            Call Hf1(eidscinrawsumnegadc(la),Float(adcneg),1.)
         endif

         if(tdcpos.gt.tmin .and. tdcpos.lt.tmax .and.
     &        tdcneg.gt.tmin .and. tdcneg.lt.tmax) then
            
            if(la .eq. 1) then
               ctothits(5) = ctothits(5) + 1
            Else if(la .eq. 2) then
               ctothits(6) = ctothits(6) + 1
            EndIf
         endif

         if(eturnon_scin_raw_hist .eq. 1) then
            Call Hf1(eidscinrawposadc(la,co),Float(escin_rawadc_pos(i)),1.)
            Call Hf1(eidscinrawnegadc(la,co),Float(escin_rawadc_neg(i)),1.)
            if(tdcpos .gt. -70000) then
               Call Hf1(eidscinrawpostdc(la,co),Float(tdcpos),1.)
            endif
            if(tdcneg .gt. -70000) then
               Call Hf1(eidscinrawnegtdc(la,co),Float(tdcneg),1.)
            endif
         EndIf
      EndDo                     ! escinrawtothits

c     --- fill tothits for group with good tdc
      Call HF1(eidscinrawtothitpos(1),Float(ctothits(1)),1.)
      Call HF1(eidscinrawtothitneg(1),Float(ctothits(2)),1.)
      Call HF1(eidscinrawtothitpos(2),Float(ctothits(3)),1.)
      Call HF1(eidscinrawtothitneg(2),Float(ctothits(4)),1.)
      Call HF1(eidscinrawtothitboth(1),Float(ctothits(5)),1.)
      Call HF1(eidscinrawtothitboth(2),Float(ctothits(6)),1.)

c     Write(*,*) 'end if e-fill-scin-raw-hist'

      Return
      end
