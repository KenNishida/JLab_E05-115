      subroutine h_calc_pedestal(ABORT,err)
*     
*     $Log: h_calc_pedestal.f,v $
*     Revision 1.1.1.1  2009/08/17 22:49  yez
*     Add Lucite sum channels
*
*     Revision 1.1.1.1  2009/07/09 23:03  yez
*     Add Lucite info for E05-115
*
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.2  2005/06/12 20:37:58  cdaq
*     change output file
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.3  2005/03/14 19:50:49  miyoshi
*     change variables names
*
*     Revision 1.2  2004/12/24 21:33:07  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.1.1.1  1999/11/01 13:54:50  ysato
*     Upgrade for HNSS
*     
*     Revision 1.14 2004/03/02 Miyoshi
*     for E01-011
*     
*     Revision 1.13  1999/02/23 18:57:19  csa
*     (JRA) Sparsify aerogel/lucite channels, cleanup
*     
*     Revision 1.12  1999/02/03 21:13:44  saw
*     Code for new Shower counter tubes
*
*     Revision 1.11  1999/01/29 17:34:57  saw
*     Add variables for second tubes on shower counter
*     
*     Revision 1.10  1996/11/07 19:49:46  saw
*     (WH) Add calculations for Lucite Cerenkov
*     
*     Revision 1.9  1996/09/05 13:15:15  saw
*     (JRA) Slight increase in threshold above pedestal
*     
*     Revision 1.8  1996/01/24 16:06:55  saw
*     (JRA) Adjust which channels are disabled on Areogel
*     
*     Revision 1.7  1996/01/17 19:03:49  cdaq
*     (JRA) Fixes, write results to file.
*     
*     Revision 1.6  1995/10/09 20:12:30  cdaq
*     (JRA) Note pedestals that differ by 2 sigma 
*     from parameter file
*     
*     Revision 1.5  1995/08/31 18:04:55  cdaq
*     (JRA) Change threshold limits
*     
*     Revision 1.4  1995/07/20  14:46:39  cdaq
*     (JRA) Cleanup statistics calculations
*     
*     Revision 1.3  1995/05/22  19:45:32  cdaq
*     (SAW) Split gen_data_data_structures 
*     into gen, hms, sos, and coin parts"
*     
*     Revision 1.2  1995/05/17  16:42:40  cdaq
*     (JRA) Add gas cerenkov and Aerogel, 
*     float integer accumulators before arithmetic
*     
*     Revision 1.1  1995/04/01  19:36:03  cdaq
*     Initial revision
*     
*     
      implicit none
      save
*     
      character*18 here
      parameter (here='h_calc_pedestal')
*     
      logical ABORT
      character*(*) err
*     
      integer*4 pln,cnt
      integer*4 blk
      integer*4 pmt
      integer*4 ind
      integer*4 roc,slot
      integer*4 signalcount
      real*4 sig2
      real*4 num
      
*     threshold set more than 1,2,or3-sigma from
*     pedestal center
      
      Real*4 sigma_threshold_hodo
      Real*4 sigma_threshold_aer
      Real*4 sigma_threshold_wat
      Real*4 sigma_threshold_luc

      Parameter(sigma_threshold_hodo=2.)
      Parameter(sigma_threshold_aer=2.)
      Parameter(sigma_threshold_wat=2.)
      Parameter(sigma_threshold_luc=2.)

*     these values are added to thresholds.
*     I(Miyoshi)'m not sure why it is 15.
      
      Real*4 add_threshold_hodo
      Real*4 add_threshold_aer
      Real*4 add_threshold_wat
      Real*4 add_threshold_luc
      
      Parameter(add_threshold_hodo=15.)
      Parameter(add_threshold_aer=15.)
      Parameter(add_threshold_wat=15.)
      Parameter(add_threshold_luc=15.)
      
      character*80 file
*     
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'hks_pedestals.cmn'
      INCLUDE 'hks_scin_parms.cmn'
      INCLUDE 'hks_filenames.cmn'
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'gen_detectorids.par' ! for ROC and slot ID
      INCLUDE 'gen_rocid.cmn'
*     
      integer HPAREID
      parameter (HPAREID=67)
*     
*     
*     HODOSCOPE PEDESTALS
*     
      ind = 0
      do pln = 1 , hnum_scin_layers
         do cnt = 1 , hnum_scin_counters(pln)
            
*     calculate new pedestal values, positive tubes first.
            num=max(1.,float(hscin_pos_ped_num(pln,cnt)))
            hscin_new_ped_pos(pln,cnt) = 
     &           float(hscin_pos_ped_sum(pln,cnt)) / num
            sig2 = float(hscin_pos_ped_sum2(pln,cnt))/num -
     $           hscin_new_ped_pos(pln,cnt)**2
            hscin_new_sig_pos(pln,cnt) = sqrt(max(0.,sig2))
            hscin_new_threshold_pos(pln,cnt) = 
     $           hscin_new_ped_pos(pln,cnt) + add_threshold_hodo
            
*     note channels with 2 sigma difference from paramter file values.
            if (abs(hscin_raw_ped_pos(pln,cnt)
     &           -hscin_new_ped_pos(pln,cnt)) .ge. 
     &           (sigma_threshold_hodo*hscin_new_sig_pos(pln,cnt))) then
               ind = ind + 1    !final value of 'ind' is saved at end of loop
               hscin_changed_layer(ind)=pln
               hscin_changed_element(ind)=cnt
               hscin_changed_sign(ind)= 1 !1=pos,2=neg.
               hscin_ped_change(ind) = hscin_new_ped_pos(pln,cnt) -
     &              hscin_raw_ped_pos(pln,cnt)
            endif               !large pedestal change
            
*     replace old peds (from param file) with calculated pedestals
            if (num.gt.hscin_min_peds .and. hscin_min_peds.ne.0) then
               hscin_raw_ped_pos(pln,cnt)=hscin_new_ped_pos(pln,cnt)
            endif
            
*     do it all again for negative tubes.
            num=max(1.,float(hscin_neg_ped_num(pln,cnt)))
            hscin_new_ped_neg(pln,cnt) = float(hscin_neg_ped_sum(pln,cnt)) 
     $           / num
            sig2 = float(hscin_neg_ped_sum2(pln,cnt))/num -
     $           hscin_new_ped_neg(pln,cnt)**2
            hscin_new_sig_neg(pln,cnt) = sqrt(max(0.,sig2))
            hscin_new_threshold_neg(pln,cnt) = 
     $           hscin_new_ped_neg(pln,cnt)+add_threshold_hodo
            
            if (abs(hscin_raw_ped_neg(pln,cnt)
     &           -hscin_new_ped_neg(pln,cnt)).ge.
     &           (sigma_threshold_hodo*hscin_new_sig_neg(pln,cnt))) then
               ind = ind + 1
               hscin_changed_layer(ind)=pln
               hscin_changed_element(ind)=cnt
               hscin_changed_sign(ind)= 2 !1=pos, 2=neg.
               hscin_ped_change(ind) = hscin_new_ped_neg(pln,cnt) -
     &              hscin_raw_ped_neg(pln,cnt)
            endif               !large pedestal change
            
            if (num.gt.hscin_min_peds .and. hscin_min_peds.ne.0) then
               hscin_raw_ped_neg(pln,cnt)=hscin_new_ped_neg(pln,cnt)
            endif
            
         enddo                  !counters
      enddo                     !layers

      hscin_num_ped_changes = ind

*     
*     AEROGEL CERENKOV PEDESTALS
*     
      Do pln = 1, HNUM_AER_LAYERS
         Do cnt = 1,HNUM_AER_COUNTERS
*     calculate new pedestal values, positive tubes first.
            num=max(1.,float(haer_pos_ped_num(pln,cnt)))
            haer_new_ped_pos(pln,cnt) = float(haer_pos_ped_sum(pln,cnt)) 
     $           / num
            sig2 = float(haer_pos_ped_sum2(pln,cnt))/num -
     $           haer_new_ped_pos(pln,cnt)**2
            haer_new_sig_pos(pln,cnt) = sqrt(max(0.,sig2))
            haer_new_threshold_pos(pln,cnt) = 
     $           haer_new_ped_pos(pln,cnt) + add_threshold_aer
            
*     note channels with 2 sigma difference from paramter file values.
            if (abs(haer_pos_ped_mean(pln,cnt)
     &           -haer_new_ped_pos(pln,cnt)).ge.
     &           (sigma_threshold_aer*haer_new_sig_pos(pln,cnt))) then
               ind = ind + 1    !final value of 'ind' is saved at end of loop
               haer_changed_layer(ind)=pln
               haer_changed_element(ind)=cnt
               haer_changed_sign(ind)= 1 !1=pos,2=neg.
               haer_ped_change(ind) = haer_new_ped_pos(pln,cnt) -
     &              haer_pos_ped_mean(pln,cnt)
            endif               !large pedestal change
            
*     replace old peds (from param file) with calculated pedestals
            if (num.gt.haer_min_peds .and. haer_min_peds.ne.0) then
               haer_pos_ped_mean(pln,cnt)=haer_new_ped_pos(pln,cnt)
            endif
            
*     do it all again for negative tubes.
            num=max(1.,float(haer_neg_ped_num(pln,cnt)))
            haer_new_ped_neg(pln,cnt) = float(haer_neg_ped_sum(pln,cnt)) 
     $           / num
            sig2 = float(haer_neg_ped_sum2(pln,cnt))/num -
     $           haer_new_ped_neg(pln,cnt)**2
            haer_new_sig_neg(pln,cnt) = sqrt(max(0.,sig2))
            haer_new_threshold_neg(pln,cnt) = 
     $           haer_new_ped_neg(pln,cnt)+add_threshold_aer
            
            if (abs(haer_neg_ped_mean(pln,cnt)
     &           -haer_new_ped_neg(pln,cnt)) .ge. 
     &           (sigma_threshold_aer*haer_new_sig_neg(pln,cnt))) then
               ind = ind + 1
               haer_changed_layer(ind)=pln
               haer_changed_element(ind)=cnt
               haer_changed_sign(ind)= 2 !1=pos, 2=neg.
               haer_ped_change(ind) = haer_new_ped_neg(pln,cnt) -
     &              haer_neg_ped_mean(pln,cnt)
            endif               !large pedestal change
            
            if (num.gt.haer_min_peds .and. haer_min_peds.ne.0) then
               haer_neg_ped_mean(pln,cnt)=haer_new_ped_neg(pln,cnt)
            endif
         EndDo                  ! counter loop
      Enddo                     ! layer loop

      haer_num_ped_changes = ind

*     
*     WATER CERENKOV PEDESTALS
*     
      Do pln = 1, HNUM_WAT_LAYERS
         Do cnt = 1,HNUM_WAT_COUNTERS
*     calculate new pedestal values, positive tubes first.
            num=max(1.,float(hwat_pos_ped_num(pln,cnt)))
            hwat_new_ped_pos(pln,cnt) = float(hwat_pos_ped_sum(pln,cnt)) 
     $           / num
            sig2 = float(hwat_pos_ped_sum2(pln,cnt))/num -
     $           hwat_new_ped_pos(pln,cnt)**2
            hwat_new_sig_pos(pln,cnt) = sqrt(max(0.,sig2))
            hwat_new_threshold_pos(pln,cnt) = 
     $           hwat_new_ped_pos(pln,cnt) + add_threshold_wat
            
*     note channels with 2 sigma difference from paramter file values.
            if (abs(hwat_pos_ped_mean(pln,cnt)
     &           -hwat_new_ped_pos(pln,cnt)).ge.
     &           (sigma_threshold_wat*hwat_new_sig_pos(pln,cnt))) then
               ind = ind + 1    !final value of 'ind' is saved at end of loop
               hwat_changed_layer(ind)=pln
               hwat_changed_element(ind)=cnt
               hwat_changed_sign(ind)= 1 !1=pos,2=neg.
               hwat_ped_change(ind) = hwat_new_ped_pos(pln,cnt) -
     &              hwat_pos_ped_mean(pln,cnt)
            endif               !large pedestal change
            
*     replace old peds (from param file) with calculated pedestals
            if (num.gt.hwat_min_peds .and. hwat_min_peds.ne.0) then
               hwat_pos_ped_mean(pln,cnt)=hwat_new_ped_pos(pln,cnt)
            endif
            
*     do it all again for negative tubes.
            num=max(1.,float(hwat_neg_ped_num(pln,cnt)))
            hwat_new_ped_neg(pln,cnt) = float(hwat_neg_ped_sum(pln,cnt)) 
     $           / num
            sig2 = float(hwat_neg_ped_sum2(pln,cnt))/num -
     $           hwat_new_ped_neg(pln,cnt)**2
            hwat_new_sig_neg(pln,cnt) = sqrt(max(0.,sig2))
            hwat_new_threshold_neg(pln,cnt) = 
     $           hwat_new_ped_neg(pln,cnt)+add_threshold_wat
            
            if (abs(hwat_neg_ped_mean(pln,cnt)
     &           -hwat_new_ped_neg(pln,cnt)) .ge. 
     &           (sigma_threshold_wat*hwat_new_sig_neg(pln,cnt))) then
               ind = ind + 1
               hwat_changed_layer(ind)=pln
               hwat_changed_element(ind)=cnt
               hwat_changed_sign(ind)= 2 !1=pos, 2=neg.
               hwat_ped_change(ind) = hwat_new_ped_neg(pln,cnt) -
     &              hwat_neg_ped_mean(pln,cnt)
            endif               !large pedestal change
            
            if (num.gt.hwat_min_peds .and. hwat_min_peds.ne.0) then
               hwat_neg_ped_mean(pln,cnt)=hwat_new_ped_neg(pln,cnt)
            endif
         EndDo                  ! counter loop
      Enddo                     ! layer loop

      hwat_num_ped_changes = ind

*     
*     LUCITE CERENKOV PEDESTALS
*     2009/07/09 Z.Ye     
*     Add hardware sum: tot=neg+pos 2009/08/17 Z.Ye
*
      Do pln = 1, HNUM_LUC_LAYERS
         Do cnt = 1,HNUM_LUC_COUNTERS
*     calculate new pedestal values, positive tubes first.
            num=max(1.,float(hluc_pos_ped_num(pln,cnt)))
            hluc_new_ped_pos(pln,cnt) = float(hluc_pos_ped_sum(pln,cnt)) 
     $           / num
            sig2 = float(hluc_pos_ped_sum2(pln,cnt))/num -
     $           hluc_new_ped_pos(pln,cnt)**2
            hluc_new_sig_pos(pln,cnt) = sqrt(max(0.,sig2))
            hluc_new_threshold_pos(pln,cnt) = 
     $           hluc_new_ped_pos(pln,cnt) + add_threshold_luc
            
*     note channels with 2 sigma difference from paramter file values.
            if (abs(hluc_pos_ped_mean(pln,cnt)
     &           -hluc_new_ped_pos(pln,cnt)).ge.
     &           (sigma_threshold_luc*hluc_new_sig_pos(pln,cnt))) then
               ind = ind + 1    !final value of 'ind' is saved at end of loop
               hluc_changed_layer(ind)=pln
               hluc_changed_element(ind)=cnt
               hluc_changed_sign(ind)= 1 !1=pos,2=neg.
               hluc_ped_change(ind) = hluc_new_ped_pos(pln,cnt) -
     &              hluc_pos_ped_mean(pln,cnt)
            endif               !large pedestal change
            
*     replace old peds (from param file) with calculated pedestals
            if (num.gt.hluc_min_peds .and. hluc_min_peds.ne.0) then
               hluc_pos_ped_mean(pln,cnt)=hluc_new_ped_pos(pln,cnt)
            endif
            
*     do it all again for negative tubes.
            num=max(1.,float(hluc_neg_ped_num(pln,cnt)))
            hluc_new_ped_neg(pln,cnt) = float(hluc_neg_ped_sum(pln,cnt)) 
     $           / num
            sig2 = float(hluc_neg_ped_sum2(pln,cnt))/num -
     $           hluc_new_ped_neg(pln,cnt)**2
            hluc_new_sig_neg(pln,cnt) = sqrt(max(0.,sig2))
            hluc_new_threshold_neg(pln,cnt) = 
     $           hluc_new_ped_neg(pln,cnt)+add_threshold_luc
            
            if (abs(hluc_neg_ped_mean(pln,cnt)
     &           -hluc_new_ped_neg(pln,cnt)) .ge. 
     &           (sigma_threshold_luc*hluc_new_sig_neg(pln,cnt))) then
               ind = ind + 1
               hluc_changed_layer(ind)=pln
               hluc_changed_element(ind)=cnt
               hluc_changed_sign(ind)= 2 !1=pos, 2=neg.
               hluc_ped_change(ind) = hluc_new_ped_neg(pln,cnt) -
     &              hluc_neg_ped_mean(pln,cnt)
            endif               !large pedestal change
            
            if (num.gt.hluc_min_peds .and. hluc_min_peds.ne.0) then
               hluc_neg_ped_mean(pln,cnt)=hluc_new_ped_neg(pln,cnt)
            endif
*     do it all again for hardware sum of positive and negative tubes.
            num=max(1.,float(hluc_tot_ped_num(pln,cnt)))
            hluc_new_ped_tot(pln,cnt) = float(hluc_tot_ped_sum(pln,cnt)) 
     $           / num
            sig2 = float(hluc_tot_ped_sum2(pln,cnt))/num -
     $           hluc_new_ped_tot(pln,cnt)**2
            hluc_new_sig_tot(pln,cnt) = sqrt(max(0.,sig2))
            hluc_new_threshold_tot(pln,cnt) = 
     $           hluc_new_ped_tot(pln,cnt)+add_threshold_luc
 
c     -- I(Z.Ye)tried not to use sum channels to determine anything.
c            if (abs(hluc_tot_ped_mean(pln,cnt)
c     &           -hluc_new_ped_tot(pln,cnt)) .ge. 
c     &           (sigma_threshold_luc*hluc_new_sig_tot(pln,cnt))) then
c               ind = ind + 1
c               hluc_changed_layer(ind)=pln
c               hluc_changed_element(ind)=cnt
c               hluc_changed_sign(ind)= 2 !1=pos, 2=tot.
c               hluc_ped_change(ind) = hluc_new_ped_tot(pln,cnt) -
c     &              hluc_tot_ped_mean(pln,cnt)
c            endif               !large pedestal change
            
            if (num.gt.hluc_min_peds .and. hluc_min_peds.ne.0) then
               hluc_tot_ped_mean(pln,cnt)=hluc_new_ped_tot(pln,cnt)
            endif

         EndDo                  ! counter loop
      Enddo                     ! layer loop

      hluc_num_ped_changes = ind


*     
*     WRITE THRESHOLDS TO FILE FOR HARDWARE SPARCIFICATION
*     --- it's already opened in g_calc_beam_pedestal
*
c     if (h_threshold_output_filename.ne.' ') then
c     file=h_threshold_output_filename
c     call g_sub_run_number(file, gen_run_number)
c     
c     open(unit=HPAREID,file=file,status='unknown')
c     
c     write(HPAREID,*) 
c     &        '# This is the ADC threshold file generated automatically'
c     write(HPAREID,*) 
c     &        'from the pedestal data from run number ',gen_run_number
      
      roc=CH_FB_ROCID
      
*     please confirm slot number for each detector...
      
*     --- HTF1x,1y,2x
      slot=HSCIN_SLOT_ID1
      signalcount=2
      write(HPAREID,*) 'slot=',slot
      call g_output_thresholds(HPAREID,roc,slot,signalcount,
     &     hnum_scin_layers,
     &     hscin_new_threshold_pos,hscin_new_threshold_neg,
     &     hscin_new_sig_pos,
     &     hscin_new_sig_neg)
      
      slot=HSCIN_SLOT_ID2
      signalcount=2
      write(HPAREID,*) 'slot=',slot
      call g_output_thresholds(HPAREID,roc,slot,signalcount,
     &     hnum_scin_layers,
     &     hscin_new_threshold_pos,hscin_new_threshold_neg,
     &     hscin_new_sig_pos,
     &     hscin_new_sig_neg)
      
*     I don't know which slot is for aerogel and water...
      
*     --- aerogel
      
      slot=HAER_SLOT_ID
      signalcount=2
      write(HPAREID,*) 'slot=',slot
      call g_output_thresholds(HPAREID,roc,slot,signalcount,
     &     hnum_aer_layers,
     &     haer_new_threshold_pos,haer_new_threshold_neg,
     &     haer_new_sig_pos,
     &     haer_new_sig_neg)
      
*     --- water
      
      slot=HWAT_SLOT_ID
      signalcount=2
      write(HPAREID,*) 'slot=',slot
      call g_output_thresholds(HPAREID,roc,slot,signalcount,
     &     hnum_wat_layers,
     &     hwat_new_threshold_pos,hwat_new_threshold_neg,
     &     hwat_new_sig_pos,
     &     hwat_new_sig_neg)

*     --- lucite
      
      slot=HLUC_SLOT_ID
      signalcount=2
      write(HPAREID,*) 'slot=',slot
      call g_output_thresholds(HPAREID,roc,slot,signalcount,
     &     hnum_luc_layers,
     &     hluc_new_threshold_pos,hluc_new_threshold_neg,
     &     hluc_new_sig_pos,
     &     hluc_new_sig_neg)
     
      close (unit=HPAREID)
         
c     endif

      return
      end
