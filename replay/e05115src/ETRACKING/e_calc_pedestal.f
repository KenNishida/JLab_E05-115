      Subroutine e_calc_pedestal(ABORT,err)
*--------------------------------------------------------
* Hodoscope pedestals calculation
* 
* $Log: e_calc_pedestal.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.2  2005/06/12 20:37:26  cdaq
* change output data file name
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.6 2004/03/02 Miyoshi
* for E01-011
* Revision 1.5  2000/03/09 01:32:39  ysato
* Update in the production run Mar.8
*
* Revision 1.4  2000/02/23 14:36:20  fujii
* Bug fix
*
* Revision 1.3  1999/12/23 19:59:24  ysato
* Compiled on Redhat Linux
*
*
* This routine will be called from g_calc_pedestal
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Logical ABORT
      Character*(*) err
*--------------------------------------------------------
      Include "hes_data_structures.cmn"
      Include "hes_scin_parms.cmn"
      Include "hes_pedestals.cmn"
      Include "hes_filenames.cmn"
      Include "gen_run_info.cmn"
      Include "gen_detectorids.par"
      Include 'gen_rocid.cmn'

      character*50 here
      parameter (here='e_calc_pedestal')
*
      integer*4 pln,cnt
      integer*4 ind
      integer*4 roc,slot
      integer*4 signalcount
      real*4 temp_vec(100)
      real*4 sig2
      real*4 num
      character*80 file

      Real*4 sig_th
      Parameter (sig_th=1.5)
*
*     We use common id for h_ped,beam_ped and e_ped
*
      integer EPAREID
      parameter (EPAREID=67)
*
*
*     HODOSCOPE PEDESTALS
*     
c     --- e05-115 all the adc is in roc HKS_FB_ROCID=1.
      roc = CH_FB_ROCID
      
      ind = 0

c     Write(*,*) 'la,co=',enum_scin_layers,enum_scin_counters
      
      do pln = 1 , enum_scin_layers
         do cnt = 1 , enum_scin_counters
*     calculate new pedestal values
            num=max(1.,float(escin_pos_ped_num(pln,cnt)))
            escin_new_ped_pos(pln,cnt) = 
     $           float(escin_pos_ped_sum(pln,cnt)) / num
            sig2 = float(escin_pos_ped_sum2(pln,cnt))/num -
     $           escin_new_ped_pos(pln,cnt)**2
            escin_new_sig_pos(pln,cnt) = sqrt(max(0.,sig2))
            escin_new_threshold_pos(pln,cnt) = 
     $           escin_new_ped_pos(pln,cnt)+
     $           escin_new_sig_pos(pln,cnt)*sig_th
            
*     note counters with 2 sigma difference from paramter file values.
            if (abs(escin_pos_ped_mean(pln,cnt)
     &           -escin_new_ped_pos(pln,cnt))
     &           .ge.(2.*escin_new_sig_pos(pln,cnt))) then
               ind = ind + 1    !final value of 'ind' is saved at end of loop
               escin_changed_layer(ind)=pln
               escin_changed_counter(ind)=cnt
               escin_pos_ped_change(ind) = escin_new_ped_pos(pln,cnt) -
     &              escin_pos_ped_mean(pln,cnt)
            endif               !large pedestal change
            
*     replace old peds (from param file) with calculated pedestals
            if (num.gt.escin_pos_min_peds 
     &           .and. escin_pos_min_peds.ne.0) then
               escin_pos_ped_mean(pln,cnt)=escin_new_ped_pos(pln,cnt)
            endif
            
*     the same thing for negative side.
            
*     calculate new pedestal values
            num=max(1.,float(escin_neg_ped_num(pln,cnt)))
            escin_new_ped_neg(pln,cnt) = 
     $           float(escin_neg_ped_sum(pln,cnt)) / num
            sig2 = float(escin_neg_ped_sum2(pln,cnt))/num -
     $           escin_new_ped_neg(pln,cnt)**2
            escin_new_sig_neg(pln,cnt) = sqrt(max(0.,sig2))
            escin_new_threshold_neg(pln,cnt) = 
     $           escin_new_ped_neg(pln,cnt)+
     $           escin_new_sig_neg(pln,cnt)*sig_th
            
*     note counters with 2 sigma difference from paramter file values.
            if (abs(escin_neg_ped_mean(pln,cnt)-escin_new_ped_neg(pln,cnt))
     &           .ge.(2.*escin_new_sig_neg(pln,cnt))) then
               ind = ind + 1    !final value of 'ind' is saved at end of loop
               escin_changed_layer(ind)=pln
               escin_changed_counter(ind)=cnt
               escin_neg_ped_change(ind) = escin_new_ped_neg(pln,cnt) -
     &              escin_neg_ped_mean(pln,cnt)
            endif               !large pedestal change
            
*     replace old peds (from param file) with calculated pedestals
            if (num.gt.escin_neg_min_peds .and. escin_neg_min_peds.ne.0) then
               escin_neg_ped_mean(pln,cnt)=escin_new_ped_neg(pln,cnt)
            endif

c     Write(*,*) '(ecalcped)la,co,pp,np=',pln,cnt,
c     &           escin_new_threshold_pos(pln,cnt),
c     &           escin_new_threshold_neg(pln,cnt)

         enddo                  !counters
      enddo                     !layers

c     Write(*,*) 'ind=',ind

      escin_num_ped_changes = ind
      
c     Do pln=1,3
c     Do cnt=1,25
c     Write(*,*) '(ecalcped)la,co,pp,np=',pln,cnt,
c     &           escin_new_threshold_pos(pln,cnt),
c     &           escin_new_threshold_neg(pln,cnt)
c     EndDo
c     EndDo
      

*     
*     WRITE THRESHOLDS TO FILE FOR HARDWARE SPARCIFICATION
*     
*     --- it is opened in g_calc_beam_pedestal.f
*     
c     if (h_threshold_output_filename.ne.' ') then
c     file=h_threshold_output_filename
c     call g_sub_run_number(file, gen_run_number)
c     
c     open(unit=EPAREID,file=file,status='unknown')
c     
c     write(EPAREID,*) 
c     &        '# This is the ADC threshold file generated automatically'
c     write(EPAREID,*) 
c     &        'from the pedestal data from run number ',gen_run_number
c     
c     EndIF
c     --- e01-011
c     --- we are using a common ROC.
      
      slot=ESCIN_SLOT_ID1
      signalcount=2
      
      write(EPAREID,*) 'slot=',slot
      call g_output_thresholds(EPAREID,roc,slot,signalcount,
     &     enum_scin_layers,escin_new_threshold_pos,
     &     escin_new_threshold_neg,
     &     escin_new_sig_pos,escin_new_sig_neg)
      
c     --- e01-011
      slot=ESCIN_SLOT_ID2
      signalcount=2
      write(EPAREID,*) 'slot=',slot
      call g_output_thresholds(EPAREID,roc,slot,signalcount,
     &     enum_scin_layers,escin_new_threshold_pos,
     &     escin_new_threshold_neg,
     &     escin_new_sig_pos,escin_new_sig_neg)
      
c     close (unit=EPAREID) ! do not close.    
c     endif !
      
      Return
      End
