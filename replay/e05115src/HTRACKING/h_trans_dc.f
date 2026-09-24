       SUBROUTINE H_TRANS_DC(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods : Translate HKS raw drift and start time 
*     -                                to decoded information 
*     -
*     -      Required Input BANKS     HKS_RAW_DC
*     -                               HKS_DECODED_SCIN
*     -
*     -      Output BANKS             HKS_DECODED_DC
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     $Log: h_trans_dc.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.4  2005/09/20 15:00:25  sumihama
*     Mod. find track candidates with Group info. by Seva
*
*     Revision 1.3  2005/06/25 06:08:25  akihiko
*     Modified calc. of hdc_raw_tdc_sub_trig
*
*     Revision 1.2  2005/06/02 22:20:28  cdaq
*     choose reference time for each rocs for hdc
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.10  2005/03/14 19:52:17  miyoshi
*     change tracking variables and histogram names
*
*     Revision 1.9  2005/03/07 19:20:52  miyoshi
*     remove unused things
*
*     Revision 1.8  2005/03/07 16:41:05  miyoshi
*     change minor bugs
*
*     Revision 1.7  2005/03/02 22:32:15  miyoshi
*     move call location
*
*     Revision 1.6  2005/03/02 20:50:41  miyoshi
*     fix time correction equation
*
*     Revision 1.5  2005/03/02 17:26:03  miyoshi
*     change histid params
*
*     Revision 1.4  2005/03/01 23:53:09  miyoshi
*     add f1tdc analysis part
*
*     Revision 1.3  2005/01/13 20:23:31  yuan
*     The hdc_xcenter & hdc_ycenter are plane by plane offsets,
*     not only one offset for the whole chamber.
*
*     Revision 1.2  2004/12/24 21:33:07  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
*     new dir
*
*     Revision 1.12  1996/09/04 20:18:35  saw
*     (??) Cosmetic
*     
*     Revision 1.11  1996/01/17 18:44:30  cdaq
*     (JRA) Change sign on sstart_time
*     
*     Revision 1.10  1995/10/11 13:54:18  cdaq
*     (JRA) Cleanup, add bypass switch to s_dc_eff call
*
*     Revision 1.9  1995/08/31 15:04:12  cdaq
*     (JRA) Add call to s_dc_eff, warn about invalid layer numbers
*     
*     Revision 1.8  1995/05/22  19:46:02  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, hks, and coin parts"
*     
*     Revision 1.7  1995/05/17  16:47:43  cdaq
*     (JRA) Add hist for all dc tdc's in one histogram.
*     
*     Revision 1.6  1995/04/06  19:52:15  cdaq
*     (JRA) SMAX_NUM_DC_LAYERS -> SDC_NUM_LAYERS
*     
*     Revision 1.5  1994/11/23  15:08:04  cdaq
*     (SPB) Recopied from hms file and modified names for HKS
*     
*     Revision 1.4  1994/04/13  18:56:40  cdaq
*     (DFG) Add call to s_fill_dc_dec_hist, remove s_raw_dump_all call
*     
*     Revision 1.3  1994/03/24  19:59:03  cdaq
*     (DFG) add print routines and flags
*     check layer number and wire number for validity
*     
*     Revision 1.2  1994/02/22  14:22:58  cdaq
*     (SAW) replace err='' with ' '
*     
*     Revision 1.1  1994/02/21  16:42:58  cdaq
*     Initial revision
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*10 here
      parameter (here= 'H_TRANS_DC')
*     
      logical ABORT
      character*(*) err
*     
      include 'hks_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_event_info.cmn'
      include 'gen_units.par'
      include 'gen_f1tdc.cmn'
      include 'gen_rocid.cmn'
      include 'hks_tracking.cmn'
      include 'hks_geometry.cmn'
      include 'hks_bypass_switches.cmn'
      include 'hks_id_histid.cmn'
      INCLUDE 'gen_data_structures.cmn'
*     
*--------------------------------------------------------
      real*4 h_drift_dist_calc
      external h_drift_dist_calc
      integer*4 ihit,gh,old_wire,old_pln,wire,pln,chamber
      integer*4 pol,i,k,vec_no,hdc_tot_hit_dc1,hdc_tot_hit_dc2
      real*4 histval,cut_min(12,6),cut_max(12,6)
      integer*4 gh1,gh2,nt,j,whichgroup,sl
      real*4 hdc_tdc_offset_cable(80) 
      data hdc_tdc_offset_cable /0.0,2.5,-1.0,-2.5,1.0,0.0,
     +                          0.0,-0.5,-0.5,-1.0,2.5,0.0,
     +                          0.5,-0.5,1.0,1.0,-4.0,-0.5,1.5,0.5,
     +                          0.0,-1.5,-1.0,-0.5,2.0,0.0,-1.0,-1.5,
     +                          0.0,-1.0,0.0,0.0,2.5,0.0,
     +                          0.0,-1.5,0.0,0.5,1.0,0.0,
     +                          1.0,-0.5,0.5,0.0,0.5,3.0,
     +                          0.0,-2.0,1.5,0.0,0.5,2.5,
     +                          -1.0,-0.5,2.0,-1.0,0.0,0.0,-1.0,-1.5,
     +                         1.0,-0.5,1.5,0.0,0.0,-1.5,-2.5,1.0,
     +                         3.5,1.0,0.0,-1.0,1.0,4.0,
     +                         4.0,0.0,-1.5,-1.0,3.0,0.0/


* 
    
      ABORT= .FALSE.
      err= ' '
      old_wire = -1
      old_pln = -1
      gh = 0
      gh1=0
      gh2=0
c   check valid the start time which due to the physics that the particle goes through both 1x and 2x in valid time difference

c      if(h1start_time.le.-100.0.or.h2start_time.le.-100.0) Return
      if(ntof.eq.0) return

      Do i=1,hmax_num_dc_layers
         hdc_hits_per_layer(i) = 0
      EndDo
    

cc      Write(*,*) '(htransdc) ev,tot=',gen_event_ID_number,
cc     &     hdc_raw_tot_hits, hstart_time, hidstarttime

c     --- first, check start_time.
      Call HF1(hidstarttime,hstart_time,1.)
      
      if (hdc_center(1).eq.0.) then !initialize hdc_center if not yet set.
         do pln = 1, hdc_num_layers
c--   hdc_xcenter and hdc_ycenter are plane by plane offsets now  L.Y.
cc            chamber = hdc_chamber_layers(pln)
            hdc_center(pln) = hdc_xcenter(pln)
     $           *sin(hdc_alpha_angle(pln))+hdc_ycenter(pln)
     $           *cos(hdc_alpha_angle(pln))
         enddo
      endif

      pol = -1
      
      if(hdc_raw_tot_hits .gt. 0 .and. pol .eq. -1) then
         Do i=1,hdc_raw_tot_hits
            hdc_raw_tdc_sub_trig(i) = hdc_raw_tdc(i)
         EndDo
      EndIF                     ! hdc_raw_tot_hits > 0

*     --- Histogram for raw dc bank 
      call h_fill_dc_raw_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

*     Are there any raw hits
      if(hdc_raw_tot_hits.le.0) Return
      
*     loop over all raw hits
c      hdc_rate_gtrig = hdc_rate_gtrig + 1.0

c     ============ RATE per wire (T.Gogami, 12/Oct/2011)==================c     
c      if(gen_event_ID_number.eq.1)then
c         open(22,file='kdc_WireRate.dat',status='replace') ! Test (T.Gogami,12/Oct/2011)
c         close(22)
c      endif
c      open(22,file='kdc_WireRate.dat',access='append')
c      write(22,*)hdc_raw_tot_hits
c      close(22)
      
      
      do ihit=1,hdc_raw_tot_hits
         pln = hdc_raw_layer_num(ihit)
         wire  = hdc_raw_wire_num(ihit)

c     ============ RATE per wire (T.Gogami, 12/Oct/2011)==================
c         open(22,file='kdc_WireRate.dat',access='append')
c         write(22,*)float(hdc_raw_tdc_sub_trig(ihit)),pln,wire
c         close(22)

c     ============  RATE  calculation  (T.Gogami,9/Mar/2011)==============
         if(float(hdc_raw_tdc_sub_trig(ihit)).ge.2100. .and.
     &        float(hdc_raw_tdc_sub_trig(ihit)).le.2400.)then
            hdc_rate_hit_acc(pln) = hdc_rate_hit_acc(pln) + 1.0            
         endif
         if(float(hdc_raw_tdc_sub_trig(ihit)).ge.2600. .and.
     &        float(hdc_raw_tdc_sub_trig(ihit)).le.2800.)then
            hdc_rate_hit_sig(pln) = hdc_rate_hit_sig(pln) + 1.0
         endif

c
         
*     check valid layer and wire number
         if(pln.gt.0 .and. pln.le. hdc_num_layers) then
c     histval=float(hdc_raw_tdc_sub_trig(ihit))
c     call hf1(hidrawtdcall,histval,1.)
*     test if tdc value less than lower limit for good hits
            if(hdc_raw_tdc_sub_trig(ihit) .lt. hdc_tdc_min_win(pln))  then
               hwire_early_mult(wire,pln)
     $              = hwire_early_mult(wire,pln)+1
            else
               if(hdc_raw_tdc_sub_trig(ihit) .gt. hdc_tdc_max_win(pln))  then
                  hwire_late_mult(wire,pln)
     $                 = hwire_late_mult(wire,pln)+1
               else
*     test for valid wire number
                  if(wire.gt.0 .and. wire.le.hdc_nrwire(pln)) then
*     test for multiple hit on the same wire
                     if(pln.eq.old_pln .and. wire.eq.old_wire) then
                        hwire_extra_mult(wire,pln) =
     $                       hwire_extra_mult(wire,pln)+1
                     else
                        
*     valid hit proceed with decoding
                        gh = gh + 1
                        hdc_layer_num(gh) = hdc_raw_layer_num(ihit)
                        hdc_wire_num(gh) = hdc_raw_wire_num(ihit)
                        hdc_tdc(gh) = hdc_raw_tdc_sub_trig(ihit)
                        if (hdc_raw_layer_num(ihit).le.6) gh1=gh1+1
                        if (hdc_raw_layer_num(ihit).gt.6) gh2=gh2+1
*     check cluster size
                        if( 
     &                       gh.gt.1 .and. 
     &                       old_pln .eq. hdc_layer_num(gh) .and. 
     &                       old_wire + 1 .eq. hdc_wire_num(gh))
     &                       then
                           hdc_cluster_size(gh) 
     &                          = hdc_cluster_size(gh-1) + 1
                           Do k=1,hdc_cluster_size(gh)-1
                              hdc_cluster_size(gh-k) =
     &                             hdc_cluster_size(gh)
                           EndDo
                        Else
                           hdc_cluster_size(gh) = 1
                        EndIf
                        
                        
*     if hdc_wire_counting(pln) is 1 then wires are number in reverse order
                        if(hdc_wire_counting(pln).eq.0) then !normal ordering
                           hdc_wire_center(gh) = hdc_pitch(pln)
     &                          * (float(wire) 
     &                          - hdc_central_wire(pln))
     &                          - hdc_center(pln)
                        else
                           hdc_wire_center(gh) = hdc_pitch(pln)
     &                          * ( (hdc_nrwire(pln) + (1 - wire))
     &                          - hdc_central_wire(pln) ) 
     &                          - hdc_center(pln)
                        endif
c     In F1TDC larger number means later time, we need to minus the hstart_time 
c     during calculate the the drift time.    
c     C.chen Oct. 10,2009     
                        
                        
                        
                        if(pln .eq. 1 .or. pln .eq. 2) then
                           sl = 6*(pln-1) 
     &                          + int((wire-hdc_wire_offset_low(pln)-1)/16) + 1
                        Else if(pln .eq. 3 .or. pln .eq. 4) then
                           sl = 12 + 8*(pln-3) 
     &                          + int((wire-hdc_wire_offset_low(pln)-1)/16) + 1
                        Else if(pln .eq. 5 .or. pln .eq. 6) then
                           sl = 28 + 6*(pln-5) 
     &                          + int((wire-hdc_wire_offset_low(pln)-1)/16) + 1
                        Else if(pln .eq. 7 .or. pln .eq. 8) then
                           sl = 40 + 6*(pln-7) 
     &                          + int((wire-hdc_wire_offset_low(pln)-1)/16) + 1
                        Else if(pln .eq. 9 .or. pln .eq. 10) then
                           sl = 52 + 8*(pln-9) 
     &                          + int((wire-hdc_wire_offset_low(pln)-1)/16) + 1
                        Else if(pln .eq. 11 .or. pln .eq. 12) then
                           sl = 68 + 6*(pln-11) 
     &                          + int((wire-hdc_wire_offset_low(pln)-1)/16) + 1
                        Endif
c     print*,sl,pln,hdc_tdc_offset_cable(sl)
                        
                        if(pln.le.6) then
                           hdc_drift_time(gh) =
     &                          float(hdc_tdc(gh)) * pol
     &                          *hdc_tdc_time_per_channel
     &                          - hdc_layer_time_zero(pln) * pol
     &                          +hdc_tdc_offset_cable(sl)
                        else
                           hdc_drift_time(gh) =
     &                          float(hdc_tdc(gh)) * pol
     &                          *hdc_tdc_time_per_channel
     &                          - hdc_layer_time_zero(pln) * pol
     &                          +hdc_tdc_offset_cable(sl)
                        endif
                        
                        
                        if(pln.le.6) then
                           hdc_drift_time_pre(gh) =-h1start_pre+
     &                          float(hdc_tdc(gh)) * pol
     &                          *hdc_tdc_time_per_channel
     &                          - hdc_layer_time_zero(pln) * pol
     &                          +hdc_tdc_offset_cable(sl)
                           
                           
                        else
                           hdc_drift_time_pre(gh) =-h2start_pre+
     &                          float(hdc_tdc(gh)) * pol
     &                          *hdc_tdc_time_per_channel
     &                          - hdc_layer_time_zero(pln) * pol
     &                          +hdc_tdc_offset_cable(sl)
                        endif
                        
                        hdc_hits_per_layer(pln) = 
     &                       hdc_hits_per_layer(pln)+1
                        
                     endif      ! end test on duplicate wire
                     old_pln = pln
                     old_wire = wire
                  endif         ! end test on valid wire number
               endif            ! end test on hdc_tdc_max_win
            endif               ! end test on hdc_tdc_min_win
         else                   ! if not a valid layer number
            write(6,*) 'H_TRANS_DC: invalid layer number = ',pln
         endif                  ! end test on valid layer number
      enddo                     ! end loop over raw hits
      
*     
*     set total number of good hits
*     
      hdc_tot_hits = gh
*     
*      call h_dc_tofcut(ABORT,err) ! by Gogami
      
c     if (hbypass_dc_eff.eq.0) call h_dc_eff !only call if there was a hit.
      
      if(gtrig_flag_kaonseg1.ge.1) then
         whichgroup=1
      elseif(gtrig_flag_kaonseg2.ge.1) then
         whichgroup=2
      elseif(gtrig_flag_kaonseg3.ge.1) then
         whichgroup=3
      elseif(gtrig_flag_kaonseg4.ge.1) then
         whichgroup=4
      elseif(gtrig_flag_kaonseg5.ge.1) then
         whichgroup=5
      elseif(gtrig_flag_kaonseg6.ge.1) then
         whichgroup=6
      else
         whichgroup=0
      endif
      
      if ( whichgroup.gt.0 ) then
         do i=1,12
            do j=1,6
               cut_min(i,j)=0
               if(i.eq.3.or.i.eq.4.or.i.eq.9.or.i.eq.10.) then
                  cut_max(i,j)=122
               else
                  cut_max(i,j)=87
               endif
            enddo
         enddo



c--   hits selection by group. Can be turn on when
c--   real data come                       L.Y. 08/20/09
cc         if (gh1.ge.hmax_pr_hits(1).or.gh2.ge.hmax_pr_hits(2)) then
cc            if (gh1.gt.4.and.gh2.gt.4) then
cc               call h_trans_dc_group(ABORT,err,gh1,gh2,cut_max,cut_min,whichgroup)
cc            endif
cc         endif
      endif
*     
*     Dump decoded banks if flag is set
      if(hdebugprintdcdec.ne.0) then
         call h_print_decoded_dc(ABORT,err)
         if (abort) then
            call g_prepend(here,err)
            return 
         endif
      endif
*     
      RETURN
      END
