      SUBROUTINE H_TRANS_DC_GROUP(ABORT,err,gh1,gh2,cut_max,cut_min,  
     &     whichgroup)
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
*     
*     Modified by 2005/09/12 21:17:43 tseva
*     it uses the fact that in HKS we use group trigger.for each trigger group
*     we have a 'well' defined region on drift chamber where the track
*     point should be. this routine uses that to reduce the number of
*     hits under the max,  so the event can be used for tracking
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
      integer*4 pol,i,k,vec_no,gh1,gh2,j,gh1temp,gh2temp
      real*4 histval,cut_min(12,6),cut_max(12,6)
      real*4 hdc_cut_min_temp(12,6), hdc_cut_max_temp(12,6)
      integer*4 letssee(12,122)
      integer*4 less(12),step,all,allbad,raw(100),dec(100)
      integer*4 gh1after,gh2after,ntafter,test,whichgroup
      real*4 tempmax,tempmin
      integer*4 toomuch
      
      ntafter=0
      
      do i=1,12
         do j=1,6
            hdc_cut_min_temp(i,j)=hdc_cut_min(i,j)
            hdc_cut_max_temp(i,j)=hdc_cut_max(i,j)
         enddo
      enddo
      
      do while (gh1.ge.hmax_pr_hits(1).or.gh2.ge.hmax_pr_hits(2))   
         
         ABORT= .FALSE.
         err= ' '
         old_wire = -1
         old_pln = -1
         gh = 0
         
         Do i=1,hmax_num_dc_layers
            hdc_hits_per_layer(i) = 0
         EndDo
         gh1temp=0
         gh2temp=0
         
c     --- first, check start_time.
         Call HF1(hidstarttime,hstart_time,1.)
         
         if (hdc_center(1).eq.0.) then !initialize hdc_center if not yet set.
            do pln = 1, hdc_num_layers
c--   hdc_xcenter and hdc_ycenter are plane by plane offsets now  L.Y.
c     c            chamber = hdc_chamber_layers(pln)
               hdc_center(pln) = hdc_xcenter(pln)
     $              *sin(hdc_alpha_angle(pln))+hdc_ycenter(pln)
     $              *cos(hdc_alpha_angle(pln))
            enddo
         endif
         
         pol = -1
c     ntafter=0
         
c     10   write(*,*)'new try'
         gh1temp=0
         gh2temp=0
         do i=1,12
            do j=1,6
               if(i.eq.3.or.i.eq.4.or.i.eq.9.or.i.eq.10.) then
                  tempmin= cut_min(i,j)+1
                  tempmax= cut_max(i,j)-1
                  if(tempmin.lt.  hdc_cut_min_temp(i,j)) then 
                     
                     cut_min(i,j)= cut_min(i,j)+1
                  endif
                  if(tempmax.gt. hdc_cut_max_temp(i,j)) then 
                     cut_max(i,j)= cut_max(i,j)-1
                     
                  endif
               else
                  tempmin= cut_min(i,j)+0.72
                  tempmax= cut_max(i,j)-0.72
                  if(tempmin.lt.  hdc_cut_min_temp(i,j)) then 
                     cut_min(i,j)= tempmin
                  endif
                  if(tempmax.gt. hdc_cut_max_temp(i,j)) then 
                     cut_max(i,j)= tempmax
                     
                  endif
               endif
            enddo
         enddo
         
c     --- fastbus case
         if(hdc_raw_tot_hits .gt. 0 .and. pol .eq. -1) then
            Do i=1,hdc_raw_tot_hits
               hdc_raw_tdc_sub_trig(i) = hdc_raw_tdc(i)
            EndDo
         EndIF                  ! hdc_raw_tot_hits > 0
         
*     --- Histogram for raw dc bank 
         call h_fill_dc_raw_hist(abort,err)
         if (abort) then
            call g_prepend(here,err)
            return
         endif
         
*     Are there any raw hits
         if(hdc_raw_tot_hits.le.0) Return
         
         all=all+1
         if(hdc_raw_tot_hits.lt.20) then
            allbad=allbad+1
         endif
         less(3)=0
         less(4)=0
         less(9)=0
         less(10)=0
         
         do ihit=1,hdc_raw_tot_hits
            pln = hdc_raw_layer_num(ihit)
            wire  = hdc_raw_wire_num(ihit)
            letssee(pln,wire)= letssee(pln,wire)+1
            less(pln)=less(pln)+1
         enddo

         do ihit=1,hdc_raw_tot_hits
            pln = hdc_raw_layer_num(ihit)
            wire  = hdc_raw_wire_num(ihit)
*     check valid layer and wire number
            if(pln.gt.0 .and. pln.le. hdc_num_layers) then
c     histval=float(hdc_raw_tdc_sub_trig(ihit))
c     call hf1(hidrawtdcall,histval,1.)
*     test if tdc value less than lower limit for good hits
               if(hdc_raw_tdc_sub_trig(ihit) .lt. hdc_tdc_min_win(pln)) then
                  hwire_early_mult(wire,pln)
     $                 = hwire_early_mult(wire,pln)+1
               else
                  if(hdc_raw_tdc_sub_trig(ihit) .gt. hdc_tdc_max_win(pln)) then
                     hwire_late_mult(wire,pln)
     $                    = hwire_late_mult(wire,pln)+1
                  else
*     test for valid wire number
                     if(wire.gt.0 .and. wire.le.hdc_nrwire(pln)) then
*     test for multiple hit on the same wire
                        if(pln.eq.old_pln .and. wire.eq.old_wire) then
                           hwire_extra_mult(wire,pln) =
     $                          hwire_extra_mult(wire,pln)+1
                        else
*     valid hit proceed with decoding
                           if((pln.le.6.and.less(pln).gt.1.and.gh1.ge.hmax_pr_hits(2)).or.
     &                          ( pln.gt.6.and.less(pln).gt.1.and.gh2.ge.hmax_pr_hits(2)) ) then
                              if(whichgroup.ge.1) then
                                 if(hdc_raw_wire_num(ihit).le.cut_max(pln,whichgroup).and.
     &                                hdc_raw_wire_num(ihit).ge.cut_min(pln,whichgroup)) then
                                    gh = gh + 1
                                    hdc_layer_num(gh) = hdc_raw_layer_num(ihit)
                                    hdc_wire_num(gh) = hdc_raw_wire_num(ihit)
                                    hdc_tdc(gh) = hdc_raw_tdc_sub_trig(ihit)
                                    if (hdc_raw_layer_num(ihit).le.6)  gh1temp=gh1temp+1
                                    if (hdc_raw_layer_num(ihit).gt.6)  gh2temp=gh2temp+1
*     check cluster size
                                    if( 
     &                                   gh.gt.1 .and. 
     &                                   old_pln .eq. hdc_layer_num(gh) .and. 
     &                                   old_wire + 1 .eq. hdc_wire_num(gh))
     &                                   then
                                       hdc_cluster_size(gh) 
     &                                      = hdc_cluster_size(gh-1) + 1
                                       Do k=1,hdc_cluster_size(gh)-1
                                          hdc_cluster_size(gh-k) =
     &                                         hdc_cluster_size(gh)
                                       EndDo
                                    Else
                                       hdc_cluster_size(gh) = 1
                                    EndIf

*     if hdc_wire_counting(pln) is 1 then wires are number in reverse order
                                    if(hdc_wire_counting(pln).eq.0) then !normal ordering
                                       hdc_wire_center(gh) = hdc_pitch(pln)
     &                                      * (float(wire) 
     &                                      - hdc_central_wire(pln))
     &                                      - hdc_center(pln)
                                    else
                                       hdc_wire_center(gh) = hdc_pitch(pln)
     &                                      * ( (hdc_nrwire(pln) + (1 - wire))
     &                                      - hdc_central_wire(pln) ) 
     &                                      - hdc_center(pln)
                                    endif
                                    
                                    
                                    hdc_drift_time(gh) = - hstart_time
     &                                   + float(hdc_tdc(gh)) * pol
     &                                   *hdc_tdc_time_per_channel
     &                                   - hdc_layer_time_zero(pln) * pol
                                    
                                    hdc_hits_per_layer(pln) = 
     &                                   hdc_hits_per_layer(pln)+1
                                    
                                 endif
                              else
                                 gh = gh + 1
                                 hdc_layer_num(gh) = hdc_raw_layer_num(ihit)
                                 hdc_wire_num(gh) = hdc_raw_wire_num(ihit)
                                 hdc_tdc(gh) = hdc_raw_tdc_sub_trig(ihit)
                                 
*     check cluster size
                                 if( 
     &                                gh.gt.1 .and. 
     &                                old_pln .eq. hdc_layer_num(gh) .and. 
     &                                old_wire + 1 .eq. hdc_wire_num(gh))
     &                                then
                                    hdc_cluster_size(gh) 
     &                                   = hdc_cluster_size(gh-1) + 1
                                    Do k=1,hdc_cluster_size(gh)-1
                                       hdc_cluster_size(gh-k) =
     &                                      hdc_cluster_size(gh)
                                    EndDo
                                 Else
                                    hdc_cluster_size(gh) = 1
                                 EndIf
*     if hdc_wire_counting(pln) is 1 then wires are number in reverse order
                                 if(hdc_wire_counting(pln).eq.0) then !normal ordering
                                    hdc_wire_center(gh) = hdc_pitch(pln)
     &                                   * (float(wire) 
     &                                   - hdc_central_wire(pln))
     &                                   - hdc_center(pln)
                                 else
                                    hdc_wire_center(gh) = hdc_pitch(pln)
     &                                   * ( (hdc_nrwire(pln) + (1 - wire))
     &                                   - hdc_central_wire(pln) ) 
     &                                   - hdc_center(pln)
                                 endif
                                 hdc_drift_time(gh) = - hstart_time
     &                                + float(hdc_tdc(gh)) * pol
     &                                *hdc_tdc_time_per_channel
     &                                - hdc_layer_time_zero(pln) * pol
                                 
                                 hdc_hits_per_layer(pln) = 
     &                                hdc_hits_per_layer(pln)+1
                              endif ! group test; SEVA
                           else !number hits test
                              gh = gh + 1
                              hdc_layer_num(gh) = hdc_raw_layer_num(ihit)
                              hdc_wire_num(gh) = hdc_raw_wire_num(ihit)
                              hdc_tdc(gh) = hdc_raw_tdc_sub_trig(ihit)
                              if (hdc_raw_layer_num(ihit).le.6)  gh1temp=gh1temp+1
                              if (hdc_raw_layer_num(ihit).gt.6)  gh2temp=gh2temp+1
*     check cluster size
                              if( 
     &                             gh.gt.1 .and. 
     &                             old_pln .eq. hdc_layer_num(gh) .and. 
     &                             old_wire + 1 .eq. hdc_wire_num(gh))
     &                             then
                                 hdc_cluster_size(gh) 
     &                                = hdc_cluster_size(gh-1) + 1
                                 Do k=1,hdc_cluster_size(gh)-1
                                    hdc_cluster_size(gh-k) =
     &                                   hdc_cluster_size(gh)
                                 EndDo
                              Else
                                 hdc_cluster_size(gh) = 1
                              EndIf
                              
*     if hdc_wire_counting(pln) is 1 then wires are number in reverse order
                              if(hdc_wire_counting(pln).eq.0) then !normal ordering
                                 hdc_wire_center(gh) = hdc_pitch(pln)
     &                                * (float(wire) 
     &                                - hdc_central_wire(pln))
     &                                - hdc_center(pln)
                              else
                                 hdc_wire_center(gh) = hdc_pitch(pln)
     &                                * ( (hdc_nrwire(pln) + (1 - wire))
     &                                - hdc_central_wire(pln) ) 
     &                                - hdc_center(pln)
                              endif

                              hdc_drift_time(gh) = - hstart_time
     &                             + float(hdc_tdc(gh)) * pol
     &                             *hdc_tdc_time_per_channel
     &                             - hdc_layer_time_zero(pln) * pol
                              
                              hdc_hits_per_layer(pln) = 
     &                             hdc_hits_per_layer(pln)+1
                           endif ! test on number of nits on each chamber SEVA
                           
                        endif   ! end test on duplicate wire
                        old_pln = pln
                        old_wire = wire
                     endif      ! end test on valid wire number
                  endif         ! end test on hdc_tdc_max_win
               endif            ! end test on hdc_tdc_min_win
            else                ! if not a valid layer number
               write(6,*) 'H_TRANS_DC: invalid layer number = ',pln
            endif               ! end test on valid layer number
         enddo                  ! end loop over raw hits
         
         hdc_tot_hits = gh
         raw(hdc_raw_tot_hits)=raw(hdc_raw_tot_hits)+1
         dec(hdc_tot_hits)=dec(hdc_tot_hits)+1
         test=0

         do i=1,12
            do j=1,6
               if ((cut_max(i,j)- hdc_cut_max_temp(i,j)).lt.1.3 ) then
                  test=test+1
               endif
               if ((cut_min(i,j)-  hdc_cut_min_temp(i,j)).gt.-1.3) then
                  test=test+1
               endif
            enddo
         enddo

         gh1=gh1temp
         gh2=gh2temp
         if(test.gt.142) then
            if (ntafter.le.15) then
               ntafter=ntafter+1
               do i=1,12
                  do j=1,6
                     if(i.eq.3.or.i.eq.4.or.i.eq.9.or.i.eq.10.) then
                        hdc_cut_min_temp(i,j)=hdc_cut_min_temp(i,j)+1
                        hdc_cut_max_temp(i,j)=hdc_cut_max_temp(i,j)-1
                     else
                        hdc_cut_min_temp(i,j)=hdc_cut_min_temp(i,j)+0.5
                        hdc_cut_max_temp(i,j)=hdc_cut_max_temp(i,j)-0.5
                     endif
                  enddo
               enddo
            else
               goto 10
            endif
         endif
      enddo

 10   RETURN
      END
