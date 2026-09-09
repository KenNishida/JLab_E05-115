      SUBROUTINE E_TRANS_DC1(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods : Translate HES raw drift and start time 
*     -                                to decoded information 
*     -
*     -      Required Input BANKS     E_RAW_DC1
*     -                               E_DECODED_SCIN
*     -
*     -      Output BANKS             E_DECODED_DC1
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     - 03/21/2004 Miyoshi 
*     - copyed from s_trans_dc1.f
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*10 here
      parameter (here= 'E_TRANS_DC1')
*     
      logical ABORT
      character*(*) err
*     
      include 'gen_run_info.cmn'
      include 'gen_event_info.cmn'
      include 'hes_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_units.par'
      include 'hes_scin_parms.cmn'
      include 'hes_tracking.cmn'
      include 'hes_geometry.cmn'
      include 'hes_id_histid.cmn'
      include 'gen_rocid.cmn'
      include 'gen_f1tdc.cmn'
*     
      integer*4 ihit,old_wire,old_pln,wire,pln,chamber
      real*4 histval
      integer*4 card,i,j,k,newpln,cluster
      Integer*4 gh,pol
      Real*4 dt,wc      
      Integer*4 evflag,iev,tdc
      integer*4 tdcsum
      Real*4 tdcave
      
*     
      ABORT= .FALSE.
      err= ' '

      old_wire = -1
      old_pln = -1
      gh = 0
      Do i=1,emax_num_dc1_layers
         edc1_hits_per_layer(i) = 0
      EndDo
      edc1_tot_hits = 0

      pol = -1
      
       
      if(edc1_raw_tot_hits .gt. 0 .and. pol .eq. -1) then
         Do i=1,edc1_raw_tot_hits
            edc1_raw_tdc_sub_trig(i) = edc1_raw_tdc(i)
         EndDo
      EndIF                     ! edc1_raw_tot_hits > 0

c     --- below, we should use edc1_raw_tdc_sub_trig or we can't select good hits 
      
      
*     --- Histogram for raw dc1 bank 
      call e_fill_dc1_raw_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif
      
*     --- initial check 
c      write(*,*) "edc1_raw_tot_hits=",edc1_raw_tot_hits 
      if(edc1_raw_tot_hits .le. 0) Return
      
*     --- loop over all raw hits
      do ihit=1,edc1_raw_tot_hits
         pln = edc1_raw_layer_num(ihit)
         wire  = edc1_raw_wire_num(ihit)
         
*     --- check valid layer and wire number
         if(pln.gt.0 .and. pln.le. emax_num_dc1_layers) then
            
*     --- test for time window
            if(edc1_raw_tdc_sub_trig(ihit) .lt. edc1_tdc_min_win(pln))  then
               edc1wire_early_mult(wire,pln)
     $              = edc1wire_early_mult(wire,pln)+1
            else if(edc1_raw_tdc_sub_trig(ihit) .gt. edc1_tdc_max_win(pln))  then
               edc1wire_late_mult(wire,pln)
     $              = edc1wire_late_mult(wire,pln)+1
            else                ! good time
               if(wire .gt. 0 .and. wire .le. edc1_nrwire(pln)) then
                  
*     --- test for multiple hit on the same wire
                  if(pln .eq. old_pln 
     &                 .and. wire .eq. old_wire) then
                     edc1wire_extra_mult(wire,pln) =
     $                    edc1wire_extra_mult(wire,pln) + 1
                     
                  else          ! the 1st wire multihit
                     
*     valid hit proceed with decoding
                     gh = gh + 1
                     edc1_layer_num(gh) = pln
                     edc1_group_num(gh) = int((edc1_layer_num(gh)-1)/2)+1
                     newpln = pln
                     edc1_wire_num(gh) = wire
                     edc1_tdc(gh) = edc1_raw_tdc_sub_trig(ihit)
*     
                     if(edc1_group_num(gh) .ne. 2 
     &                    .and. edc1_group_num(gh) .ne. 4) then
                        edc1_slot_num(gh) = 7*(pln-1)
     &                       +int((wire-2-1)/16) + 1
                     Else if(pln .eq. 3 .or. pln .eq. 8) then
                        edc1_slot_num(gh) = 7*(pln-1)
     &                       +int((wire-1)/16) + 1
                     Else if(pln .eq. 4 .or. pln .eq. 7) then
                        edc1_slot_num(gh) = 7*(pln-1)
     &                       +int((wire+6-1)/16) + 1
                     EndIF

*     check cluster size
                     if( 
     &                    gh.gt.1 .and. 
     &                    old_pln .eq. newpln .and. 
     &                    old_wire + 1 .eq. edc1_wire_num(gh))
     &                    then
                        edc1_cluster_size(gh) 
     &                       = edc1_cluster_size(gh-1) + 1
                        Do k=1,edc1_cluster_size(gh)-1
                           edc1_cluster_size(gh-k) =
     &                          edc1_cluster_size(gh)
                        EndDo
                     Else
                        edc1_cluster_size(gh) = 1
                     EndIf
                     
                     
*     if edc1_wire_counting(pln) is 1 
*     then wires are number in reverse order                           
*     Now we use Takahashi's definition.
*     Wire position in wire coordinate is 
*     S0 + ('wire number' - 1) * S1
*     S0 is edc1_central_wire(pln) and S1 is edc_pitch(pln)
*     we still use old traditional vector name for SOS/HMS system.
*     11/21/2003 Miyoshi
                     
                     if(edc1_wire_counting(pln).eq.0) then
                        edc1_wire_center(gh) = 
     &                       edc1_central_wire(pln)
     &                       - float(wire-1)*edc1_pitch(pln)
c                        write(*,*) pln,float(wire),edc1_wire_center(gh),
c     &                   edc1_central_wire(pln),edc1_pitch(pln)
                     else       ! for EDC1, counting should be 1.
                        Write(*,*) 'Wrong order in e_trans_dc1!'
                        Write(*,*) 'check edc1.param.'
                     endif

*     Use estart_time when calculate edc1_raw_tdc_sub_trig
c                     edc1_drift_time(gh) = - estart_time
                     edc1_drift_time(gh) = 0
     &                    + float(edc1_tdc(gh)) * pol
     &                    *edc1_tdc_time_per_channel
     &                    - edc1_layer_time_zero(edc1_slot_num(gh)) * pol
c     &                    - edc1_layer_time_zero(pln)
                     
                     edc1_hits_per_layer(pln)
     &                    =edc1_hits_per_layer(pln)+1
                  endif         ! end test on duplicate wire
                  old_pln = pln
                  old_wire = wire
               endif            ! end test on valid wire number
            endif               ! end test on good timing
         else                   ! if not a valid layer number
            write(6,*) 'E_TRANS_DC1: invalid layer number = ',pln
         endif                  ! end test on valid layer number
         if(gh.ge.EMAX_DC1_DEC_HITS) goto 200
      enddo                     ! end loop over raw hits
      
*      Do i=1,emax_num_dc1_layers
*            write(*,*) i,edc1_hits_per_layer(i)
*      EndDo
      
 200  continue
cc 200  write(*,*) 'Max EDC1-hits',gen_event_ID_number
*     set total number of good hits
      
      edc1_tot_hits = gh
      
      RETURN
      END
