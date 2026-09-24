       SUBROUTINE E_TRANS_DC2(ABORT,err)
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
*     $Log: e_trans_dc2.f,v $
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
      parameter (here= 'E_TRANS_DC2')
*     
      logical ABORT
      character*(*) err
*     
      include 'hes_data_structures.cmn'
      include 'gen_constants.par'
      include 'gen_event_info.cmn'
      include 'gen_units.par'
      include 'gen_f1tdc.cmn'
      include 'gen_rocid.cmn'
      include 'hes_tracking.cmn'
      include 'hes_geometry.cmn'
      include 'hes_id_histid.cmn'
         INCLUDE 'gen_data_structures.cmn'
*     
*--------------------------------------------------------
      real*4 h_drift_dist_calc
      external h_drift_dist_calc
      integer*4 ihit,gh,old_wire,old_pln,wire,pln,chamber
      integer*4 pol,i,k,vec_no,edc2_tot_hit_dc1,edc2_tot_hit_dc2
      real*4 histval,cut_min(12,6),cut_max(12,6)
      integer*4 nt,j,whichgroup
      
      integer*4 itrk1,itrk_min
      real*4 xedc1,xpedc1
      real*4 yedc1,ypedc1
      real*4 xedc2,xpedc2
      real*4 yedc2,ypedc2
      real*4 sedc2,wcenter,alpha,ds,ds_pre,ds_min
      real*4 raddeg
      parameter (raddeg=3.14159265/180.)
      integer*4 slot
      real*4 ddis
      real*4 e_dc2_drift_dist_calc 
      external e_dc2_drift_dist_calc 
*     
      ABORT= .FALSE.
      err= ' '
      old_wire = -1
      old_pln = -1
      gh = 0

      Do i=1,emax_num_dc2_layers
         edc2_hits_per_layer(i) = 0
      EndDo
      

cc      Write(*,*) '(htransdc) ev,tot=',gen_event_ID_number,
cc     &     edc2_raw_tot_hits

c     --- first, check start_time.
*      Call HF1(hidstarttime,hstart_time,1.)
      
*      if (edc2_center(1).eq.0.) then !initialize edc2_center if not yet set.
         do pln = 1, edc2_num_layers
c--   edc2_xcenter and edc2_ycenter are plane by plane offsets now  L.Y.
cc            chamber = edc2_chamber_layers(pln)
*            edc2_center(pln) = edc2_xcenter(pln)
*     $           *sin(edc2_alpha_angle(pln))+edc2_ycenter(pln)
*     $           *cos(edc2_alpha_angle(pln))
            edc2_center(pln) = 0. 
         enddo
*      endif
      
      pol = -1
      
c     --- fastbus case
      if(edc2_raw_tot_hits .gt. 0 .and. pol .eq. -1) then
         Do i=1,edc2_raw_tot_hits
            edc2_raw_tdc_sub_trig(i) = edc2_raw_tdc(i)
         EndDo
      EndIF                     ! edc2_raw_tot_hits > 0

*     --- Histogram for raw dc bank 
      call e_fill_dc2_raw_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

*     Are there any raw hits
      if(edc2_raw_tot_hits.le.0) Return
      
*     loop over all raw hits
      do ihit=1,edc2_raw_tot_hits
         pln = edc2_raw_layer_num(ihit)
         wire  = edc2_raw_wire_num(ihit)
*     check valid layer and wire number
         if(pln.gt.0 .and. pln.le. emax_num_dc2_layers) then
c     histval=float(edc2_raw_tdc_sub_trig(ihit))
c     call hf1(hidrawtdcall,histval,1.)
*     test if tdc value less than lower limit for good hits
            if(edc2_raw_tdc_sub_trig(ihit) .lt. edc2_tdc_min_win(pln))  then
               edc2wire_early_mult(wire,pln)
     $              = edc2wire_early_mult(wire,pln)+1
            else
               if(edc2_raw_tdc_sub_trig(ihit) .gt. edc2_tdc_max_win(pln))  then
                  edc2wire_late_mult(wire,pln)
     $                 = edc2wire_late_mult(wire,pln)+1
               else
*     test for valid wire number
                  if(wire.gt.0 .and. wire.le.edc2_nrwire(pln)) then
*     test for multiple hit on the same wire
                     if(pln.eq.old_pln .and. wire.eq.old_wire) then
                        edc2wire_extra_mult(wire,pln) =
     $                       edc2wire_extra_mult(wire,pln)+1
                     else
                        
*     valid hit proceed with decoding
                        gh = gh + 1
                        edc2_layer_num(gh) = pln 
                        edc2_wire_num(gh) = wire 
                        edc2_tdc(gh) = edc2_raw_tdc_sub_trig(ihit)
                     
                        if((pln.eq.1) .or. (pln.eq.2)) then
                           edc2_slot_num(gh) = 6*(pln-1)
     &                       +int((wire-edc2_wire_offset_low(pln)-1)/16) +1
                        else if((pln.eq.3) .or. (pln.eq.4)) then
                           edc2_slot_num(gh) = 12 + 8*(pln-3)
     &                       +int((wire-edc2_wire_offset_low(pln)-1)/16) +1
                        else  
                           edc2_slot_num(gh) = 28 + 6*(pln-5)
     &                       +int((wire-edc2_wire_offset_low(pln)-1)/16) +1
                        EndIF
*     check cluster size
                        if( 
     &                       gh.gt.1 .and. 
     &                       old_pln .eq. pln .and. 
     &                       old_wire + 1 .eq. wire)
     &                       then
                           edc2_cluster_size(gh) 
     &                          = edc2_cluster_size(gh-1) + 1
                           Do k=1,edc2_cluster_size(gh)-1
                              edc2_cluster_size(gh-k) =
     &                             edc2_cluster_size(gh)
                           EndDo
                        Else
                           edc2_cluster_size(gh) = 1
                        EndIf


*     if edc2_wire_counting(pln) is 1 then wires are number in reverse order
                        if(edc2_wire_counting(pln).eq.0) then !normal ordering
                           edc2_wire_center(gh) = edc2_pitch(pln)
     &                          * (float(wire) 
     &                          - edc2_central_wire(pln))
     &                          - edc2_center(pln)
                        else
                           edc2_wire_center(gh) = edc2_pitch(pln)
     &                          * ( (edc2_nrwire(pln) + (1 - wire))
     &                          - edc2_central_wire(pln) ) 
     &                          - edc2_center(pln)
                        endif

c                        print *, "edc2_tdc(gh),tzero,slot",
c     &                  edc2_tdc(gh),edc2_layer_time_zero(edc2_slot_num(gh)),
c     &                  edc2_slot_num(gh)
                        edc2_drift_time(gh) = 0
     &                       + float(edc2_tdc(gh)) * pol
     &                       *edc2_tdc_time_per_channel
     &                       - edc2_layer_time_zero(edc2_slot_num(gh)) * pol
c                        print *, "edc2_drift_time(gh)",edc2_drift_time(gh)
                     

                        edc2_hits_per_layer(pln) = 
     &                       edc2_hits_per_layer(pln)+1

                     endif      ! end test on duplicate wire
                     old_pln = pln
                     old_wire = wire
                  endif         ! end test on valid wire number
               endif            ! end test on edc2_tdc_max_win
            endif               ! end test on edc2_tdc_min_win
         else                   ! if not a valid layer number
            write(6,*) 'E_TRANS_DC2: invalid layer number = ',pln
         endif                  ! end test on valid layer number
      enddo                     ! end loop over raw hits
      
*     
*     set total number of good hits
*     
      edc2_tot_hits = gh
*    
ccc DK added from here in order to adjust starttime for EDC2
      if (.not.gen_event_ts_flag(2)) then !coin, hks case
         if (entracks_fp.gt.0) then
            do ihit=1,edc2_tot_hits
               pln=edc2_layer_num(ihit) 
               wire=edc2_wire_num(ihit)
               wcenter=edc2_wire_center(ihit) 
               do itrk1=1,entracks_fp
                  xedc1=ex_fp(itrk1)
                  xpedc1=exp_fp(itrk1)
                  yedc1=ey_fp(itrk1)
                  ypedc1=eyp_fp(itrk1)
                  xedc2=xedc1+xpedc1*edc2_zpos(pln)
                  yedc2=yedc1+ypedc1*edc2_zpos(pln)
                  alpha=edc2_alpha_angle(pln)-90*raddeg
                  sedc2=xedc2*cos(alpha)+yedc2*sin(alpha)
                  ds=sedc2-wcenter
c                  write(*,*) "pln,itrk1,ds",pln,itrk1,ds
                  if(itrk1.eq.1) then
                     ds_min=ds
                     itrk_min=itrk1
                  elseif(abs(ds).le.abs(ds_min)) then
                     ds_min=ds
                     itrk_min=itrk1
                  endif
               enddo 
c               write(*,*) "itrk_min,ds_min,entracks_fp",itrk_min,ds_min,pln 
               edc2_drift_time(ihit) = 
     &         edc2_drift_time(ihit) -starttime2(itrk_min)
c            print *, 'trans', ihit,edc2_drift_time(ihit),starttime2(itrk_min)  
            enddo
         endif
      endif
ccc to here 
c      print *, "edc2_tot_hits before",edc2_tot_hits
      gh=0
      do ihit=1,edc2_tot_hits
         pln = edc2_layer_num(ihit)
         slot = edc2_slot_num(ihit)
         ddis = e_dc2_drift_dist_calc(pln,slot,edc2_drift_time(ihit))
c         print *, "ihit,pln,slot,dtime,ddis",ihit,pln,slot,edc2_drift_time(ihit),ddis
         if (ddis.gt.-1.) then
            gh=gh+1
            edc2_layer_num(gh)=edc2_layer_num(ihit)
            edc2_slot_num(gh)=edc2_slot_num(ihit)
            edc2_drift_time(gh)=edc2_drift_time(ihit)
            edc2_wire_center(gh)=edc2_wire_center(ihit)
            edc2_wire_num(gh)=edc2_wire_num(ihit)
            edc2_cluster_size(gh)=edc2_cluster_size(ihit)
            edc2_tdc(gh)=edc2_tdc(ihit)
         endif
      enddo
      edc2_tot_hits = gh
c      print *, "edc2_tot_hits after",edc2_tot_hits

*     
*     Dump decoded banks if flag is set
*      if(hdebugprintdcdec.ne.0) then
*         call h_print_decoded_dc(ABORT,err)
*         if (abort) then
*            call g_prepend(here,err)
*            return
*         endif
*      endif
*     
      RETURN
      END
