      subroutine h_pattern_recognition(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods :  Finds SOS Space points 
*     -
*     -      Required Input BANKS     SOS_DECODED_DC
*     -
*     -      Output BANKS             SOS_FOCAL_LAYER
*     -                               SOS_DECODED_DC hit coordinates
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     -   Created 30-AUG-1993   D. F. Geesaman
*     -   Modified 19-JAN-1994  DFG    Include standard error form
*     $Log: h_pattern_recognition.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.8  2005/03/14 19:52:17  miyoshi
*     change tracking variables and histogram names
*
*     Revision 1.7  2005/03/07 19:20:17  miyoshi
*     remove hdc central time from drift time correction
*
*     Revision 1.6  2005/03/07 16:34:37  miyoshi
*     add new hist for hdc
*
*     Revision 1.5  2005/03/02 22:32:15  miyoshi
*     move call location
*
*     Revision 1.4  2005/01/13 20:26:56  yuan
*     Wire velocity corrections are changed due to the location of
*     HDC preamp cards.
*
*     Revision 1.3  2004/12/24 21:35:57  miyoshi
*     change name plane to layer
*
*     Revision 1.2  2004/12/24 19:47:07  miyoshi
*     change minor
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.10  1996/09/05 20:09:36  saw
*     (JRA) Cosmetic
*     
*     Revision 1.9  1996/04/30 17:34:56  saw
*     (JRA) Histogram the card id.
*     
*     Revision 1.8  1996/01/17 19:01:21  cdaq
*     (JRA) Add code for easy space points
*     
*     Revision 1.7  1995/10/10 16:13:47  cdaq
*     (JRA) Remove sdc_sing_wcenter, cosmetics.
*     
*     Revision 1.6  1995/07/20 18:58:50  cdaq
*     (SAW) Declare sind and cosd for f2c compatibility
*     
*     Revision 1.5  1995/05/22  19:45:43  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.4  1995/04/06  19:36:53  cdaq
*     (SAW) Hopefully improve wire velocity correction for SOS chambers
*     
*     Revision 1.3  1994/12/06  15:33:06  cdaq
*     (SAW) First pass at wire velocity correction for Brookhaven chambers
*     
*     Revision 1.2  1994/11/22  21:48:45  cdaq
*     (SPB) Recopied from hms file and modified names for SOS
*     (SAW) Improved some code hardwired for 3 chambers.  NOTE: the wire velocity
*     correction stuff at the end is HMS specific.  This needs to
*     be worked on.
*     
*     Revision 1.1  1994/02/21  16:15:19  cdaq
*     Initial revision
*     
*     
*     This routine finds the space points in each chamber using wire center
*     locations.
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*21 here
      parameter (here= 'h_pattern_recognition')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hks_tracking.cmn'
      INCLUDE 'hks_geometry.cmn'
*     
*     local variables
      integer*4 hit_number(hmax_chamber_hits)
      integer*4 space_point_hits(hmax_space_points,hmax_hits_per_point+2)
      integer*4 pln, wiren, cardn, isp, ihit, hit
      integer*4 i,j,k,xx,xxprime
      integer*4 xlayer,xprimelayer
      integer*4 ich, ip, pch
      logical easy_space_point
*     
      real*4 space_points(hmax_space_points,2)
      real*4 xdist,ydist
      real*4 time_corr
      real*4 h_drift_dist_calc
      external h_drift_dist_calc
      
*     
*     temporary initialization
      ABORT= .FALSE.
      err=' '
*     
*     
      ihit = 0
      hnspace_points_tot = 0

c     --- fill dc decorded hist
      
      call h_fill_dc_dec_hist(ABORT,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

      if(hdc_tot_hits .le. 0) Return

      do ich=1,hdc_num_chambers
         easy_space_point = .false.
         hnspace_points(ich)=0        
         hncham_hits(ich)=0
*     
*     For this loop to work, hdc_layers_per_chamber must be
*     the number of layers per chamber.  (And all chambers must have the
*     same number of layers.)
*     
         pch = hdc_layers_per_chamber
         do ip=(ich-1)*pch+1,ich*pch
            hncham_hits(ich)=hncham_hits(ich)+hdc_hits_per_layer(ip)
         enddo
         xlayer=3+(ich-1)*pch
         xprimelayer=4+(ich-1)*pch
         if(hncham_hits(ich).ge.hmin_hit(ich) .and.
     $        hncham_hits(ich).lt.hmax_pr_hits(ich))  then
            do i=ihit+1,ihit+hncham_hits(ich)
               hit_number(i)=i
               if(hdc_layer_num(i).eq.xlayer) xx=i
               if(hdc_layer_num(i).eq.xprimelayer) xxprime=i
            enddo
            if((hdc_hits_per_layer(xlayer).eq.1) .and.
     &           (hdc_hits_per_layer(xprimelayer).eq.1).and.
     &           ((hdc_wire_center(xx)-hdc_wire_center(xxprime))**2.lt.
     &           (hspace_point_criterion(ich))) .and.
     &           (hncham_hits(ich).le.6)) then
               call h_find_easy_space_point(hncham_hits(ich),
     &              hit_number(ihit+1),
     &              hdc_wire_center(ihit+1),hdc_layer_num(ihit+1),
     &              hspace_point_criterion(ich),
     &              hmax_space_points,xx-ihit,
     &              xxprime-ihit,easy_space_point,hnspace_points(ich),
     &              space_points,space_point_hits)
               if (.not.easy_space_point) call h_find_space_points(
     &              hncham_hits(ich),
     &              hit_number(ihit+1),hdc_wire_center(ihit+1),
     &              hdc_layer_num(ihit+1),hspace_point_criterion(ich),
     &              hxsp(1),hysp(1),hmax_space_points,
     &              hnspace_points(ich), space_points, space_point_hits)
            else
               call h_find_space_points(hncham_hits(ich),
     &              hit_number(ihit+1),
     &              hdc_wire_center(ihit+1),
     &              hdc_layer_num(ihit+1),hspace_point_criterion(ich),
     &              hxsp(1),hysp(1),hmax_space_points,
     &              hnspace_points(ich), space_points, space_point_hits)
            endif
*     
            if (hnspace_points(ich).gt.50)
     >           hnspace_points(ich)=0
            if (hnspace_points(ich).gt.0) then
*     If two hits in same layer, choose one with minimum drift time
               call h_choose_single_hit(ABORT,err,hnspace_points(ich),
     &              space_point_hits)
*     Select on minimum number of combinations and hits
               call select_space_points(hmax_space_points,
     &              hnspace_points(ich),
     &              space_points,space_point_hits,hmin_hit(ich),
     &              hmin_combos(ich),
     $              easy_space_point)
            endif
            
            
            do i=1,hnspace_points(ich)
               k=hnspace_points_tot+i
               hspace_points(k,1)=space_points(i,1)
               hspace_points(k,2)=space_points(i,2)
               hspace_point_hits(k,1)=space_point_hits(i,1)
               hspace_point_hits(k,2)=space_point_hits(i,2)
               do j=1,space_point_hits(i,1)
                  hspace_point_hits(k,j+2)=space_point_hits(i,j+2)
               enddo
            enddo
         endif
         hnspace_points_tot = hnspace_points_tot + hnspace_points(ich)
         ihit = ihit + hncham_hits(ich)
      enddo                     ! chamber loop
*     
*     Now we know rough hit positions in the chambers so we can make
*     wire velocity drift time corrections for each hit in the space point
*     
*     Assume all wires for a layer are read out on the same side (l/r or t/b).
*     If the wire is closer to horizontal, read out left/right.  If nearer
*     vertical, assume top/bottom.  (Note, this is not always true for the
*     HKS u and v layers.  They have 1 card each on the side, but the overall
*     time offset per card will cancel much of the error caused by this.  The
*     alternative is to check by card, rather than by layer and this is harder.
*     
* For HKS DC, All read out are from the t/b sides, but the cards are
* on both sides for same plane according to hdc_card_pos.
*       L.Yuan  01/12/05
      if(hnspace_points_tot.gt.0) then
         do isp=1,hnspace_points_tot
            xdist = hspace_points(isp,1)
            ydist = hspace_points(isp,2)
            do ihit=1,hspace_point_hits(isp,1)
               hit = hspace_point_hits(isp,ihit+2)
               pln = hdc_layer_num(hit)
               wiren = hdc_wire_num(hit)
               cardn = hdc_card_no(wiren,pln) 
               if (cardn.ne.0) then
                  time_corr = (-hdc_card_pos(cardn)*ydist+hdc_length_y/2.)
     &                 *hdc_readout_corr(pln)/hdc_wire_velocity
               else
                  time_corr = 0.
               endif
               hdc_drift_time(hit) = hdc_drift_time(hit) 
     &              + hdc_drifttime_sign(pln)*time_corr
               hdc_drift_dis(hit) = h_drift_dist_calc
     &              (pln,hdc_wire_num(hit),hdc_drift_time(hit))
*     
*     djm 8/25/94
*     Stuff drift time and distance into registered variables 
*     for histogramming and tests.
*     In the case of two separated hits per layer, 
*     the last one will be histogrammed.
*     
               hdc_sing_drifttime(pln) = hdc_drift_time(hit)
               hdc_sing_driftdis(pln) = hdc_drift_dis(hit)
               hdc_sing_cardid(pln) =
     &              hdc_card_no(hdc_wire_num(hit),hdc_layer_num(hit))
            enddo
         enddo
      endif
*     
*     write out results if debugflagpr is set
      if(hdebugflagpr.ne.0) then
         call h_print_pr
      endif
*
      return
      end
