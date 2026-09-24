      subroutine e_dc2_pattern_recognition(ABORT,err)
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
*     $Log: e_dc2_pattern_recognition.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
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
      parameter (here= 'e_dc2_pattern_recognition')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'hes_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hes_tracking.cmn'
      INCLUDE 'hes_geometry.cmn'
      include 'gen_run_info.cmn'
      include 'hes_bypass_swiches.cmn' !Added by Gogami (3Feb2011)
*     
*     local variables
      integer*4 hit_number(edc2max_chamber_hits)
      integer*4 space_point_hits(edc2max_space_points,edc2max_hits_per_point+2)
      integer*4 pln, wiren, cardn, isp, ihit, hit
      integer*4 i,j,k,xx,xxprime
      integer*4 xlayer,xprimelayer
      integer*4 ip, pch
      integer*4 dindex
      logical easy_space_point
*     
      real*4 space_points(edc2max_space_points,2)
      real*4 xdist,ydist
      real*4 time_corr
      real*4 e_dc2_drift_dist_calc
      external e_dc2_drift_dist_calc
      
      integer*4 etrig2
      
*     
*     temporary initialization
      ABORT= .FALSE.
      err=' '
*     
c     =======OUTPUT CHAMBER INFROMATION=====================
c     ============Added by Gogami (3Feb2011)================
      if(edc_hitinfo_flag.eq.1)then
         open(24,file='hes_cham.dat',access='append')   
         write(24,*)edc2_tot_hits
         write(24,*)edc2_hits_per_layer
         do dindex=1,edc2_tot_hits
            write(24,*)edc2_layer_num(dindex),edc2_wire_num(dindex),
     &           edc2_wire_center(dindex),edc2_drift_time(dindex)
         enddo
         close(24)
      endif
c     ========================================================

*     
      ihit = 0
      edc2nspace_points_tot = 0

c     --- fill dc decorded hist
      
      call e_fill_dc2_dec_hist(ABORT,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

      if(edc2_tot_hits .le. 0) Return

      easy_space_point = .false.
      edc2nspace_points=0        
      edc2ncham_hits=0
*     
*     For this loop to work, edc2_layers_per_chamber must be
*     the number of layers per chamber.  (And all chambers must have the
*     same number of layers.)
*     
      pch = edc2_layers_per_chamber
      do ip=1,pch
         edc2ncham_hits=edc2ncham_hits+edc2_hits_per_layer(ip)
      enddo
      xlayer=3
      xprimelayer=4

cc      print *, 'hit chamber', edc2ncham_hits, edc2_hits_per_layer(2)

      if(edc2ncham_hits.ge.edc2min_hit .and.
     $        edc2ncham_hits.lt.edc2max_pr_hits)  then
         ! ihit is always 0? DK 
         do i=ihit+1,ihit+edc2ncham_hits
            hit_number(i)=i
            if(edc2_layer_num(i).eq.xlayer) xx=i
            if(edc2_layer_num(i).eq.xprimelayer) xxprime=i
         enddo

cc         print *, 'hits2', edc2_tot_hits
cc         print *, 'xx,xxprime', xx,xxprime

         if (xx.gt.0 .and.xxprime.gt.0) then
            if((edc2_hits_per_layer(xlayer).eq.1) .and.
     &        (edc2_hits_per_layer(xprimelayer).eq.1).and.
     &        ((edc2_wire_center(xx)-edc2_wire_center(xxprime))**2.lt.
     &        (edc2space_point_criterion)) .and.
     &        (edc2ncham_hits.le.6)) then
               call e_dc2_find_easy_space_point(edc2ncham_hits,
     &         hit_number(ihit+1),
     &         edc2_wire_center(ihit+1),
     &         edc2_layer_num(ihit+1),
     &         edc2space_point_criterion,
     &         edc2max_space_points,
     &         xx-ihit,xxprime-ihit,
     &         easy_space_point,
     &         edc2nspace_points,space_points,space_point_hits)
               if (.not.easy_space_point) 
     &          call e_find_space_points(edc2ncham_hits,
     &           hit_number(ihit+1),
     &           edc2_wire_center(ihit+1),
     &           edc2_layer_num(ihit+1),
     &           edc2space_point_criterion,
     &           edc2xsp(1),edc2ysp(1),
     &           edc2max_space_points,
     &           edc2nspace_points, space_points, space_point_hits)
            endif
         else
            call e_find_space_points(edc2ncham_hits,
     &        hit_number(ihit+1),
     &        edc2_wire_center(ihit+1),
     &        edc2_layer_num(ihit+1),
     &        edc2space_point_criterion,
     &        edc2xsp(1),edc2ysp(1),
     &        edc2max_space_points,
     &        edc2nspace_points, space_points, space_point_hits)
         endif
*     
cDK         if (edc2nspace_points.gt.50)
         if (edc2nspace_points.gt.100)
     >           edc2nspace_points=0
         if (edc2nspace_points.gt.0) then
*     If two hits in same layer, choose one with minimum drift time
            call e_dc2_choose_single_hit(ABORT,err,edc2nspace_points,
     &              space_point_hits)
*     Select on minimum number of combinations and hits
            call select_space_points(edc2max_space_points,
     &              edc2nspace_points,
     &              space_points,space_point_hits,edc2min_hit,
     &              edc2min_combos,
     $              easy_space_point)
         endif
            
            
         do i=1,edc2nspace_points
            k=edc2nspace_points_tot+i
            edc2space_points(k,1)=space_points(i,1)
            edc2space_points(k,2)=space_points(i,2)
            edc2space_point_hits(k,1)=space_point_hits(i,1)
            edc2space_point_hits(k,2)=space_point_hits(i,2)
            do j=1,space_point_hits(i,1)
               edc2space_point_hits(k,j+2)=space_point_hits(i,j+2)
            enddo
         enddo
        endif
        edc2nspace_points_tot = edc2nspace_points_tot + edc2nspace_points
        ihit = ihit + edc2ncham_hits
cc        print *, 'nspace, points', edc2nspace_points_tot
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
* on both sides for same plane according to edc2_card_pos.
*       L.Yuan  01/12/05
      if(edc2nspace_points_tot.gt.0) then
         do isp=1,edc2nspace_points_tot
            xdist = edc2space_points(isp,1)
            ydist = edc2space_points(isp,2)
            do ihit=1,edc2space_point_hits(isp,1)
               hit = edc2space_point_hits(isp,ihit+2)
               pln = edc2_layer_num(hit)
               wiren = edc2_wire_num(hit)
               cardn = edc2_card_no(wiren,pln) 
               if (cardn.ne.0) then
                  time_corr = (edc2_card_pos(cardn)*ydist+edc2_length_y/2.)
     &                 *edc2_readout_corr(pln)/edc2_wire_velocity
c                  time_corr = (-edc2_card_pos(cardn)*ydist+edc2_length_y/2.)
c     &                 *edc2_readout_corr(pln)/edc2_wire_velocity
               else
                  time_corr = 0.
               endif
c               print *, 'pattern recognition', xdist,ydist,isp,hit,edc2_drift_time(hit) 
c               edc2_drift_time(hit) = edc2_drift_time(hit) 
c     &              + edc2_drifttime_sign(pln)*time_corr
c               edc2_drift_dis(hit) = e_dc2_drift_dist_calc
c     &              (pln,edc2_wire_num(hit),edc2_drift_time(hit))
               edc2_drift_dis(hit) = e_dc2_drift_dist_calc
     &              (pln,cardn,edc2_drift_time(hit))
c               write(*,*)"pln,dtime,ddis",pln,edc2_drift_time(hit),edc2_drift_dis(hit)
*     
*     djm 8/25/94
*     Stuff drift time and distance into registered variables 
*     for histogramming and tests.
*     In the case of two separated hits per layer, 
*     the last one will be histogrammed.
*     
cDK               edc2_sing_drifttime(pln) = edc2_drift_time(hit)
cDK              edc2_sing_driftdis(pln) = edc2_drift_dis(hit)
cDK              edc2_sing_cardid(pln) =
cDK    &              edc2_card_no(edc2_wire_num(hit),edc2_layer_num(hit))
            enddo
         enddo
      endif
*     
*     write out results if debugflagpr is set
*      if(hdebugflagpr.ne.0) then
*         call e_print_pr
*      endif
*
      return
      end
