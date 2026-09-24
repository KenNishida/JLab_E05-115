      subroutine e_dc1_cleanup(ABORT,err)

*     drift distances and chooses the set with the minimum chi**2
*     It then fills the SDC_WIRE_COORD variable for each hit in a good
*     space point.
*
      implicit none
      save
      INCLUDE 'hes_data_structures.cmn'
      INCLUDE 'hes_tracking.cmn'
      INCLUDE 'hes_geometry.cmn'
      INCLUDE 'gen_event_info.cmn'
*     
*     local variables
*     
      character*12 here
      parameter (here= 'e_dc1_cleanup')
*     
      logical ABORT
      character*(*) err
      integer*4 isp, ihit,iswhit, idummy, pmloop, ich,i,gh,gt,j,hit
      integer*4 nplusminus
      integer*4 numhits,npaired,ihit2
      integer*4 hits(edc1max_hits_per_point), pl
      integer*4 sl
      real*4 wcoord(edc1max_hits_per_point)
      integer*4 layer, isa_y1, isa_y2,itrack
      integer*4 pln_flag(emax_num_dc1_layers)
      integer*4 num_plnhits(edc1max_space_points)
      integer*4 num_uvhits(edc1max_space_points)
      real*4 plusminus(edc1max_hits_per_point)
      real*4 plusminusbest(edc1max_hits_per_point)
      real*4 chi2,residual
      real*4 minchi2,stub(4)
      real*4 n_missing_hits
      real*4 drift_direction
      real*4 raddeg
      real*4 dt
      real*8 dray(enum_fpray_param)
      real*8 TT(enum_fpray_param)
      real*8 AA(enum_fpray_param,enum_fpray_param)
      integer*4 ierr
      parameter (raddeg=3.14159265/180.)
      logical smallAngOk
      integer*4 remap(enum_fpray_param)
      data remap/5,6,3,4/
      save remap
      Real*4 e_drift_dist_calc
      external e_drift_dist_calc

*     
      ABORT= .FALSE.
      err=' '
      
      if (edc1nspace_points_tot.le.0) return 
         
      do isp=1,edc1nspace_points_tot ! loop over all space points
         numhits=edc1space_point_hits(isp,1)
         gh=0
         num_plnhits(isp)=0
         num_uvhits(isp)=0
c         print *, "numhits",numhits
         do i=1,emax_num_dc1_layers
            pln_flag(i)=0 
         enddo
         do ihit=1,numhits
            hit = edc1space_point_hits(isp,2+ihit)
            pl = EDC1_LAYER_NUM(hit)
            sl = EDC1_SLOT_NUM(hit)
            dt=edc1_drift_time(hit)-starttime(isp)
            edc1_drift_dis(hit) = e_drift_dist_calc(pl,sl,dt)
            if (edc1_drift_dis(hit).gt.-1) then
c            print *, "pl,dt,ddis",pl,dt,edc1_drift_dis(hit)
               gh=gh+1
               hit=edc1space_point_hits(isp,2+ihit)
               edc1space_point_hits(isp,2+gh)=hit
c               edc1_layer_num(gh)=edc1_layer_num(hit)
c               edc1_slot_num(gh)=edc1_slot_num(hit)
c               edc1_group_num(gh)=edc1_group_num(hit)
c               edc1_drift_time(gh)=edc1_drift_time(hit)
c               edc1_wire_center(gh)=edc1_wire_center(hit)
c               edc1_wire_num(gh)=edc1_wire_num(hit)
c               edc1_cluster_size(gh)=edc1_cluster_size(hit)
c               edc1_tdc(gh)=edc1_tdc(hit)
               pln_flag(pl)=1
           endif
         enddo
c         print *, "gh",gh
         do i=1,emax_num_dc1_layers
            if (pln_flag(i).eq.1) num_plnhits(isp)=num_plnhits(isp)+1 
            if (pln_flag(i).eq.1.and.(i.eq.3.or.i.eq.4.or.i.eq.7.or.i.eq.8)) 
     &          num_uvhits(isp)=num_uvhits(isp)+1 
         enddo
         edc1space_point_hits(isp,1)=num_plnhits(isp)
c         print *, "isp,num_plnhits",isp,num_plnhits(isp)
      enddo
      
      gt=0
      do isp=1,edc1nspace_points_tot ! loop over all space points
c         print *, "isp,nplnhit", isp,num_plnhits(isp)
c         if (num_plnhits(isp).ge.edc1min_hit) then
         if (num_plnhits(isp).ge.edc1min_hit.and.num_uvhits(isp).ge.3) then
            gt=gt+1
            starttime(gt)=starttime(isp)
            starttime2(gt)=starttime2(isp)
            numhits=edc1space_point_hits(isp,1)
            do ihit=1,numhits+2
               edc1space_point_hits(gt,ihit)=edc1space_point_hits(isp,ihit)
            enddo
         endif
      enddo
c      print *, "gt",gt
      edc1nspace_points_tot=gt

      return
      end
      

