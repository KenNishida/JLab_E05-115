      subroutine htracking_open_file(abort,err)
      implicit none
      include 'gen_run_info.cmn'
      logical abort
      character*(*) err
      integer*4 status
      abort=.false.
      err=' '
      call htrk_open(gen_run_number,status)
      if(status.ne.0) then
         abort=.true.
         err='Cannot open paw/htracking<run>.root (ROOT writer)'
      endif
      return
      end

      subroutine htracking_record_event
      implicit none
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'h_stage_diag.cmn'
      integer*4 isp,lrgen,lrfit
      lrgen=0
      lrfit=0
      do isp=1,hst_maxsp
         lrgen=lrgen+hst_lrplan(isp)
         lrfit=lrfit+hst_lrfit(isp)
      enddo
      call htrk_end(hst_spraw,lrgen,lrfit,hst_linkeval,
     &     hntracks_fp,hst_tofstat)
      return
      end

      subroutine htracking_reset_ids
      implicit none
      include 'htracking_ids.cmn'
      integer*4 i
      do i=1,3600
         htrk_dec_id_at(i)=0
      enddo
      do i=1,500
         htrk_local_sp_id(i)=0
         htrk_global_sp_id(i)=0
      enddo
      htrk_next_sp_id=0
      htrk_next_hitset_id=0
      htrk_next_track_id=0
      htrk_next_link_id=0
      do i=1,240
         htrk_track_id_at(i)=0
      enddo
      return
      end

*     Track-space-point array in H_LINK_STUBS is column-major with
*     the first index (track) varying fastest. Convert explicitly.
      subroutine htracking_snapshot_track(itrk,parent,linkid,nsp,
     &     track_space_points)
      implicit none
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'htracking_ids.cmn'
      integer*4 itrk,parent,linkid,nsp,track_space_points(
     &     HNTRACKS_MAX,hmax_space_points+1)
      integer*4 spids(hmax_space_points),i,j,parentid,trackid
      if(itrk.lt.1.or.itrk.gt.HNTRACKS_MAX) return
      if(nsp.lt.1.or.nsp.gt.hmax_space_points) return
      if(htrk_track_id_at(itrk).eq.0) then
         htrk_next_track_id=htrk_next_track_id+1
         htrk_track_id_at(itrk)=htrk_next_track_id
      endif
      trackid=htrk_track_id_at(itrk)
      parentid=0
      if(parent.gt.0.and.parent.le.HNTRACKS_MAX) then
         parentid=htrk_track_id_at(parent)
      endif
      do i=1,nsp
         j=track_space_points(itrk,i+1)
         spids(i)=0
         if(j.ge.1.and.j.le.hmax_space_points) then
            spids(i)=htrk_global_sp_id(j)
         endif
      enddo
      call htrk_track_snapshot(trackid,parentid,linkid,nsp,spids)
      return
      end

      subroutine h_stage_diag_reset
*     Reset counts for each HKS event.
      implicit none
      include 'h_stage_diag.cmn'
      integer i
      hst_tofstat=0
      hst_spstat=0
      hst_lrstat=0
      hst_linkstat=0
      hst_numsp=0
      hst_linkeval=0
      hst_linkpass=0
      do i=1,12
         hst_pre(i)=-1
         hst_post(i)=-1
      enddo
      do i=1,2
         hst_spraw(i)=0
         hst_spfinal(i)=0
      enddo
      do i=1,hst_maxsp
         hst_spch(i)=0
         hst_sphits(i)=0
         hst_spalt(i)=0
         hst_lrplan(i)=0
         hst_lrfit(i)=0
      enddo
      return
      end
