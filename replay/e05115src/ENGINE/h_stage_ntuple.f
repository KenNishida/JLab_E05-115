      subroutine h_stage_ntuple_init(ABORT,err)
*     Book separate event and space-point stage ntuples.
      implicit none
      logical ABORT,FAIL,HEXIST
      character*(*) err
      include 'h_stage_ntuple.cmn'
      include 'gen_run_info.cmn'
      integer recl,status
      character*80 olddir,why
      character*256 file
      character*8 evtag(44),sptag(9),trtag(12)
      data evtag /'run','event','evtype','tofstat',
     & 'pretotal','posttot',
     & 'pre01','pre02','pre03','pre04','pre05','pre06',
     & 'pre07','pre08','pre09','pre10','pre11','pre12',
     & 'post01','post02','post03','post04','post05','post06',
     & 'post07','post08','post09','post10','post11','post12',
     & 'spraw1','spraw2','sp1','sp2','lrplan','lrfit',
     & 'linkall','linkeval','linkpass','nfp','nphys',
     & 'spstat','lrstat','linkstat'/
      data sptag /'run','event','chamber','spidx','nhits',
     & 'nalt','lrplan','lrfit','precut'/
      data trtag /'run','event','track','trkhit',
     & 'layer','wire','tdc','driftt','driftd',
     & 'resid','chi2dof','hitidx'/

      ABORT=.false.
      err=' '
      hst_open=.false.
      hst_io=0
      hst_name='HSTAGE'
      hst_dir=' '
      call g_IO_control(hst_io,'ANY',ABORT,err)
      if (ABORT) return
      file='paw/hstage%d.rzdat'
      call g_sub_run_number(file,gen_run_number)
      call HCDIR(olddir,'R')
      recl=4096
      call HROPEN(hst_io,hst_name,file,'N',recl,status)
      if (status.ne.0) then
         ABORT=.true.
         write(err,'(a,i6)') 'HSTAGE: HROPEN status=',status
         call g_IO_control(hst_io,'FREE',FAIL,why)
         call HCDIR(olddir,' ')
         return
      endif
      call HBOOKN(9040,'HKS tracking stages',44,
     &            hst_name,8000,evtag)
      call HBOOKN(9041,'HKS space point stages',9,
     &            hst_name,8000,sptag)
      call HBOOKN(9042,'Hits used by HKS FP tracks',12,
     &            hst_name,8000,trtag)
      call HCDIR(hst_dir,'R')
      hst_open=HEXIST(9040).and.HEXIST(9041)
     &         .and.HEXIST(9042)
      if (.not.hst_open) then
         ABORT=.true.
         err='HSTAGE: HBOOKN failed'
         call HREND(hst_name)
         call g_IO_control(hst_io,'FREE',FAIL,why)
         close(hst_io)
      else
         write(6,*) 'HSTAGE: opened ',trim(file)
      endif
      call HCDIR(olddir,' ')
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

      subroutine h_stage_ntuple_keep(ABORT,err)
*     One event row and one row per selected space point.
      implicit none
      logical ABORT
      character*(*) err
      include 'h_stage_ntuple.cmn'
      include 'h_stage_diag.cmn'
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'gen_event_info.cmn'
      include 'gen_run_info.cmn'
      real*4 ev(44),sp(9),tr(12)
      integer i,j,it,ih,pl,pretotal,posttotal,lrplan,lrfit
      character*80 olddir
      ABORT=.false.
      err=' '
      if (.not.hst_open) return

      pretotal=0
      posttotal=0
      do i=1,12
         if (hst_pre(i).ge.0) pretotal=pretotal+hst_pre(i)
         if (hst_post(i).ge.0) posttotal=posttotal+hst_post(i)
      enddo
      lrplan=0
      lrfit=0
      do i=1,min(hst_numsp,hst_maxsp)
         lrplan=lrplan+hst_lrplan(i)
         lrfit=lrfit+hst_lrfit(i)
      enddo
      ev(1)=real(gen_run_number)
      ev(2)=real(gen_event_ID_number)
      ev(3)=real(gen_event_type)
      ev(4)=real(hst_tofstat)
      ev(5)=-1.
      ev(6)=-1.
      if (hst_tofstat.ge.1) ev(5)=real(pretotal)
      if (hst_tofstat.ge.1) ev(6)=real(posttotal)
      do i=1,12
         ev(6+i)=real(hst_pre(i))
         ev(18+i)=real(hst_post(i))
      enddo
      ev(31)=real(hst_spraw(1))
      ev(32)=real(hst_spraw(2))
      ev(33)=real(hst_spfinal(1))
      ev(34)=real(hst_spfinal(2))
      ev(35)=real(lrplan)
      ev(36)=real(lrfit)
      ev(37)=real(hst_spfinal(1))*real(hst_spfinal(2))
      ev(38)=real(hst_linkeval)
      ev(39)=real(hst_linkpass)
      ev(40)=real(hntracks_fp)
      ev(41)=real(hnphysics)
      ev(42)=real(hst_spstat)
      ev(43)=real(hst_lrstat)
      ev(44)=real(hst_linkstat)

      call HCDIR(olddir,'R')
      call HCDIR(hst_dir,' ')
      call HFN(9040,ev)
      sp(1)=real(gen_run_number)
      sp(2)=real(gen_event_ID_number)
      do i=1,min(hst_numsp,hst_maxsp)
         sp(3)=real(hst_spch(i))
         sp(4)=real(i)
         sp(5)=real(hst_sphits(i))
         sp(6)=real(hst_spalt(i))
         sp(7)=real(hst_lrplan(i))
         sp(8)=real(hst_lrfit(i))
         sp(9)=-1.
         if(hst_lrstat.ne.0) sp(9)=real(precutflag(i))
         call HFN(9041,sp)
      enddo
*     Save every hit actually assigned to each FP track.
      tr(1)=real(gen_run_number)
      tr(2)=real(gen_event_ID_number)

      do it=1,hntracks_fp
         tr(3)=real(it)
         tr(11)=hchi2perdof_fp(it)

         do j=2,min(hntrack_hits(it,1)+1,hntrackhits_max+1)
            ih=hntrack_hits(it,j)
            if (ih.lt.1.or.ih.gt.hdc_tot_hits) cycle

            pl=hdc_layer_num(ih)
            if (pl.lt.1.or.pl.gt.hdc_num_layers) cycle

            tr(4)=real(j-1)
            tr(5)=real(pl)
            tr(6)=real(hdc_wire_num(ih))
            tr(7)=real(hdc_tdc(ih))
            tr(8)=hdc_drift_time(ih)
            tr(9)=hdc_drift_dis(ih)
            tr(10)=hdc_single_residual(it,pl)
            tr(12)=real(ih)

            call HFN(9042,tr)
         enddo
      enddo
      call HCDIR(olddir,' ')
      return
      end

      subroutine h_stage_ntuple_shutdown(ABORT,err)
*     Flush both ntuples and close stage RZ file.
      implicit none
      logical ABORT
      character*(*) err
      include 'h_stage_ntuple.cmn'
      integer cycle,io
      character*80 olddir
      ABORT=.false.
      err=' '
      if (.not.hst_open) return
      call HCDIR(olddir,'R')
      call HCDIR(hst_dir,' ')
      cycle=0
      call HROUT(9040,cycle,' ')
      cycle=0
      call HROUT(9041,cycle,' ')
      cycle=0
      call HROUT(9042,cycle,' ')
      call HREND(hst_name)
      io=hst_io
      call g_IO_control(io,'FREE',ABORT,err)
      close(hst_io)
      call HCDIR(olddir,' ')
      hst_open=.false.
      return
      end
