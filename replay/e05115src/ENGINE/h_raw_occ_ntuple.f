
      subroutine h_raw_occ_ntuple_init(ABORT,err)
*     Book event-wise (9030) and raw-hit-wise (9031) RWN.
      implicit none
      logical ABORT,FAIL,HEXIST
      character*(*) err
      include 'h_raw_occ_ntuple.cmn'
      include 'gen_run_info.cmn'
      integer recl,status
      character*80 olddir,why
      character*256 file
      character*8 tags(34),hits(12)
      data tags /'run','event','evtype','rawtot',
     &     'raw01','raw02','raw03','raw04','raw05','raw06',
     &     'raw07','raw08','raw09','raw10','raw11','raw12',
     &     'kdc1','kdc2','nfp','nphys','decstat','dectot',
     &     'dec01','dec02','dec03','dec04','dec05','dec06',
     &     'dec07','dec08','dec09','dec10','dec11','dec12'/
      data hits /'run','event','layer','wire','rawidx','tdc',
     &     'tdcused','inwin','accepted','decidx','driftt','driftd'/

      ABORT=.false.
      err=' '
      hraw_open=.false.
      hraw_id=9030
      hraw_io=0
      hraw_name='HRAW'
      hraw_dir=' '
      call g_IO_control(hraw_io,'ANY',ABORT,err)
      if (ABORT) return

      file='paw/hraw%d.rzdat'
      call g_sub_run_number(file,gen_run_number)
      call HCDIR(olddir,'R')
      recl=4096
      call HROPEN(hraw_io,hraw_name,file,'N',recl,status)
      if (status.ne.0) then
         ABORT=.true.
         write(err,'(a,i6)') 'HRAW: HROPEN status=',status
         call g_IO_control(hraw_io,'FREE',FAIL,why)
         call HCDIR(olddir,' ')
         return
      endif

      call HBOOKN(hraw_id,'HKS raw and selected occupancy',34,
     &            hraw_name,8000,tags)
      call HBOOKN(9031,'HKS raw hit details',12,
     &            hraw_name,8000,hits)
      call HCDIR(hraw_dir,'R')
      hraw_open=HEXIST(hraw_id).and.HEXIST(9031)
      if (.not.hraw_open) then
         ABORT=.true.
         err='HRAW: HBOOKN failed'
         call HREND(hraw_name)
         call g_IO_control(hraw_io,'FREE',FAIL,why)
         close(hraw_io)
      else
         write(6,*) 'HRAW: opened ',trim(file),' IDs=9030,9031'
      endif
      call HCDIR(olddir,' ')
      return
      end

      subroutine h_raw_occ_ntuple_reset
*     Clear the acceptance map before processing an HKS event.
      implicit none
      include 'h_raw_occ_diag.cmn'
      integer i

      hraw_decstat=0
      do i=1,hraw_max
         hraw_decidx(i)=0
         hraw_driftt(i)=-9999.
         hraw_driftd(i)=-9999.
      enddo
      return
      end

      subroutine h_raw_occ_ntuple_keep(ABORT,err)
*     One event row and one row per raw hit.
      implicit none
      logical ABORT
      character*(*) err
      include 'h_raw_occ_ntuple.cmn'
      include 'h_raw_occ_diag.cmn'
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'gen_event_info.cmn'
      include 'gen_run_info.cmn'
      real*4 vals(34),hit(12)
      integer i,l,nraw(12),ndec(12),kdc1,kdc2,ntot
      character*80 olddir

      ABORT=.false.
      err=' '
      if (.not.hraw_open) return

      do i=1,12
         nraw(i)=0
         ndec(i)=0
      enddo
      kdc1=0
      kdc2=0
      ntot=0

      do i=1,hdc_raw_tot_hits
         l=hdc_raw_layer_num(i)
         if (l.ge.1.and.l.le.12) then
            nraw(l)=nraw(l)+1
            if (hraw_decstat.eq.1.and.hraw_decidx(i).gt.0) then
               ndec(l)=ndec(l)+1
               ntot=ntot+1
            endif
         endif
      enddo

      do i=1,6
         kdc1=kdc1+nraw(i)
         kdc2=kdc2+nraw(i+6)
      enddo

*     Fill event-wise multiplicity tree.
      vals(1)=real(gen_run_number)
      vals(2)=real(gen_event_ID_number)
      vals(3)=real(gen_event_type)
      vals(4)=real(hdc_raw_tot_hits)
      do i=1,12
         vals(4+i)=real(nraw(i))
      enddo
      vals(17)=real(kdc1)
      vals(18)=real(kdc2)
      vals(19)=real(hntracks_fp)
      vals(20)=real(hnphysics)
      vals(21)=real(hraw_decstat)
      vals(22)=-1.
      do i=1,12
         vals(22+i)=-1.
      enddo

      if (hraw_decstat.eq.1) then
         vals(22)=real(ntot)
         do i=1,12
            vals(22+i)=real(ndec(i))
         enddo
      endif

      call HCDIR(olddir,'R')
      call HCDIR(hraw_dir,' ')
      call HFN(hraw_id,vals)

*     Fill hit-wise tree: exactly one entry per raw hit.
      hit(1)=real(gen_run_number)
      hit(2)=real(gen_event_ID_number)

      do i=1,hdc_raw_tot_hits
         l=hdc_raw_layer_num(i)
         hit(3)=real(l)
         hit(4)=real(hdc_raw_wire_num(i))
         hit(5)=real(i)
         hit(6)=real(hdc_raw_tdc(i))
         hit(7)=-9999.
         hit(8)=-1.
         hit(9)=-1.
         hit(10)=-1.

         if (hraw_decstat.eq.1) then
            hit(7)=real(hdc_raw_tdc_sub_trig(i))
            if (l.ge.1.and.l.le.hdc_num_layers) then
               hit(8)=0.
               if (hdc_raw_tdc_sub_trig(i).ge.hdc_tdc_min_win(l)
     &             .and.hdc_raw_tdc_sub_trig(i).le.
     &                  hdc_tdc_max_win(l)) hit(8)=1.
            endif
            hit(9)=0.
            hit(10)=real(hraw_decidx(i))
            if (hraw_decidx(i).gt.0) hit(9)=1.
         endif

         hit(11)=hraw_driftt(i)
         hit(12)=hraw_driftd(i)
         call HFN(9031,hit)
      enddo

      call HCDIR(olddir,' ')
      return
      end

      subroutine h_raw_occ_ntuple_shutdown(ABORT,err)
*     Flush both RWN and close the RZ file.
      implicit none
      logical ABORT
      character*(*) err
      include 'h_raw_occ_ntuple.cmn'
      integer cycle,io
      character*80 olddir

      ABORT=.false.
      err=' '
      if (.not.hraw_open) return
      call HCDIR(olddir,'R')
      call HCDIR(hraw_dir,' ')
      cycle=0
      call HROUT(hraw_id,cycle,' ')
      cycle=0
      call HROUT(9031,cycle,' ')
      call HREND(hraw_name)
      io=hraw_io
      call g_IO_control(io,'FREE',ABORT,err)
      close(hraw_io)
      call HCDIR(olddir,' ')
      hraw_open=.false.
      return
      end
