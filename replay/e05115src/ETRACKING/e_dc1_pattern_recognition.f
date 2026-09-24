      subroutine e_dc1_pattern_recognition(ABORT,err)
      
      IMPLICIT NONE
      SAVE
*     
      character*21 here
      parameter (here= 'e_dc1_pattern_recognition')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'hes_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hes_tracking.cmn'
      INCLUDE 'hes_geometry.cmn'
      INCLUDE 'hes_id_histid.cmn'
      INCLUDE 'gen_event_info.cmn'
      include 'gen_run_info.cmn'
      include 'hes_bypass_swiches.cmn' !Added by Gogami (3Feb2011)

*     
*     local variables
      Integer*4 hit_number(edc1max_chamber_hits)
      Integer*4 space_point_hits(edc1max_space_points,edc1max_hits_per_point+2)
*     hit numbers for each space point
*     (n,1) number of hits
*     (n,2) number of combanations attached to the point
*     (n,3),(n,4),... hits associated with space point
*     n : number of group
      
      Integer*4 wiren, cardn, isp, ihit,ihit2,hit,clus
      Integer*4 pln,pln1,pln2,wn,wn2,glp,glp2,slot
      Integer*4 pln3,pln4,hit3,hit4
      Integer*4 i,j,k,l,xx,xxprime,wire
      Integer*4 xlayer,xprimelayer,hit1,hit2
      Integer*4 ip,have_layer_hits,have_uvlayer_hits,hits,uvhits,have_multi
      Real*4 space_points(edc1max_space_points,2)
      Real*4 xdist,ydist,wc,wc2,x1,x2,x3,dx1,dx2
      Real*4 u1,v1,du1,dv1
      Real*4 time_corr
      Real*4 testx,testy
      Real*4 wcvec(emax_num_dc1_layers)
      Real*4 ddis 
      Integer*4 pr_hits(emax_num_dc1_layers),pl_skip(emax_num_dc1_layers)
      Integer*4 pr_hits_number
     &     (emax_num_dc1_layers,edc1_max_wires_per_layer*2)
      Integer*4 gl_hits(emax_num_dc1_layers),gl_skip(emax_num_dc1_layers)
      Integer*4 gl_number
     &     (emax_num_dc1_layers,edc1_max_wires_per_layer*2)
      Integer*4 new_enspace_points
      
      Integer*4 n_of_group
*     group_hits(n,1) : total number of hits.
*     group_hits(n,2) : number of layer hits.
*     group_hits(n,3,4,5) : hit number. 
      Integer*4 group_hits(emax_dc1_hits,5)
      Integer*4 oddeven,oddeven2,join_group
      Integer*4 multiply,multiply2,hn,dupflag,it,is,old
      Integer*4 temp_space_point_tot,addflag

*     internal variables
      integer*4 ntest_points,mark(edc1max_space_points)
      real*4 test_points(edc1max_space_points,2)
      real*4 determinate,min_sq
      integer*4 ncombo,min_combo
      integer*4 combos(edc1max_space_points,2)
      integer*4 pair(edc1max_space_points,2)
      real*4 sqdistance_test
      Integer*4 track_lunno
      Parameter(track_lunno=51)
      Integer*4 etrig
      Integer*4 dindex
      Real*4 e_drift_dist_calc
      external e_drift_dist_calc
*     
*     temporary initialization
      ABORT= .FALSE.
      err=' '

c     =======OUTPUT CHAMBER INFROMATION=====================
c     ============Added by Gogami (3Feb2011)================
      if(edc_hitinfo_flag.eq.1)then
         etrig=etrig+1      
*     if you belive initialized value is 1, then you
*     can use below code.
*     Create dat file in the case of trig==1
         if(etrig .eq. 1)then
            open(23,file='hes_cham.dat',status='replace')
*     Add to the dat file in the case of  trig > 1
         else
            open(23,file='hes_cham.dat',access='append')   
         endif
         
*     Write data out to the dat file 
         write(23,*)"start"
         write(23,*)gen_run_number
         write(23,*)etrig
         write(23,*)edc1_tot_hits
         write(23,*)edc1_hits_per_layer
         do dindex=1,edc1_tot_hits
            write(23,*)edc1_layer_num(dindex),edc1_wire_num(dindex),
     &           edc1_wire_center(dindex),edc1_drift_time(dindex)
         enddo
         close(23)
      endif
*************************************************************
*     --- Histogram for dc1 dec bank 
*************************************************************
      call e_fill_dc1_dec_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif
      
*************************************************************
*     --- initial check
*************************************************************
c      write(*,*) "edc1_tot_hits=",edc1_tot_hits
      If(edc1_tot_hits .le. 0) Return

*************************************************************
*     --- reset local variables
*************************************************************
      ihit = 0
      edc1nspace_points_tot = 0
      edc1nspace_points = 0        
      edc1ncham_hits = 0
      new_enspace_points = 0
      Do i=1,int(emax_num_dc1_layers/2)
         gl_hits(i) = 0
      EndDo
      Do i=1,edc1max_space_points
         Do j=1,edc1max_hits_per_point+2
            edc1space_point_hits(i,j) = 0
            space_point_hits(i,j) = 0
         EndDo
      EndDo
      n_of_group=0
      Do i=1,emax_dc1_hits
         Do j=1,5
            group_hits(i,j)=0
         EndDo
      EndDo
c     --- pr_hits : layer multi hit
      Do i = 1,emax_num_dc1_layers
         pr_hits(i) = 0
         pl_skip(i) = 0
      EndDo
      if(edc1max_pr_hits .eq. 0) edc1max_pr_hits = 60
      
*************************************************************
*     --- record edc1_tot_hits no. into pr_hits_number
*     --- pr_hits_number is not used in other program
*     --- in case pr_hits .gt. # of wires * 0.5, 
*     --- don't use these planes for tracking. This is not realistic.
*************************************************************
      Do i=1,edc1_tot_hits
         pln = edc1_layer_num(i)
         if(pl_skip(pln) .eq. 0) then
            if(pr_hits(pln) .lt. edc1max_pr_hits) then
               pr_hits(pln) = pr_hits(pln) + 1
               pr_hits_number(pln,pr_hits(pln)) = i
            Else
               Write(track_lunno,*) 
     &              '(e_pattern...) pr_hits exceed ',
     &              'edc1max_pr_hits=',edc1max_pr_hits,' ev=',
     &              gen_event_id_number,' layer=',pln
               pl_skip(pln) = 1
               pr_hits(pln) = 0
            EndIf
         EndIf
      EndDo
      
*************************************************************
*     --- check pr hits in all planes
*************************************************************
      have_layer_hits = 0
      Do i = 1,emax_num_dc1_layers
         if(pr_hits(i) .gt. 0) then
            have_layer_hits = have_layer_hits + 1
         EndIf
      EndDo     

*************************************************************
*     --- check pr hits in U/V planes
*************************************************************
      have_uvlayer_hits = 0
      Do i = 3, 4
         if(pr_hits(i) .gt. 0) then
            have_uvlayer_hits = have_uvlayer_hits + 1
         EndIf
      EndDo     
      Do i = 7, 8
         if(pr_hits(i) .gt. 0) then
            have_uvlayer_hits = have_uvlayer_hits + 1
         EndIf
      EndDo     
      
*************************************************************
*     --- check minimum layer hits required for tracking
*************************************************************
      if(have_layer_hits.ge.edc1min_hit.and.have_uvlayer_hits.ge.3) then

c     --- check multiplicity for all 'layer'(1-10) combination.
c     --- if you have 2 hits per layer, 
c     --- 2^10=1024 is generated. This is not good.
c     --- so, make group for all 'group'(1-5) combination later.
         
         multiply = 1
         Do i=1,10
            if(pr_hits(i) .gt. 0) then
               multiply = multiply * pr_hits(i)
            EndIF
         EndDo
         
*************************************************************
**     --- sort hit groups
*************************************************************
c         write(*,*) "----------------------------------------"
c         write(*,*) "event",gen_event_id_number
c         Do i=1,edc1_tot_hits
c            pln = edc1_layer_num(i)
c            wn = edc1_wire_num(i)
c            write(*,*) "pln,wn",pln,wn
c         enddo
         n_of_group = 0
         Do i=1,edc1_tot_hits
            pln = edc1_layer_num(i) ! 1 to edc1_num_layers
            if(pl_skip(pln) .eq. 0) then 
               oddeven = pln - int(pln/2)*2 ! 1 or 0 
               wn = edc1_wire_num(i)
               wc = edc1_wire_center(i)
               clus = edc1_cluster_size(i)
               if(n_of_group .eq. 0) then ! first, make new group
                  n_of_group = n_of_group + 1
                  group_hits(n_of_group,1) = 1 
                  group_hits(n_of_group,2) = 1
                  group_hits(n_of_group,3) = i
c                 group_hits(n,1) : total number of hits.
c                 group_hits(n,2) : number of layer hits.
c                 group_hits(n,3,4,5) : hit number. 
               Else             ! > 0
                  if(oddeven .eq. 1) then ! make new group for odd 
                     n_of_group = n_of_group + 1
                     group_hits(n_of_group,1) = 1 
                     group_hits(n_of_group,2) = 1
                     group_hits(n_of_group,3) = i
                  Else          ! join to the existing group for even
                     join_group = 0
                     Do j=1,n_of_group
                        ihit = group_hits(j,3) ! the first hit
                        pln2 = edc1_layer_num(ihit)
                        oddeven2 = pln2 - int(pln2/2)*2 ! 1 or 0 
                        wn2 = edc1_wire_num(ihit)
                        wc2 = edc1_wire_center(ihit)
                        if(oddeven2 .eq. 1 .and.
     &                       pln .eq. pln2 + 1) then 
*     a pair layer, odd and even
*     ex: a pair is 1-55&2-54&55. we don't accept hits more than 30 degree. 
*     note that only layer7&8 has a different pair number, ex 7-55 and 8-55&56. 
c                           if(
c     &                          (pln2 .eq. 7 .and. 
c     &                          wn .ge. wn2 .and. wn .le. wn2+1) 
c     &                          .OR.
c     &                          (pln2 .ne. 7 .and. 
c     &                          wn .ge. wn2-1 .and. wn .le. wn2) 
c     &                          ) then
                           if(abs(wc-wc2).lt.0.5) then
                              join_group = 1
                              if(group_hits(j,1) .eq. 2) then ! fill 2.
                                 n_of_group = n_of_group + 1
                                 group_hits(n_of_group,1) = 2
                                 group_hits(n_of_group,2) = 1
                                 group_hits(n_of_group,3) = group_hits(j,3)
                                 group_hits(n_of_group,4) = i
c                                 write(*,*) "j2",
c     >                                edc1_layer_num(group_hits(j,3)),
c     >                                edc1_wire_num(group_hits(j,3)),
c     >                                edc1_layer_num(i),
c     >                                edc1_wire_num(i)
                              Else ! new add
c                                 write(*,*) "j1",
c     >                                edc1_layer_num(group_hits(j,3)),
c     >                                edc1_wire_num(group_hits(j,3)),
c     >                                edc1_layer_num(i),
c     >                                edc1_wire_num(i)
                                 group_hits(j,1) = 2
                                 group_hits(j,4) = i
                              EndIf
                           EndIf ! wire number check   
                        EndIF   ! adjacent layer check
                     EndDo      ! j : group loop
                     if(join_group .eq. 0) then
                        n_of_group = n_of_group + 1
                        group_hits(n_of_group,1) = 1
                        group_hits(n_of_group,2) = 1
                        group_hits(n_of_group,3) = i
                     EndIF
                  EndIF         ! odd or even
               EndIf            ! group > 0 or not
            EndIf               ! pl_skip = 0 or not
         EndDo                  ! i : edc1_tot_hits loop
c         write(*,*) "n_of_group",n_of_group

*************************************************************
*     --- combine all groups.
*     --- in case plane hits exceed # of wires,
*     --- we don't use these planes.
*************************************************************
         Do i = 1,emax_num_dc1_layers
            gl_hits(i) = 0
            gl_skip(i) = 0
         EndDo
         Do i=1,n_of_group
            ihit=group_hits(i,3)
            pln=edc1_group_num(ihit)
            if(gl_skip(pln) .eq. 0) then
               if(gl_hits(pln) .lt. edc1max_pr_hits) then
                  gl_hits(pln) = gl_hits(pln) + 1
                  gl_number(pln,gl_hits(pln)) = i
               Else
                  Write(track_lunno,*) 
     &                 '(e_pattern...) ',
     &                 'group hits exceed edc1max_pr_hits=',edc1max_pr_hits,
     &                 ' ev=',
     &                 gen_event_ID_number,' group=',pln
                  gl_skip(pln) = 1
                  gl_hits(pln) = 0 ! reset
               EndIf
            EndIf               ! gl_skip=0 or not
         EndDo
         
*************************************************************
*     --- let's compare multiply with multiply2. 
*     sort from 10 layers v.s. sort from 5 groups
*************************************************************
         multiply2 = 1
         Do i=1,5
            if(gl_hits(i) .gt. 0) then
               multiply2 = multiply2 * gl_hits(i)
            EndIF
         EndDo

         if((gl_hits(1)*gl_hits(2)*gl_hits(3)*gl_hits(4)*gl_hits(5))
     &        .gt.edc1max_space_points) then
            edc1nspace_points_tot=0
c            write(*,*) "RETURN",
c     >           gen_event_id_number,
c     >           gl_hits(1)*gl_hits(2)*gl_hits(3)*gl_hits(4)*gl_hits(5)

            Return
         endif

         If(multiply2 .gt. 0) then
            new_enspace_points = 0
            edc1nspace_points = 1
c            write(*,*)"group loop start"
            Do i = 1,5          ! group loop
               if(gl_hits(i) .gt. 0) then
                  new_enspace_points = gl_hits(i) * edc1nspace_points
c                  write(*,*)
c     &             i,gl_hits(i),new_enspace_points,edc1nspace_points
                  if(new_enspace_points .gt. edc1max_space_points) then
                     write(*,*) "enspace points exceed! event=",
     &                    gen_event_id_number," new_enspace_points=",
     &                    new_enspace_points, " group=",
     &                    i
                     Do j=1,5
                        write(*,*) 'pln=',j,' gl_hits=',gl_hits(j)
                     enddo
                     gl_hits(i) = 1
                     new_enspace_points = edc1nspace_points
                  Endif
                  if(gl_hits(i) .gt. 1 .and. 
     &                 space_point_hits(edc1nspace_points,1) .gt. 0) then
                     Do j=2,gl_hits(i)
                        Do k=1,edc1nspace_points
                           space_point_hits(edc1nspace_points*(j-1)+k,1) =
     &                          space_point_hits(k,1)
                           Do l=3,space_point_hits(k,1)+2
                              space_point_hits(edc1nspace_points*(j-1)+k,l) 
     &                             = space_point_hits(k,l)
                           EndDo
                        EndDo
                     EndDo
                  EndIF
                  Do k=1,gl_hits(i)
                     glp = gl_number(i,k)
                     Do j = (k-1)*edc1nspace_points+1,edc1nspace_points*k
                        Do l=1,group_hits(glp,1)
                           space_point_hits(j,1) = 
     &                          space_point_hits(j,1) + 1
                           hits = space_point_hits(j,1) + 2
                           space_point_hits(j,hits) = 
     &                          group_hits(glp,2+l)
                        EndDo
                     EndDo
                  EndDo
                  edc1nspace_points = new_enspace_points
               EndIF            ! end of layer_hits > 0
            EndDo               ! plane loop               
         EndIF                  ! multiply > 10         
      EndIf                     ! layer hits more than emin_hits

c      write(*,*) "new_enspace_points=",new_enspace_points
c      Do isp=1,new_enspace_points
c         write(*,*) "edc1_tot_hits=",edc1_tot_hits
c         write(*,*) "space point=", isp
c         write(*,*) "space_point_hits=",space_point_hits(isp,1)
c         do ihit=1,space_point_hits(isp,1)
c            hit = space_point_hits(isp,ihit+2)
c            pln = edc1_layer_num(hit)
c            wiren = edc1_wire_num(hit)
c            write(*,*) "  pln=",pln," wire=",wiren
c         Enddo
c      Enddo

*************************************************************
*     --- next, check the difference of x-x'1,2,3
*************************************************************
      x1=-1000
      x2=-1000
      x3=-1000
      u1=-1000
      v1=-1000
      dx1=-1000
      dx2=-1000

      if(new_enspace_points.gt.0) then
         do isp=1,new_enspace_points
            uvhits = 0
            mark(isp) = 1
            do ihit=1,space_point_hits(isp,1)
               hit = space_point_hits(isp,ihit+2)
               pln = edc1_layer_num(hit)
* Count hits in U/V planes
               if(pln.eq.3.or.pln.eq.4.or.pln.eq.7.or.pln.eq.8) then
                  uvhits = uvhits + 1
               endif
               wc = edc1_wire_center(hit)
               if(pln .eq. 1 .or. pln .eq. 2) then
                  x1 = wc
               Else if(pln .eq. 3 .or. pln .eq. 4) then
                  u1 = wc
               Else if(pln .eq. 5 .or. pln .eq. 6) then
                  x2 = wc
               Else if(pln .eq. 7 .or. pln .eq. 8) then
                  v1 = wc
               Else if(pln .eq. 9 .or. pln .eq. 10) then
                  x3 = wc
               EndIf
            enddo               ! edc1space_point_hits(isp,1) loop
c            if(x1*x2*x3.gt. 0) then
            if(x1.ne.-1000.and.x2.ne.-1000.and.x3.ne.-1000) then
               dx1 = x2-x1
               dx2 = x3-x2
               du1 = (x1+x2)/2-u1
               dv1 = (x2+x3)/2-v1
               Call HF1(eiddc1edistancetest,dx1-dx2,1.)
c               Call HF1(eideangletest,atan((x1-x3)/30.75),1.)
               if(u1.ne.-1000.and.v1.ne.-1000
     >              .and.abs(dx1-dx2) .lt. 5.) then
c                  Call HF1(eidedistanceu1test,du1,1.)
c                  Call HF1(eidedistancev1test,dv1,1.)
                  if(abs(du1).gt.10.or.abs(dv1).gt.10) then
                     mark(isp) = 0
                  endif
               endif
c               if(abs(dx1-dx2) .gt. 20.) then
               if((abs(dx1-dx2) .gt. 5.)
     >              .or.(abs(atan((x1-x3)/30.75)).gt.0.6)) then
                  mark(isp) = 0
               EndIf
            EndIf               ! we have all x plane hits or not
            if(space_point_hits(isp,1).lt.edc1min_hit.or.uvhits.le.2) then
               mark(isp) = 0
            endif
         EndDo                  ! enspace_point_tot loop
*     --- fill new common block.
         edc1nspace_points_tot = 0
         Do i=1,new_enspace_points
            if(mark(i) .gt. 0) then
               edc1nspace_points_tot = edc1nspace_points_tot + 1
               isp = edc1nspace_points_tot
               edc1space_point_hits(isp,1) = space_point_hits(i,1)
               edc1space_point_hits(isp,2) = space_point_hits(i,2)
               Do j=1,edc1space_point_hits(isp,1)
                  edc1space_point_hits(isp,2+j) = space_point_hits(i,2+j)
               EndDo
            EndIf
         EndDo
      EndIF                     ! new_enspace_points>0 or not

c      write(*,*) "event=",gen_event_id_number,
c     >     " edc1nspace_points_tot=",edc1nspace_points_tot
c      Do isp=1,edc1nspace_points_tot
c         write(*,*) " space point=", isp
c            do ihit=1,edc1space_point_hits(isp,1)
c               hit = edc1space_point_hits(isp,ihit+2)
c               pln = edc1_layer_num(hit)
c               wiren = edc1_wire_num(hit)
c               write(*,*) "  pln=",pln," wire=",wiren
c            Enddo
c      Enddo
      
*************************************************************
*     --- fill drift distance and correct drift time by cards
*************************************************************
ccc drift distance should not be calculated here ! DK 2010/12/02
c      if(edc1nspace_points_tot.gt.0) then
c         do isp=1,edc1nspace_points_tot
c            do ihit=1,edc1space_point_hits(isp,1)
c               hit = edc1space_point_hits(isp,ihit+2)
c               pln = edc1_layer_num(hit)
c               wiren = edc1_wire_num(hit)
c               slot = edc1_slot_num(hit)
c               edc1_drift_dis(hit) = e_drift_dist_calc
c     &              (pln,slot,edc1_drift_time(hit))
c            enddo               ! edc1space_point_hits(isp,1) loop
c         EndDo                  ! enspace_point_tot loop
c      endif                     ! enspace_point_tot > 0
      
      return
      end





