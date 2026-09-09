      subroutine select_space_points_seva(nspace_point_len,
     & nspace_points,space_points,space_point_hits,min_hits,min_combos,
     & easy_space_point)
*     This routine goes through the list of space_points and space_point_hits
*     found by find_space_points and only accepts those with 
*     number of hits > min_hits
*     number of combinations > min_combos
*     dfg              30 august 1993
* $Log: select_space_points_seva.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:48  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
* new dir
*
* Revision 1.3  1996/01/17 19:20:48  cdaq
* (JRA) Add eash_space_point argument
*
* Revision 1.2  1994/02/23 13:52:40  cdaq
* (SAW) Change 2nd arg of space_points_hits declaration from 1 to *
*
* Revision 1.1  1994/02/21  16:44:23  cdaq
* Initial revision
*
      implicit none
       INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hks_tracking.cmn'
      include 'hks_geometry.cmn'
      INCLUDE 'gen_event_info.cmn'
         INCLUDE 'gen_run_info.cmn' 
*     inputs
      integer*4 nspace_point_len        ! dimension variable for two-d arrays
      integer*4 nspace_points,plane1,plane2          ! number of input points     
					! on return it is the number of valid
                                        ! space points
      integer*4 space_points(nspace_point_len,2),finalnum,goodhit(hmax_dc_hits)
      integer*4 space_point_hits(nspace_point_len,*)
      integer*4 min_hits,startnum,broji1_ss,broji2_ss,nhdc             ! minimum number of hits in valid point
      integer*4 min_combos,hits_plane(12,44),nhit_plane(12)              ! minimum number of combos
      integer*4 rem_plane,init_space_point_count,flag2(hmax_space_points),flag
      integer*4 layer1,layer2,hit1,hit2,drifttime1,drifttime2,wire,k,j,point,i
      logical easy_space_point          ! flag for having found easy space pt.
*
*     outputs
*     note nspace_points, space_points, and space_point_hits are all 
*     modified by the action of this routine
*     local variables
      integer*4 space_point_count,ploop,hloop 
      integer*4 min_hits_seva, count_hits_seva,flag1(122)
      integer*4 count_mini_space_point(3)
      integer*4 mini_space_point_hits(3,hmax_space_points,2)
      integer*4 iii,kkk,flag_used,jjj
*
      integer*4 hits(hmax_hits_per_point),new_count
      integer*4 temp_space_points(hmax_space_points,333)
      integer*4 temp_space_point_hits(hmax_space_points,hmax_hits_per_point+2)
      integer*4 glupost,gluppost1
c      common/aaaaa/broji1_ss,broji2_ss
c      common/glup/glupost,gluppost1
      
      space_point_count=0
      init_space_point_count=0
c      write(*,*) 'POCETAK',nspace_points 
c       do ploop=1,nspace_points
c          startnum = space_point_hits(ploop,1)
c          write(*,*) 'MA STA JE TO A', (space_point_hits(ploop,j),j=1,startnum+2)
c          write(*,*) 'B', (HDC_LAYER_NUM(space_point_hits(ploop,j)),j=3,startnum+2)
c          write(*,*) 'C', (hdc_wire_num(space_point_hits(ploop,j)),j=3,startnum+2)
c          enddo

      do ploop=1,nspace_points    ! BEGIN SPACE LOOP
         startnum = space_point_hits(ploop,1)
c                   write(*,*)'HITS  aaa', (space_point_hits(ploop,j),j=3,startnum+2)

* if easy_space_point, then the number of combos is not filled.
c        if(space_point_hits(ploop,2).ge.min_combos.or.easy_space_point.and.space_point_hits(ploop,1).ge.min_hits) then
         if(space_point_hits(ploop,2).ge.0.or.
     &      easy_space_point.and.space_point_hits(ploop,1).ge.min_hits) then
            do j=1,12
               nhit_plane(j)=0
            enddo
            do j=1,122
               flag1(j)=0
            enddo
c            startnum = space_point_hits(ploop,1)!DK
            finalnum=0
            do j=3,startnum+2
               goodhit(j) = 1
            enddo
        
cccccccccccccccccccccc ORIG CODE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c           write(*,*) 'INITIAL POINTS',space_point_count,hmax_space_points,(space_point_hits(ploop,j),j=3,startnum+2)
c           write(*,*) 'layer',(hdc_layer_num(space_point_hits(ploop,j)),j=3,startnum+2)
c           write(*,*) 'wire',(hdc_wire_num(space_point_hits(ploop,j)),j=3,startnum+2)
            do j=3,startnum+2
               hit1 = space_point_hits(ploop,j)
               layer1 = hdc_layer_num(hit1)
               nhit_plane(layer1)=nhit_plane(layer1)+1
               hits_plane(layer1,nhit_plane(layer1))=hit1
            enddo  
c           write(*,*)'HITS', (space_point_hits(ploop,j),j=3,startnum+2)
            if (layer1.gt.6) then
               nhdc=2
            else
               nhdc=1
            endif
            do j=3,startnum+1
               hit1 = space_point_hits(ploop,j)
               layer1 = hdc_layer_num(hit1)
               drifttime1 = hdc_drift_time(hit1)
               do k=j+1,startnum+2
                  hit2 = space_point_hits(ploop,k)
                  layer2 = hdc_layer_num(hit2)
                  drifttime2 = hdc_drift_time(hit2)
                  if(layer1 .eq. layer2 ) then
                     if(drifttime1.gt.drifttime2) then
                        goodhit(j) = 0
                     else                      !if equal times, choose 1st hit(arbitrary)
                        goodhit(k) = 0
                     endif
                  endif                       ! end test on equal layers
               enddo                         ! end loop on k
            enddo    
            finalnum=0
            do j=3,startnum+2
               hit1 = space_point_hits(ploop,j)
               layer1 = hdc_layer_num(hit1)
c              write(*,*) 'hit cond',goodhit(j),hit1,layer1,hdc_wire_num(hit1)
               if(goodhit(j).gt.0) then
                  finalnum = finalnum + 1
                  hits(finalnum)=space_point_hits(ploop,j)
               endif                         ! end check on good hit
            enddo
c           write(*,*) 'PA KOLIKO',ploop,startnum, finalnum
            if(finalnum.ge.min_hits) then ! IF ON NUMBER OF MIN_HITS 
CCCCCCCCCCCCCCCCCCCCCCCC ORIG POINT SELECTION
c              if(finalnum.ge.5) then
c             glupost=glupost+1
c             gluppost1=gluppost1+startnum

c             endif
c             write(*,*) 'GLUP',glupost, gluppost1
*     copy good hits to space_point_hits
               space_point_count=space_point_count+1
               temp_space_points(space_point_count,1)
     &            = space_points(ploop,1)
               temp_space_points(space_point_count,2)
     &            = space_points(ploop,2)
               temp_space_point_hits(space_point_count,1) 
     &            = finalnum
               temp_space_point_hits(space_point_count,2) 
     &            = space_point_hits(ploop,2)

               do j = 1, finalnum
                  temp_space_point_hits(space_point_count,j+2)=hits(j)
               enddo                           ! end of copy 
c              write(*,*) 'add hits',space_point_count,(temp_space_point_hits(space_point_count,j),j = 1, finalnum+2)
c              write(*,*) 'add layer hits',(hdc_layer_num(temp_space_point_hits(space_point_count,j+2)),j = 1, finalnum)
               temp_space_point_hits(space_point_count,hmax_hits_per_point+1)=666666
               temp_space_point_hits(space_point_count,hmax_hits_per_point+2)=555
CCCCCCCCCCCCCCCCCCCCCCCCC END ORIG POINT  SELECTION

c              write(*,*) 'SELECTION',ploop,finalnum,startnum
               init_space_point_count=init_space_point_count+1
               if ((hmax_space_points-space_point_count).gt.
     &                 nspace_points) then ! CONDITION ON MAX NUMBER OF SPACE POINTS
                  if (startnum.eq.finalnum) then
c                 if (finalnum.lt.0) then
                     space_point_count=space_point_count+1
                     temp_space_points(space_point_count,1)
     &                  = space_points(ploop,1)
                     temp_space_points(space_point_count,2)
     &                  = space_points(ploop,2)
                     temp_space_point_hits(space_point_count,1)
     &                   = finalnum
                     temp_space_point_hits(space_point_count,hmax_hits_per_point+1)
     &                   = init_space_point_count
                     temp_space_point_hits(space_point_count,hmax_hits_per_point+2)=555
c                    write(*,*) 'PISI ONE BEZ PROMJENE'
                     do hloop=2,finalnum+2
                        temp_space_point_hits(space_point_count,hloop)=
     &                  space_point_hits(ploop,hloop)
c                       write(*,*) 'bez promjene',temp_space_point_hits(space_point_count,1),space_point_hits(space_point_count,hloop),temp_space_point_hits(space_point_count,hmax_hits_per_point+2)
                     enddo
                  else
c                 write(*,*) 'IPAK IDEM TU'
c                 elseif (finalnum.lt.0) then
c                 elseif (startnum.gt.finalnum) then
c                    init_space_point_count=init_space_point_count+1
                     if (nhdc.eq.1) then
                        rem_plane=0
                     else
                        rem_plane=6
                     endif
                     do j=1,3      ! create mini space point
                        plane1=rem_plane+2*j-1
                        plane2=rem_plane+2*j
                        count_mini_space_point(j)=0
                        if (nhit_plane(plane1).gt.0.and.
     &                          nhit_plane(plane2).gt.0) then
c                          count_mini_space_point(j)=count_mini_space_point(j)+1
                           do kkk=1,nhit_plane(plane1)
                              hit1=hits_plane(plane1,kkk)
                              flag_used=0
                              do iii=1,nhit_plane(plane2)
                                 hit2=hits_plane(plane2,iii)
c                             write(*,*) 'WIRE1',hdc_wire_num(hit1),hdc_wire_num(hit2),hdc_layer_num(hit1),hdc_layer_num(hit2),
c     &plane1,plane2
                                 if (abs(hdc_wire_num(hit1)-hdc_wire_num(hit2)).lt.2) then
c                                   write(*,*) 'WIRE',hdc_wire_num(hit1),hdc_wire_num(hit2) 
                                    count_mini_space_point(j)
     &                                 = count_mini_space_point(j)+1
                                    mini_space_point_hits(j,count_mini_space_point(j),1)
     &                                 = hits_plane(plane1,kkk)
                                    mini_space_point_hits(j,count_mini_space_point(j),2)
     &                                 = hits_plane(plane2,iii)
                                    flag_used=1
                                    flag1(iii)=1
                                 endif
                              enddo

c                             if (flag_used.eq.0) then
                              if (flag_used.gt.-100) then
                                 count_mini_space_point(j)
     &                              = count_mini_space_point(j)+1
                                 mini_space_point_hits(j,count_mini_space_point(j),1)
     &                              = hits_plane(plane1,kkk)
                                 mini_space_point_hits(j,count_mini_space_point(j),2)=0
                              endif
                              do iii=1,nhit_plane(plane2)
c                                if (flag1(iii).eq.0) then
                                 if (flag1(iii).gt.-100) then
                                    count_mini_space_point(j)
     &                                 = count_mini_space_point(j)+1
                                    mini_space_point_hits(j,count_mini_space_point(j),2)
     &                                 = hits_plane(plane2,iii)
                                    mini_space_point_hits(j,count_mini_space_point(j),1)=0
                                 endif
                              enddo
                           enddo
                        else
                           if (nhit_plane(plane1).gt.0) then 
                              do k=1,nhit_plane(plane1)
                                 count_mini_space_point(j)
     &                              = count_mini_space_point(j)+1
                                 mini_space_point_hits(j,count_mini_space_point(j),1)
     &                              = hits_plane(plane1,k)
                                 mini_space_point_hits(j,count_mini_space_point(j),2)=0
                              enddo
                           endif
                           if (nhit_plane(plane2).gt.0) then
                              do k=1,nhit_plane(plane2)
                                 count_mini_space_point(j)
     &                              = count_mini_space_point(j)+1 
                                 mini_space_point_hits(j,count_mini_space_point(j),2)
     &                              = hits_plane(plane2,k)
                                 mini_space_point_hits(j,count_mini_space_point(j),1)=0
                              enddo
                           endif
                        endif
                     enddo          ! END create mini space point

                     do iii=1,count_mini_space_point(1)
                        do jjj=1,count_mini_space_point(2)
                           do kkk=1,count_mini_space_point(3)
                              min_hits_seva=0
                              if (mini_space_point_hits(1,iii,1).gt.0)
     &                           min_hits_seva=min_hits_seva+1
                              if (mini_space_point_hits(1,iii,2).gt.0)
     &                           min_hits_seva=min_hits_seva+1
                              if (mini_space_point_hits(2,jjj,1).gt.0)
     &                           min_hits_seva=min_hits_seva+1
                              if (mini_space_point_hits(2,jjj,2).gt.0)
     &                           min_hits_seva=min_hits_seva+1
                              if (mini_space_point_hits(3,kkk,1).gt.0)
     &                           min_hits_seva=min_hits_seva+1
                              if (mini_space_point_hits(3,kkk,2).gt.0)
     &                           min_hits_seva=min_hits_seva+1
                              if (min_hits_seva.ge.min_hits.and.
     &                           (hmax_space_points-space_point_count).gt.nspace_points) then
                                 space_point_count=space_point_count+1
                                 temp_space_points(space_point_count,1)
     &                              = space_points(ploop,1)
                                 temp_space_points(space_point_count,2)
     &                              = space_points(ploop,2)
                                 temp_space_point_hits(space_point_count,hmax_hits_per_point+1)
     &                              = init_space_point_count
                                 temp_space_point_hits(space_point_count,hmax_hits_per_point+2)
     &                              = space_point_count
                                 count_hits_seva=0
                                 if (mini_space_point_hits(1,iii,1).gt.0) then
                                    count_hits_seva=count_hits_seva+1
                                    temp_space_point_hits(space_point_count,count_hits_seva+2)
     &                                 = mini_space_point_hits(1,iii,1)
                                 endif
                                 if (mini_space_point_hits(1,iii,2).gt.0) then
                                    count_hits_seva=count_hits_seva+1
                                    temp_space_point_hits(space_point_count,count_hits_seva+2)
     &                                 = mini_space_point_hits(1,iii,2)
                                 endif
                                 if (mini_space_point_hits(2,jjj,1).gt.0) then
                                    count_hits_seva=count_hits_seva+1
                                    temp_space_point_hits(space_point_count,count_hits_seva+2)
     &                                 = mini_space_point_hits(2,jjj,1)
                                 endif
                                 if (mini_space_point_hits(2,jjj,2).gt.0) then
                                    count_hits_seva=count_hits_seva+1
                                    temp_space_point_hits(space_point_count,count_hits_seva+2)
     &                                 = mini_space_point_hits(2,jjj,2)
                                 endif
                                 if (mini_space_point_hits(3,kkk,1).gt.0) then
                                    count_hits_seva=count_hits_seva+1
                                    temp_space_point_hits(space_point_count,count_hits_seva+2)
     &                                 = mini_space_point_hits(3,kkk,1)
                                 endif
                                 if (mini_space_point_hits(3,kkk,2).gt.0) then
                                    count_hits_seva=count_hits_seva+1
                                    temp_space_point_hits(space_point_count,count_hits_seva+2)
     &                                 = mini_space_point_hits(3,kkk,2)
                                 endif
                                 temp_space_point_hits(space_point_count,1)
     &                              =count_hits_seva
                                 temp_space_point_hits(space_point_count,2)
     &                              =space_point_hits(ploop,2)
c                                if (gen_event_ID_number.eq.68202) then
c                                   if (space_point_count.lt.22153) then
c           write(*,*) 'STAGE', gen_event_ID_number,space_point_count,
c     &(temp_space_point_hits(space_point_count,k),k=1,temp_space_point_hits(space_point_count,1)+2)
c                                   endif
c                                endif
C                                write(*,*) 'NOVA TOCKA',space_point_count
C                                do k=3,count_hits_seva+2         
C                                   write(*,*) 'NOVI',space_point_hits(space_point_count,k),
C     &                             hdc_wire_num(space_point_hits(space_point_count,k)),count_hits_seva
c                                enddo
c                                do hloop=1,space_point_hits(ploop,1)+2
c                                   space_point_hits(space_point_count,hloop)
c     &                                = space_point_hits(ploop,hloop)
c                                   write(*,*) 'bez promjene', space_point_hits(space_point_count,hloop)
c                                enddo
                              endif
                           enddo
                        enddo
                     enddo
c                    if (startnum.gt.finalnum) then
c                       broji1_ss=broji1_ss+1
c                    else
c                       broji2_ss=broji2_ss+1
c                    endif
c                    do k=1,12
c                       do j=3,startnum+2
c                          hit1 = space_point_hits(ploop,j)
c                          layer1 = hdc_layer_num(hit1)
c                          if (k.eq.layer1) then
c                             if (startnum.gt.finalnum) then
c                                wire  = hdc_raw_wire_num(hit1)
c                                write(*,*) layer1,wire,goodhit(j),hdc_drift_time(hit1)
c                             else
c                                write(*,*) 'sve OK'
c                             endif
c                          endif
c                       enddo
c                    enddo
                  endif
               ENDIF ! ENDIF CONDITION ON MAX NUMBER OF SPACE POINTS
            endif ! ENDIF ON NUMBER OF MIN_HITS
         endif ! ENDIF ON MIN COMBOS
      enddo                ! END SPACE LOOP

c       if (gen_event_ID_number.eq.68202) then
c          do i=1,nspace_points
c          write(*,*)'HITS', (space_point_hits(i,j),j=1,space_point_hits(i,1)+2)
c          enddo
c      do i=1,space_point_count
c           write(*,*) 'TOCKe',i,(temp_space_point_hits(i,k), k=1,temp_space_point_hits(i,1)+2)
c         enddo
c         endif

      nspace_points=space_point_count
      if (nspace_points.gt.0) then
c        write(*,*) 'BROJ TOCAKA',nspace_points,init_space_point_count,ploop,temp_space_point_hits(1,1),
c     &     temp_space_point_hits(1,2),temp_space_point_hits(1,3)
         do k=1,temp_space_point_hits(1,1)+2
            space_point_hits(1,k)=temp_space_point_hits(1,k)
         enddo
         space_point_hits(1,hmax_hits_per_point+1)
     &      = temp_space_point_hits(1,hmax_hits_per_point+1)
         space_point_hits(1,hmax_hits_per_point+2)
     &      = temp_space_point_hits(1,hmax_hits_per_point+2)
         space_points(1,1)=temp_space_points(1,1)
         space_points(1,2)=temp_space_points(1,2)
c        if (gen_event_ID_number.eq.68202) then
c           write(*,*) 'PRVA TOCKA',(space_point_hits(1,k), k=1,space_point_hits(1,1)+2)
c           write(*,*) 'DRUGA TOCKA',(temp_space_point_hits(2,k), k=1,temp_space_point_hits(2,1)+2)
c           write(*,*) 'TRECA TOCKA',(temp_space_point_hits(3,k), k=1,temp_space_point_hits(3,1)+2)

c        endif
         if (nspace_points.gt.1) then
            new_count=1
            do i=2,nspace_points
               flag=0
c              write(*,*) (space_point_hits(i,k),k=1,space_point_hits(i,1)+2), space_point_hits(i,hmax_hits_per_point+1)
c              write(*,*) 'Usporedba',i,j
               do j=1,new_count
c                 if (gen_event_ID_number.eq.68202) then
c                    if (i.lt.10.and.j.lt.11) then
c                       write(*,*) 'Usporedba',gen_event_ID_number,i,j
c                       write(*,*) 'CCCC',space_point_hits(i,1),(space_point_hits(i,k),k=3,space_point_hits(i,1)+2),
c     &                    space_point_hits(i,hmax_hits_per_point+1)
c                       write(*,*) 'DDDD',space_point_hits(j,1),(space_point_hits(j,k),k=3,space_point_hits(j,1)+2), 
c     &                    space_point_hits(j,hmax_hits_per_point+1)
c                    endif
c                 endif
                  flag2(j)=0
                  if ((temp_space_point_hits(i,1).eq.
     &                  temp_space_point_hits(j,1)).and.
     &                ((temp_space_point_hits(i,hmax_hits_per_point+1).eq.
     &                  temp_space_point_hits(j,hmax_hits_per_point+1).and.
     &                  temp_space_point_hits(i,hmax_hits_per_point+1).gt.66666).or.
     &                  (temp_space_point_hits(i,hmax_hits_per_point+1).lt.66666.and.
     &                  temp_space_point_hits(j,hmax_hits_per_point+1).lt.66666))
     &               ) then
                     do kkk=1,temp_space_point_hits(j,1)+3
                        flag1(kkk)=0
                     enddo
                     do k=3,temp_space_point_hits(i,1)+2
                        do kkk=3,temp_space_point_hits(i,1)+2
                           if (temp_space_point_hits(i,k).eq.
     &                        temp_space_point_hits(j,kkk)) then
                              flag1(k)=1
c                             write(*,*) 'flag1:', flag1(k),space_point_hits(i,k),space_point_hits(j,kkk)
                           endif
                        enddo
                     enddo
                     do kkk=3,temp_space_point_hits(j,1)+2
                        if ( flag1(kkk).eq.0) flag2(j)=1
c                       write(*,*) 'FLAG 2:',j,flag2(j),flag1(kkk)
                     enddo
                  else
                     flag2(j)=1
c                    write(*,*) 'FLAG 3:',j,flag2(j)
                  endif
               enddo
               flag=1
               do k=1,new_count
                  if (flag2(k).eq.0) flag=0
               enddo
c              write(*,*) 'FLAG',flag
               if (flag.eq.1) then
c                 write(*,*) 'ovu sam dodao',new_count,i,(space_point_hits(i,k),k=1,space_point_hits(i,1)+2), 
c     &              space_point_hits(i,hmax_hits_per_point+1)
                  new_count=new_count+1
c                 if (new_count.lt.13.and.gen_event_ID_number.eq.68202) then
c                       write(*,*) 'hit PRIIE',(temp_space_point_hits(i,k),k=3,temp_space_point_hits(i,1)+2)
c                       write(*,*) 'layer PRIIE',(hdc_layer_num(temp_space_point_hits(i,k)),k=3,temp_space_point_hits(i,1)+2)
c                       write(*,*) 'wire PRIIE',(hdc_wire_num(temp_space_point_hits(i,k)),k=3,temp_space_point_hits(i,1)+2)
c                 endif
                  do k=1,temp_space_point_hits(i,1)+2
                     space_point_hits(new_count,k)
     &                  = temp_space_point_hits(i,k)
                  enddo
                  space_point_hits(new_count,hmax_hits_per_point+1)
     &                  = temp_space_point_hits(i,hmax_hits_per_point+1)
                  space_point_hits(new_count,hmax_hits_per_point+2)
     &                  = temp_space_point_hits(i,hmax_hits_per_point+2)
                  space_points(new_count,1)=temp_space_points(i,1)
                  space_points(new_count,2)=temp_space_points(i,2)
c               if (new_count.lt.13.and.gen_event_ID_number.eq.68202) then
c                         write(*,*) 'hit',(space_point_hits(new_count,k),k=3,space_point_hits(new_count,1)+2)
c        write(*,*) 'layer',(hdc_layer_num(space_point_hits(new_count,k)),k=3,space_point_hits(new_count,1)+2)
c        write(*,*) 'wire',(hdc_wire_num(space_point_hits(new_count,k)),k=3,space_point_hits(new_count,1)+2)
c                  endif
               endif
            enddo
            nspace_points= new_count
         endif
      endif
c          write(*,*) 'NOVI BROJ TOCAKA',nspace_points,init_space_point_count,ploop,hmax_hits_per_point
c      do j=1,nspace_points
c       if (j.lt.13.and.gen_event_ID_number.eq.68209) then
c       if (j.lt.13) then
c        write(*,*) 'space points seva u spcu:',j,space_point_hits(j,1),space_point_hits(j,2),
c     & (space_point_hits(j,k),k=3,space_point_hits(j,1)+2), space_point_hits(j,hmax_hits_per_point+1)
c        write(*,*) 'hit u spceu:',(space_point_hits(j,k),k=3,space_point_hits(j,1)+2)
c        write(*,*) 'layer u spceu:',(hdc_layer_num(space_point_hits(j,k)),k=3,space_point_hits(j,1)+2)
c        write(*,*) 'wire u spceu:',(hdc_wire_num(space_point_hits(j,k)),k=3,space_point_hits(j,1)+2)
c        endif
c        enddo

      return
      end
