      subroutine e_dc2_choose_single_hit(ABORT,err,nspace_points,
     & space_point_hits)
*--------------------------------------------------------
*-
*-   Purpose and Methods :  This routine looks at all hits in a space
*-                          point. If two hits are in the same layer it
*-                          rejects the one with the longer drift time
*-
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
*-   Created 28-JUN-1994   D. F. Geesaman
* $Log: h_choose_single_hit.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.2  2004/12/24 21:33:07  miyoshi
* change name plane to layer
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.3  1996/01/17 19:04:08  cdaq
* (JRA) Misc changes
*
* Revision 1.2  1995/05/22 19:45:33  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/11/22  21:06:12  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*50 here
      parameter (here= 'e_dc2_choose_single_hit')
      integer*4 nspace_points
      
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'hes_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hes_tracking.cmn'
      include 'hes_geometry.cmn'
      INCLUDE 'gen_event_info.cmn' ! matsu

*
      integer*4 space_point_hits(edc2max_space_points,emax_dc2_hits)
     
*
*     local variables
      integer*4 point,startnum,finalnum,goodhit(emax_dc2_hits)
      integer*4 layer1,layer2,hit1,hit2,deltatime
      integer*4 hits(emax_dc2_hits),multipleflag(12)
      integer*4 hits_multi(emax_dc2_hits)
      integer*4 i,j,k,l,hit3,hit4,layer3,layer4
      integer*4 hit5,hit6,layer5,layer6
      integer*4  hit7,hit8,layer7,layer8
      integer*4  hit9,hit10,layer9,layer10
      integer*4 hitid(12,emax_dc2_hits)
      integer*4 ignoreflag(edc2max_hits_per_point+2)
      integer*4 hdtmin(12),hdtmax(12),minindex(12),maxindex(12)
      integer*4 multiplehitid(12,emax_dc2_hits)
      integer*4 nmultiple(emax_dc2_hits),kicknum,multinum
      integer*4 multilayerflag(12),multinum_per_layer(12)
      integer*4 nlayer,multipletestflag,imul(12)
      integer*4 hits_per_layer(12),ihit_layer(12,emax_dc2_hits)
      integer*4 ich,loopu,loopd,firstlayer,pln,leftindex(12)
      integer*4 sloop,slooplayer,layerindex
      integer*4 ilayer,isel(12),ipln,test(12)
      integer*4 ii1,ii2,ii3,ii4,ii5,ii6
c      integer*4 space_points_multi(hmax_space_points_multi)
      real*4 drifttime1,drifttime2,drifttime3,drifttime4,drifttime5
      real*4 drifttime6,drifttime7,drifttime8,drifttime9,drifttime10
      real*4 wc
c      integer*4 add,m
c      integer*4 add1,add2
c      integer*4 eventid
c      add=0
    
c      eventid=eventid+1
      
*
*     temporary initialization
      ABORT= .FALSE.
      err=' '
*
      
      
             
c      do i=1,100
c         space_points_multi(i)=0
c           print*,space_points_multi(i)
c      enddo
*     
      do i=1,edc2max_hits_per_point+2 
         ignoreflag(i)=0
      enddo

cDK       print*, "evid,nspace_points",gen_event_ID_number,nspace_points
*     loop over all space points
      do point =1,nspace_points
c        add=0
c        add1=0
c        add2=0
c        print*,'point',' ',point
         multipletestflag=0
         startnum = space_point_hits(point,1)
cDK         print*, "evid,startnum",gen_event_ID_number,startnum
         finalnum=0
         kicknum=0
         multinum=1
         nlayer=0
         loopu=1
         loopd=6
c        print*,loopu,loopd
         do i=loopu,loopd
            multinum_per_layer(i)=0
            multilayerflag(i)=0
         enddo
       
c        do m=3,startnum+1         
         
c         write(59,*)eventid,point,m-2,space_point_hits(point,m),edc2_layer_num(space_point_hits(point,m)),
c     +                 edc2_drift_time(space_point_hits(point,m))
         
c        enddo
c----
c for the layer loop, we need do some improvement
c What we need to do is first test which chamber the space point in, then choose the loop sequece
c Do it later
         do j=3,startnum+2
            goodhit(j) = 1
         enddo

         do i=loopu,loopd
            hdtmin(i)=500
            hdtmax(i)=-100
            hits_per_layer(i)=0
            multipleflag(i)=0
            imul(i)=0
            minindex(i)=0
            maxindex(i)=0
         enddo

         do j=loopu,loopd
            do i=1,edc2max_hits_per_point
               hitid(j,i)=0
            enddo
         enddo
c         print*,loopu,loopd
          
         do j=3,startnum+1
            hit3 = space_point_hits(point,j)
            layer3 = edc2_layer_num(hit3)
            drifttime3 = edc2_drift_time(hit3)  
            do k=j+1,startnum+2
               hit4 = space_point_hits(point,k)
               layer4 = edc2_layer_num(hit4)
               drifttime4 = edc2_drift_time(hit4)
               if(layer3 .eq. layer4) then 
c                 print*,'layer',layer3
                  multipleflag(layer3)=1
                  hitid(layer3,j)=1
                  hitid(layer3,k)=1
               endif                         
            enddo
         enddo
      
         do i=loopu,loopd
c            ignoreflag=0
            if( multipleflag(i).gt.0) then
cDK               print*, "evid,startnum",gen_event_ID_number,startnum
               do j=3,startnum+2
                  hit5 = space_point_hits(point,j)
                  layer5 = edc2_layer_num(hit5)
                  drifttime5 = edc2_drift_time(hit5)
cDK                  print*, "layer5",i,hit5,layer5,drifttime5
               enddo
               do j=3,startnum+2
                  ignoreflag(j)=0
                  hit5 = space_point_hits(point,j)
                  layer5 = edc2_layer_num(hit5)
                  drifttime5 = edc2_drift_time(hit5)
                  wc=edc2_wire_center(hit5)
                  do l=1,12
                     if (layer5.eq.l) then
                        call hf1(7000+l,drifttime5,1.)
                     endif
                  enddo
c           print*,'hitid', hitid(i,j)
c            print*,layer5,i
                  if(layer5.eq.i) then  
                     if(hitid(layer5,j).gt.0) then
                        if(drifttime5.ge.-5.and.drifttime5.le.125) then
                           if(drifttime5.le.hdtmin(layer5)) then
cDK                              write(*,*)"drifttime5",j,i,drifttime5,wc,hit5
                              minindex(layer5)=j
                              hdtmin(layer5)=drifttime5
                           endif             
                           ignoreflag(j)=1
                        else
                           goodhit(j)=0
                        endif ! drifttime>=0 
                     endif  !hitid    
                  endif          
               enddo   !end loop j

               do j=3,startnum+2
                  hit5 = space_point_hits(point,j)
                  layer5 = edc2_layer_num(hit5)
                  drifttime5 = edc2_drift_time(hit5)
                  if(ignoreflag(j).gt.0) then
                     if(layer5.eq.i.and.drifttime5.lt.-5) then  
                        goodhit(j)=0
                     endif
                  elseif(ignoreflag(j).eq.0) then 
                     if(layer5.eq.i) then  
                        if(hitid(layer5,j).gt.0) then
                           if(drifttime5.gt.hdtmax(layer5)) then
                              maxindex(layer5)=j
                              hdtmax(layer5)=drifttime5
                           endif !hdtmax
                        endif
                     endif
                  endif
               enddo
c          if(maxindex(i).ne.0) then
c          print*,maxindex(i)          
c          print*,goodhit(maxindex(i))
c          endif
cDK               write(*,*)"minindex",i,minindex(i)
               if(minindex(i).ne.0) then
                  leftindex(i)=minindex(i)
               elseif(maxindex(i).ne.0) then
                  leftindex(i)=maxindex(i)
               else
                  print*,'please check your code!'
               endif
            endif
         enddo   
         
         do i=loopu,loopd
            if( multipleflag(i).gt.0) then
c              print*,minindex(i),maxindex(i),i
               do k=3,startnum+2 
                  hit6 = space_point_hits(point,k)
                  layer6 = edc2_layer_num(hit6)
                  drifttime6 = edc2_drift_time(hit6)
                  if(layer6.eq.i) then
                     if(k.ne.leftindex(i))then
                        goodhit(k)=0
                     endif 
                     if(goodhit(k).gt.0) then

                     endif
cDK                     write(*,*)"leftindex",i,leftindex(i),k,goodhit(k)
                  endif
               enddo
            endif
         enddo
   
         do j=3,startnum+2
            hit6 = space_point_hits(point,j)
            layer6 = edc2_layer_num(hit6)
            drifttime6 = edc2_drift_time(hit6)
cDK            write(*,*) "j,layer,goodhit,dtime",j,layer6,goodhit(j),drifttime6
            if(goodhit(j).gt.0) then
               finalnum = finalnum + 1
               hits(finalnum)=space_point_hits(point,j)
            endif                         ! end check on good hit
         enddo
         space_point_hits(point,1) = finalnum
cDK         print*, "evid,finalnum",gen_event_ID_number,finalnum

*     copy good hits to space_point_hits
c        do j = 1, finalnum
c          space_point_hits(point,j+2) = hits(j)
c        enddo    
c       print*,finalnum

c-------------------------------------------------------------------     

c       if(finalnum.gt.4)  then
c        write(35,*) finalnum,kicknum,startnum
c        print*,finalnum,kicknum,startnum
c        endif
c--- calculate the number of optional space points by  C1(1)*C2(1)*....C12(1)
C Ci is the hits number per layer e.g.  there is 3 hits on first  layer C1(1)=3
         if(finalnum.gt.4) then
c          print*,finalnum         
            do i=loopu,loopd                  
               do j=3,startnum+2
                  hit7 = space_point_hits(point,j)
                  layer7 = edc2_layer_num(hit7)
                  drifttime7 = edc2_drift_time(hit7)             
                  if(goodhit(j).eq.0.and.layer7.eq.i) then          
                     if(drifttime7.ge.-5.and.drifttime7.le.125) then
                        multinum_per_layer(i)=multinum_per_layer(i)+1        
                        multilayerflag(i)=1
                        multipletestflag=1         
                     endif
                  endif        
               enddo
               if(multilayerflag(i).gt.0) then
                  nlayer=nlayer+1
               endif
            enddo
  
c       if(multipletestflag.gt.0) then
c       do i=loopu,loopd
c          if(multilayerflag(i).gt.0) then
c            print*,multinum_per_layer(i)
c          endif
c       enddo
c       endif
 
            do i=loopu,loopd
               if(multilayerflag(i).gt.0) then
                  if(multinum_per_layer(i).gt.0) then          
                     multinum_per_layer(i)=multinum_per_layer(i)+1
                     multinum=multinum*multinum_per_layer(i)
                  endif
               endif
            enddo
       
c          if(multipletestflag.gt.0) then
c           print*,multinum
c         do i=loopu,loopd
c          if(multilayerflag(i).gt.0) then
c            print*,multinum_per_layer(i)
c          endif
c       enddo
c       endif
 
     
c       multinum=multinum
c      write(34,*)multinum
c if the multinum >10, then choose the option that all the hits the earliest one inside the timing window
c That is keep above choice 
 
cDK            write(*,*) "multipletestflag,multinum",multipletestflag,multinum
            if(multipletestflag.gt.0.and.multinum.le.10.and.multinum.gt.0) then
               do i=loopu,loopd
                  do j=3,startnum+2
                     hit9 = space_point_hits(point,j)
                     layer9 = edc2_layer_num(hit9)
                     drifttime9 = edc2_drift_time(hit9) 
                     if(layer9.eq.i) then
                        if(goodhit(j).gt.0) then
                           hits_per_layer(i)=hits_per_layer(i)+1
                           ihit_layer(i,hits_per_layer(i))=hit9
                        else
                           if( multilayerflag(i).gt.0) then
                              if(drifttime9.gt.-5.and.drifttime9.le.125) then
                                 hits_per_layer(i)=hits_per_layer(i)+1
                                 ihit_layer(i,hits_per_layer(i))=hit9
                              endif
                           endif   ! multilayerflag(i)>0
                        endif
                     endif !layer
                  enddo
               enddo   
c---------------------------------------------------------------------                         
               do sloop=loopu,loopd
                  if(hits_per_layer(sloop).gt.0) then
                     imul(sloop)=1   
                  endif
               enddo
               slooplayer=loopu
               k=0
               if(finalnum.eq.5) then  
                  do ilayer=loopu,loopd
                     if(hits_per_layer(ilayer).eq.0) then
                        layerindex=ilayer
c                print*,layerindex
                     endif
                  enddo
c            print*,test
c             print*,layerindex
                  if(layerindex.eq.loopd) then
                     do ii1=1,hits_per_layer(slooplayer)
                        imul(slooplayer)=ii1               
                        do ii2=1,hits_per_layer(slooplayer+1)
                           imul(slooplayer+1)=ii2
                           do ii3=1,hits_per_layer(slooplayer+2) 
                              imul(slooplayer+2)=ii3
                              do ii4=1,hits_per_layer(slooplayer+3)
                                 imul(slooplayer+3)=ii4
                                 do ii5=1,hits_per_layer(slooplayer+4)
                                    imul(slooplayer+4)=ii5
c                                   print*,'more'
                                    do pln=loopu,loopd
c                                      print*,'less'
                                       if(hits_per_layer(pln).gt.0) then
                                          k=k+1          
c               print*,imul(pln),ihit_layer(pln,imul(pln)),space_point_hits(point,k+2)     
                                          space_point_hits(point,k+2)=
     &                                     ihit_layer(pln,imul(pln))  
c          print*,pln,edc2_layer_num(space_point_hits(point,k+2)),space_point_hits(point,k+2),point            
c                       print*, ihit_layer(pln,imul(pln))        
                                       endif 
                                    enddo
                                 enddo ! ii5
                              enddo ! ii4                      
                           enddo !ii3  endif
                        enddo    !ii2
                     enddo  !ii1
                  endif !layerindex
                  
                  if(layerindex.eq.loopd-1) then           
                     do ii1=1,hits_per_layer(slooplayer)
                        imul(slooplayer)=ii1               
                        do ii2=1,hits_per_layer(slooplayer+1)
                           imul(slooplayer+1)=ii2
                           do ii3=1,hits_per_layer(slooplayer+2) 
                              imul(slooplayer+2)=ii3
                              do ii4=1,hits_per_layer(slooplayer+3)
                                 imul(slooplayer+3)=ii4
                                 do ii5=1,hits_per_layer(slooplayer+5)
                                    imul(slooplayer+5)=ii5
                                    do pln=loopu,loopd
                                       if(hits_per_layer(pln).gt.0) then
                                          k=k+1  
                                          space_point_hits(point,k+2)=
     &                                     ihit_layer(pln,imul(pln))   
c              print*,pln,edc2_layer_num(space_point_hits(point,k+2)),space_point_hits(point,k+2),point            
                                       endif     
                                    enddo
                                 enddo ! ii5
                              enddo ! ii4                      
                           enddo !ii3  endif
                        enddo    !ii2
                     enddo  !ii1
                  endif  !layerindex
          
                  if(layerindex.eq.loopd-2) then           
                     do ii1=1,hits_per_layer(slooplayer)
                        imul(slooplayer)=ii1               
                        do ii2=1,hits_per_layer(slooplayer+1)
                           imul(slooplayer+1)=ii2
                           do ii3=1,hits_per_layer(slooplayer+2) 
                              imul(slooplayer+2)=ii3
                              do ii4=1,hits_per_layer(slooplayer+4)
                                 imul(slooplayer+4)=ii4
                                 do ii5=1,hits_per_layer(slooplayer+5)
                                    imul(slooplayer+5)=ii5
                                    do pln=loopu,loopd
                                       if(hits_per_layer(pln).gt.0) then
                                          k=k+1               
                                          space_point_hits(point,k+2)=
     &                                     ihit_layer(pln,imul(pln))
c           print*,pln,edc2_layer_num(space_point_hits(point,k+2)),space_point_hits(point,k+2),point          
                                       endif     
                                    enddo
                                 enddo ! ii5
                              enddo ! ii4                      
                           enddo !ii3  endif
                        enddo    !ii2
                     enddo  !ii1
                  endif  !layerindex
         
                  if(layerindex.eq.loopd-3) then           
                     do ii1=1,hits_per_layer(slooplayer)
                        imul(slooplayer)=ii1               
                        do ii2=1,hits_per_layer(slooplayer+1)
                           imul(slooplayer+1)=ii2
                           do ii3=1,hits_per_layer(slooplayer+3) 
                              imul(slooplayer+3)=ii3
                              do ii4=1,hits_per_layer(slooplayer+4)
                                 imul(slooplayer+4)=ii4
                                 do ii5=1,hits_per_layer(slooplayer+5)
                                    imul(slooplayer+5)=ii5
                                    do pln=loopu,loopd
                                       if(hits_per_layer(pln).gt.0) then
                                          k=k+1               
                                          space_point_hits(point,k+2)=
     &                                     ihit_layer(pln,imul(pln))  
c                    print*,space_point_hits(point,k+2)    
c          print*,pln,edc2_layer_num(space_point_hits(point,k+2)),space_point_hits(point,k+2),point                              
                                       endif     
                                    enddo
                                 enddo ! ii5
                              enddo ! ii4                      
                           enddo !ii3  endif
                        enddo    !ii2
                     enddo  !ii1
                  endif  !layerindex
           
                  if(layerindex.eq.loopd-4) then           
                     do ii1=1,hits_per_layer(slooplayer)
                        imul(slooplayer)=ii1               
                        do ii2=1,hits_per_layer(slooplayer+2)
                           imul(slooplayer+2)=ii2
                           do ii3=1,hits_per_layer(slooplayer+3) 
                              imul(slooplayer+3)=ii3
                              do ii4=1,hits_per_layer(slooplayer+4)
                                 imul(slooplayer+4)=ii4
                                 do ii5=1,hits_per_layer(slooplayer+5)
                                    imul(slooplayer+5)=ii5
                                    do pln=loopu,loopd
                                       if(hits_per_layer(pln).gt.0) then
                                          k=k+1               
                                          space_point_hits(point,k+2)=
     &                                     ihit_layer(pln,imul(pln))     
c           print*,pln,edc2_layer_num(space_point_hits(point,k+2)),space_point_hits(point,k+2),point                              
                                       endif     
                                    enddo
                                 enddo ! ii5
                              enddo ! ii4                      
                           enddo !ii3  endif
                        enddo    !ii2
                     enddo  !ii1
                  endif  !layerindex
           
                  if(layerindex.eq.loopu) then           
                     do ii1=1,hits_per_layer(slooplayer+1)
                        imul(slooplayer+1)=ii1               
                        do ii2=1,hits_per_layer(slooplayer+2)
                           imul(slooplayer+2)=ii2
                           do ii3=1,hits_per_layer(slooplayer+3) 
                              imul(slooplayer+3)=ii3
                              do ii4=1,hits_per_layer(slooplayer+4)
                                 imul(slooplayer+4)=ii4
                                 do ii5=1,hits_per_layer(slooplayer+5)
                                    imul(slooplayer+5)=ii5
                                    do pln=loopu,loopd
                                       if(hits_per_layer(pln).gt.0) then
                                          k=k+1               
                                          space_point_hits(point,k+2)=
     &                                      ihit_layer(pln,imul(pln))  
c           print*,pln,edc2_layer_num(space_point_hits(point,k+2)),space_point_hits(point,k+2),point                    
                                       endif     
                                    enddo
                                 enddo ! ii5
                              enddo ! ii4                      
                           enddo !ii3  endif
                        enddo    !ii2
                     enddo  !ii1
                  endif  !layerindex
           
               endif !finalnum

               if(finalnum.eq.6) then
                  do ii1=1,hits_per_layer(slooplayer)
                     imul(slooplayer)=ii1               
                     do ii2=1,hits_per_layer(slooplayer+1)
                        imul(slooplayer+1)=ii2
                        do ii3=1,hits_per_layer(slooplayer+2) 
                           imul(slooplayer+2)=ii3
                           do ii4=1,hits_per_layer(slooplayer+3)
                              imul(slooplayer+3)=ii4
                              do ii5=1,hits_per_layer(slooplayer+4)
                                 imul(slooplayer+4)=ii5
                                 do ii6=1,hits_per_layer(slooplayer+5)
                                    imul(slooplayer+5)=ii6
                                    do pln=loopu,loopd
                                       if(hits_per_layer(pln).gt.0) then
                                          k=k+1               
                                          space_point_hits(point,k+2)=
     &                                     ihit_layer(pln,imul(pln))    
c          print*,pln,edc2_layer_num(space_point_hits(point,k+2)),space_point_hits(point,k+2),point          
                                       endif     
                                    enddo
                                 enddo ! ii6
                              enddo ! ii5
                           enddo ! ii4                      
                        enddo !ii3  endif
                     enddo    !ii2
                  enddo  !ii1
               endif  ! finalnum 
c           print*,int(k/finalnum),multinum
               
c               if(multinum*finalnum.gt.k) then 
c                  space_points_multi(point)=int(k/finalnum)
c               else
c                  space_points_multi(point)=multinum
c               endif
c            print*,k,finalnum,multinum,finalnum*multinum
c           if(k.ne.finalnum*multinum) then
c            print*,'point',finalnum,multinum,k
c           endif
           
c           print*,'check'
c           do i=3,multinum*finalnum+2
c             print*,space_point_hits(point,i),edc2_layer_num(space_point_hits(point,i)),point,nspace_points
c           enddo
c         print*,'eventid'
c         print*,k,multinum*finalnum
c         print*,'check_more'
c            print*,multinum,space_points_multi(point)

            else
*     copy good hits to space_point_hits
               do j = 1, finalnum
                  space_point_hits(point,j+2) = hits(j)
c             print*,space_point_hits(point,j+2)
               enddo 
c            space_points_multi(point)=1
            endif   ! mutipletestflag and multinum<10
    
         else
*     copy good hits to space_point_hits
            do j = 1, finalnum
               space_point_hits(point,j+2) = hits(j)
            enddo 
c           space_points_multi(point)=1
         endif  ! finalnum>4
c         print*, space_points_multi(point)
c        write(49,*) add1, add2  
         
c         if(space_points_multi(point).gt.0) then
c          print*,'point',finalnum,space_points_multi(point)
c          do j=1,finalnum
c            print*,space_point_hits(point,j+2)
c           enddo
c         endif

      enddo                             ! end loop on space points      
c          do i=1,nspace_points         
c             if(space_points_multi(i).gt.0) then
c             finalnum=space_point_hits(i,1)*space_points_multi(i)
c            else
c            finalnum=space_point_hits(i,1)
c           endif
c          
c          do j=1,finalnum
c            hit1=space_point_hits(i,j+2)            
c      print*,space_points_multi(i),i,space_point_hits(i,1),hit1,edc2_layer_num(hit1)
c           
c          enddo
c        enddo
*******    
     
c      print*,'eventid'
      return
      end
