      subroutine H_LINK_STUBS(ABORT,err)
*     This subroutine compares all the space-point-stubs found in
*     h_LEFT_RIGHT.f and links together stubs to form tracks.
*     The criterion are that the stubs are in different chambers and
*     each of the four track parameters are within limit:
*     hxt_track_criterion      for x_t
*     hyt_track_criterion      for y_t
*     htx_track_criterion      for t_x
*     hty_track_criterion      for t_y
*     
*     d.f. geesaman           7-September 1993
*     $Log: h_link_stubs.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.6  2005/03/14 19:52:17  miyoshi
*     change tracking variables and histogram names
*
*     Revision 1.5  2005/03/08 14:56:56  miyoshi
*     add link stub debug hist
*
*     Revision 1.4  2005/03/07 16:37:52  miyoshi
*     change vector array
*
*     Revision 1.3  2005/03/02 17:26:03  miyoshi
*     change histid params
*
*     Revision 1.2  2004/12/24 21:35:57  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     
*     Revision 1.8 2004/04 Miyoshi
*     fix bug to make duplicated tracks
*     
*     Revision 1.7  1996/09/05 19:55:23  saw
*     (DVW) Added some track tests
*     
*     Revision 1.6  1996/01/17 19:01:38  cdaq
*     (JRA)
*     
*     Revision 1.5  1995/08/31 18:44:51  cdaq
*     (JRA) Calculate dpos (pos. track - pos. hit) variables
*     
*     Revision 1.4  1995/05/22  19:45:42  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.3  1995/04/01  20:42:57  cdaq
*     (SAW) Fix typos
*     
*     Revision 1.2  1994/06/07  04:41:19  cdaq
*     (DFG) Add switch to include single stub tracks
*     
*     Revision 1.1  1994/02/21  16:14:56  cdaq
*     Initial revision
*     
*     The logic is :
*     1) loop over all space points as seeds   isp1
*     2) Check if this space point is all ready in a track
*     3) loop over all succeeding space points isp2
*     4) check if there is a track-criterion match
*     either add to existing track
*     or if there is another point in same chamber
*     make a copy containing isp2 rather than 
*     other point in same chamber
*     5) If ssingle_stub is set, make a track of all single
*     stubs.
*     
      implicit none
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'hks_id_histid.cmn'
      include 'hks_geometry.cmn'

*     Derek added these next lines
      include 'hks_bypass_switches.cmn'
      INCLUDE 'gen_event_info.cmn'
      external h_chamnum
      integer*4 h_chamnum
      
*     local variables
*     
      logical ABORT
      character*12 here
      parameter (here='H_LINK_STUBS')
      character*(*) err
      integer*4  isp1,isp2,isp  !  loop index on space points
      integer*4  ihit           !  loop index on hits
      integer*4  spindex,spoint,duppoint
      integer*4  sptracks       !  number of tracks with this seed
      integer*4  stub_tracks(HNTRACKS_MAX)
      integer*4  numhits
      integer*4  itrack         !  loop index on tracks
      integer*4  track
      integer*4  track_space_points(HNTRACKS_MAX,hmax_space_points+1)
      integer*4  tryflag        ! flag to loop over rest of points
      integer*4  newtrack       ! make a new track
      real*4 dposx,dposy,dposxp,dposyp
      real*4 y1,y2
      
**************************************
*     local variables by T. Miyoshi
      
      Integer*4 i,j,k
      Integer*4 linkhitnum(hmax_space_points,hmax_num_dc_layers)
      Integer*4 coinhitnum,pln
      Integer*4 linkflag(hmax_space_points)
      Integer*4 tempflag,tempflag2
      Integer*4 zeronum
      Integer*4 jj
      Integer*4 trackskip,newlinkflag,isp3
      
***************************************
      
      
*     Derek added these next lines
      
      if (hbypass_track_eff_files.eq.0) then
       open(unit=16,file='scalers/htrackstubs.txt',status='unknown',
     $      access='append')
      endif
      
      hstubtest = 0
*     
      ABORT= .FALSE.
      err=' '

      HNTRACKS_FP=0

      Do i=1,HNTRACKS_MAX
       HNTRACK_HITS(i,1)=0
      EndDo
      
      tempflag=0
      tempflag2=0
      
      if(ntof.eq.0) return
*     
c      print*,hnspace_points_tot 

      if(hsingle_stub .eq. 0 ) then       
       
*     loop over all pairs of space points
       if(hnspace_points_tot.ge.2) then ! return if less than 2 space points
        
*     search double stub. added by T. Miyoshi
        
        Do isp1 = 1,hnspace_points_tot
         linkflag(isp1)=1       ! usually the space point is on.
      
         Do isp2 = 1,hspace_point_hits(isp1,1)

          pln = hdc_layer_num( hspace_point_hits(isp1,isp2+2) )
          linkhitnum(isp1,pln) = hspace_point_hits(isp1,isp2+2)
         EndDo          
         
*     isp2 is from isp1-1 to 0.
         isp2=isp1-1
         
         Do while(isp2 .gt. 0)  
          coinhitnum=0   
          zeronum=0
          Do pln=1,12 
           If(linkhitnum(isp1,pln) .ne. 0 .and. 
     &          linkhitnum(isp2,pln) .ne. 0.and.
     &          linkhitnum(isp1,pln) .eq. linkhitnum(isp2,pln)) then  
            coinhitnum = coinhitnum + 1
           EndIf
           if(linkhitnum(isp1,pln) .eq. 0 .and. 
     &          linkhitnum(isp2,pln) .eq. 0) then
            zeronum = zeronum + 1
           EndIf
          EndDo
         
          if(coinhitnum.eq.6) then
           linkflag(isp1)=0
           tempflag=1
          EndIf
          
          if(coinhitnum.eq.5.and.zeronum.eq.7) then                       
           linkflag(isp1)=0
           tempflag2=1
          endif
          isp2=isp2-1 
         EndDo                  ! isp2>0
        EndDo                   ! end of isp1 loop 
        
***   end of new condition ***
        
*     if linkflag=0, this set is the same as the one before. skip it.
        
        do isp1=1,hnspace_points_tot-1 ! loop over all points
         if(precutflag(isp1).ne.0) then
          if(linkflag(isp1) .ne. 0) then ! add one condition by T.Miyoshi
           
*     is this point all ready associated with a track?
           tryflag=1
          
           if(HNTRACKS_FP .gt. 0) then
            do itrack=1,HNTRACKS_FP
             if(track_space_points(itrack,1) .gt. 0) then
              do isp2 = 1,track_space_points(itrack,1)
               if(track_space_points(itrack,isp2+1) .eq. isp1) then
                tryflag=0       ! space point all ready in a track
               endif            ! end test on found point
              enddo             ! track_space_points loop
             endif              ! end test of >0  point
            enddo               ! end loop over tracks
           endif                ! hntracks_fp >0
*     if space point not all ready part of a track then look for matches
           if( tryflag .eq. 1) then
            newtrack=1
           
            do isp2=isp1+1,hnspace_points_tot
             if(precutflag(isp2).ne.0) then ! add by C.CHEN
              if(linkflag(isp2).ne.0) then ! add one condition by T.Miyoshi
               
*     are these stubs in the same chamber. If so then skip
               if(h_chamnum(isp1) .ne. h_chamnum(isp2)) then
                if(prelinkflag(isp1,isp2).ne.0) then
              
*     does this stub match
              
*     since single chamber angular resolution is ~50mr, and the maximum y'
*     angle is about 30mr, use difference between y AT CHAMBERS, rather than
*     at focal plane.  (project back to chamber, to take out y' uncertainty)
                 dposx = hbeststub(isp2,1)-hbeststub(isp1,1)
                 y1=hbeststub(isp1,2)+hdc_1_zpos*hbeststub(isp1,4)
                 y2=hbeststub(isp2,2)+hdc_2_zpos*hbeststub(isp2,4)
                 dposy=y2-y1
                 dposxp= hbeststub(isp2,3)-hbeststub(isp1,3)
                 dposyp= hbeststub(isp2,4)-hbeststub(isp1,4)
c     --- debug histogram
                 Call HF1(hidxtcriterion,dposx,1.)
                 Call HF1(hidytcriterion,dposy,1.)
                 Call HF1(hidxptcriterion,dposxp,1.)
                 Call HF1(hidyptcriterion,dposyp,1.)
*     Derek added this for track tests...
       
                 if (abs(dposx).LT.abs(hstubminx)) hstubminx = dposx
                 if (abs(dposy).LT.abs(hstubminy)) hstubminy = dposy
                 if (abs(dposxp).LT.abs(hstubminxp)) hstubminxp = dposxp
                 if (abs(dposyp).LT.abs(hstubminyp)) hstubminyp = dposyp

                 if (hbypass_track_eff_files.eq.0) then
                  if (abs(hstubminx) .gt. hxt_track_criterion) then
                   write(16,*) 'event # ',gen_event_ID_number,
     $                  ' hstubminx = ',hstubminx
                  endif
                  if (abs(hstubminy) .gt. hyt_track_criterion) then
                   write(16,*) 'event # ',gen_event_ID_number,
     $                  '  hstubminy =            ',hstubminy
                  endif
                  if (abs(hstubminxp) .gt. hxpt_track_criterion) then
                   write(16,*) 'event # ',gen_event_ID_number,
     $                  ' hstubminxp =                      ',
     $                  hstubminxp
                  endif
                  if (abs(hstubminyp) .gt. hypt_track_criterion) then
                   write(16,*) 'event # ',gen_event_ID_number,
     $                  ' hstubminyp =                                 ',
     &                  hstubminyp
                  endif
                  close(16)
                 endif
                 
                 if        (abs(dposx) .lt. hxt_track_criterion
     $                .and. abs(dposy) .lt. hyt_track_criterion
     $                .and. abs(dposxp).lt. hxpt_track_criterion
     $                .and. abs(dposyp).lt. hypt_track_criterion) then
c     print*,"newtrack=",newtrack,tryflag

                  if(newtrack.eq.1) then
*     Derek add this next line
                   hstubtest=1
*     make a new track
*     track<=NNTRACKS_MAX
*     
                   if(HNTRACKS_FP.lt.HNTRACKS_MAX) then
                    
                    HNTRACKS_FP=HNTRACKS_FP+1 ! increment the number of tracks
                    sptracks=1  ! one track with this seed
                    stub_tracks(1)=HNTRACKS_FP
                    track_space_points(HNTRACKS_FP,1)=2
                    track_space_points(HNTRACKS_FP,2)=isp1
                    track_space_points(HNTRACKS_FP,3)=isp2
                    hx_sp1(hntracks_fp)=hbeststub(isp1,1)
                    hx_sp2(hntracks_fp)=hbeststub(isp2,1)
                    hy_sp1(hntracks_fp)=hbeststub(isp1,2)
                    hy_sp2(hntracks_fp)=hbeststub(isp2,2)
                    hxp_sp1(hntracks_fp)=hbeststub(isp1,3)
                    hxp_sp2(hntracks_fp)=hbeststub(isp2,3)
                    newtrack=0  ! make no more track in this loop

                   endif        ! end test on too many tracks                
                  else          ! newtrack=0
*     check if there is another space point in same chamber
c     print*,"newtrack",newtrack,hntracks_fp,tryflag,isp1,isp2
                   itrack = 0
                   do while (itrack.lt.sptracks)
                    itrack = itrack+1
                    track = stub_tracks(itrack)
                    spoint = 0
                    duppoint = 0
                    do isp=1,track_space_points(track,1)
                     if(h_chamnum(isp2).eq.
     &                    h_chamnum(track_space_points(track,isp+1))) then
                      spoint = isp
                     endif
                     if(isp2.eq.track_space_points(track,isp+1)) then
                      duppoint = 1
                     endif                   
                    enddo       ! end loop over sp in tracks with isp1
                    
*     if there is no other space point in this chamber
*     add this space point to current track(2)
                 
                    if(duppoint.eq.0) then
                     if(spoint.eq.0) then
                      
                      spindex = track_space_points(track,1) + 1
                      track_space_points(track,1) = spindex
                      track_space_points(track,spindex+1) = isp2
                      
*     if there is another point in the same chamber in this track
*     create a new track with all the same space points except spoint
                   
                     else       ! spoint > 0
                   
*     track =< HNTRACKS_MAX     
*     check if track_space_points is completely the same
                      trackskip=0
                      isp3=hntracks_fp
                      Do while(isp3.gt.1)
                       
                       if(track_space_points(track,1).eq.
     &                      track_space_points(isp3,1)) then
                        
                        newlinkflag=0
                        
                        do isp=1,track_space_points(track,1)
                         
                         if(isp.ne.spoint) then
                          if(track_space_points(isp3,isp+1).eq.
     &                         track_space_points(track,isp+1)) then
                           newlinkflag=newlinkflag+1
                          endif
                          
                         elseif(isp.eq.spoint) then
                          if(track_space_points(isp3,isp+1).eq.isp2) then
                           newlinkflag=newlinkflag+1
                          endif
                         endif  ! end check for dup on copy
                         
                        enddo   ! track_space_point loop
                        
                       endIf    ! end n of track sp is the same
                       
                       if(newlinkflag.eq.2) then
                        trackskip=1
                       endif
                       isp3=isp3-1
                      EndDo     ! hntracks_fp loop
                      
*     trackskip = 1 : this track can be skipped. 
                   
                      if(trackskip.ne.1) then ! skip double track
                       
                       if(HNTRACKS_FP.lt.HNTRACKS_MAX) then
                        
                        HNTRACKS_FP=HNTRACKS_FP+1 ! increment the number of tracks

                        sptracks= sptracks+1 ! one track with this seed
                        stub_tracks(sptracks) = HNTRACKS_FP
                        track_space_points(HNTRACKS_FP,1)
     $                       =track_space_points(track,1)
                        do isp=1,track_space_points(track,1)
                         
                         if(isp.ne.spoint) then
                          track_space_points(HNTRACKS_FP,isp+1)=
     &                         track_space_points(track,isp+1)
                         elseif(isp.eq.spoint) then
                          track_space_points(HNTRACKS_FP,isp+1)= isp2
                         endif  ! end check for dup on copy
                         
                        enddo   ! end copy of track                     
                       endif    ! end if on too many tracks
                      endif     ! end of track skip condition 
                      
                     endif      ! end if on same chamber
                    endif       ! end if on  duplicate point
                   enddo        ! end do while over tracks with isp1
                  endif
                 endif
                 
                endif
                
               endif            ! end test on same chamber
               
              endif             ! end of linkflag(isp2).ne.0 condition
             endif              ! end precutflag          
            enddo               ! end loop over new space points isp2
           endif                ! end test on tryflag
    
          endif                 ! end of linkflag(isp1).ne.0 condition
         endif                  ! end precutflag
c     print*,hntracks_fp
        enddo                   ! end outer loop over space points isp1
       endif                    ! end if on space points > 2
c     print*,"hntracks_fp" ,hntracks_fp    
      else                      ! if hsingle_stub .ne. 0
*     
*     when hsingle_stub is set, make each space point a track
*     This will have poor resolution but may be appropriate for debugging
*     
       do isp1=1,hnspace_points_tot ! loop over all points
        
        if(HNTRACKS_FP.lt.HNTRACKS_MAX) then ! are there too many
         HNTRACKS_FP=HNTRACKS_FP+1 ! increment the number of tracks
         track_space_points(HNTRACKS_FP,1)= 1
         track_space_points(HNTRACKS_FP,2)= isp1
        endif                   ! end if on too many tracks
       enddo                    ! end loop over all space points
       
      endif                     ! end test on ssingle_stub





*     
*     now list all hits on a track
*     
      if(HNTRACKS_FP.gt.0)   then 
       do itrack = 1,HNTRACKS_FP ! loop over all tracks
        HNTRACK_HITS(itrack,1) = 0
        do isp1 = 1,track_space_points(itrack,1)
         spindex = track_space_points(itrack,isp1+1)
         numhits = hspace_point_hits(spindex,1)
         do ihit = 1,numhits
          if(HNTRACK_HITS(itrack,1).lt.HNTRACKHITS_MAX) then 
           HNTRACK_HITS(itrack,1) = HNTRACK_HITS(itrack,1)+1
           HNTRACK_HITS(itrack,HNTRACK_HITS(itrack,1)+1) =
     &          hspace_point_hits(spindex,ihit+2)       

c add DD 2010/7/11
           hdc_wire_coord_sp(itrack,
     &          hntrack_hits(itrack,hntrack_hits(itrack,1)+1))=
     &          hdc_wire_coord_sp(spindex,hspace_point_hits(spindex,ihit+2))
c add DD 2010/7/11
          endif                 ! end test on too many hits
         enddo                  ! end loop over space point hits
        enddo                   ! end loop over space points
       enddo                    ! end loop over all tracks
       
      endif                     ! end of hntracks_fp > 0
      
      
      if(hdebuglinkstubs.ne.0) then
       call h_print_links
      endif
c      print*,"final hntracks_fp",hntracks_fp
      return
      end
*********
*     Local Variables:
*     mode: fortran
*     fortran-if-indent: 1
*     fortran-do-indent: 1
*     End:
