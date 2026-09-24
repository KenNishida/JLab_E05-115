      SUBROUTINE E_SELECT_GOOD_TRACKS(ABORT,err)
*--------------------------------------------------
*     -
*     -   Purpose and Methods : Select the good tracks 
*     -   through the Enge
*     -                              
*     -      Required Input BANKS
*     -
*     -      Output BANKS
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     -
*     - $Log: e_select_good_tracks.f,v $
*     - Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*     -
*     - e05115 src repository for software development
*     -
*     - Revision 1.2  2005/06/23 17:17:29  cdaq
*     - Bug fix HES optics
*     -
*     - Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*     -
*     -
*     - Revision 1.5  2005/04/08 18:59:55  miyoshi
*     - change selection
*     -
*     - Revision 1.4  2005/03/14 19:49:34  miyoshi
*     - change comments
*     -
*     - Revision 1.3  2005/03/10 16:48:24  miyoshi
*     - change tracking variable name
*     -
*     - Revision 1.2  2005/01/18 23:37:12  miyoshi
*     - fix bug when comparing tracks
*     -
*     - Revision 1.1.1.1  2004/08/30 21:21:41  miyoshi
*     - new dir
*     -
*     - Revision 1.5 03/24/04 Miyoshi
*     - for E01-011
*     -
*     - Revision 1.4  1995/07/20 19:01:37  cdaq
*     - (CC) Fix bug in best chisq finding
*     -
c     Revision 1.3  1995/05/22  19:45:55  cdaq
c     (SAW) Split gen_data_data_structures 
c     - into gen, hms, sos, and coin parts"
c     
c     Revision 1.2  1995/04/06  19:44:04  cdaq
c     (JRA) Fix some latent HMS variable names
c     
c     Revision 1.1  1995/02/23  13:29:49  cdaq
c     Initial revision
c     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*50 here
      parameter (here= 'E_SELECT_GOOD_TRACKS')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'hes_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hes_physics_sing.cmn'
      INCLUDE 'hes_tracking.cmn'
      INCLUDE 'hes_id_histid.cmn'
      Include 'gen_event_info.cmn'
      
*     
*     local variables 
      integer*4 i,j,tnew,ihit,hit,pln,jj,pl1,pl2
      integer*4 goodtrack,track
      integer*4 mark(ENTRACKS_PRE_MAX)
      integer*4 have_layer_hits1(EMAX_NUM_DC1_LAYERS)
      integer*4 have_layer_hits2(EMAX_NUM_DC2_LAYERS)
      real*4 chi2perdeg,chi2min
      real*4 dx,dy,dxp,dyp,res
      integer hit1,hit2,pln1,pln2,nwire1,nwire2,ihit1,ihit2,n_shared_wires
      integer num_wires(ENTRACKS_PRE_MAX,EMAX_NUM_DC1_LAYERS),pairs
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '
      
      if(entracks_pre .gt. 0) then
         
         Do i=1,ENTRACKS_PRE
            mark(i) = 1
         EndDo
         
*     --- add cut condition as you want
         if(edc1track_chi2min .eq. 0) edc1track_chi2min = 1000.
         Do i=1,entracks_pre
            do j=1,EMAX_NUM_DC1_LAYERS
               num_wires(i,j)= -1000
c               edc1_single_residual(i,j) = 100000
               edc1_tcoord(i,j) = 100000
               edc1_wcoord(i,j) = 100000
               edc1_wnum(i,j) = 100000
            enddo
            
            Do ihit1=2,entrack_hits_pre(i,1)+1
               hit1 = entrack_hits_pre(i,ihit1)
               pln1 = edc1_layer_num(hit1)
               nwire1=edc1_wire_num(hit1)
               num_wires(i,pln1)= nwire1
            enddo
            if(
     &           edc1chi2perdof_pre(i,enum_fitting) .gt. edc1track_chi2min
     &        ) then
               mark(i) = 0
            EndIF
         EndDo
         
c     Write(*,*) 'ev,pre=',gen_event_ID_number,entracks_pre
c     Do i=1,entracks_pre
c      write(*,*) 'hit,chi2,x,y=',
c     &        entrack_hits_pre(i,1),edc1chi2perdof_pre(i,enum_fitting),
c     &        ex_fp_pre1(i,enum_fitting),ey_fp_pre1(i,enum_fitting)
c     EndDo
         
*     --- check very close track as one
         if(entracks_pre .gt. 1) then
            Do i = 1,entracks_pre-1
               Do j = i+1,entracks_pre
                  dx = abs(ex_fp_pre1(i,enum_fitting)-
     &                  ex_fp_pre1(j,enum_fitting))
                  dy = abs(ey_fp_pre1(i,enum_fitting)-
     &                  ey_fp_pre1(j,enum_fitting))
                  dxp = abs(exp_fp_pre1(i,enum_fitting)-
     &                   exp_fp_pre1(j,enum_fitting))
                  dyp = abs(eyp_fp_pre1(i,enum_fitting)-
     &                   eyp_fp_pre1(j,enum_fitting))
                  Call HF1(eidtrkdx,dx,1.)
                  Call HF1(eidtrkdy,dy,1.)
                  Call HF1(eidtrkdxp,dxp,1.)
                  Call HF1(eidtrkdyp,dyp,1.)
                  if(mark(i) .eq. 1 .and. mark(j) .eq. 1) then
c                     write(*,*) "dx,dxp,dy,dyp",dx,dxp,dy,dyp 
                     if(
     &                    dx .lt. HES_DX_MIN .and.
     &                    dy .lt. HES_DY_MIN .and.
     &                    dxp .lt. HES_DXP_MIN .and.
     &                    dyp .lt. HES_DYP_MIN
     &                    ) then
c                        write(*,*) "recognized as one track"
                        if(edc1chi2perdof_pre(i,enum_fitting) 
     &                       .le. edc1chi2perdof_pre(j,enum_fitting)) then
                           mark(i) = 1
                           mark(j) = 0
                        Else
                           mark(i) = 0
                           mark(j) = 1
                        EndIf
                     EndIF
c                     write(*,*) "i,j,mark(i),mark(j)",i,j,mark(i),mark(j) 
                  EndIF
ccccccc SEVA : there must be a better selection criterion
                  if (esevaflag.ne.0) then
                     n_shared_wires=0
                     if(mark(i) .eq. 1 .and. mark(j) .eq. 1) then         
                        Do ihit1=2,entrack_hits_pre(i,1)+1
                           hit1 = entrack_hits_pre(i,ihit1)
                           pln1 = edc1_layer_num(hit1)
                           nwire1=edc1_wire_num(hit1)
                           Do ihit2=2,entrack_hits_pre(j,1)+1
                              hit2 = entrack_hits_pre(j,ihit2)
                              pln2 = edc1_layer_num(hit2)
                              nwire2=edc1_wire_num(hit2)
                              if (pln1.eq.pln2.and.nwire1.eq.nwire2) then
                                 n_shared_wires=n_shared_wires+1
                              endif
                           enddo
                        enddo
                        if ( n_shared_wires.gt.6) then
                           if(edc1chi2perdof_pre(i,enum_fitting) 
     &                           .le. edc1chi2perdof_pre(j,enum_fitting)) then
                              mark(i) = 1
                              mark(j) = 0
                           Else
                              mark(i) = 0
                              mark(j) = 1
                           EndIf
                        endif 
                        pairs=0
                        if(mark(i) .eq. 1 .and. mark(j) .eq. 1.and.
     &                        n_shared_wires.gt.5) then        
                           do jj=1,EMAX_NUM_DC1_LAYERS,2
                              if ( num_wires(i,jj).gt.-1000.and.
     &                           num_wires(i,jj).eq.num_wires(j,jj).and.
     &                           num_wires(i,jj+1).eq.num_wires(j,jj+1) )
     &                           pairs= pairs+1
                           enddo
                        endif
                        if ( pairs.gt.1) then
                           if(edc1chi2perdof_pre(i,enum_fitting) 
     &                           .le. edc1chi2perdof_pre(j,enum_fitting)) then
                              mark(i) = 1
                              mark(j) = 0
                           Else
                              mark(i) = 0
                              mark(j) = 1
                           EndIf
                        endif
                     endif
                  endif
cccccc SEVA's correction end               
               EndDo
            EndDo
         EndIF                  ! entracks_pre>1
         
         tnew = 0
         Do i=1,entracks_pre
c            write(*,*) "i,mark(i)",i,mark(i)
            if(mark(i) .eq. 1) then
               if(tnew .eq. 0) then ! 1st hit
                  tnew = 1
                  ex_fp(1) = ex_fp_pre1(i,enum_fitting)
                  ey_fp(1) = ey_fp_pre1(i,enum_fitting)
                  ez_fp(1) = 0.
                  exp_fp(1) = exp_fp_pre1(i,enum_fitting)
                  eyp_fp(1) = eyp_fp_pre1(i,enum_fitting)
                  enfree_fp(1) = enfree_pre(i)
                  echi2_fp(1) = edc1chi2_pre(i,enum_fitting)
                  echi2perdof_fp(1) = edc1chi2perdof_pre(i,enum_fitting)
                  entrack_hits(1,1) = entrack_hits_pre(i,1)
                  if (esevaflag.ne.0) then
                     do ihit=1,on_track_hits_pre(i,1)+1
                        on_track_hits(1,ihit)=on_track_hits_pre(i,ihit)
                     enddo
                  endif
*     --- preparation for checking layer hits.
                  Do ihit=1,edc1_num_layers
                     have_layer_hits1(ihit) = 0
                  EndDo
*     --- fill layer hits on/off.
                  Do ihit=2,entrack_hits(1,1)+1
                     entrack_hits(1,ihit) = 
     &                    entrack_hits_pre(i,ihit)
                     hit = entrack_hits(1,ihit)
                     pl1 = edc1_layer_num(hit)
c                     pl2 = edc2_layer_num(hit)
                     have_layer_hits1(pl1)=1
                     edc1_single_residual(1,pl1) =
     &                    edc1_residual_pre(i,pl1,enum_fitting)
                     edc1_track_coord(1,pl1) =
     &                    edc1_track_coord_pre(i,pl1,enum_fitting)
                  EndDo
*     --- if have no hits, reset to default.
                  Do ihit=1,edc1_num_layers
                     if(have_layer_hits1(pl1) .eq. 0) then
                        edc1_single_residual(1,pl1) = 10000.
                        edc1_track_coord(1,pl1)= 10000.
                     EndIF
                  EndDo
               Else             ! tnew>0
                  j=tnew
                  Do while(j .gt. 0)
c     --- start tnew. if current chi2 less than chi2(j),
c     --- move (j) to (j+1) if j+1<=entracks_max
                     if(edc1chi2perdof_pre(i,enum_fitting) 
     &                    .lt. echi2perdof_fp(j)) then
                        if(j+1 .le. entracks_max) then
                           ex_fp(j+1) = ex_fp(j)
                           ey_fp(j+1) = ey_fp(j)
                           ez_fp(j+1) = 0.
                           exp_fp(j+1) = exp_fp(j)
                           eyp_fp(j+1) = eyp_fp(j)
                           enfree_fp(j+1) = enfree_fp(j)
                           echi2_fp(j+1) = echi2_fp(j)
                           echi2perdof_fp(j+1) = echi2perdof_fp(j)
                           entrack_hits(j+1,1) = entrack_hits(j,1)
                           if (esevaflag.ne.0) then
                              do ihit=1,on_track_hits(j,1)+1
                                 on_track_hits(j+1,ihit)=on_track_hits(j,ihit)
                              enddo
                           endif
                           Do ihit=2,entrack_hits(j+1,1)+1
                              entrack_hits(j+1,ihit) = 
     &                             entrack_hits(j,ihit)
                           EndDo
c     ================================================
c                              if(j.gt.1500)then
c                                 write(*,*)"e_select_good_tracks.f(1): "
c     &                                ,j+1, "   Danger !"
c                              endif
c     =================================================
                           Do ihit=1,edc1_num_layers
                              edc1_single_residual(j+1,ihit) = 
     &                             edc1_single_residual(j,ihit)
                              edc1_track_coord(j+1,ihit) = 
     &                             edc1_track_coord(j,ihit)
                           EndDo
                        EndIf   ! J=1<=entracks_max or not
c     --- if j=1, this is the 1st index, so we should fill values now.
                        if(j .eq. 1) then
                           ex_fp(1) = ex_fp_pre1(i,enum_fitting)
                           ey_fp(1) = ey_fp_pre1(i,enum_fitting)
                           ez_fp(1) = 0.
                           exp_fp(1) = exp_fp_pre1(i,enum_fitting)
                           eyp_fp(1) = eyp_fp_pre1(i,enum_fitting)
                           enfree_fp(1) = enfree_pre(i)
                           echi2_fp(1) = edc1chi2_pre(i,enum_fitting)
                           echi2perdof_fp(1) = 
     &                          edc1chi2perdof_pre(i,enum_fitting)
                           entrack_hits(1,1) = entrack_hits_pre(i,1)
*     --- preparation for checking layer hits.
                           Do ihit=1,edc1_num_layers
                              have_layer_hits1(ihit) = 0
                           EndDo
*     --- fill layer hits on/off.
                           if (esevaflag.ne.0) then
                              do ihit=1,on_track_hits(i,1)+1
                                 on_track_hits(1,ihit)=on_track_hits_pre(i,ihit)
                              enddo
                           endif
                           Do ihit=2,entrack_hits(1,1)+1
                              entrack_hits(1,ihit) = 
     &                             entrack_hits_pre(i,ihit)
                              hit = entrack_hits(1,ihit)
                              pl1 = edc1_layer_num(hit)
                              have_layer_hits1(pl1)=1
                              edc1_single_residual(1,pl1) =
     &                             edc1_residual_pre(i,pl1,enum_fitting)
                              edc1_track_coord(1,pl1) =
     &                             edc1_track_coord_pre(i,pl1,enum_fitting)
                              edc1_tcoord(1,pl1) =
     &                             edc1_track_coord_pre(i,pl1,enum_fitting)!track coord(by Gogami)
                              edc1_wcoord(1,pl1) = 
     &                             edc1_track_coord_pre(i,pl1,enum_fitting)!wire coord(by Gogami)
     &                             -edc1_residual_pre(i,pl1,enum_fitting)
                              edc1_wnum(1,pl1) = edc1_wire_num(hit)!wire number(by Gogami)
                           EndDo
*     --- if have no hits, reset to default.
                           Do ihit=1,edc1_num_layers
                              if(have_layer_hits1(pl1) .eq. 0) then
                                 edc1_single_residual(1,pl1) = 10000.
                                 edc1_track_coord(1,pl1)= 10000.
                              EndIF
                           EndDo
                        EndIf   ! j=1 or not
c     --- if current chi2 is greater than chi2(j),
c     --- new values are put in arrays(j+1) 
                     Else       ! current chi2 > chi2(j)
                        if(j+1 .le. entracks_max) then
                           ex_fp(j+1) = ex_fp_pre1(i,enum_fitting)
                           ey_fp(j+1) = ey_fp_pre1(i,enum_fitting)
                           ez_fp(j+1) = 0.
                           exp_fp(j+1) = exp_fp_pre1(i,enum_fitting)
                           eyp_fp(j+1) = eyp_fp_pre1(i,enum_fitting)
                           enfree_fp(j+1) = enfree_pre(i)
                           echi2_fp(j+1) = edc1chi2_pre(i,enum_fitting)
                           echi2perdof_fp(j+1) = 
     &                          edc1chi2perdof_pre(i,enum_fitting)
                           entrack_hits(j+1,1) = entrack_hits_pre(i,1)
*     --- preparation for checking layer hits.
                           Do ihit=1,edc1_num_layers
                              have_layer_hits1(ihit) = 0
                           EndDo
*     --- fill layer hits on/off.
                           if (esevaflag.ne.0) then
                              do ihit=1,on_track_hits_pre(j,1)+1
                                 on_track_hits(j+1,ihit)=on_track_hits_pre(i,ihit)
                              enddo
                           endif
                           Do ihit=2,entrack_hits(j+1,1)+1
                              entrack_hits(j+1,ihit) = 
     &                             entrack_hits_pre(i,ihit)
                              hit = entrack_hits(j+1,ihit)
                              pl1 = edc1_layer_num(hit)
                              have_layer_hits1(pl1)=1
c     ================================================
c                              if(j+1.gt.1500)then
c                                 write(*,*)"e_select_good_tracks.f : "
c     &                                ,j+1, "   Danger !"
c                              endif
c     =================================================
                              edc1_single_residual(j+1,pl1) =
     &                             edc1_residual_pre(i,pl1,enum_fitting)
                              edc1_track_coord(j+1,pl1) =
     &                             edc1_track_coord_pre(i,pl1,enum_fitting)
                              edc1_tcoord(j+1,pl1) =
     &                             edc1_track_coord_pre(i,pl1,enum_fitting) !track coord(by Gogami)
                              edc1_wcoord(j+1,pl1) = 
     &                             edc1_track_coord_pre(i,pl1,enum_fitting) !wire coord(by Gogami)
     &                             -edc1_residual_pre(i,pl1,enum_fitting)
                              edc1_wnum(j+1,pl1) = edc1_wire_num(hit) !wire number(by Gogami)
                           EndDo
*     --- if have no hits, reset to default.
                           Do ihit=1,edc1_num_layers
                              if(have_layer_hits1(pl1) .eq. 0) then
                                 edc1_single_residual(j+1,pl1) = 10000.
                                 edc1_track_coord(j+1,pl1)= 10000.
                              EndIF
                           EndDo
                        EndIf   ! j+1<entracks_max
c     --- since we found chi2 position, we don't need comparison.
                        j=1
                     EndIF      ! current chi2<chi2(j) or not
                     j=j-1
                  EndDo         ! j>0 while loop
                  if(tnew+1 .le. entracks_max) tnew=tnew+1
               EndIF            ! tnew>0 or not   
            EndIf               ! mark=1 : keep the track
         EndDo                  ! track loop
         Call HF1(eidtrkdiff,float(entracks_pre-tnew),1.)
         entracks_fp = tnew
      endif
         
      
      Call HF1(eiddc1ntracksfp,float(entracks_fp),1.)
      Call HF1(eiddc1ntracksfpzoom,float(entracks_fp),1.)
      if(entracks_fp .gt. 0) then
         Call HF1(eiddc1chi2perdoffp,echi2perdof_fp(1),1.)
         Call HF1(eiddc1chi2perdoffpzoom,echi2perdof_fp(1),1.)
         Call HF1(eiddc1fpx,ex_fp(1),1.)
         Call HF1(eiddc1fpy,ey_fp(1),1.)
         Call HF1(eiddc1fpxp,exp_fp(1),1.)
         Call HF1(eiddc1fpyp,eyp_fp(1),1.)
         Call HF2(eiddc1fpxy,ex_fp(1),ey_fp(1),1.)
         Call HF2(eiddc1fpxpyp,exp_fp(1),eyp_fp(1),1.)
         Call HF2(eiddc1fpxxp,ex_fp(1),exp_fp(1),1.)
         Call HF2(eiddc1fpxyp,ex_fp(1),eyp_fp(1),1.)
         Call HF2(eiddc1fpyxp,ey_fp(1),exp_fp(1),1.)
         Call HF2(eiddc1fpyyp,ey_fp(1),eyp_fp(1),1.)
         Do ihit=2,entrack_hits(1,1)+1
            hit = entrack_hits(1,ihit)
            pl1 = edc1_layer_num(hit)
            res = edc1_single_residual(1,pl1)
            Call HF1(eiddc1singleresidual(pl1),res,1.)
            call hf2(5720+pl1,ex_fp(1),res,1.)
         EndDo
      EndIF                     ! entracks_fp>0

      return
      end
