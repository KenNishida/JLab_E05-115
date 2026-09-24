      SUBROUTINE H_SELECT_GOOD_TRACKS(ABORT,err)
*--------------------------------------------------
*     -
*     -   Purpose and Methods : Select the good tracks 
*     -   through the HKS
*     -                              
*     -      Required Input BANKS
*     -
*     -      Output BANKS
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     -
*     - $Log: h_select_good_tracks.f,v $
*     - Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*     -
*     - e05115 src repository for software development
*     -
*     - Revision 1.2  2005/07/06 02:32:38  sumihama
*     - Mod. hist
*     -
*     - Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*     -
*     -
*     - Revision 1.5  2005/04/08 20:51:40  miyoshi
*     - arrange index according to chi2
*     -
*     - Revision 1.4  2005/03/14 19:52:17  miyoshi
*     - change tracking variables and histogram names
*     -
*     - Revision 1.3  2005/01/24 19:44:33  miyoshi
*     - fix a bug of event selection
*     -
*     - Revision 1.2  2004/12/24 21:35:57  miyoshi
*     - change name plane to layer
*     -
*     - Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
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
      parameter (here= 'H_SELECT_GOOD_TRACKS')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      INCLUDE 'hks_physics_sing.cmn'
      INCLUDE 'hks_tracking.cmn'
      Include 'hks_id_histid.cmn'
      include 'hks_geometry.cmn'
*     
*     local variables 
      integer*4 i,j,tnew,ihit,hit,pln,itrk,sl,wi
      integer*4 goodtrack,track
      integer*4 mark(HNTRACKS_MAX)
      integer*4 have_layer_hits(HMAX_NUM_DC_LAYERS)
      real*4 dx,dy,dxp,dyp,res
      real*4 dt,wc
      
      
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '
      

      if(hntracks_fp .gt. 0) then
         HKS_DX_MIN=1e-4
         HKS_DY_MIN=1e-4
         HKS_DXP_MIN=1e-4
         HKS_DYP_MIN=1e-4
         Do i=1,HNTRACKS_MAX
            mark(i) = 1
         EndDo
         
         if(htrack_chi2min .eq. 0) htrack_chi2min = 1000.
         
*     --- add cut condition as you want
         Do i=1,hntracks_fp
            if(
     &           hchi2perdof_fp(i) .gt. htrack_chi2min
     &           ) then
               mark(i) = 0
            EndIF
         EndDo
         
*     --- check very close track as one
         if(hntracks_fp .gt. 1) then
            Do i = 1,hntracks_fp-1
               Do j = i+1,hntracks_fp
                  dx = abs(hx_fp(i)-hx_fp(j))
                  dy = abs(hy_fp(i)-hy_fp(j))
                  dxp = abs(hxp_fp(i)-hxp_fp(j))
                  dyp = abs(hyp_fp(i)-hyp_fp(j))
                  Call HF1(hidtrkdx,dx,1.)
                  Call HF1(hidtrkdy,dy,1.)
                  Call HF1(hidtrkdxp,dxp,1.)
                  Call HF1(hidtrkdyp,dyp,1.)
c                   print*,HKS_DX_MIN,HKS_DY_MIN
                   write(15,*) dx,dy,dxp,dyp
                  if(mark(i) .eq. 1 .and. mark(j) .eq. 1) then
                     if(
     &                    dx .lt. HKS_DX_MIN .and.
     &                    dy .lt. HKS_DY_MIN .and.
     &                    dxp .lt. HKS_DXP_MIN .and.
     &                    dyp .lt. HKS_DYP_MIN
     &                    ) then
                        if(hchi2perdof_fp(i) .le. hchi2perdof_fp(j)) then
                           mark(i) = 1
                           mark(j) = 0
                        Else
                           mark(i) = 0
                           mark(j) = 1
                        EndIf
                     EndIF      ! chi2 comparison
                  EndIF         ! mark = 1
               EndDo            ! track j
            EndDo               ! track i
         EndIF                  ! track>1
         
*     --- refill HKS_FOCAL_PLANE common block
*     --- Since I(Miyoshi) 
*     --- am not sure we use edel_fp, ignore them.
*     --- see hks_data_structures.cmn
         
         tnew = 0
         hdc_bestchi2_index = -1 
         hdc_bestchi2 = 1000000
         Do i=1,hntracks_fp
            if(mark(i) .eq. 1) then
               tnew = tnew + 1
               hx_fp(tnew) = hx_fp(i)
               hy_fp(tnew) = hy_fp(i)
               hz_fp(tnew) = 0.
               hxp_fp(tnew) = hxp_fp(i)
               hyp_fp(tnew) = hyp_fp(i)
               hnfree_fp(tnew) = hnfree_fp(i)
               hchi2_fp(tnew) = hchi2_fp(i)
               hchi2perdof_fp(tnew) = hchi2perdof_fp(i)
               hntrack_hits(tnew,1) = hntrack_hits(i,1)
*     --- preparation for checking layer hits.
               Do ihit=1,hdc_num_layers
                  have_layer_hits(ihit) = 0
               EndDo
*     --- fill layer hits on/off.
               Do ihit=2,hntrack_hits(tnew,1)+1
                  hntrack_hits(tnew,ihit) = 
     &                 hntrack_hits(i,ihit)
                  hit = hntrack_hits(tnew,ihit)
                  pln = hdc_layer_num(hit)
                  have_layer_hits(pln)=1
                  hdc_single_residual(tnew,pln) =
     &                 hdc_single_residual(i,pln)
                  hdc_track_coord(tnew,pln) =
     &                 hdc_track_coord(i,pln)
                  hdc_layer_wirecenter(tnew,pln)=hdc_wire_center(hit)
                  hdc_layer_wirecoord(tnew,pln)=hdc_wire_coord(hit)
c                  hdc_drift_distance(tnew,pln)=HDC_DRIFT_DIS(hit)
                  hdc_drift_distance(tnew,pln)=hdc_drift_dis(hit)
                  dt=hdc_drift_time(hit)
                  hdc_layer_drift_time(tnew,pln)=dt
                  wi=hdc_wire_num(hit)

                 if(hxp_fp(tnew).le.
     +            0.16+0.25/100.0*hx_fp(tnew).and.
     +           hxp_fp(tnew).ge.
     +                 -0.25+0.25/100.0*hx_fp(tnew)) then

             call hf1(hiddcdrifttime(pln),dt,1.)
            if(hturnon_dc_dec_hist .ne. 0 ) then
             if(pln .eq. 1 .or. pln .eq. 2) then
               sl = 6*(pln-1) 
     &              + int((wi-hdc_wire_offset_low(pln)-1)/16) + 1
            Else if(pln .eq. 3 .or. pln .eq. 4) then
               sl = 12 + 8*(pln-3) 
     &              + int((wi-hdc_wire_offset_low(pln)-1)/16) + 1
            Else if(pln .eq. 5 .or. pln .eq. 6) then
               sl = 28 + 6*(pln-5) 
     &              + int((wi-hdc_wire_offset_low(pln)-1)/16) + 1
            Else if(pln .eq. 7 .or. pln .eq. 8) then
               sl = 40 + 6*(pln-7) 
     &              + int((wi-hdc_wire_offset_low(pln)-1)/16) + 1
            Else if(pln .eq. 9 .or. pln .eq. 10) then
               sl = 52 + 8*(pln-9) 
     &              + int((wi-hdc_wire_offset_low(pln)-1)/16) + 1
            Else if(pln .eq. 11 .or. pln .eq. 12) then
               sl = 68 + 6*(pln-11) 
     &              + int((wi-hdc_wire_offset_low(pln)-1)/16) + 1
            EndIf
            
           Call HF1(hiddcdrifttimesl(sl),dt,1.)
        EndIF
      endif
      
      EndDo
*     --- if have no hits, reset to default.
      Do ihit=1,hdc_num_layers
         if(have_layer_hits(pln) .eq. 0) then
            hdc_single_residual(i,pln) = 10000.
            hdc_track_coord(i,pln)= 10000.
            hdc_layer_wirecenter(i,pln)= 10000.
            hdc_layer_wirecoord(i,pln)= 10000.
         EndIF
      EndDo
      if(hchi2perdof_fp(tnew) .le. hdc_bestchi2) then
         hdc_bestchi2_index = tnew
c     hdc_bestchi2 = hchi2perdof_fp(itrk) !itrk should 

         hdc_bestchi2 = hchi2perdof_fp(tnew)
      EndIF
      EndIf                     ! mark=1 : keep the track
      
      
      EndDo                     ! track loop
      
      Call HF1(hidtrkdiff,float(hntracks_fp-tnew),1.)
      
      hntracks_fp = tnew
      
      EndIf                     ! hntracks_fp>0
      
      Call HF1(hidhntracksfp,float(hntracks_fp),1.)
      Call HF1(hidhntracksfpzoom,float(hntracks_fp),1.)
      if(hntracks_fp .gt. 0) then
         if(hxp_fp(1).le.0.16+0.25/100.0*hx_fp(1).and.
     +        hxp_fp(1).ge.-0.25+0.25/100.0*hx_fp(1)) then
            Call HF1(hidchi2perdoffp,hchi2perdof_fp(1),1.)
            Call HF1(hidchi2perdoffpzoom,hchi2perdof_fp(1),1.)
            
            Call HF1(hidhxfp,hx_fp(1),1.)
            Call HF1(hidhyfp,hy_fp(1),1.)
            Call HF1(hidhxpfp,hxp_fp(1),1.)
            Call HF1(hidhypfp,hyp_fp(1),1.)
            Call HF2(hidfpxy,hx_fp(1),hy_fp(1),1.)
            Call HF2(hidfpxpyp,hxp_fp(1),hyp_fp(1),1.)
            Call HF2(hidfpxxp,hx_fp(1),hxp_fp(1),1.)
            Call HF2(hidfpxyp,hx_fp(1),hyp_fp(1),1.)
            Call HF2(hidfpyxp,hy_fp(1),hxp_fp(1),1.)
            Call HF2(hidfpyyp,hy_fp(1),hyp_fp(1),1.)
            Do ihit=2,hntrack_hits(1,1)+1
               hit = hntrack_hits(1,ihit)
               pln = hdc_layer_num(hit)
               res = hdc_single_residual(1,pln)
               
               Call HF1(hidsingresidual(pln),res,1.)
            EndDo
         endif
         
c     itrk = hdc_bestchi2_index
c     if(hdc_bestchi2_index .eq. 0) itrk=hntracks_fp
c     if(itrk .gt. 0) then
c     Call HF1(hidchi2final,hchi2perdof_fp(itrk),1.)
c     Call HF1(hidchi2finalzoom,hchi2perdof_fp(itrk),1.)
c     Do ihit=2,hntrack_hits(itrk,1)+1
c     hit = hntrack_hits(itrk,ihit)
c     pln = hdc_layer_num(hit)
c     res = hdc_single_residual(itrk,pln)
c     Call HF1(hidsingresfinal(pln),res,1.)
c     EndDo
c     Call HF1(hidhxfpfinal,hx_fp(itrk),1.)
c     Call HF1(hidhyfpfinal,hy_fp(itrk),1.)
c     Call HF1(hidhxpfpfinal,hxp_fp(itrk),1.)
c     Call HF1(hidhypfpfinal,hyp_fp(itrk),1.)
c     Call HF2(hidfpxyfinal,hx_fp(itrk),hy_fp(itrk),1.)
c     Call HF2(hidfpxpypfinal,hxp_fp(itrk),hyp_fp(itrk),1.)
c     EndIf                  ! itrk>0
      EndIF                     ! hntracks_fp>0
      
      
c     Write(*,*) '(hselectgoodtrack) hntracks_fp=',hntracks_fp
      
      return
      end
      
