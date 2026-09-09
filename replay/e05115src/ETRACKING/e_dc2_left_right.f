      subroutine e_dc2_left_right(ABORT,err)
*     Warning: This routine contains lots of gobbledeguk that won't work if the
*     number of chambers is changed to 3.
*     
*     
*     This routine fits stubs to all possible left-right combinations of
*     drift distances and chooses the set with the minimum chi**2
*     It then fills the SDC_WIRE_COORD variable for each hit in a good
*     space point.
*     d. f. geesaman           31 August 1993
*     $Log: e_dc2_left_right.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.5  2005/03/14 19:52:17  miyoshi
*     change tracking variables and histogram names
*
*     Revision 1.4  2005/03/07 16:35:33  miyoshi
*     add drift distance hist
*
*     Revision 1.3  2004/12/24 21:35:57  miyoshi
*     change name plane to layer
*
*     Revision 1.2  2004/12/24 19:47:07  miyoshi
*     change minor
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.11  1996/09/05 19:54:51  saw
*     (JRA) Cosmetic
*     
*     Revision 1.10  1996/01/17 19:01:59  cdaq
*     (JRA)
*     
*     Revision 1.9  1995/10/10 15:59:06  cdaq
*     (JRA) Remove sdc_sing_wcoord stuff
*     
*     Revision 1.8  1995/08/31 18:44:23  cdaq
*     (JRA) Fix some logic in small angle L/R determination loop
*     
*     Revision 1.7  1995/07/20  18:57:39  cdaq
*     (SAW) Declare jibset for f2c compatibility
*     
*     Revision 1.6  1995/05/22  19:45:42  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.5  1995/05/11  21:05:32  cdaq
*     (JRA) Fix errors in left right selection.  Add some commented out code
*     
*     Revision 1.4  1995/04/01  20:42:35  cdaq
*     (SAW) Fix typos
*     
*     Revision 1.3  1994/12/01  21:55:08  cdaq
*     (SAW) Generalize for variable # of chambers.
*     Add Small Ang approx for Brookhaven chambers
*     
*     Revision 1.2  1994/11/22  21:14:25  cdaq
*     (SPB) Recopied from hms file and modified names for SOS
*     (SAW) Don't count on Mack's monster if statement working for
*     sdc_num_chambers > 2
*     
*     Revision 1.1  1994/02/21  16:14:42  cdaq
*     Initial revision
*     
*     
      implicit none
      save

      include 'hes_data_structures.cmn'
      include 'hes_tracking.cmn'
      include 'hes_geometry.cmn'
      include 'hes_id_histid.cmn'
*     
      external jbit             ! cernlib bit routine
      integer*4 jbit
*     integer*4 jibset                  ! Declare to help f2c
*     
*     local variables
*     
      character*12 here
      parameter (here= 'e_dc2_left_right')
*     
      logical ABORT
      character*(*) err
      integer*4 isp, ihit,iswhit, idummy, pmloop, ich,hit,la, i, gh
      integer*4 nplusminus
      integer*4 numhits,npaired,ihit2, itrack
      integer*4 hits(edc2max_hits_per_point), pl(edc2max_hits_per_point)
      integer*4 pindex
      real*4 wc(edc2max_hits_per_point)
      integer*4 layer, isa_y1, isa_y2
      integer*4 plusminusknown(edc2max_hits_per_point)
      real*4 plusminus(edc2max_hits_per_point)
      real*4 plusminusbest(edc2max_hits_per_point)
      real*4 chi2,dispol, drift_direction
      real*4 minchi2
      real*4 stub(4)
      logical smallAngOk
*     
      ABORT= .FALSE.
      err=' '


*     --- Histogram for dc sp bank 
      call e_fill_dc2_sp_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif

      if(edc2nspace_points_tot .le. 0) Return
      
      do isp=1,edc2nspace_points_tot ! loop over all space points
         glayeredc2(isp) = 0
         minchi2=1e10
         smallAngOK = .FALSE.
         isa_y1 = 0
         isa_y2 = 0
         numhits=edc2space_point_hits(isp,1)
         nplusminus=2**numhits
*     
*     Identify which layer the space point is in.
*     
         do ihit=1,numhits
            hits(ihit)=edc2space_point_hits(isp,2+ihit)
            pl(ihit)=EDC2_LAYER_NUM(hits(ihit))
            
            glayeredc2(isp)=ibset(glayeredc2(isp),pl(ihit)-1)
            
c     if(pl(ihit).ge.1 .and. pl(ihit).le.6)then
c     glayeredc21(isp)=jibset(glayeredc21(isp),pl(ihit)-1)
c     else
c     glayeredc22(isp)=jibset(glayeredc22(isp),pl(ihit)-7)
c     endif
cccc            print *, 'ihit', ihit, hits(ihit), pl(ihit)
            
            wc(ihit)=EDC2_WIRE_CENTER(hits(ihit))
            plusminusknown(ihit) = 0
            if(pl(ihit).eq.2 ) isa_y1 = ihit
            if(pl(ihit).eq.5 ) isa_y2 = ihit
         enddo
         
         
*     djm 10/2/94 check bad sdc pattern units to set the index for the inverse
*     matrix SAAINV(i,j,pindex).
*     
         if(glayeredc2(isp).eq.X'3F') then
            pindex=edc2_num_layers+1
         else if (glayeredc2(isp).eq.X'3E') then
            pindex= 1
         else if (glayeredc2(isp).eq.X'3D') then
            pindex= 2
         else if (glayeredc2(isp).eq.X'3B') then
            pindex= 3
         else if (glayeredc2(isp).eq.X'37') then
            pindex= 4
         else if (glayeredc2(isp).eq.X'2F') then
            pindex= 5
         else if (glayeredc2(isp).eq.X'1F') then
            pindex= 6
         else
            pindex=-1
         endif
         
*     check if small angle L/R determination of Y and Y' layers is possible
*DK         if(hSmallAngleApprox.ne.0) then
*DK            if(h_hms_style_chambers.eq.1) then
*DK               if(isa_y1.gt.0 .AND. isa_y2.gt.0) then
*DK                  if(wc(isa_y2).le.wc(isa_y1)) then
*DK                     plusminusknown(isa_y1) = -1
*DK                     plusminusknown(isa_y2) = 1
*DK                  else
*DK                     plusminusknown(isa_y1) = 1
*DK                     plusminusknown(isa_y2) = -1
*DK                  endif
*DK                  nplusminus = 2**(numhits-2)
*DK               endif
*DK            else                ! HKS chambers
     
*     Brookhaven chamber L/R code
*     Can we assume that hits are sorted by layer?  As best I (SAW) can
*     tell, we can not.
*     
               ihit = 1
               npaired = 0
               do ihit=1,numhits
                  if(pl(ihit)-2*(pl(ihit)/2) .eq. 1) then ! Odd layer
                     do ihit2=1,numhits ! Look for the adjacent layer
                        if(pl(ihit2)-pl(ihit).eq.1) then ! Adjacent layer found
                           if(wc(ihit2).le.wc(ihit)) then
                              plusminusknown(ihit) = -1
                              plusminusknown(ihit2) = 1
                           else
                              plusminusknown(ihit) = 1
                              plusminusknown(ihit2) = -1
                           endif
                           npaired = npaired + 2
                        endif
                     enddo
                  endif
               enddo
*DK            endif
            nplusminus = 2**(numhits-npaired)
*     Let's hope that following code will work with nplusminus = 1
*DK         endif
         
*     use bit value of integer word to set + or -
         do pmloop=0,nplusminus-1
            iswhit = 1
            do ihit=1,numhits
               if(plusminusknown(ihit).ne.0) then
                  plusminus(ihit) = float(plusminusknown(ihit))
               else
                  if(jbit(pmloop,iswhit).eq.1) then
                     plusminus(ihit)=1.0
                  else
                     plusminus(ihit)=-1.0
                  endif
                  iswhit = iswhit + 1
               endif
            enddo
            
            if (pindex.ge.0 .and. pindex.le.14) then
               call e_dc2_find_best_stub(numhits,hits,pl,pindex,
     &              plusminus,stub,chi2)
*DK               if(hdebugstubchisq.ne.0) then
*DK                  write(hluno,'(12H hks pmloop=,i4,8H   chi2=,e14.6)')
*DK     &                 pmloop,chi2
*DK               endif
cc               print *, 'hks pmloop=',pmloop,plusminus, chi2 
               if (chi2.lt.minchi2)  then
                  minchi2=chi2
                  do idummy=1,numhits
                     plusminusbest(idummy)=plusminus(idummy)
                  enddo
                  do idummy=1,4
                     edc2beststub(isp,idummy)=stub(idummy)
                  enddo
               endif            ! end if on lower chi2
            else                ! if pindex<0 or >14
cc               write(6,*) 'pindex=',pindex,' in h_left_right'
            endif
         enddo                  ! end loop on possible left-right

cc               write(6,*) 'pindex=',pindex,' in h_left_right', minchi2
*     
*     calculate final coordinate based on plusminuhbest
*     
         do ihit=1,numhits
            hit = edc2space_point_hits(isp,2+ihit)
            la = EDC2_LAYER_NUM(hit)
            dispol = plusminusbest(ihit)*EDC2_DRIFT_DIS(hit)
            EDC2_WIRE_COORD(hit)= EDC2_WIRE_CENTER(hit) + dispol
            Call HF1(eiddc2driftdis(la),dispol,1.)
         enddo
*     
*     stubs are calculated in rotated coordinate system
*     use first hit to determine chamber
*
         layer=EDC2_LAYER_NUM(hits(1))
         stub(3)=(edc2beststub(isp,3) - edc2tanbeta(layer))
     &        /(1.0 + edc2beststub(isp,3)*edc2tanbeta(layer))
         stub(4)=edc2beststub(isp,4)
     &        /(edc2beststub(isp,3)*edc2sinbeta(layer)+edc2cosbeta(layer))
         
         stub(1)=edc2beststub(isp,1)*edc2cosbeta(layer) 
     &        - edc2beststub(isp,1)*stub(3)*edc2sinbeta(layer)
         stub(2)=edc2beststub(isp,2) 
     &        - edc2beststub(isp,1)*stub(4)*edc2sinbeta(layer)
         edc2beststub(isp,1)=stub(1)
         edc2beststub(isp,2)=stub(2)
         edc2beststub(isp,3)=stub(3)
         edc2beststub(isp,4)=stub(4)
*     
      enddo                             ! end loop over space points

**********************************************************
* link with entracks_pre defined in e_dc1_left_right.f
* DK
**********************************************************

cc      print *, 'tot sp', edc2nspace_points_tot
      entracks_pre = edc2nspace_points_tot
      Do itrack=1,entracks_pre
c     --- reset vectors  
         edc2chi2_pre(itrack) = -100000.
         edc2chi2perdof_pre(itrack) = -100000.
cc         ex_fp_pre1(itrack,1) = edc1beststub(itrack,1)
cc         ey_fp_pre1(itrack,1) = edc1beststub(itrack,2)
cc         exp_fp_pre1(itrack,1) = edc1beststub(itrack,3)
cc        eyp_fp_pre1(itrack,1) = edc1beststub(itrack,4)
*     --- ez_fp=0
         numhits = edc2space_point_hits(itrack,1)
         entrack_hits_pre(itrack,1) = 0
         Do i=1,EMAX_NUM_DC2_LAYERS
            edc2_track_coord_pre(itrack,i) = -10000.
            edc2_residual_pre(itrack,i) = -10000.
         EndDo
         gh = 0
         Do ihit=1,numhits
cc            print *, 'insider1', ihit
            gh = gh + 1
            entrack_hits_pre(itrack,gh+1) = 
     &           edc2space_point_hits(itrack,ihit+2)
            ihit2 = edc2space_point_hits(itrack,ihit+2)
            drift_direction = 
     &           plusminusbest(ihit)*EDC2_DRIFT_DIS(ihit2)
            layer=EDC2_LAYER_NUM(ihit2)
            EDC2_WIRE_COORD(ihit2)=
     &           EDC2_WIRE_CENTER(ihit2) +
     &           drift_direction            
cc            print *, ' inside', ihit, ihit2, plusminusbest(ihit), EDC2_DRIFT_DIS(ihit2) 
         EndDo                  ! numhits loop
         entrack_hits_pre(itrack,1) = gh
cc         print *, 'out', entrack_hits_pre(itrack,1), itrack, entracks_pre
      EndDo                     ! entracks_fp loop

cc      print *, 'out2', entrack_hits_pre(itrack,1), itrack, entracks_pre
*     
*     write out results if sdebugflagstubs is set
      if(edebugflagstubs.ne.0) then
         call e_print_dc2_stubs
      endif
      return
      end
