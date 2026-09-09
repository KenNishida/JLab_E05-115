      subroutine e_dc1_left_right(ABORT,err)

*     Warning: This routine contains lots of gobbledeguk that won't work if the
*     number of chambers is changed to 3.
*     
*     
*     This routine fits stubs to all possible left-right combinations of
*     drift distances and chooses the set with the minimum chi**2
*     It then fills the SDC_WIRE_COORD variable for each hit in a good
*     space point.
*     d. f. geesaman           31 August 1993
*     $Log: e_dc1_left_right.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.2  2005/03/10 16:48:24  miyoshi
*     change tracking variable name
*
*     Revision 1.1.1.1  2004/08/30 21:21:41  miyoshi
*     new dir
*
      
*     For HKS DC test. Adapted from s_track
*     L.Yuan 06/07/2003
      
*     Revision 1.12  1999/11/04 20:36:47  saw
*     Linux/G77 compatibility fixes
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
      INCLUDE 'hes_data_structures.cmn'
      INCLUDE 'hes_tracking.cmn'
      INCLUDE 'hes_geometry.cmn'
      INCLUDE 'gen_event_info.cmn'
*     
      external jbit             ! cernlib bit routine
      integer*4 jbit
      integer*4 jibset          ! Declare to help f2c
      integer*4 jieor           ! Declare to help f2c
*     
*     local variables
*     
      character*12 here
      parameter (here= 'e_dc1_left_right')
*     
      logical ABORT
      character*(*) err
      integer*4 isp, ihit,iswhit, idummy, pmloop, ich,i,gh,j
      integer*4 nplusminus
      integer*4 numhits,npaired,ihit2
      integer*4 hits(edc1max_hits_per_point), pl(edc1max_hits_per_point)
      integer*4 pindex
      real*4 wc(edc1max_hits_per_point),wn(edc1max_hits_per_point)
      integer*4 layer, isa_y1, isa_y2,itrack
      integer*4 plusminusknown(edc1max_hits_per_point)
      real*4 plusminus(edc1max_hits_per_point)
      real*4 plusminusbest(edc1max_hits_per_point)
      real*4 chi2,residual
      real*4 minchi2,stub(4)
      real*4 n_missing_hits
      real*4 drift_direction
      logical smallAngOk
      integer*4 remap(enum_fpray_param)
      data remap/5,6,3,4/
      save remap

*     
      ABORT= .FALSE.
      err=' '
      

      entracks_pre = 0
************************************************************      
*     --- Histogram for dc sp bank 
************************************************************      
      call e_fill_dc1_sp_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif
      
      if (edc1nspace_points_tot.le.0) return 
      
************************************************************      
*     --- edc1nspace_points_tot Loop start 
************************************************************      
      do isp=1,edc1nspace_points_tot ! loop over all space points

*     --- reset vector
         Do ihit=1,EMAX_NUM_DC1_LAYERS
            edc1_i_have_hit(ihit) = 0
         EndDo

         minchi2=10000000000000.0
         smallAngOK = .FALSE.
         isa_y1 = 0
         isa_y2 = 0
         numhits=edc1space_point_hits(isp,1)
         nplusminus=2**numhits
     
************************************************************      
**     Identify which layer the space point is in.
************************************************************      
         ich = 1
         do ihit=1,numhits
            hits(ihit) = edc1space_point_hits(isp,2+ihit)
            pl(ihit) = EDC1_LAYER_NUM(hits(ihit))
            wc(ihit) = EDC1_WIRE_CENTER(hits(ihit))
            wn(ihit) = EDC1_WIRE_NUM(hits(ihit))
            edc1_i_have_hit(pl(ihit)) = 1
            plusminusknown(ihit) = 0
         enddo
         
*     djm 10/2/94 check bad edc1 pattern units 
*     to set the index for the inverse
*     matrix SAAINV(i,j,pindex).
************************************************************      
**     Check missing layers 
************************************************************      
         pindex = 11
         n_missing_hits = 0
         Do ihit=1,EMAX_NUM_DC1_LAYERS
*            Write(*,*) 'layer',ihit,edc1_i_have_hit(ihit)
            if(edc1_i_have_hit(ihit).eq.0) then
               pindex = ihit
               n_missing_hits = n_missing_hits + 1
            endif
         EndDo
*     now we require 9 hits
         if(n_missing_hits .gt. 1) pindex = -1
         
************************************************************      
**     Brookhaven chamber L/R code
**     Can we assume that hits are sorted by layer?  As best I (SAW) can
**     tell, we can not.
************************************************************      
     
************************************************************      
***     Get plusminusknown for paired layers
***     wn(odd layer) < wn (even layer) -> 1
***     wn(odd layer) > wn (even layer) -> -1
************************************************************      
         npaired = 0
         do ihit=1,numhits-1
            if(pl(ihit)-2*(pl(ihit)/2) .eq. 1) then ! ihit is for odd layer
               do ihit2=2,numhits ! Look for the adjacent layer
                  if(pl(ihit2)-pl(ihit).eq.1) then ! Adjacent layer found
c     
c     caution for layer 7&8. order of wire number is defferent
c     from others due to up-down symmetry. (11/20/2003 Miyoshi)
c     
c                     write(*,*) gen_event_id_number,ihit,ihit2,pl(ihit),pl(ihit2),wn(ihit),wn(ihit2)
                     if(pl(ihit) .eq. 3) then !layer 7&8
                        if(wn(ihit2).lt.wn(ihit)) then
                           plusminusknown(ihit) =   1
                           plusminusknown(ihit2) = -1
                        else
                           plusminusknown(ihit) =  -1
                           plusminusknown(ihit2) =  1
                        EndIf
                     Else       ! others
                        if(wn(ihit2).le.wn(ihit)) then
                           plusminusknown(ihit) =   1
                           plusminusknown(ihit2) = -1
                        else
                           plusminusknown(ihit) =  -1
                           plusminusknown(ihit2) =  1
                        endif
                     EndIf
                     npaired = npaired + 2 ! Num. of paired hits 
                  endif
               enddo
            endif ! odd layer
         enddo
         
         nplusminus = 2**(numhits-npaired) 
         
*     Let's hope that following code will work with nplusminus = 1
*     use bit value of integer word to set + or -
         
     
************************************************************      
***     pmloop is for non-pair layer +/-. the best one is filled.
************************************************************      
     
         do pmloop=0,nplusminus-1
            iswhit = 1
            do ihit=1,numhits
               if(plusminusknown(ihit).ne.0) then ! paired
                  plusminus(ihit) = plusminusknown(ihit) 
               else
                  if(jbit(pmloop,iswhit).eq.1) then
                     plusminus(ihit)=1.0
                  else
                     plusminus(ihit)=-1.0
                  endif
                  iswhit = iswhit + 1
               endif
            enddo

************************************************************      
*     check missing layer or not (1-10 and 11).
*     solve left-right by 3by3 tracking.
*     Fill plusminusbest, ebeststub
************************************************************      

            if (pindex.ge.0 .and. pindex.le.11) then
c     get chi2 and stub (coordinates from local line fit)
               call e_dc1_find_best_stub
     &              (numhits,hits,pl,pindex,plusminus,stub,chi2)
               if(edebugstubchisq.ne.0) then
                  write(*,*) edc1nspace_points_tot, isp 
                  write(*,'('' hes pmloop='',i4,''   chi2='',e14.6)')
     &                 pmloop,chi2
               endif
               do idummy=1,numhits
                  plusminusbest(idummy)=plusminus(idummy)
               enddo
               do idummy=1,4
                  edc1beststub(isp,idummy)=stub(idummy) ! stub(4) is 0
               enddo
               
               if (chi2.lt.minchi2)  then
                  minchi2=chi2
                  do idummy=1,numhits 
                     plusminusbest(idummy)=plusminus(idummy)
                  enddo
                  do idummy=1,4
                     edc1beststub(isp,idummy)=stub(idummy)
                  enddo
               endif            ! end if on lower chi2
            else                ! if pindex<0 or >12
c     Write(*,*) 'Missing layer gt 1 in e_left_right'
            endif
         enddo                  ! end loop on possible left-right
         
     
************************************************************      
*     calculate final coordinate based on plusminusbest
************************************************************      
     
         do ihit=1,numhits
            hits(ihit) = edc1space_point_hits(isp,2+ihit)
            drift_direction = 
     &           plusminusbest(ihit)*EDC1_DRIFT_DIS(hits(ihit))
            layer=EDC1_LAYER_NUM(hits(ihit))
            EDC1_WIRE_COORD(hits(ihit))=
     &           EDC1_WIRE_CENTER(hits(ihit)) +
     &           drift_direction
            pl(ihit) = EDC1_LAYER_NUM(hits(ihit))
            wc(ihit) = EDC1_WIRE_CENTER(hits(ihit))
            wn(ihit) = EDC1_WIRE_NUM(hits(ihit))
c      Write(*,*) pl(ihit),plusminusbest(ihit)
c      write(*,*) wc(ihit),wn(ihit)
         enddo
*     
*     stubs are calculated in rotated coordinate system
*     use first hit to determine chamber
         layer=EDC1_LAYER_NUM(hits(1))
         stub(3)=(edc1beststub(isp,3) - edc1tanbeta(layer))
     &        /(1.0 + edc1beststub(isp,3)*edc1tanbeta(layer))
         stub(4)=edc1beststub(isp,4)
     &        /(edc1beststub(isp,3)*edc1sinbeta(layer)+edc1cosbeta(layer))
         
         stub(1)=edc1beststub(isp,1)*edc1cosbeta(layer) 
     &        - edc1beststub(isp,1)*stub(3)*edc1sinbeta(layer)
         stub(2)=edc1beststub(isp,2) 
     &        - edc1beststub(isp,1)*stub(4)*edc1sinbeta(layer)
         edc1beststub(isp,1)=stub(1)
         edc1beststub(isp,2)=stub(2)
         edc1beststub(isp,3)=stub(3)
         edc1beststub(isp,4)=stub(4)
      enddo                     ! end loop over space points
      
*     --- fill entracks_pre
      
      entracks_pre = edc1nspace_points_tot
      Do itrack=1,entracks_pre
c     --- reset vectors  
cDK         edc1chi2_pre(itrack,1) = -100000.
cDK         edc1chi2perdof_pre(itrack,1) = -100000.
cDK         ex_fp_pre(itrack,1) = edc1beststub(itrack,1)
cDK         ey_fp_pre(itrack,1) = edc1beststub(itrack,2)
cDK         exp_fp_pre(itrack,1) = edc1beststub(itrack,3)
cDK         eyp_fp_pre(itrack,1) = edc1beststub(itrack,4)
*     --- ez_fp=0
         numhits = edc1space_point_hits(itrack,1)
         entrack_hits_pre(itrack,1) = 0
cDK         Do i=1,EMAX_NUM_DC1_LAYERS
cDK            edc1_track_coord_pre(itrack,i,1) = -10000.
cDK            edc1_residual_pre(itrack,i,1) = -10000.
cDK         EndDo
         gh = 0
         Do ihit=1,numhits
            gh = gh + 1
            entrack_hits_pre(itrack,gh+1) = 
     &           edc1space_point_hits(itrack,ihit+2)
            ihit2 = entrack_hits_pre(itrack,gh+1)
            drift_direction = 
     &           plusminusbest(ihit)*EDC1_DRIFT_DIS(ihit2)
            layer=EDC1_LAYER_NUM(ihit2)
            EDC1_WIRE_COORD(ihit2)=
     &           EDC1_WIRE_CENTER(ihit2) +
     &           drift_direction            
cDK            edc1_track_coord_pre(itrack,layer,1)=
cDK     &           edc1layer_coeff(remap(1),layer)*ex_fp_pre(itrack,1)
cDK     &           +edc1layer_coeff(remap(2),layer)*ey_fp_pre(itrack,1)
cDK     &           +edc1layer_coeff(remap(3),layer)*exp_fp_pre(itrack,1)
cDK     &           +edc1layer_coeff(remap(4),layer)*eyp_fp_pre(itrack,1)!DK add
c            Write(*,*) layer,hits(ihit),edc1_wire_coord(hits(ihit))
         EndDo                  ! numhits loop
         entrack_hits_pre(itrack,1) = gh
      EndDo                     ! entracks_fp loop

c      Write(*,*) '(eleft)*',gen_event_ID_number,entracks_pre
c     Do i=1,entracks_pre
c     Write(*,*) '(eleft)*',i,entrack_hits_pre(i,1)
c     Write(*,*) '(eleft)*',
c     &        (entrack_hits_pre(i,1+j),j=1,entrack_hits_pre(i,1))
c     EndDo
      
      
*     write out results if edebugflagstubs is set
      if(edebugflagstubs.ne.0) then
         call e_print_dc1_stubs
      endif
      
      return
      end
      

