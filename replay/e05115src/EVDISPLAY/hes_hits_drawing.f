      subroutine hes_hits_drawing(Abort,err)
*     
*     routine to draw hits on HES detector
*     okayasu 02JUN05
*
*     In HES side, the Z direction is from right to left on HIGZ window
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'hes_hits_drawing')
*     
      logical ABORT
      character*(*) err
*     
      include 'gen_pawspace.cmn'
      include 'gen_event_info.cmn'

      include 'hes_data_structures.cmn'
      include 'hes_tracking.cmn'
      include 'hes_geometry.cmn'
      include 'hes_id_histid.cmn'
      include 'hes_scin_parms.cmn'

      include 'evdisplay_info.cmn'

*     Common variables definition
      integer i, j, k, ii, jj, kk
      integer wire_order

*     Variable definition for HES arm.   
      real*4  x1, y1, x2, y2, z1, z2, xa(2), ya(2), za(2)

      SAVE
c     --1---------2---------3---------4---------5---------6---------7--
*     
      ABORT = .FALSE.
      err= ' '

c     --1---------2---------3---------4---------5---------6---------7--
*
*     DC
*
      do i=1,entracks_fp

        za(1) = -wn_co2(6)
        za(2) = -wn_co2(5)

c        xa(1) = exp_fp(i) * za(1) + ex_fp(i)
c        xa(2) = exp_fp(i) * za(2) + ex_fp(i)
        xa(1) = - exp_fp(i) * za(1) + ex_fp(i)
        xa(2) = - exp_fp(i) * za(2) + ex_fp(i)

c        ya(1) = exp_fp(i) * za(1) + ex_fp(i)
c        ya(2) = exp_fp(i) * za(2) + ex_fp(i)
        ya(1) = eyp_fp(i) * za(1) - ey_fp(i)
        ya(2) = eyp_fp(i) * za(2) - ey_fp(i)

        call IGSET('PLCI',1.)                                          ! set polyline color index: black

        call ISELNT(nt(3))
        call IPL(2,za,xa)

        call ISELNT(nt(4))
        call IPL(2,za,ya)

      enddo


c     --1---------2---------3---------4---------5---------6---------7--
*
*     EHODO1, EHODO2 and EHODO3


      do i=1,escin_tot_hits
        if (escin_good_hits(i).eq.1) then

          ii = escin_layer_num(i)
          jj = escin_counter_num(i)

          if ( ii.le.2 ) then
            x1 = escin_xcenter(ii,jj) - escin_width    /2.
            x2 = escin_xcenter(ii,jj) + escin_width    /2.
            y1 = 0.                   - escin_height   /2.
            y2 = 0.                   + escin_height   /2.
            z1 = escin_zpos(ii)       - escin_thickness/2.
            z2 = escin_zpos(ii)       + escin_thickness/2.
          else
            x1 = escin_xcenter(ii,jj) - 107.9 / 2.
            x2 = escin_xcenter(ii,jj) + 107.9 / 2.
            y1 = 0.                   -   2.0 / 2.
            y2 = 0.                   +   2.0 / 2.
            z1 = escin_zpos(ii)       -   2.0 / 2.
            z2 = escin_zpos(ii)       +   2.0 / 2.
          endif

          call IGSET('FACI',16.)                                     ! fill area color index    : pink
          call IGSET('FAIS',1.)                                      ! fill area interior style : solid
          call IGSET('BORD',1.)                                      ! draw border
          call IGSET('PLCI',1.)                                      ! polyline color index (for border) : black

          call ISELNT(nt(3))
          call IGBOX(-z2,-z1,x1,x2)

          call ISELNT(nt(4))
          call IGBOX(-z2,-z1,y1,y2)

          call IGSET('FACI',0.)                                      ! fill area color index    : white
          call IGSET('FAIS',0.)                                      ! fill area interior style : hollow
          call IGSET('BORD',1.)                                      ! draw border
          call IGSET('PLCI',1.)                                      ! polyline color index (for border) : black

        endif
      enddo

c     --1---------2---------3---------4---------5---------6---------7--


      RETURN
      END

