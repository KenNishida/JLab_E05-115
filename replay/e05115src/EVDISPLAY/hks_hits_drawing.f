      subroutine hks_hits_drawing(Abort,err)
*     
*     routine to draw Hits on HKS detector
*     
*     Author:	Masashi Kaneta
*     Date:     28 May 2005
*     
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'hks_hits_drawing')
*     
      logical ABORT
      character*(*) err
*     
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'hks_geometry.cmn'
      include 'hks_id_histid.cmn'
      include 'gen_pawspace.cmn'


      include 'hks_scin_parms.cmn'
      include 'hks_scin_tof.cmn'
      include 'hks_water_parms.cmn'
      include 'hks_aero_parms.cmn'

      include 'evdisplay_info.cmn'



      integer i, j, k, ii, jj, kk
      integer wire_order
      real*4  x1, y1, x2, y2, z1, z2, xa(2), ya(2), za(2)              ! (x,y,z) is a set of direction in a cordinate of HKS

      integer counter

*     
      SAVE
c     --1---------2---------3---------4---------5---------6---------7--
*     
      ABORT = .FALSE.
      err= ' '


c     --1---------2---------3---------4---------5---------6---------7--
*
*     DC1 and DC2
*

      if (hntracks_fp.gt.0) then
        write(*,*) 'hntracks_fp =', hntracks_fp
      endif

      do i=1,hntracks_fp
        za(1) = wn_co1(5)
        za(2) = wn_co1(6)

        xa(1) = hxp_fp(i) * za(1) + hx_fp(i)
        xa(2) = hxp_fp(i) * za(2) + hx_fp(i)

        ya(1) = hxp_fp(i) * za(1) + hx_fp(i)
        ya(2) = hxp_fp(i) * za(2) + hx_fp(i)

        call IGSET('PLCI',1.)                                          ! set polyline color index: black

        call ISELNT(nt(1))                                             ! select normalization transformation
        call IPL(2,za,xa)

        call ISELNT(nt(2))                                             ! select normalization transformation
        call IPL(2,za,ya)

      enddo


c     --1---------2---------3---------4---------5---------6---------7--
*
*     Aerogel Cherenkov Counters
*
c     The valiable we are using here are stored in e01011src/HTRACKING/h_water.f

      do i=1,haer_tot_hits                                             ! at least one side PMT has a hit (TDC is valid range)

        if (haer_both_hits(i).eq.1) then                               ! both sode of PMT has valid TDC

          j = haer_layer_num(i)
          k = haer_counter_num(i)

          x1 = haer_box_xcenter(j,k) - haer_width    /2.
          x2 = haer_box_xcenter(j,k) + haer_width    /2.
          y1 = haer_box_ycenter(j,k) - haer_height   /2.
          y2 = haer_box_ycenter(j,k) + haer_height   /2.
          z1 = haer_box_zpos(j)      - haer_thickness/2.
          z2 = haer_box_zpos(j)      + haer_thickness/2.

          call IGSET('FACI',16.)                                       ! fill area color index    : pink
          call IGSET('FAIS',1.)                                        ! fill area interior style : solid
          call IGSET('BORD',1.)                                        ! draw border
          call IGSET('PLCI',1.)                                        ! polyline color index (for border) : black

          call ISELNT(nt(1))                                           ! select normalization transformation
          call IGBOX(z1,z2,x1,x2)

          call ISELNT(nt(2))                                           ! select normalization transformation
          call IGBOX(z1,z2,y1,y2)

          call IGSET('FACI',0.)                                        ! fill area color index    : white
          call IGSET('FAIS',0.)                                        ! fill area interior style : hollow
          call IGSET('BORD',1.)                                        ! draw border
          call IGSET('PLCI',1.)                                        ! polyline color index (for border) : black

        endif

      enddo

c     --1---------2---------3---------4---------5---------6---------7--
*
*     Scintillatoe Hodoscopes, 1x, 1y, 2x
*

c     The valiable we are using here are stored in e01011src/HTRACKING/h_trans_scin.f

      counter = 0
      do i=1,hscin_tot_hits
        j = hscin_layer_num(i)
        k = hscin_counter_num(i)

        if (hgood_pos(i).and.hgood_neg(i)) then                        ! values of two logical variables are stored in HTRACKING/h_trans_scin.f
                                                                       ! 'good' means ADC and TDC are in a valid range.

          if (j.eq.1) then                                             ! HTOF 1X

c           draw a filled box on scintillator rod which has valid hit

            x1 = hscin_1x_center(k)               - hscin_width(1)/2.
            x2 = hscin_1x_center(k)               + hscin_width(1)/2.
            y1 = (hscin_1x_top+hscin_1x_bot) / 2. - hscin_1x_size /2.
            y2 = (hscin_1x_top+hscin_1x_bot) / 2. + hscin_1x_size /2.
            z1 = hscin_zpos(j)                    - 2.0           /2.
            z2 = hscin_zpos(j)                    + 2.0           /2.
         
            call IGSET('FACI',16.)                                     ! fill area color index    : pink
            call IGSET('FAIS',1.)                                      ! fill area interior style : solid
            call IGSET('BORD',1.)                                      ! draw border
            call IGSET('PLCI',1.)                                      ! polyline color index (for border) : black

            call ISELNT(nt(1))                                         ! select normalization transformation
            call IGBOX(z1,z2,x1,x2)
         
            call ISELNT(nt(2))                                         ! select normalization transformation
            call IGBOX(z1,z2,y1,y2)

            call IGSET('FACI',0.)                                      ! fill area color index    : white
            call IGSET('FAIS',0.)                                      ! fill area interior style : hollow
            call IGSET('BORD',1.)                                      ! draw border
            call IGSET('PLCI',1.)                                      ! polyline color index (for border) : black


c           draw a circle at hit position on scintillator rod which has valid hit

            x1 = hscin_1x_center(k)
            y1 = hscin_hit_coord(i)
            z1 = hscin_zpos(j)

            call IGSET('PLCI',float(10+counter))

            call ISELNT(nt(1))                                         ! select normalization transformation
            call IGARC(z1,x1,2.00,2.00,0.0,0.0)

            call ISELNT(nt(2))                                         ! select normalization transformation
            call IGARC(z1,y1,2.00,2.00,0.0,0.0)

            call IGSET('PLCI',1.)
          endif


          if (j.eq.2) then                              ! HTOF 1Y

c           draw a filled box on scintillator rod which has valid hit

            x1 = (hscin_1y_right+hscin_1y_left)/2. - hscin_1y_size /2.
            x2 = (hscin_1y_right+hscin_1y_left)/2. + hscin_1y_size /2.
            y1 = hscin_1y_center(k)                - hscin_width(2)/2.
            y2 = hscin_1y_center(k)                + hscin_width(2)/2.
            z1 = hscin_zpos(j)                     - 2.0           /2.
            z2 = hscin_zpos(j)                     + 2.0           /2.
            
            call IGSET('FACI',16.)                                     ! fill area color index    : pink
            call IGSET('FAIS',1.)                                      ! fill area interior style : solid
            call IGSET('BORD',1.)                                      ! draw border
            call IGSET('PLCI',1.)                                      ! polyline color index (for border) : black

            call ISELNT(nt(1))                                         ! select normalization transformation
            call IGBOX(z1,z2,x1,x2)
            
            call ISELNT(nt(2))                                         ! select normalization transformation
            call IGBOX(z1,z2,y1,y2)

            call IGSET('FACI',0.)                                      ! fill area color index    : white
            call IGSET('FAIS',0.)                                      ! fill area interior style : hollow
            call IGSET('BORD',1.)                                      ! draw border
            call IGSET('PLCI',1.)                                      ! polyline color index (for border) : black


c           draw a circle at hit position on scintillator rod which has valid hit

            x1 = hscin_hit_coord(i)
            y1 = hscin_1y_center(k)
            z1 = hscin_zpos(j)

            call IGSET('PLCI',float(10+counter))

            call ISELNT(nt(1))                                         ! select normalization transformation
            call IGARC(z1,x1,2.00,2.00,0.0,0.0)

            call ISELNT(nt(2))                                         ! select normalization transformation
            call IGARC(z1,y1,2.00,2.00,0.0,0.0)

            call IGSET('PLCI',1.)
          endif

          if (j.eq.3) then                                             ! HTOF 2X

c           draw a filled box on scintillator rod which has valid hit

            x1 = hscin_2x_center(k)             - hscin_width(3)/2.
            x2 = hscin_2x_center(k)             + hscin_width(3)/2.
            y1 = (hscin_2x_top+hscin_2x_bot)/2. - hscin_2x_size /2.
            y2 = (hscin_2x_top+hscin_2x_bot)/2. + hscin_2x_size /2.
            z1 = hscin_zpos(j)                  - 2.0           /2.
            z2 = hscin_zpos(j)                  + 2.0           /2.

            call IGSET('FACI',16.)                                     ! fill area color index    : pink
            call IGSET('FAIS',1.)                                      ! fill area interior style : solid
            call IGSET('BORD',1.)                                      ! draw border
            call IGSET('PLCI',1.)                                      ! polyline color index (for border) : black

            call ISELNT(nt(1))                                         ! select normalization transformation
            call IGBOX(z1,z2,x1,x2)

            call ISELNT(nt(2))                                         ! select normalization transformation
            call IGBOX(z1,z2,y1,y2)

            call IGSET('FACI',0.)                                      ! fill area color index    : white
            call IGSET('FAIS',0.)                                      ! fill area interior style : hollow
            call IGSET('BORD',1.)                                      ! draw border
            call IGSET('PLCI',1.)                                      ! polyline color index (for border) : black

c           draw a circle at hit position on scintillator rod which has valid hit

            x1 = hscin_2x_center(k)
            y1 = hscin_hit_coord(i)
            z1 = hscin_zpos(j)

            call IGSET('PLCI',float(10+counter))

            call ISELNT(nt(1))                                         ! select normalization transformation
            call IGARC(z1,x1,2.00,2.00,0.0,0.0)

            call ISELNT(nt(2))                                         ! select normalization transformation
            call IGARC(z1,y1,2.00,2.00,0.0,0.0)

            call IGSET('PLCI',1.)
          endif


          counter = counter +1
        endif

c       test
         write(*,'(a9,i2,a13,i2,a8,f6.0,f6.0,a8,i8,i8)')
     +      'Layer #: ', j, ', counter #: ', k,
     +      ', ADC = ', HSCIN_ADC_POS(i), HSCIN_ADC_NEG(i),
     +      ', TDC = ', HSCIN_TDC_POS(i), HSCIN_TDC_NEG(i)



      enddo                                                            ! end of hit loop


c     --1---------2---------3---------4---------5---------6---------7--
*
*     Water Cherenkov Counters
*

c     the valiable we are using here are stored in e01011src/HTRACKING/h_water.f

      do i=1,hwat_tot_hits                                             ! at least one side PMT has a hit (TDC is valid range)

        if (hwat_both_hits(i).eq.1) then                               ! both sode of PMT has hit

          j = hwat_layer_num(i)
          k = hwat_counter_num(i)

          x1 = hwat_box_xcenter(j,k) - hwat_width/2.
          x2 = hwat_box_xcenter(j,k) + hwat_width/2.
          y1 = hwat_box_ycenter(j,k) - hwat_height/2.
          y2 = hwat_box_ycenter(j,k) + hwat_height/2.
          z1 = hwat_box_zpos(j)      - hwat_thickness/2.
          z2 = hwat_box_zpos(j)      + hwat_thickness/2.

          call IGSET('FACI',16.)                                       ! fill area color index    : pink
          call IGSET('FAIS',1.)                                        ! fill area interior style : solid
          call IGSET('BORD',1.)                                        ! draw border
          call IGSET('PLCI',1.)                                        ! polyline color index (for border) : black

          call ISELNT(nt(1))                                           ! select normalization transformation
          call IGBOX(z1,z2,x1,x2)                     

          call ISELNT(nt(2))                                           ! select normalization transformation
          call IGBOX(z1,z2,y1,y2)                     

          call IGSET('FACI',0.)                                        ! fill area color index    : white
          call IGSET('FAIS',0.)                                        ! fill area interior style : hollow
          call IGSET('BORD',1.)                                        ! draw border
          call IGSET('PLCI',1.)                                        ! polyline color index (for border) : black

        endif

      enddo



      RETURN
      END

