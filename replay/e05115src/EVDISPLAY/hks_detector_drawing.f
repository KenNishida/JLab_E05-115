      subroutine hks_detector_drawing(Abort,err)
*     
*     routine to draw HKS detector
*     
*     Author:	Masashi Kaneta
*     Date:     28 May 2005
*     
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'hks_detector_drawing')
*     
      logical ABORT
      character*(*) err
*     

      include 'gen_event_info.cmn'
      include 'gen_run_info.cmn'
      include 'gen_pawspace.cmn'

      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'hks_geometry.cmn'
      include 'hks_id_histid.cmn'
      include 'hks_scin_parms.cmn'
      include 'hks_water_parms.cmn'
      include 'hks_aero_parms.cmn'
      include 'hks_lucite_parms.cmn'

      include 'evdisplay_info.cmn'

      integer i, j, k, ii, jj, kk
      integer iwk
      integer wire_order
      real*4  x1, y1, x2, y2, z1, z2, xa(2), ya(2), za(2)              ! (x,y,z) is a set of direction in a cordinate of HKS

      character*80 mess


*     

      SAVE
c     --1---------2---------3---------4---------5---------6---------7--
*     
      ABORT = .FALSE.
      err= ' '


c     --1---------2---------3---------4---------5---------6---------7--
*
*     Axes
*

      call ISELNT(nt(1))                                               ! select normalization transformation
      call IGSET('TXFP',-130.)
      call IGSET('LAOF',  0.20)
      call IGSET('TMSI',  2.0)                                         ! in world cordinate
      call IGSET('TANG',  0.0)                                         ! in degree
      call IGAXIS(wn_co1(5) + (wn_co1(6)-wn_co1(5))*0.05,              ! for z axis of HKS
     +            wn_co1(6) - (wn_co1(6)-wn_co1(5))*0.01,
     +            wn_co1(1) + (wn_co1(2)-wn_co1(1))*0.05,
     +            wn_co1(1) + (wn_co1(2)-wn_co1(1))*0.05,
     +            wn_co1(5) + (wn_co1(6)-wn_co1(5))*0.05,
     +            wn_co1(6) - (wn_co1(6)-wn_co1(5))*0.05,
     +            510,'SD+')

      call IGSET('TXAL',30.)                                           ! right, bottom
      call IGSET('CHHE',6.0)                                           ! in world cordinate
      call ITX(wn_co1(6) - (wn_co1(6)-wn_co1(5))*0.03,
     +         wn_co1(1) + (wn_co1(2)-wn_co1(1))*0.02,
     +         'z axis')

      call IGSET('LAOF',  0.20)
      call IGAXIS(wn_co1(5) + (wn_co1(6)-wn_co1(5))*0.05,              ! for x axis of HKS
     +            wn_co1(5) + (wn_co1(6)-wn_co1(5))*0.05,
     +            wn_co1(1) + (wn_co1(2)-wn_co1(1))*0.10,
     +            wn_co1(2) - (wn_co1(2)-wn_co1(1))*0.01,
     +            wn_co1(1) + (wn_co1(2)-wn_co1(1))*0.10,
     +            wn_co1(2) - (wn_co1(2)-wn_co1(1))*0.01,
     +            510,'SD-')

      call IGSET('TXAL',30.)                                           ! right, bottom
      call IGSET('CHHE',6.0)                                           ! world cordinate
      call IGSET('TANG',90.0)                                          ! in degree
      call ITX(wn_co1(5) + (wn_co1(6)-wn_co1(5))*0.08,
     +         wn_co1(2) - (wn_co1(2)-wn_co1(1))*0.01,
     +         'x axis')

      call ISELNT(nt(2))
      call IGSET('LAOF', 0.50)
      call IGSET('TANG',  0.0)                                         ! in degree
      call IGAXIS(wn_co1(5) + (wn_co1(6)-wn_co1(5))*0.05,              ! for y axis of HKS
     +            wn_co1(5) + (wn_co1(6)-wn_co1(5))*0.05,
     +            wn_co1(3) + (wn_co1(4)-wn_co1(3))*0.05,
     +            wn_co1(4) - (wn_co1(4)-wn_co1(3))*0.00,
     +            wn_co1(3) + (wn_co1(4)-wn_co1(3))*0.05,
     +            wn_co1(4) - (wn_co1(4)-wn_co1(3))*0.00,
     +            510,'SD-')

      call IGSET('TXAL', 0.)                                           ! left, bottom
      call IGSET('CHHE',6.0)                                           ! world cordinate
      call IGSET('TANG',90.0)                                          ! in degree
      call ITX(wn_co1(5) + (wn_co1(6)-wn_co1(5))*0.08,
     +         wn_co1(3) - (wn_co1(4)-wn_co1(3))*0.01,
     +         'y axis')




      call ISELNT(nt(5))
      call IGSET('TANG',0.0)                                           ! in degree
      call IGSET('TXAL',21.)                                           ! center, top
      call IGSET('CHHE',0.1)                                           ! world cordinate
      call ITX(2.5,0.99,'Top view')

      call ISELNT(nt(6))
      call IGSET('TXAL',20.)                                           ! center, bottom
      call IGSET('CHHE',0.1)                                           ! world cordinate
      call ITX(2.5,0.01,'Side view')

      call IGSET('FACI',0.)                                            ! fill area color index    : white
      call IGSET('FAIS',0.)                                            ! fill area interior style : hollow
      call IGSET('BORD',1.)                                            ! draw border
      call IGSET('PLCI',1.)                                            ! polyline color index (for border) : black

c     --1---------2---------3---------4---------5---------6---------7--
*
*     DC1 and DC2
* 

      do i=1,HMAX_NUM_CHAMBERS
        do j=3,4                                                       ! x, x' layer only
          ii = HMAX_NUM_DC_LAYERS/HMAX_NUM_CHAMBERS * (i-1) + j        ! layer (plane) number

          z1 = hdc_zpos(ii)                                            ! layer z position is given for each layer

          do k=1,hdc_nrwire(ii)

c           the following decording is reffered from HTRACKING/h_trans_dc.f

c           if hdc_wire_counting (plane) is 1 then wires are number in reverse order

            if (hdc_wire_counting(ii).eq.0) then                       ! normal ordering
              wire_order = float(k)
            else                                                       ! reversed ordering
              wire_order = float( hdc_nrwire(ii) - k + 1 )
            endif
            x1 = hdc_pitch(ii)
     +          * ( float(wire_order) - hdc_central_wire(ii) ) 
     +          - hdc_center(ii)
                        
            y1 = hdc_ycenter(ii) - hdc_length_y/2. 
            y2 = hdc_ycenter(ii) + hdc_length_y/2. 

            ya(1) = y1
            ya(2) = y2
            za(1) = z1
            za(2) = z1

            call ISELNT(nt(1))                                         ! select normalization transformation
            call IGARC(z1,x1,0.10,0.10,0.,0.)
           
            call ISELNT(nt(2))                                         ! select normalization transformation
            call IPL(2,za,ya)

          enddo
        enddo
      enddo

      call ISELNT(nt(1))
      call IGSET('TXFP',-130.)
      call IGSET('TXAL',20.)
      call IGSET('CHHE',4.0)
      call IGSET('TXCI',14.)
      call ITX(-50.,70.,'DC1'   )
      call ITX(-50.,65.,'x, x\047')
      call ITX( 50.,70.,'DC2'   )
      call ITX( 50.,65.,'x, x\047')


c     --1---------2---------3---------4---------5---------6---------7--
*
*     Aerogel Cherenkov Counters
*
      do i=1, HNUM_AER_LAYERS
        do j=1, HNUM_AER_COUNTERS
          x1 = haer_box_xcenter(i,j) - haer_width/2.
          x2 = haer_box_xcenter(i,j) + haer_width/2.
          y1 = haer_box_ycenter(i,j) - haer_height/2.
          y2 = haer_box_ycenter(i,j) + haer_height/2.
          z1 = haer_box_zpos(i)      - haer_thickness/2.
          z2 = haer_box_zpos(i)      + haer_thickness/2.

          call ISELNT(nt(1))                                           ! select normalization transformation
          call IGBOX(z1,z2,x1,x2)                                      ! draw a box (x1,x2,y1,y2), bttom-left = (x1,y1), top-right=(x2,y2)

          call ISELNT(nt(2))                                           ! select normalization transformation
          call IGBOX(z1,z2,y1,y2)

        enddo
      enddo

      call ISELNT(nt(1))
      call IGSET('TXFP',-130.)
      call IGSET('TXAL',20.)
      call IGSET('CHHE',4.0)
      call IGSET('TXCI',14.)
      call ITX(haer_box_zpos(2),97.,'Aerogel'  )
      call ITX(haer_box_zpos(2),91.,'Cherenkov')

c     --1---------2---------3---------4---------5---------6---------7--
*
*     Scintillatoe Hodoscopes, 1x, 1y, 2x
*
      do i=1,hscin_1x_nr
        x1 = hscin_1x_center(i)               - hscin_width(1)/2.
        x2 = hscin_1x_center(i)               + hscin_width(1)/2.
        y1 = (hscin_1x_top+hscin_1x_bot) / 2. - hscin_1x_size /2.
        y2 = (hscin_1x_top+hscin_1x_bot) / 2. + hscin_1x_size /2.
        z1 = hscin_zpos(1)                    - 2.0           /2.
        z2 = hscin_zpos(1)                    + 2.0           /2.

        call ISELNT(nt(1))                                             ! select normalization transformation
        call IGBOX(z1,z2,x1,x2)

        call ISELNT(nt(2))                                             ! select normalization transformation
        call IGBOX(z1,z2,y1,y2)
      enddo

      call ISELNT(nt(1))
      call IGSET('TXFP',-130.)
      call IGSET('TXAL',20.)
      call IGSET('CHHE',4.0)
      call IGSET('TXCI',14.)
      call ITX(hscin_zpos(1),80.,'HTOF 1X')

      do i=1,hscin_1y_nr
        x1 = (hscin_1y_right+hscin_1y_left)/ 2. - hscin_1y_size /2.
        x2 = (hscin_1y_right+hscin_1y_left)/ 2. + hscin_1y_size /2.
        y1 = hscin_1y_center(i)                 - hscin_width(2)/2.
        y2 = hscin_1y_center(i)                 + hscin_width(2)/2.
        z1 = hscin_zpos(2)                      - 2.0           /2.
        z2 = hscin_zpos(2)                      + 2.0           /2.

        call ISELNT(nt(1))                                             ! select normalization transformation
        call IGBOX(z1,z2,x1,x2)

        call ISELNT(nt(2))                                             ! select normalization transformation
        call IGBOX(z1,z2,y1,y2)
      enddo

      call ISELNT(nt(1))
      call IGSET('TXFP',-130.)
      call IGSET('TXAL',20.)
      call IGSET('CHHE',4.0)
      call IGSET('TXCI',14.)
      call ITX(hscin_zpos(2), 60.,'HTOF 1Y')

      do i=1,hscin_2x_nr

        x1 = hscin_2x_center(i)              - hscin_width(3)/2.
        x2 = hscin_2x_center(i)              + hscin_width(3)/2.
        y1 = (hscin_2x_top+hscin_2x_bot)/ 2. - hscin_2x_size /2.
        y2 = (hscin_2x_top+hscin_2x_bot)/ 2. + hscin_2x_size /2.
        z1 = hscin_zpos(3)                   - 2.0           /2.
        z2 = hscin_zpos(3)                   + 2.0           /2.

        call ISELNT(nt(1))                                             ! select normalization transformation
        call IGBOX(z1,z2,x1,x2)

        call ISELNT(nt(2))                                             ! select normalization transformation
        call IGBOX(z1,z2,y1,y2)
      enddo

      call ISELNT(nt(1))
      call IGSET('TXFP',-130.)
      call IGSET('TXAL',30.)
      call IGSET('CHHE',4.0)
      call IGSET('TXCI',14.)
      call ITX(hscin_zpos(3), 90.,'HTOF 2X')


c     --1---------2---------3---------4---------5---------6---------7--
*
*     Water Cherenkov Counters
*

      do i=1,HNUM_WAT_LAYERS
        do j=1,HNUM_WAT_COUNTERS
          x1 = hwat_box_xcenter(i,j) - hwat_width/2.
          x2 = hwat_box_xcenter(i,j) + hwat_width/2.
          y1 = hwat_box_ycenter(i,j) - hwat_height/2.
          y2 = hwat_box_ycenter(i,j) + hwat_height/2.
          z1 = hwat_box_zpos(i)      - hwat_thickness/2.
          z2 = hwat_box_zpos(i)      + hwat_thickness/2.

          call ISELNT(nt(1))                                           ! select normalization transformation
          call IGBOX(z1,z2,x1,x2)

          call ISELNT(nt(2))                                           ! select normalization transformation
          call IGBOX(z1,z2,y1,y2)
        enddo
      enddo

      call ISELNT(nt(1))
      call IGSET('TXFP',-130.)
      call IGSET('TXAL',0.)
      call IGSET('CHHE',4.0)
      call IGSET('TXCI',14.)
      call ITX(hwat_box_zpos(1),102.,'Water')
      call ITX(hwat_box_zpos(1), 92.,'Cherenkov')

c     --1---------2---------3---------4---------5---------6---------7--
*
*     Lucite Cherenkov Counters
*

      do i=1,HNUM_LUC_LAYERS
        do j=1,HNUM_LUC_COUNTERS
          x1 = hluc_box_xcenter(i,j) - hluc_width/2.
          x2 = hluc_box_xcenter(i,j) + hluc_width/2.
          y1 = hluc_box_ycenter(i,j) - hluc_height/2.
          y2 = hluc_box_ycenter(i,j) + hluc_height/2.
          z1 = hluc_box_zpos(i)      - hluc_thickness/2.
          z2 = hluc_box_zpos(i)      + hluc_thickness/2.

          call ISELNT(nt(1))                                           ! select normalization transformation
          call IGBOX(z1,z2,x1,x2)

          call ISELNT(nt(2))                                           ! select normalization transformation
          call IGBOX(z1,z2,y1,y2)
        enddo
      enddo

      call ISELNT(nt(1))
      call IGSET('TXFP',-130.)
      call IGSET('TXAL',0.)
      call IGSET('CHHE',4.0)
      call IGSET('TXCI',14.)
      call ITX(hluc_box_zpos(1),102.,'Lucite')
      call ITX(hluc_box_zpos(1), 92.,'Cherenkov')

c     --1---------2---------3---------4---------5---------6---------7--
*
*     draw run number and event id number
*

      call ISELNT(nt(1))                                               ! select normalization transformation
      
      call IGSET('TXAL',1.)                                            ! leftt, top
      call IGSET('CHHE',6.0)                                           ! world cordinate
      call IGSET('TANG',0.0)                                           ! in degree
      call IGSET('TXCI',4.)                                            ! Text color index: blue

      write(mess,'(a5,i8)') 'Run: ', gen_run_number
      call ITX(wn_co1(5) + (wn_co1(6)-wn_co1(5))*0.15,
     +         wn_co1(2) - (wn_co1(2)-wn_co1(1))*0.01,
     +         mess)

      write(mess,'(a7,i8)') 'Event: ', gen_event_ID_number
      call ITX(wn_co1(5) + (wn_co1(6)-wn_co1(5))*0.15,
     +         wn_co1(2) - (wn_co1(2)-wn_co1(1))*0.05,
     +         mess)
     
      call IGSET('TXCI',1.)                                            ! Text color index: black



c     for test
      write(*,*) 'event # = ', gen_event_ID_number
      write(*,*) 'HDC_TOT_HITS   / HDC_RAW_TOT_HITS   =',
     +            HDC_TOT_HITS, '/', HDC_RAW_TOT_HITS
      write(*,*) 'HSCIN_TOT_HITS / HSCIN_RAW_TOT_HITS =',
     +            HSCIN_TOT_HITS,'/',HSCIN_RAW_TOT_HITS
      write(*,*) 'HAER_TOT_HITS  / HAER_RAW_TOT_HITS =',
     +            HAER_TOT_HITS,'/',HAER_RAW_TOT_HITS
      write(*,*) 'HWAT_TOT_HITS  / HWAT_RAW_TOT_HITS =',
     +            HWAT_TOT_HITS,'/',HWAT_RAW_TOT_HITS
      write(*,*) 'HLUC_TOT_HITS  / HLUC_RAW_TOT_HITS =',
     +            HLUC_TOT_HITS,'/',HLUC_RAW_TOT_HITS

c     --1---------2---------3---------4---------5---------6---------7--

      RETURN
      END

