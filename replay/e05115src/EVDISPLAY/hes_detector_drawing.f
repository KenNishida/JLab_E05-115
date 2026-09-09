      subroutine hes_detector_drawing(Abort,err)
*     
*     routine to draw HES detector
*     okayasu 02JUN05
*
*     In HES side, the Z direction is from right to left on HIGZ window
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'hes_detector_drawing')
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
c
c     axis
c

      call ISELNT(nt(3))                                               ! select normalization transformation
      call IGSET('TXFP',-60.)
      call IGAXIS(-wn_co2(5)-(wn_co2(6)-wn_co2(5))*0.05,
     +            -wn_co2(6)+(wn_co2(6)-wn_co2(5))*0.05,               ! for z axis of HES
     +             wn_co2(1)+(wn_co2(2)-wn_co2(1))*0.05,
     +             wn_co2(1)+(wn_co2(2)-wn_co2(1))*0.05,
     +             wn_co2(5)+(wn_co2(6)-wn_co2(5))*0.05,
     +             wn_co2(6)-(wn_co2(6)-wn_co2(5))*0.05,
     +             510,'-')

      call IGSET('TXAL',30.)                                           ! right, bottom
      call IGSET('CHHE',4.0)                                           ! in world cordinate
      call IGSET('TANG',0.0)                                           ! in degree
      call ITX(-wn_co2(5) - (wn_co2(6)-wn_co2(5))*0.01,
     +          wn_co2(1) + (wn_co2(2)-wn_co2(1))*0.01,
     +         'z axis')

      call IGSET('LAOF',2.80)
      call IGAXIS(-wn_co2(5)-(wn_co2(6)-wn_co2(5))*0.08,                ! for x axis of HES
     +            -wn_co2(5)-(wn_co2(6)-wn_co2(5))*0.08,
     +             wn_co2(1)+(wn_co2(2)-wn_co2(1))*0.10,
     +             wn_co2(2)-(wn_co2(2)-wn_co2(1))*0.02,
     +             wn_co2(1)+(wn_co2(2)-wn_co2(1))*0.10,
     +             wn_co2(2)-(wn_co2(2)-wn_co2(1))*0.02,
     +             510,'D+')

      call IGSET('TXAL',30.)                                           ! right, bottom
      call IGSET('CHHE',4.0)                                           ! world cordinate
      call IGSET('TANG',90.0)                                          ! in degree
      call ITX(-wn_co2(5) - (wn_co2(6)-wn_co2(5))*0.02,
     +          wn_co2(2) - (wn_co2(2)-wn_co2(1))*0.01,	
     +         'x axis')

      call ISELNT(nt(4))                                               ! select normalization transformation
      call IGSET('TXAL',0.)                                            ! left, bottom
      call IGSET('CHHE',1.5)                                           ! world cordinate
      call IGSET('LAOF',2.50)
      call IGAXIS(-wn_co2(5)-(wn_co2(6)-wn_co2(5))*0.08,               ! for y axis of HES
     +            -wn_co2(5)-(wn_co2(6)-wn_co2(5))*0.08,
     +             wn_co2(3)+(wn_co2(4)-wn_co2(3))*0.02,
     +             wn_co2(4)-(wn_co2(4)-wn_co2(3))*0.10,
     +             wn_co2(3)+(wn_co2(4)-wn_co2(3))*0.02,
     +             wn_co2(4)-(wn_co2(4)-wn_co2(3))*0.10,
     +             510,'D+')
      call IGSET('CHHE',1.5)                                           ! world cordinate
      call ITX(-wn_co2(5) - (wn_co2(6)-wn_co2(5))*0.02,
     +          wn_co2(3) + (wn_co2(4)-wn_co2(3))*0.01,	
     +         'y axis')


      call IGSET('TANG',0.0)                                           ! in degree

      call IGSET('FACI',0.)                                            ! fill area color index    : white
      call IGSET('FAIS',0.)                                            ! fill area interior style : hollow
      call IGSET('BORD',1.)                                            ! draw border
      call IGSET('PLCI',1.)                                            ! polyline color index (for border) : black


c     --1---------2---------3---------4---------5---------6---------7--
c     Start of HES description
*
*     DC
*

      do i=edc1_num_layers, 4                                          ! layer number, only x
        do ii=0,1
          z1 = edc1_zpos(i+ii) 
          do j=1,edc1_nrwire(i+ii)                                       !  number of wires/layer          
            if (edc1_wire_counting(i+ii).eq.0) then
               wire_order = float(j)
            else
               wire_order = float( edc1_nrwire(i+ii) - j + 1 )
            endif
            x1 = edc1_central_wire(i+ii) - edc1_pitch(i+ii) * wire_order

            y1 = 0. - edc1_length_y / 2.
            y2 = 0. + edc1_length_y / 2.
            ya(1) =  y1
            ya(2) =  y2
            za(1) = -z1
            za(2) = -z1

            call ISELNT(nt(3))
            call IGARC(-z1,x1,0.05,0.05,0.,0.)                           ! draw wires

            call ISELNT(nt(4))
            call IPL(2,za,ya)                                            ! draw wires
          enddo
        enddo
      enddo

      call ISELNT(nt(3))
c      call IGBOX(-15.375,15.375,-50.,50.)                              ! draw detector box

      call IGSET('TXFP',-130.)
      call IGSET('TXAL',20.)                                           ! center, bottom
      call IGSET('CHHE',4.0)
      call IGSET('TXCI',14.)
      call ITX(  0.,63.,'EDC' )
      call IGSET('CHHE',3.0)
      call ITX(-edc1_zpos(1) ,53.,'1\047, 1')
      call ITX(-edc1_zpos(5) ,53.,'3\047, 3')
      call ITX(-edc1_zpos(9) ,53.,'5\047, 5')

c     --1---------2---------3---------4---------5---------6---------7--
*
*     EHODO1, EHODO2 and EHODO3
*     x, y and z is HES coordinate system. okayasu 5/29/2005


      do i=1,2                                                         ! EHOD3 has different number of rod from EHOD1, 2
                                                                       ! then only EHOD1 and 2 are in this loop
        do j=1,ENUM_SCIN_COUNTERS
          x1 = escin_xcenter(i,j) - escin_width    /2.
          x2 = escin_xcenter(i,j) + escin_width    /2.
          y1 = 0.                 - escin_height   /2.
          y2 = 0.                 + escin_height   /2.
          z1 = escin_zpos(i)      - escin_thickness/2.
          z2 = escin_zpos(i)      + escin_thickness/2.
          if ( (i.eq.1).or.(i.eq.2) ) then
            call ISELNT(nt(3))
            call IGBOX(-z2,-z1,x1,x2)

            call ISELNT(nt(4))
            call IGBOX(-z2,-z1,y1,y2)
          endif
        enddo
      enddo

*     EHODO3 is directly assigned here! okayasu 06/01/05

      x1 = escin_xcenter(3,1) - 107.9 / 2.
      x2 = escin_xcenter(3,1) + 107.9 / 2.
      y1 = 0.                 -   2.0 / 2.
      y2 = 0.                 +   2.0 / 2.
      z1 = escin_zpos(3)      -   2.0 / 2.
      z2 = escin_zpos(3)      +   2.0 / 2.

      call ISELNT(nt(3))
      call IGBOX(-z2,-z1,x1,x2)

      call IGSET('TXFP',-130.)
      call IGSET('TXAL',20.)                                           ! center, bottom
      call IGSET('CHHE',4.0)
      call IGSET('TXCI',14.)
      call ITX(-escin_zpos(1),64.,'ETOF 1')
      call ITX(-escin_zpos(2),58.,'ETOF 2')

      call IGSET('TXAL',30.)                                           ! right, bottom
      call ITX(-escin_zpos(3),50.,'ETOF 3')


      call ISELNT(nt(4))
      call IGBOX(-z2,-z1,y1,y2)

      call IGSET('TXCI',1.)

 
c     for test
      write(*,*) 'event # = ', gen_event_ID_number
      write(*,*) 'EDC1_TOT_HITS   / EDC1_RAW_TOT_HITS   =',
     +            EDC1_TOT_HITS, '/', EDC1_RAW_TOT_HITS
      write(*,*) 'ESCIN_TOT_HITS / ESCIN_RAW_TOT_HITS =',
     +            ESCIN_TOT_HITS,'/',ESCIN_RAW_TOT_HITS


c     --1---------2---------3---------4---------5---------6---------7--


      RETURN
      END

