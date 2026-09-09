      subroutine evdisplay_init(Abort,err)

      implicit none
*
      character*50 here
      parameter (here= 'evdisplay_init')
*     
      logical ABORT
      character*(*) err
*     
      integer iwk

      integer i
      character*1 arg

      include 'gen_pawspace.cmn'
      include 'evdisplay_info.cmn'

c     ---1---------2---------3---------4--------5---------6--------7--

      call MZEBRA(-3)                                                 ! Initialize storage in /PAWC/
      call MZPAW(G_sizePAW,' ')                                       ! G_sizePAW is defined in gen_pawspace
      call IGINIT(0)                                                  ! Initialize HIGZ

      do iwk=1,2
        call IOPWK(iwk,1,1)                                           ! open workstation iwk, KONID, workstation type 1
                                                                      ! (geometry is set in ~/higz_windows.dat)

        call ISCR(iwk, 0, 1., 1., 1.)                                 ! white
        call ISCR(iwk, 1, 0., 0., 0.)                                 ! black
        call ISCR(iwk, 2, 1., 0., 0.)                                 ! red
        call ISCR(iwk, 3, 0., 1., 0.)                                 ! light green
        call ISCR(iwk, 4, 0., 0., 1.)                                 ! blue
        call ISCR(iwk, 5, 1., 1., 0.)                                 ! yellow
        call ISCR(iwk, 6, 1., 0., 1.)                                 ! magenta (red-purple)
        call ISCR(iwk, 7, 0., 1., 1.)                                 ! cyan (light blue)

        call ISCR(iwk, 8, 0.50, 0.50, 0.50)                           ! gray
        call ISCR(iwk, 9, 0.75, 0.75, 0.75)                           ! silver
        call ISCR(iwk,10, 0.50, 0.00, 0.00)                           ! maroon
        call ISCR(iwk,11, 0.50, 0.50, 0.00)                           ! olive
        call ISCR(iwk,12, 0.00, 0.50, 0.00)                           ! green
        call ISCR(iwk,13, 0.00, 0.50, 0.50)                           ! teal
        call ISCR(iwk,14, 0.00, 0.00, 0.50)                           ! navy
        call ISCR(iwk,15, 0.50, 0.00, 0.50)                           ! purple

        call ISCR(iwk,16, 1.00, 0.75, 0.75)                           ! pink
      enddo

c     Note; Coordinates definition was based on the Hall C log 96253.
c     okayasu 01Jun05

      ! for HKS top view
      call ISWN(nt(1), wn_co1(5), wn_co1(6),wn_co1(1),wn_co1(2))      ! set normalization transformation
      call ISVP(nt(1),       0.0,       1.0,     0.25,      1.0)      ! set view point on workstation (HIGZ window)

      ! for HKS side view
      call ISWN(nt(2), wn_co1(5), wn_co1(6),wn_co1(3),wn_co1(4))
      call ISVP(nt(2),       0.0,       1.0,      0.0,     0.25)

      ! for ENGE top view
      call ISWN(nt(3),-wn_co2(6),-wn_co2(5),wn_co2(1),wn_co2(2))
      call ISVP(nt(3),       0.0,       1.0,     0.20,      1.0)

      ! for ENGE side view
      call ISWN(nt(4),-wn_co2(6),-wn_co2(5),wn_co2(3),wn_co2(4))
      call ISVP(nt(4),       0.0,       1.0,      0.0,     0.20)


      ! for message
      call ISWN(nt(5),       0.0,       5.0,      0.0,      1.0)
      call ISVP(nt(5),       0.0,       1.0,      0.8,      1.0)

      ! for message
      call ISWN(nt(6),       0.0,       5.0,      0.0,      1.0)
      call ISVP(nt(6),       0.0,       1.0,      0.0,      0.2)


      call IUWK(0,1)                                                  ! workstation ID=0 (all workstations), refresh entire display


      show_all_events = .TRUE.
      wait_next_event = .TRUE.

c     ---1---------2---------3---------4--------5---------6--------7--
      write(*,*) ''
      write(*,*) '+-------------------------------------------------+'
      write(*,*) '|        One Event Display for E01-011 (HKS)      |'
      write(*,*) '|                                                 |'
      write(*,*) '|         Yuichi Okayasu and Masashi Kaneta       |'
      write(*,*) '|               Department of Physics,            |'
      write(*,*) '|                 Tohoku University               |'
      write(*,*) '|                                                 |'
      write(*,*) '|                    June. 2005                   |'
      write(*,*) '|                                                 |'
      write(*,*) '+-------------------------------------------------+'

      i = -999
      do while ( i.lt.0 )
        write(*,*) ''
        write(*,*) 'Do you want to see all events (=0)'
        write(*,*) '                or only valid events (=1)?'
        read(*,*) arg
        if ( arg .eq. '0' ) then
          show_all_events = .TRUE.
          i = 0
        else if ( arg .eq. '1' ) then
          show_all_events = .FALSE.
          i = 1
        else
          write(*,*) 'Your input value is not valid.'
        endif
      enddo


      i = -999
      do while ( i.lt.0 )
        write(*,*) ''
        write(*,*) 'Do you want to stop event display'
        write(*,*) '          after one event display (=0)'
        write(*,*) '          or no waiting (=1)?'
        read(*,*) arg
        if ( arg .eq. '0' ) then
          wait_next_event = .TRUE.
          i = 0
        else if ( arg .eq. '1' ) then
          wait_next_event = .FALSE.
          i = 1
        else
          write(*,*) 'Your input value is not valid.'
        endif
      enddo

      return
      end
