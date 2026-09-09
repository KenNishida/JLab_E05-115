      function hks_has_valid_hits()

      implicit none

      logical hks_has_valid_hits
*
      character*50 here
      parameter (here= 'hks_has_valid_hits')
*     
      integer i,j

      include 'hks_data_structures.cmn'
      include 'hks_scin_tof.cmn'

c     ---1---------2---------3---------4--------5---------6--------7--

      hks_has_valid_hits = .FALSE.                                    ! initialization

      if (hntracks_fp.gt.0) hks_has_valid_hits = .TRUE.               ! check for DC tracking 

      j = 0
      do i=1,hscin_tot_hits                                           ! check for scintillator hodoscope (HTOF 1X, 1Y, 2X)
        if (hgood_pos(i).and.hgood_neg(i)) then                       ! each side has valid ADC and valid TDC
          j = j + 1
        endif
      enddo
      if (j.gt.0) hks_has_valid_hits = .TRUE.

      j = 0
      do i=1,haer_tot_hits                                            ! check for aerogel cherenkov
        if (haer_both_hits(i).eq.1) then                              ! each side has valid TDC
          j = j + 1
        endif
      enddo
      if (j.gt.0) hks_has_valid_hits = .TRUE.


      j = 0
      do i=1,hwat_tot_hits                                            ! check for water cherenkov
        if (hwat_both_hits(i).eq.1) then                              ! each side has valid TDC
          j = j + 1
        endif
      enddo
      if (j.gt.0) hks_has_valid_hits = .TRUE.


      return
      end
