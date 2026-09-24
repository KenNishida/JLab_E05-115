      function hes_has_valid_hits()

      implicit none

      logical hes_has_valid_hits
*
      character*50 here
      parameter (here= 'hes_has_valid_hits')
*     
      integer i,j

      include 'hes_data_structures.cmn'

c     ---1---------2---------3---------4--------5---------6--------7--

      hes_has_valid_hits = .FALSE.                                   ! initialization

      if (entracks_fp.gt.0) hes_has_valid_hits = .TRUE.              ! check for DC tracking 

      j = 0
      do i=1,escin_tot_hits                                           ! check for scintillator hodoscope
        if (escin_good_hits(i).eq.1) then                             ! each side has valid ADC and valid TDC
          j = j + 1
        endif
      enddo
      if (j.gt.0) hes_has_valid_hits = .TRUE.


      return
      end
