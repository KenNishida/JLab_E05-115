      subroutine both_detector_drawing(Abort,err)
*     
*     routine to draw HES and HKS detectors
*     
*     Author:	Masashi Kaneta
*     Date:     28 May 2005
*     
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'both_detector_drawing')
*     
      logical ABORT
      character*(*) err
*     
      include 'gen_pawspace.cmn'
      include 'evdisplay_info.cmn'

      integer iwk
      real*4  r(2)
      character*80 temp_char

      logical  hks_has_valid_hits
      logical  hes_has_valid_hits

      external hks_has_valid_hits
      external hes_has_valid_hits
*     
      SAVE
c     --1---------2---------3---------4---------5---------6---------7--
*     
      ABORT = .FALSE.
      err= ' '


      do iwk = 1,2
        call igqwk(iwk,'MXDS',r)
        call ISWKWN(iwk, 0.0,  1.0, 0.0,  1.0)
        call ISWKVP(iwk, 0.0, r(1), 0.0, r(2))
        call IUWK(iwk,1)
      enddo

c      call ISCLIP(0)

      if (show_all_events .or. hks_has_valid_hits()
     +                    .or. hes_has_valid_hits() ) then
        iwk=1
        call IACWK(iwk)                                                 ! activate the workstation
        call ICLRWK(iwk,1)                                              ! clear the output area
        call hes_detector_drawing(Abort,err)
        call IDAWK(iwk)                                                 ! deactivate the workstation

        iwk=2
        call IACWK(iwk)                                                 ! activate the workstation
        call ICLRWK(iwk,1)                                              ! clear the output area
        call hks_detector_drawing(Abort,err)
        call IDAWK(iwk)                                                 ! deactivate the workstation
      endif


      if (show_all_events .or. hes_has_valid_hits() ) then
        iwk=1
        call IACWK(iwk)                                                 ! activate the workstation
        call hes_hits_drawing(Abort,err)
        call IUWK(iwk,1)                                                ! refresh the workstation iwk
        call IDAWK(iwk)                                                 ! deactivate the workstation
      endif

      if (show_all_events .or. hks_has_valid_hits() ) then
        iwk=2
        call IACWK(iwk)                                                 ! activate the workstation
        call hks_hits_drawing(Abort,err)
        call IUWK(iwk,1)                                                ! refresh the workstation iwk
        call IDAWK(iwk)                                                 ! deactivate the workstation
      endif



      if (show_all_events .or. hks_has_valid_hits()
     +                    .or. hes_has_valid_hits() ) then
        if (wait_next_event) then
          read(*,*) temp_char
        endif 
      endif 



      return

      end
c     --1---------2---------3---------4---------5---------6---------7--

