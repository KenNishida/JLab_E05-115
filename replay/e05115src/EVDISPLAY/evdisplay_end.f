      subroutine evdisplay_end(Abort,err)

      implicit none
*
      character*50 here
      parameter (here= 'evdisplay_end')
*     
      logical ABORT
      character*(*) err
*     
      include 'gen_pawspace.cmn'
c     ---1---------2---------3---------4--------5---------6--------7--

      call IGEND

      return
      end
