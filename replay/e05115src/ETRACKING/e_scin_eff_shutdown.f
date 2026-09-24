      Subroutine e_scin_eff_shutdown(lunout,ABORT,err)
*--------------------------------------------------------
*     Estimate HODOSCOPE efficiency relative to SSD hit
*     
*     $Log: e_scin_eff_shutdown.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.1.1.1  2004/08/30 21:21:41  miyoshi
*     new dir
*
*     Revision 1.2  2000/03/09 01:32:42  ysato
*     Update in the production run Mar.8
*     
*     Revision 1.1  1999/12/23 19:59:28  ysato
*     Compiled on Redhat Linux
*     
*     Revision 1.1  1999/11/03 16:47:53  ysato
*     Internal modification
*     
*     
*     November 3, 1999        Y.Sato        A first draft
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Character*50 here
      Parameter (here='e_scin_eff_shutdown')
*
      Include "hes_data_structures.cmn"
      Include "hes_statistics.cmn"

      Integer lunout
      Logical ABORT
      Character*(*) err

      Integer*4 plane

      ABORT= .FALSE.
      err= ' '

*     --- SCIN rough efficiency calculation ---
c     do plane=1,ENUM_SCIN_PLANES
c     if(escin_rough_tot_events(plane).gt.0) then
c     escin_plane_rough_eff(plane) = float(escin_rough_events(plane)) 
c     &           / float(escin_rough_tot_events(plane))
c     endif
c     enddo

      return
      end
