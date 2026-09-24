      Subroutine e_prt_physics(ABORT,err)
*--------------------------------------------------------
*     Purpose and Methods : Dump hnss_physics banks
*     
*     $Log: e_prt_physics.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.2  1999/12/23 19:59:27  ysato
*     Compiled on Redhat Linux
*     
*     Revision 1.1  1999/11/03 16:47:50  ysato
*     Internal modification
*     
*     
*     Required Input Banks   hnss_physics
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_prt_plane')
*     
      Logical ABORT
      Character*(*) err
      
      Include "hes_data_structures.cmn"
      
      Integer*4 j
      
      ABORT= .FALSE.
      err= ' '
      
c      if(e_ntracks.GT.0) then
c         write(*,'(''        ***ENGE_PHYSICS BANKS***'')')
c         write(*,'(''     E_NTRACKS='',I4)') E_NTRACKS
c         write(*,'('' Num  Plane  SSD mul   Pos     TIME'',
c     &        ''    SCIN mul  Ch    TIME''
c     &        ''  Momentum  Time'')')
c         write(*,'(1x,i2,3x,i2,9x,i2,2f8.2,
c     &        9x,i2,3x,i2,f8.3,f8.4,f8.3)')
c     &        (j,e_plane_num(j),e_ssd_mul(j),e_ssd_pos(j),e_ssd_time(j),
c     &        e_scin_mul(j),e_scin_channel_num(j),e_scin_time(j),
c     &        e_fp_mom(j),e_fp_time(j),
c     &        j=1,e_ntracks )
c      endif
      
      return
      end
