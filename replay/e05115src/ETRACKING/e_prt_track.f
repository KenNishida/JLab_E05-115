      Subroutine e_prt_track(ABORT,err)
*--------------------------------------------------------
* Print hnss_track_test Bank
*
* $Log: e_prt_track.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:41  miyoshi
* new dir
*
* Revision 1.2  2000/03/09 01:32:41  ysato
* Update in the production run Mar.8
*
* Revision 1.1  1999/12/23 19:59:27  ysato
* Compiled on Redhat Linux
*
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Character*50 here
      Parameter (here='e_prt_track')
*
      Logical ABORT
      Character*(*) err

      Include 'enge_data_structures.cmn'

      Integer*4 j

      ABORT= .FALSE.
      err= ' '

      write(*,'(''        ***ENGE_TRACK_TEST BANKS***'')')
      write(*,'(''     ENTRACKS_TEST='',I4)') ENTRACKS_TEST
      if(ENTRACKS_TEST.GT.0) then
        write(*,'('' Num  Plane  SSD Ch     TIME'',
     &        ''    SCIN mul  Ch      TIME'')')
        write(*,'(1x,i4,3x,i2,7x,i4,2x,f8.2,
     &            8x,i2,3x,i2,2x,f8.3)')
     &       (j,etrack_plane_num(j),etrack_ssd_channel_num(j),
     &       etrack_ssd_time(j),etrack_scin_mul(j),
     &       etrack_scin_channel_num(j),etrack_scin_time(j),
     &       j=1,ENTRACKS_TEST )
      endif
      
      return
      end
