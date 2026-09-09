      Subroutine e_prt_raw_misc(ABORT,err)
*--------------------------------------------------------
* Dump all raw HNSS_RAW_MISC bank
*
* $Log: e_prt_raw_misc.f,v $
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
* Revision 1.1  2000/03/09 02:04:49  ysato
* Update in the production run Mar.8
*
*
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Character*50 here
      Parameter (here='e_prt_raw_misc')
*
      Logical ABORT
      Character*(*) err

      Include 'enge_data_structures.cmn'

      Integer*4 j

      write(*,'(''        ENGE_RAW_MISC BANKS'')')
      write(*,'(''     emisc_raw_nhits='',I4)') emisc_raw_nhits
      if(emisc_raw_nhits.GT.0) then
        write(*,'('' Num  Plane    Channel      ADC     TDC'')')
        write(*,'(1x,i4,2x,i3,7x,i4,4x,2i8)')
     &       (j,emisc_raw_plane_num(j),emisc_raw_channel_num(j),
     &        emisc_raw_adc(j),emisc_raw_tdc(j),j=1,emisc_raw_nhits )
      endif

      return
      end
