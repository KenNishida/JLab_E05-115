      Subroutine e_prt_raw_scin(ABORT,err)
*--------------------------------------------------------
* Dump all raw HNSS_RAW_SCIN bank
*
* $Log: e_prt_raw_scin.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
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
      Parameter (here='e_prt_raw_scin')
*
      Logical ABORT
      Character*(*) err

      Include 'enge_data_structures.cmn'

      Integer*4 j

      write(*,'(''        ENGE_RAW_SCIN BANKS'')')
      write(*,'(''     escin_raw_nhits='',I4)') escin_raw_nhits
      if(escin_raw_nhits.GT.0) then
        write(*,'('' Num  Plane    Channel      ADC     TDC'')')
        write(*,'(1x,i4,2x,i3,7x,i4,4x,2i8)')
     &       (j,escin_raw_plane_num(j),escin_raw_channel_num(j),
     &        escin_raw_adc(j),escin_raw_tdc(j),j=1,escin_raw_nhits )
      endif

      return
      end
