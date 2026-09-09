      Subroutine k_prt_decoded(ABORT,err)
*--------------------------------------------------------
* Purpose and Methods : Dump hnss_decoded banks
*
* $Log: e_prt_decoded.f,v $
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
* Revision 1.1  1999/11/03 16:47:50  ysato
* Internal modification
*
*
* Required Input Banks   hnss_decoded
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Character*50 here
      Parameter (here='k_prt_decoded')
*
      Logical ABORT
      Character*(*) err

      Include "hnss_data_structures.cmn"

      Integer*4 ssd_plane
      Integer*4 i,j
*
      ABORT= .FALSE.
      err= ' '
*
      Write(6,*) '/HNSS_DECODED/'
      Do ssd_plane=1,KNUM_SSD_PLANES
         Do i=1,kssd_nhits(ssd_plane)
            Write(6,*)   'SSD:p,ch,tdc;',
     &           ssd_plane,
     &           kssd_channel_num(ssd_plane,i),
     &           kssd_tdc(ssd_plane,i)
            Do j=1,kscin_mul(ssd_plane,i)
               Write(6,*) '    SCIN:ch,tdc,adc;',
     &              kscin_channel_num(ssd_plane,i,j),
     &              kscin_tdc(ssd_plane,i,j),
     &              kscin_adc(ssd_plane,i,j)
            EndDo
         EndDo
      EndDo

      return
      end
