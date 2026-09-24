      subroutine e_fill_target_hist(Abort,err)
*
*     routine to fill histograms with ENGE_TARGET varibles
*
*     Author:	D. F. Geesaman
*     Date:     3 May 1994
*
* $Log: e_fill_target_hist.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.3  2005/07/03 04:18:31  sumihama
* Mod. hbook sor s.s
*
* Revision 1.2  2005/06/23 17:17:02  cdaq
* Add histgrams for ENGE optics
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1  2005/04/08 18:40:39  miyoshi
* change name
*
* Revision 1.2  2005/03/02 16:28:05  miyoshi
* move hist cmn file
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.3  1995/05/22 19:45:38  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.2  1994/08/18  04:31:47  cdaq
* (SAW) Indentation changes
*
* Revision 1.1  1994/05/13  03:04:19  cdaq
* Initial revision
*
*-
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*50 here
      parameter (here= 'e_fill_target_hist')
*
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 itrk

*
      include 'hes_data_structures.cmn'
      include 'hes_id_histid.cmn'
*     
      SAVE
*--------------------------------------------------------
*
      ABORT= .FALSE.
      err= ' '
*
* Make sure there is at least 1 track
      if(ENTRACKS_FP .gt. 0 ) then
* Loop over all hits
        do itrk=1,ENTRACKS_FP
          call hf1(eidxtar,EX_TAR(itrk),1.)
          call hf1(eidytar,EY_TAR(itrk),1.)
          call hf1(eidxptar,EXP_TAR(itrk),1.)
          call hf1(eidyptar,EYP_TAR(itrk),1.)
          call hf1(eiddeltatar,EDELTA_TAR(itrk),1.)
          call hf1(eidptar,EP_TAR(itrk),1.)
          call hf2(eidtarxy,EX_TAR(itrk),ey_tar(itrk),1.)
          call hf2(eidtarxpyp,exp_tar(itrk),eyp_TAR(itrk),1.)
          call hf2(eidtarxxp,ex_tar(itrk),exp_TAR(itrk),1.)
          call hf2(eidtarxyp,ex_tar(itrk),eyp_TAR(itrk),1.)
          call hf2(eidtaryxp,ey_tar(itrk),exp_TAR(itrk),1.)
          call hf2(eidtaryyp,ey_tar(itrk),eyp_TAR(itrk),1.)
          call hf1(eidxsv,EX_SV(itrk),1.)
          call hf1(eidysv,EY_SV(itrk),1.)
          call hf2(eidxysv,EX_sv(itrk),ey_sv(itrk),1.)
*     
* 
        enddo                           ! end loop over hits
      endif                             ! end test on zero hits       
      RETURN
      END
