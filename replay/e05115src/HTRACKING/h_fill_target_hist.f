      subroutine h_fill_target_hist(Abort,err)
*     
*     routine to fill histograms with HKS_TARGET varibles
*     
*     Author:	D. F. Geesaman
*     Date:     3 May 1994
*     $Log: h_fill_target_hist.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.2  2005/07/03 04:17:41  sumihama
*     Mod. hbook sor s.s
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.2  2005/04/19 17:50:45  miyoshi
*     add new hist
*
*     Revision 1.1  2005/04/08 19:06:41  miyoshi
*     change name for target hist
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.3  1995/05/22 19:45:38  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.2  1994/08/18  04:31:47  cdaq
*     (SAW) Indentation changes
*     
*     Revision 1.1  1994/05/13  03:04:19  cdaq
*     Initial revision
*     
*     -
*--------------------------------------------------------
      IMPLICIT NONE
*     
      character*50 here
      parameter (here= 'h_fill_target_hist')
*     
      logical ABORT
      character*(*) err
      real*4  histval
      integer*4 itrk
      
*     
      include 'hks_data_structures.cmn'
      include 'hks_id_histid.cmn'
*     
      SAVE
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '
*     
*     Make sure there is at least 1 track
      if(HNTRACKS_FP .gt. 0 ) then
*     Loop over all hits
         do itrk=1,HNTRACKS_FP
            call hf1(hidhx_tar,HX_TAR(itrk),1.)
            call hf1(hidhy_tar,HY_TAR(itrk),1.)
            call HF2(hidtarxpyp,hxp_tar(itrk),hyp_tar(itrk),1.)
            call hf1(hidhz_tar,HZ_TAR(itrk),1.)
            call hf1(hidhxp_tar,HXP_TAR(itrk),1.)
            call hf1(hidhyp_tar,HYP_TAR(itrk),1.)
            call hf1(hidhdelta_tar,HDELTA_TAR(itrk),1.)
            call hf1(hidhp_tar,HP_TAR(itrk),1.)
            call hf1(hidhpwide,hp_tar(itrk),1.)
c S.S
            call hf1(hidxsv,hx_sv(itrk),1.)
            call hf1(hidysv,hy_sv(itrk),1.)
            call HF2(hidxysv,hx_sv(itrk),hy_sv(itrk),1.)
         enddo                  ! end loop over hits
      endif                     ! end test on zero hits       

      RETURN
      END
