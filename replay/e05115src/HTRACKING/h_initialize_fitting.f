      subroutine h_initialize_fitting
*     This subroutine does the MINUIT initialization for track fitting
*
*     d.f. geesaman               8 Sept 1993
* $Log: h_initialize_fitting.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.3  1996/09/05 20:09:06  saw
* (JRA) Cosmetic
*
* Revision 1.2  1995/05/22 19:45:41  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.1  1994/02/21  16:14:30  cdaq
* Initial revision
*
*
      implicit none
      external H_FCNCHISQ
      real*8 H_FCNCHISQ
      include "hks_data_structures.cmn"
      include "hks_tracking.cmn"
*     local variables
      integer*4 ierr,dummy
      integer*4 mlunin,mlunsave
      real*8 arglis(10)
      parameter(mlunin=5)
      parameter(mlunsave=10)
*     initialize MINUIT lun settings
      call MNINIT(mlunin,hluno,mlunsave)
*     set print to -1 (no output)
      arglis(1)=-1
      call MNEXCM(H_FCNCHISQ,'SET PRI',arglis,1,ierr,dummy)      
      call MNSETI( ' Track fitting in HKS Spectrometer')
      return
      end



