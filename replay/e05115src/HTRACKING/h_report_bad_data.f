      SUBROUTINE H_REPORT_BAD_DATA(lunout,ABORT,errmsg)

*--------------------------------------------------------
*
*   Purpose and Methods: Output warnings for possible hardware problems
*          in file 'bad<runnum>.txt' (unit=lunout)
*
*      NOTE: Nothing should be written to the file unless there is a warning
*             to be reported.  (i.e. check for error messages before writing
*             headers.
*
*  Required Input BANKS: 
*
*                Output: ABORT           - success or failure
*                      : err             - reason for failure, if any
* 
* author: John Arrington
* created: 8/17/95
* $Log: h_report_bad_data.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.2  2004/12/24 21:35:57  miyoshi
* change name plane to layer
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
*
* Revision 1.4 2004/03/02 Miyoshi
* for E01-011
*
* Revision 1.3  1996/09/05 20:14:25  saw
* (JRA) Don't report difference between input pedestals and pedestals from
*       pedestal events
*
* Revision 1.2  1996/01/17 18:59:46  cdaq
* (JRA) Warn when pedestals change too much
*
* Revision 1.1  1995/08/31 20:43:52  cdaq
* Initial revision
*
*--------------------------------------------------------

      IMPLICIT NONE
*
      character*17 here
      parameter (here= 'H_REPORT_BAD_DATA')
*
      logical ABORT
      character*(*) errmsg
*
      include 'hks_data_structures.cmn'
      include 'hks_pedestals.cmn'

      integer*4 lunout
      integer*4 ind
      integer*4 icol,irow

      character*4 pln(hnum_scin_layers)
      character*2 cnt(hnum_scin_elements)
      character*1 sgn(2)
      save

      data pln/'hH1X','hH1Y','hH2X','hH2Y'/
      data cnt/'01','02','03','04','05','06','07','08',
     &      '09','10','11','12','13','14','15','16','17','18'/
      data sgn/'+','-'/


! Remove reporting of difference between pedestals and input pedestals
! from parameter files now that we always use the pedestal events.
!
* report channels where the pedestal analysis differs from the param file.
!      if ((hhodo_num_ped_changes)
!     &     .gt. 0) then
!
!        write(lunout,*) '  SOS detectors with large (>2sigma) pedestal changes'
!        write(lunout,*)
!        write(lunout,*) ' Signal  Pedestal change(new-old)'
!
!        if (hhodo_num_ped_changes.gt.0) then
!          do ind=1,hhodo_num_ped_changes
!            write(lunout,'(2x,a4,a2,a1,f9.1)')
!     $           pln(hhodo_changed_layer(ind))
!     $           ,cnt(hhodo_changed_element(ind))
!     $           ,sgn(hhodo_changed_sign(ind)),hhodo_ped_change(ind)
!          enddo
!        endif
!
!      endif              ! are there pedestal changes to report?

      return
      end
