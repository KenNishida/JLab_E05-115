      SUBROUTINE H_SCIN_EFF_SHUTDOWN(lunout,ABORT,errmsg)
*--------------------------------------------------------
*-
*-   Purpose and Methods : Analyze scintillator information for each track 
*-
*-      Required Input BANKS     SOS_SCIN_TOF
*-                               GEN_DATA_STRUCTURES
*-
*-   Output: ABORT           - success or failure
*-         : err             - reason for failure, if any
*- 
* author: John Arrington
* created: 2/15/95
*
* s_scin_eff calculates efficiencies for the hodoscope.
* s_scin_eff_shutdown does some final manipulation of the numbers.
*
* $Log: h_scin_eff_shutdown.f,v $
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
* Revision 1.9  1999/02/23 18:59:27  csa
* (JRA) Remove sdebugcalcpeds stuff
*
* Revision 1.8  1996/09/05 20:15:12  saw
* (JRA) Cosmetic
*
* Revision 1.7  1996/01/17 18:58:53  cdaq
* (JRA) Add debug control flag around write statements
*
* Revision 1.6  1995/08/31 15:08:52  cdaq
* (JRA) Dump bad counter infomation
*
* Revision 1.5  1995/07/20  19:00:54  cdaq
* (SAW) Move data statement for f2c compatibility
*
* Revision 1.4  1995/05/22  19:45:54  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/05/17  16:44:17  cdaq
* (JRA) Write out list of potential PMT problems
*
* Revision 1.2  1995/05/11  21:17:34  cdaq
* (JRA) Add position calibration variables
*
* Revision 1.1  1995/03/13  18:18:07  cdaq
* Initial revision
*
*--------------------------------------------------------
      IMPLICIT NONE
*
      character*19 here
      parameter (here= 'H_SCIN_EFF_SHUTDOWN')
*
      logical ABORT
      character*(*) errmsg
*     
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hks_scin_parms.cmn'
      include 'hks_scin_tof.cmn'
      include 'hks_statistics.cmn'
      include 'hks_tracking.cmn'

      logical written_header
      integer pln,cnt
      integer lunout
      real*4 num_real,nhits_real
      real*4 p1,p2,p3           !prob. of having both tubes fire for layers1-4
      real*4 p123               !prob. of having combos fire
      
      character*4 layername(HNUM_SCIN_LAYERS)
      data layername/'hH1X','hH1Y','hH2X','hH2Y'/
      
      save
      
      written_header = .false.
      
!     fill sums over counters
      do pln=1,hnum_scin_layers
         hstat_trksum(pln)=0
         hstat_possum(pln)=0
         hstat_negsum(pln)=0
         hstat_andsum(pln)=0
         hstat_orsum(pln)=0
         do cnt=1,hnum_scin_counters(pln)
            num_real=float(max(1,hscin_zero_num(pln,cnt)))
            hscin_zero_pave(pln,cnt)=float(hscin_zero_pos(pln,cnt))/num_real
            hscin_zero_nave(pln,cnt)=float(hscin_zero_neg(pln,cnt))/num_real
            hstat_trksum(pln)=hstat_trksum(pln)+hstat_trk(pln,cnt)
            hstat_possum(pln)=hstat_possum(pln)+hstat_poshit(pln,cnt)
            hstat_negsum(pln)=hstat_negsum(pln)+hstat_neghit(pln,cnt)
            hstat_andsum(pln)=hstat_andsum(pln)+hstat_andhit(pln,cnt)
            hstat_orsum(pln)=hstat_orsum(pln)+hstat_orhit(pln,cnt)
*     
*     write out list of possible problms
*     
            nhits_real = max(1.,float(hstat_trk(pln,cnt)))
            hstat_neff(pln,cnt)=float(hstat_neghit(pln,cnt))/nhits_real
            hstat_peff(pln,cnt)=float(hstat_poshit(pln,cnt))/nhits_real
            hstat_oeff(pln,cnt)=float(hstat_orhit(pln,cnt))/nhits_real
            hstat_aeff(pln,cnt)=float(hstat_andhit(pln,cnt))/nhits_real
            if (nhits_real .gt. 100.) then !dump bad counter information
               if (hstat_peff(pln,cnt).le.hstat_mineff) then
                  if (.not.written_header) then
                     write(lunout,*)
                     write(lunout,'(a,f6.3)') 
     $                    ' HKS scintilators with effic. < '
     $                    ,hstat_mineff
                     written_header = .true.
                  endif
                  write(lunout,'(5x,a4,i2,a,f7.4)') 
     $                 layername(pln),cnt,'+',hstat_peff(pln,cnt)
               endif
               if (hstat_neff(pln,cnt).le.hstat_mineff) then
                  if (.not.written_header) then
                     write(lunout,*)
                     write(lunout,'(a,f6.3)') ' HKS scintillators ',
     $                    'with tracking based effic. < '
     $                    ,hstat_mineff
                     written_header = .true.
                  endif
                  write(lunout,'(5x,a4,i2,a,f7.4)') 
     $                 layername(pln),cnt,'-',hstat_neff(pln,cnt)
               endif
            endif
         enddo
         hstat_poseff(pln)=hstat_possum(pln)
     $        /max(1.,float(hstat_trksum(pln)))
         hstat_negeff(pln)=hstat_negsum(pln)
     $        /max(1.,float(hstat_trksum(pln)))
         hstat_andeff(pln)=hstat_andsum(pln)
     $        /max(1.,float(hstat_trksum(pln)))
         hstat_oreff(pln)=hstat_orsum(pln)
     $        /max(1.,float(hstat_trksum(pln)))
      enddo
      
      write(lunout,*) ' '
      p1=hstat_andeff(1)
      p2=hstat_andeff(2)
      p3=hstat_andeff(3)
      
!     probability that ONLY the listed layers had triggers
      p123 = p1*p2*p3
      heff_3_of_3=p123

      return
      end
