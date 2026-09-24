      SUBROUTINE H_SCIN_EFF(ABORT,errmsg)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods : Analyze scintillator information for each track 
*     -
*     -      Required Input BANKS     SOS_SCIN_TOF
*     -                               GEN_DATA_STRUCTURES
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     author: John Arrington
*     created: 2/13/95
*     
*     s_scin_eff calculates efficiencies for the hodoscope.
*     
*     $Log: h_scin_eff.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.3  2005/04/08 20:40:57  miyoshi
*     change names
*
*     Revision 1.2  2004/12/24 21:33:07  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.7  1996/01/17 18:59:15  cdaq
*     (JRA) Fix typos
*     
*     Revision 1.6  1995/08/31 15:08:15  cdaq
*     (JRA) Fill dpos (pos. track - pos. hit) histograms
*     
*     Revision 1.5  1995/07/20  19:00:29  cdaq
*     (SAW) Put nint around some things for Ultrix compat.  Put h in front of
*     various *good variables.
*     
*     Revision 1.4  1995/05/22  19:45:54  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.3  1995/05/11  21:17:23  cdaq
*     (JRA) Add position calibration variables
*     
*     Revision 1.2  1995/04/06  19:43:37  cdaq
*     (JRA) Fix some latent HMS variable names
*     
*     Revision 1.1  1995/02/23  15:42:08  cdaq
*     Initial revision
*     
*--------------------------------------------------------
      IMPLICIT NONE
*     
      character*50 here
      parameter (here= 'H_SCIN_EFF')
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
      include 'hks_id_histid.cmn'
      
      integer*4 itrk,jtrk
      integer pln,cnt
      integer hit_cnt(hnum_scin_layers)
      integer nhit
      real dist, histval
      real hit_pos(hnum_scin_layers),hit_dist(hnum_scin_layers)
      save
      
*     find counters on track, and distance from center.
      
      Do itrk=1,HNPHYSICS
         jtrk = HPHYS_NTRACK(itrk)
         if (hschi2perdeg(itrk).le.hstat_maxchisq) 
     &        hstat_numevents=hstat_numevents+1
         
         hit_pos(1)=hsx_fp(itrk) + hsxp_fp(itrk)
     &        *(hscin_zpos(1)+0.5*hscin_1x_dzpos)
         hit_cnt(1)=nint((hit_pos(1)-hscin_center(1,1))/hscin_1x_spacing)+1
         hit_cnt(1)=max(min(hit_cnt(1),nint(hnum_scin_counters(1))),1)
         hit_dist(1)=hit_pos(1)-(hscin_1x_spacing*(hit_cnt(1)-1)
     &        +hscin_center(1,1))
         
         hit_pos(2)=hsy_fp(itrk) + hsyp_fp(itrk)
     &        *(hscin_zpos(2)+0.5*hscin_1y_dzpos)
         hit_cnt(2)=nint((hscin_center(2,1)-hit_pos(2))/hscin_1y_spacing)+1
         hit_cnt(2)=max(min(hit_cnt(2),nint(hnum_scin_counters(2))),1)
         hit_dist(2)=hit_pos(2)-(hscin_center(2,1)-hscin_1y_spacing
     &        *(hit_cnt(2)-1))
         
         hit_pos(3)=hsx_fp(itrk) + hsxp_fp(itrk)
     &        *(hscin_zpos(3)+0.5*hscin_2x_dzpos)
         hit_cnt(3)=nint((hit_pos(3)-hscin_center(3,1))/hscin_2x_spacing)+1
         hit_cnt(3)=max(min(hit_cnt(3),nint(hnum_scin_counters(3))),1)
         hit_dist(3)=hit_pos(3)-(hscin_2x_spacing*(hit_cnt(3)-1)
     &        +hscin_center(3,1))
         
*     Fill dpos (pos. track - pos. hit) histograms
         do nhit=1,hscin_tot_hits
            pln=hscin_layer_num(nhit)
            histval = hscin_center(pln,hscin_counter_num(nhit))
     &           -hit_pos(pln)
            call hf1(hidscindpos(pln),histval,1.)
         enddo
         
*     Record position differences between track and center of scin. and
*     increment 'should have hit' counters
         do pln=1,hnum_scin_layers
            cnt=hit_cnt(pln)
            dist=hit_dist(pln)
            if(abs(dist).le.hstat_slop .and. !hit in middle of scin.
     &           hschi2perdeg(itrk).le.hstat_maxchisq) then
               hstat_trk(pln,hit_cnt(pln))=hstat_trk(pln,hit_cnt(pln))+1
            endif
         enddo
         
         do nhit=1,hscin_tot_hits
            cnt=hscin_counter_num(nhit)
            pln=hscin_layer_num(nhit)
            
*     Record the hits if track is near center of track and the chisquared of the 
*     track is good.
            if(abs(hit_dist(pln)).le.hstat_slop .and. cnt.eq.hit_cnt(pln) .and. 
     &           hschi2perdeg(itrk).le.hstat_maxchisq) then
               
               if (hgood_tdc_pos(jtrk,nhit)) then
                  if (hgood_tdc_neg(jtrk,nhit)) then !both fired
                     hstat_poshit(pln,hit_cnt(pln))=hstat_poshit(pln,hit_cnt(pln))+1
                     hstat_neghit(pln,hit_cnt(pln))=hstat_neghit(pln,hit_cnt(pln))+1
                     hstat_andhit(pln,hit_cnt(pln))=hstat_andhit(pln,hit_cnt(pln))+1
                     hstat_orhit(pln,hit_cnt(pln))=hstat_orhit(pln,hit_cnt(pln))+1
                  else          !pos fired
                     hstat_poshit(pln,hit_cnt(pln))=hstat_poshit(pln,hit_cnt(pln))+1
                     hstat_orhit(pln,hit_cnt(pln))=hstat_orhit(pln,hit_cnt(pln))+1
                  endif
               else             !no pos tdc
                  if (hgood_tdc_neg(jtrk,nhit)) then !neg fired
                     hstat_neghit(pln,hit_cnt(pln))=hstat_neghit(pln,hit_cnt(pln))+1
                     hstat_orhit(pln,hit_cnt(pln))=hstat_orhit(pln,hit_cnt(pln))+1
                  endif         !if neg tdc fired.
               endif            !if pos tdc fired.
               
            endif               !if hit was on good track.
            
            
*     Increment pos/neg/both fired.  Track indepenant, so no chisquared cut (but
*     note that only scintillators on the track are examined.
            
            if (hgood_tdc_pos(jtrk,nhit)) then
               if (hgood_tdc_neg(jtrk,nhit)) then !both fired
                  hbothgood(pln,cnt)=hbothgood(pln,cnt)+1
               else             !pos fired
                  hposgood(pln,cnt)=hposgood(pln,cnt)+1
               endif
            else
               if (hgood_tdc_neg(jtrk,nhit)) then !neg fired
                  hneggood(pln,cnt)=hneggood(pln,cnt)+1
               endif
            endif
            
         enddo                  !loop over hsnum_pmt_hit
      EndDo

      return
      end
