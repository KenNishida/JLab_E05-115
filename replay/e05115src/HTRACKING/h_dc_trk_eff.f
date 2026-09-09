      SUBROUTINE H_DC_TRK_EFF(ABORT,errmsg)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods : Analyze DC information for each track 
*     -
*     -      Required Input BANKS     SOS_STATISTICS
*     -                               GEN_DATA_STRUCTURES
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     author: John Arrington
*     created: 9/5/95
*     
*     s_dc_trk_eff calculates efficiencies for the drift chambers,
*     using the tracking information.
*     
*     $Log: h_dc_trk_eff.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.2  2004/12/24 21:33:07  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.2  1996/01/17 17:09:36  cdaq
*     (JRA) Change array sizes from sdc_num_layers to SMAX_NUM_DC_LAYERS
*     
*     Revision 1.1  1995/10/09 20:02:37  cdaq
*     Initial revision
*     
*--------------------------------------------------------
      IMPLICIT NONE
*     
      character*12 here
      parameter (here= 'H_DC_TRK_EFF')
*     
      logical ABORT
      character*(*) errmsg
*     
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      include 'hks_tracking.cmn'
      include 'hks_geometry.cmn'
      
      integer*4 pln,hit,ihit,itrk,i
      integer*4 iwire(HMAX_NUM_DC_LAYERS)
      integer*4 ihitwire
      real*4 hitwire
      real*4 hitdist(HMAX_NUM_DC_LAYERS)
      
      save
      
      Do i=1,hnphysics
         itrk = hphys_ntrack(i)
         
*     find nearest wire, and increment 'should have fired' counter.
         do pln=1,hdc_num_layers
            hitwire = hdc_central_wire(pln) +
     &           (hdc_track_coord(itrk,pln)+
     &           hdc_center(pln))/hdc_pitch(pln) 
            hitdist(pln) = (hitwire - nint(hitwire))*hdc_pitch(pln)
            
            if (hdc_wire_counting(pln).eq.0) then !normal wire numbering.
               ihitwire = nint(hitwire)
            else                !backwards numbering.
               ihitwire = (hdc_nrwire(pln) + 1 ) - nint(hitwire)
            endif
            iwire(pln) = max(1,min(hdc_nrwire(pln),ihitwire))
            if (ihitwire.ne.iwire(pln)) hitdist(pln)=99. !if had to reset wire,
                                !make it a 'miss'
            
            if (abs(hitdist(pln)).le.0.3) then !hit close to wire.
               hdc_shouldhit(pln,iwire(pln)) = hdc_shouldhit(pln,iwire(pln)) + 1
            endif
         enddo
         
*     note, this does not look for hits on the track which were NOT in the space
*     point used to fit the track!  (though this is probably OK).
         
         do ihit=2,hntrack_hits(itrk,1)+1
            hit=hntrack_hits(itrk,ihit)
            pln=hdc_layer_num(hit)
            if (iwire(pln).eq.hdc_wire_num(hit) .and. 
     &           abs(hitdist(pln)).le.0.3)then
               hdc_didhit(pln,iwire(pln)) = hdc_didhit(pln,iwire(pln)) + 1
            endif
         enddo
         
      EndDo ! hnphysics loop
      
      return
      end
