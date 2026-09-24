      subroutine h_track_ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the SOS Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
* $Log: e_dc_ntuple_keep.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.2  2005/01/04 23:56:21  miyoshi
* add detector ntuple
*
* Revision 1.1  2005/01/03 19:26:30  miyoshi
* add edc1 ntuple
*
* Revision 1.1.1.1  2004/08/30 21:21:38  miyoshi
* new dir
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='h_track_ntuple_keep')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'hks_scin_parms.cmn'
      INCLUDE 'hks_physics_sing.cmn'
      INCLUDE 'hks_tracking.cmn'
      INCLUDE 'hks_statistics.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'gen_run_info.cmn'
*     
      logical HEXIST    !CERNLIB function
*
      integer m,i,j,k,ihit,pln,hit
      real*4 ddis(hdc_num_layers)
      real*4 dt(hdc_num_layers)

      real proton_mass
      parameter ( proton_mass = 0.93827247 ) ! [GeV/c^2]
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(.NOT.h_track_ntuple_exists) RETURN !nothing to do
*     
c      Do i=1,enphysics
c      Do i=1,hmax_dc1_dec_hits
      Do i=1,hntracks_fp
*      Do i=1,hmax_dc_hits
*     --- put cut condition here 
c         if(
c     &        esp(i) .gt. 0
c     &        edc1_layer_num(i) .gt. 0
c     &     ) then
         
         do j = 1,hdc_num_layers
            dt(j)=-1000
            ddis(j)=-1000
         enddo

         do ihit = 2,hntrack_hits(i,1)+1
            hit = hntrack_hits(i,ihit)
            pln = hdc_layer_num(hit)
            dt(pln) = hdc_drift_time(hit)
            ddis(pln) = hdc_drift_dis(hit)
         enddo
            
            m= 0
            
            Do j=1,hdc_num_layers 
               m= m+1
               h_track_ntuple_contents(m)=hdc_single_residual(i,j) ! 1
            Enddo
             
            Do j=1,hdc_num_layers 
               m= m+1
               h_track_ntuple_contents(m)=hdc_track_coord(i,j) ! 11
            Enddo
            
            Do j=1,hdc_num_layers 
               m= m+1
               h_track_ntuple_contents(m)= ddis(j) ! 21
            Enddo
            
            Do j=1,hdc_num_layers 
               m= m+1
               h_track_ntuple_contents(m)= dt(j) ! 31
            Enddo
            
            Do j=1,hdc_num_layers 
               m= m+1
               h_track_ntuple_contents(m)= 
     &          hdc_layer_wirecenter(i,j) ! 31
            Enddo
            
            m= m+1
            h_track_ntuple_contents(m)= hchi2perdof_fp(i) ! 41
            
            m= m+1
            h_track_ntuple_contents(m)= hx_fp(i) ! 42
            m= m+1
            h_track_ntuple_contents(m)= hy_fp(i) ! 43
            m= m+1
            h_track_ntuple_contents(m)= hxp_fp(i) ! 44
            m= m+1
            h_track_ntuple_contents(m)= hyp_fp(i) ! 45
            
  
*     Fill ntuple for this event
            ABORT= .NOT.HEXIST(h_track_ntuple_ID)
            IF(ABORT) THEN
               call G_build_note(':Ntuple ID#$ does not exist',
     &              '$',h_track_ntuple_ID,' ',0.,' ',err)
               call G_add_path(here,err)
            ELSE
               call HFN(h_track_ntuple_ID,h_track_ntuple_contents)
            ENDIF
*     
      EndDo
      
      RETURN
      END      
