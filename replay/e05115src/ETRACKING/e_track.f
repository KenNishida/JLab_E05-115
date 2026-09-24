      SUBROUTINE E_TRACK(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods :  Finds and fits tracks in SOS focal plane 
*     -
*     -      Required Input BANKS     ENGE_DECODED_DC
*     -
*     -      Output BANKS             ENGE_FOCAL_PLANE
*     -                               ENGE_DECODED_DC hit coordinates
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     -   Created 19-JAN-1994   D. F. Geesaman
*     $Log: e_track.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.2  2005/09/19 22:05:51  sumihama
*     Mod. EDC tracking by Akhiko and starttime from EHODO
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.4  2005/04/08 19:03:05  miyoshi
*     correct bugs for existing data readout
*
*     Revision 1.3  2005/03/10 16:48:24  miyoshi
*     change tracking variable name
*
*     Revision 1.2  2005/02/22 19:47:12  miyoshi
*     remove dc raw hits condition
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*      
*     Revision 1.6 03/22/2004 Miyoshi
*     for E01-011
*     
*     Revision 1.5  1996/09/04 20:19:45  saw
*     (JRA) Initialize sstubmin variables
*     
*     Revision 1.4  1995/10/11 12:31:21  cdaq
*     (JRA) Only call tracking routines when it is warranted
*     
*     Revision 1.3  1995/05/22 19:46:00  cdaq
*     (SAW) Split gen_data_data_structures 
*     into gen, hms, sos, and coin parts"
*     
*     Revision 1.2  1994/04/13  18:51:49  cdaq
*     (DFG) Add call to s_fill_dc_fp_hist
*     
*     Revision 1.1  1994/02/21  16:42:12  cdaq
*     Initial revision
*     
*--------------------------------------------------------
*     
      IMPLICIT NONE
      SAVE
*     
      character*7 here
      parameter (here= 'E_TRACK')
*     
      logical ABORT
      character*(*) err
      integer*4 ierr
      character*5  line_err
      integer*4 dummy,temp_number(12),i,j
      Real*4 dummy_r
*     
      INCLUDE 'hes_data_structures.cmn'
      INCLUDE 'hes_tracking.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      Include 'gen_event_info.cmn'
      Include 'gen_run_info.cmn'
*     
*--------------------------------------------------------
*     
      edc1_tot_hits = 0
      call e_trans_dc1(ABORT,err)
      if(ABORT) then
         call G_add_path(here,err)
         return
      endif 
      
*     --- check if tracking has been done.
*     output file : output/hes_track_output_%d.dat
c      Write(*,*) '(etrack) etrack?',e_track_output_on
      if(e_track_output_on .eq. 1) then ! tracking output exists.
         if(edc1_tot_hits .le. 0) then ! no dc data for dummy analysis
            if(e_kept_track_index .eq. 0) then
               Read(86,*) e_kept_track_ID_number,e_kept_ntrack 
               e_kept_track_index = 1
            EndIF
            if(e_kept_track_ID_number .eq. gen_event_ID_number) then
               if(e_kept_ntrack .gt. 0) then
                  entracks_fp = e_kept_ntrack
                  Do i=1,entracks_fp
                     Read(86,*) ex_fp(i),ey_fp(i),exp_fp(i),eyp_fp(i),
     &                    (temp_number(j),j=1,10)
c     Write(*,*) '(etrack)',ex_fp(i),ey_fp(i),
c     &                    exp_fp(i),eyp_fp(i),
c     &                    (temp_number(j),j=1,10)
                  EndDo         ! track loop
               EndIF            ! ntrack>0
               e_kept_track_index = 0
            Else                ! not match
c---  first ,kept=0 < genID=1
               Do while(e_kept_track_ID_number .lt. gen_event_ID_number)
c---  Here trackID < eventID.
c---  h_kept_track_index = 0 ; the next readout is track ID and ntrack.
                  if(e_kept_track_index .eq. 0) then
                     Read(86,*) e_kept_track_ID_number,e_kept_ntrack
                     e_kept_track_index = 1
                  EndIF
                  if(e_kept_track_ID_number .eq. gen_event_ID_number) then
                     if(e_kept_ntrack .gt. 0) then
                        entracks_fp = e_kept_ntrack
                        Do i=1,entracks_fp
                           Read(86,*) ex_fp(i),ey_fp(i),exp_fp(i),
     &                          eyp_fp(i),
     &                          (temp_number(j),j=1,10)
c     Write(*,*) '(etrack)',ex_fp(i),ey_fp(i),
c     &                          exp_fp(i),eyp_fp(i),
c     &                          (temp_number(j),j=1,10)
                        EndDo   ! track loop
                     EndIF      ! ntrack>0
                     e_kept_track_index = 0
                  EndIF         ! keptID=genID
               EndDo            ! keptID<genID
            EndIf               ! keptID=genID
         EndIF                  ! edc1_tot_hits=0
         Do i=1,entracks_fp
            echi2perdof_fp(i) = 1.0
         EndDo
         if(gen_event_ID_number .gt. 28390 .and. 
     &      gen_event_ID_number .lt. 29390  ) then
            Write(*,*) 'ntrack=',entracks_fp,' ev=',gen_event_ID_number
     &           ,' keptid=',e_kept_track_ID_number
            Do i=1,entracks_fp
               Write(*,'(10Hx,y,xp,yp=,4(f8.4,X))') ex_fp(i),ey_fp(i),
     &              exp_fp(i),eyp_fp(i)
            EndDo
         EndIf
      EndIF                     ! e_track_output_on=1
      
c     Write(*,*) '(e_track) entracks_fp=',entracks_fp


      edc1nspace_points_tot = 0 
      if(e_track_output_on .ne. 1) then ! output file doesn't exist
* get drift distance         
         call e_dc1_pattern_recognition(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif      
         

* "pre_link" to find a correct hit in EHODO and get starttime by MIZUKI
         if (gen_run_number.ge.50000) then
            call e_dc1_pre_link
         Endif
         
         call e_dc1_cleanup(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif

         entracks_pre = 0
         call e_dc1_left_right(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif

         call E_DC1_TRACK_FIT(ABORT,err,ierr)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif
         
         if(ierr.ne.0) then
            line_err=' '
            call CSETDI(ierr,line_err,1,5)
            err='MINUIT ERROR IN E_DC1_TRACK_FIT' // line_err
            call G_add_path(here,err)
            call G_LOG_MESSAGE(err)
         endif

         call E_SELECT_GOOD_TRACKS(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif                  ! end test on E_SELECT_GOOD_TRACKS ABORT
c         entracks_pre = 0
        
         edc2_tot_hits = 0
         call e_trans_dc2(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif 

         call e_dc2_pattern_recognition(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif      
         
         call e_dc2_left_right(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif
     
         call E_DC2_TRACK_FIT(ABORT,err,ierr)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif

         
*     Check for internal error in E_TRACK_FIT
         if(ierr.ne.0) then
            line_err=' '
            call CSETDI(ierr,line_err,1,5)
            err='MINUIT ERROR IN E_DC2_TRACK_FIT' // line_err
            call G_add_path(here,err)
            call G_LOG_MESSAGE(err)
         endif
      EndIF                     ! e_track_output_on .ne. 1
      
      return
      end
