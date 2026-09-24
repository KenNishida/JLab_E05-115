      SUBROUTINE H_TRACK(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods :  Finds and fits tracks in HKS focal plane 
*     -
*     -      Required Input BANKS     HKS_DECODED_DC
*     -
*     -      Output BANKS             HKS_FOCAL_PLANE
*     -                               HKS_DECODED_DC hit coordinates
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     -   Created 19-JAN-1994   D. F. Geesaman
*     $Log: h_track.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.2  2005/07/06 02:32:26  sumihama
*     Mod. hist
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.4  2005/04/08 20:52:52  miyoshi
*     change if statement
*
*     Revision 1.3  2005/03/14 19:52:17  miyoshi
*     change tracking variables and histogram names
*
*     Revision 1.2  2004/12/24 19:47:07  miyoshi
*     change minor
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.5  1996/09/04 20:19:45  saw
*     (JRA) Initialize sstubmin variables
*     
*     Revision 1.4  1995/10/11 12:31:21  cdaq
*     (JRA) Only call tracking routines when it is warranted
*     
*     Revision 1.3  1995/05/22 19:46:00  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.2  1994/04/13  18:51:49  cdaq
*     (DFG) Add call to s_fill_dc_fp_hist
*     
*     Revision 1.1  1994/02/21  16:42:12  cdaq
*     Initial revision
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*7 here
      parameter (here= 'H_TRACK')
*     
      logical ABORT
      character*(*) err
      integer*4 ierr
      character*5  line_err
      integer*4 dummy,temp_number(12),i,j
      Real*4 dummy_r
*     

      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'hks_tracking.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      Include 'gen_event_info.cmn'
      Include 'hks_id_histid.cmn'
      
*--------------------------------------------------------
*     
*     
*     --- check if tracking has been done.
c     Write(*,*) '(htrack) htrack?',h_track_output_on
      if(h_track_output_on .eq. 1) then ! tracking output exists.
         if(hdc_tot_hits .le. 0) then ! no dc data for dummy analysis
c     Write(*,*) '(htrack) htrack?(2)',h_track_output_on,
c     &           h_kept_track_index,h_kept_ntrack
            if(h_kept_track_index .eq. 0) then
               Read(88,*,ERR=71) h_kept_track_ID_number,h_kept_ntrack
c     Write(*,*) h_kept_track_ID_number,h_kept_ntrack
               h_kept_track_index = 1
            EndIF
            if(h_kept_track_ID_number .eq. gen_event_ID_number) then
               if(h_kept_ntrack .gt. 0) then
                  hntracks_fp = h_kept_ntrack
                  Do i=1,hntracks_fp
                     Read(88,*) hx_fp(i),hy_fp(i),hxp_fp(i),hyp_fp(i),
     &                    (temp_number(j),j=1,12)
c     Write(*,*) '(htrack)',hx_fp(i),hy_fp(i),
c     &                    hxp_fp(i),hyp_fp(i),
c     &                    (temp_number(j),j=1,12)
                  EndDo         ! track loop
               EndIF            ! ntrack>0
               h_kept_track_index = 0
            Else                ! not match
c---  first ,kept=0 < genID=1
               Do while(h_kept_track_ID_number .lt. gen_event_ID_number)
c---  Here trackID < eventID.
c---  h_kept_track_index = 0 ; the next readout is track ID and ntrack.
                  if(h_kept_track_index .eq. 0) then
                     Read(88,*) h_kept_track_ID_number,h_kept_ntrack
c     Write(*,*) h_kept_track_ID_number,h_kept_ntrack
                     h_kept_track_index = 1
                  EndIF
                  if(h_kept_track_ID_number .eq. gen_event_ID_number) then
                     if(h_kept_ntrack .gt. 0) then
                        hntracks_fp = h_kept_ntrack
                        Do i=1,hntracks_fp
                           Read(88,*) hx_fp(i),hy_fp(i),hxp_fp(i),hyp_fp(i),
     &                          (temp_number(j),j=1,12)
c     Write(*,*) '(htrack)',hx_fp(i),hy_fp(i),
c     &                          hxp_fp(i),hyp_fp(i),
c     &                          (temp_number(j),j=1,12)
                        EndDo   ! track loop
                     EndIF      ! ntrack>0
                     h_kept_track_index = 0
                  EndIF         ! keptID=genID
               EndDo            ! keptID<genID
            EndIf               ! keptID=genID
         EndIF                  ! hdc_tot_hits=0

         Do i=1,hntracks_fp
            hdc_bestchi2_index = i
            hchi2perdof_fp(i) = 1.0
         EndDo
         if(gen_event_ID_number .gt. 28390 .and. 
     &      gen_event_ID_number .lt. 29390  ) then
            Write(*,*) 'ntrack=',hntracks_fp,' ev=',gen_event_ID_number
     &           ,' keptid=',h_kept_track_ID_number
            Do i=1,hntracks_fp
               Write(*,'(10Hx,y,xp,yp=,4(f8.4,X))') hx_fp(i),hy_fp(i),
     &              hxp_fp(i),hyp_fp(i)
            EndDo
         EndIf
      EndIF                     ! h_track_output_on=1
      
c     Write(*,*) '(h_track) hntracks_fp=',hntracks_fp
      
*     
      if(h_track_output_on .ne. 1) then
         call H_PATTERN_RECOGNITION(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif  
        call H_TOF_PRE(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif
*     
      
    
*     
         call H_LEFT_RIGHT(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif
 
         call H_TRACK_SELECTION_PRE(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif

         call H_SELECT_GOOD_COMBINATION(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif


*     
         hstubminx = 999999.
         hstubminy = 999999.
         hstubminxp = 999999.
         hstubminyp = 999999.
         call H_LINK_STUBS(ABORT,err)
         if(ABORT) then
            call G_add_path(here,err)
            return
         endif
      EndIF
      
c*     --- fill hntracks_fp
c      Call HF1(hidhntracksfp,float(hntracks_fp),1.)
c      Call HF1(hidhntracksfpzoom,float(hntracks_fp),1.)

      if (hntracks_fp .gt. 0) then
*---  if we recode tracking results, skip tracking here.
         if(h_track_output_on .ne. 1) then
            call H_TRACK_FIT(ABORT,err,ierr)
            if(ABORT) then
               call G_add_path(here,err)
               return
            endif
         EndIf
      EndIf                     ! hntracks_fp>0
      
*     --- Histogram for dc track bank 
      call h_fill_dc_track_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif
      
*     Check for internal error in H_TRACK_FIT
      if(ierr.ne.0) then
         line_err=' '
         call CSETDI(ierr,line_err,1,5)
         err='MUNUIT ERROR IN H_TRACK_FIT' // line_err
         call G_add_path(here,err)
         call G_LOG_MESSAGE(err)
      endif

      goto 72

 71   Write(*,*) 'Error in readout of lun=88'
 72   continue

      return
      end
