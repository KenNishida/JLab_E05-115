      SUBROUTINE G_initialize(ABORT,err)
*----------------------------------------------------------------------
*     -       Prototype hall C initialize routine
*     - 
*     -   Purpose and Methods : Initialization is performed and status returned
*     - 
*     -   Output: ABORT      - success or failure
*     -         : err        - reason for failure, if any
*     - 
*     -   Created   9-Nov-1993   Kevin B. Beard
*     -   Modified 20-Nov-1993   Kevin B. Beard
*     -   Modified 02-Jul-1999   Jinghua Liu for HNSS
*     
*     Revision 2.1   2000/02/16 15:47:13  jinghua
*     (JLiu) Removed the entire "hack*" part
*     
*     Revision 2.0   1999/07/02 12:00:00  jinghua
*     (JLiu) Change HMS to HNSS
*     
*     Revision 1.21  1996/11/05 21:41:36  saw
*     (SAW) Use CTP routines as functions rather than subroutines for
*     porting.
*     
*     Revision 1.20  1996/09/04 14:37:56  saw
*     (JRA) Open output file for charge scalers
*     
*     Revision 1.19  1996/04/29 19:47:42  saw
*     (JRA) Add call to engine_command_line
*     
*     Revision 1.18  1996/01/22 15:18:12  saw
*     (JRA) Add call to g_target_initialize.  Remove call to
*     g_kludge_up_kinematics
*     
*     Revision 1.17  1996/01/16 18:24:47  cdaq
*     (JRA) Get kinematics for runinfo event, create a tcl stats screen.  Groupify
*     CTP calls
*     
*     Revision 1.16  1995/10/09 18:42:57  cdaq
*     (SAW) Move loading of ctp_kinematics database to before CTP loading.  Take
*     ntuple inialization out of spec specific init routines into a all ntuple
*     init routine.
*     
*     Revision 1.15  1995/09/01 14:29:41  cdaq
*     (JRA) Zero run time variable, read kinematics database after last book
*     
*     Revision 1.14  1995/07/27  19:36:41  cdaq
*     (SAW) Relocate data statements for f2c compatibility, check error returns
*     on thload calls and quit if important files are missing.
*     
*     Revision 1.13  1995/05/22  20:41:40  cdaq
*     (SAW) Split g_init_histid into h_init_histid and s_init_histid
*     
*     Revision 1.12  1995/04/01  19:47:22  cdaq
*     (SAW) One report file for each of g, h, s, c instead of a single report file
*     Allow %d for run number in filenames
*     
*     Revision 1.11  1994/10/11  18:39:40  cdaq
*     (SAW) Add some hacks for event display
*     
*     Revision 1.10  1994/09/21  19:52:57  cdaq
*     (SAW) Cosmetic change
*     
*     Revision 1.9  1994/08/30  14:47:41  cdaq
*     (SAW) Add calls to clear the test flags and scalers
*     
*     Revision 1.8  1994/08/18  03:45:01  cdaq
*     (SAW) Correct typo in adding hack stuff
*     
*     Revision 1.7  1994/08/04  03:08:11  cdaq
*     (SAW) Add call to Breuer's hack_initialize
*     
*     Revision 1.6  1994/06/22  20:55:14  cdaq
*     (SAW) Load report templates
*     
*     Revision 1.5  1994/06/04  02:35:59  cdaq
*     (KBB) Make sure CTP files are non-blank before trying to thload them
*     
*     Revision 1.4  1994/04/12  20:59:21  cdaq
*     (SAW) Add call to calculation of histid's for hfilled histograms
*     
*     Revision 1.3  1994/03/24  22:02:31  cdaq
*     Reorganize for online compatibility
*     
*     Revision 1.2  1994/02/11  18:34:49  cdaq
*     Split off CTP variables registration from initialize routines
*     
*     Revision 1.1  1994/02/04  22:00:26  cdaq
*     Initial revision
*     
*     -
*     - All standards are from "Proposal for Hall C Analysis Software
*     - Vade Mecum, Draft 1.0" by D.F.Geesamn and S.Wood, 7 May 1993
*     -
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*12 here
      parameter (here= 'G_initialize')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_filenames.cmn'               !all setup files
      INCLUDE 'hes_filenames.cmn'
      INCLUDE 'hks_filenames.cmn'
      INCLUDE 'coin_filenames.cmn'
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_pawspace.cmn'        
*     !includes sizes of special CERNLIB space
      INCLUDE 'gen_run_info.cmn'
      include 'gen_scalers.cmn'
      include 'hes_data_structures.cmn'
      include 'hks_data_structures.cmn'
*
      integer ierr
      logical HES_ABORT,HKS_ABORT
      character*132 HES_err,HKS_err
*
      character*72 file
      logical*4 first_time                      ! Allows routine to be called 
      save first_time
      data first_time /.true./                  ! by online code
*     
*--------------------------------------------------------
*     
      ABORT= .FALSE.            !clear any old flags
      err= ' '                  !erase any old errors
      HES_err= ' '
      HKS_err= ' '
*     
*     set the runtime variable to avoid divide by zero during report
*     
      g_run_time = 0.0001
*     
*     Book the histograms, tests and parameters
*     
      if(first_time) then
         call HLIMIT(G_sizeHBOOK) !set in "gen_pawspace.cmn"
      endif
*     Load and book all the CTP files
*     
*     
      if((first_time.or.g_parm_rebook).and.g_ctp_parm_filename.ne.' ') then
         file = g_ctp_parm_filename
         call g_sub_run_number(file,gen_run_number)
         if(thload(file).ne.0) then
            ABORT = .true.
            err = file
         endif
         ierr = thbook()        ! Assert parm values
      endif                     ! so that ctp_database can override
*     
*     
*     Now if there is a g_ctp_kinematics_filename set, pass the run number
*     to it to set CTP variables.  Parameters placed in this file will
*     override values defined in the CTP input files.
*     
      if(.not.ABORT.and.g_ctp_kinematics_filename.ne.' ') then
         write(6,'(a,a60)') 'KINEMATICS FROM ',
     &        g_ctp_kinematics_filename(1:60)
         call g_ctp_database(ABORT, err
     $        ,gen_run_number, g_ctp_kinematics_filename)
         IF(ABORT) THEN
            call G_add_path(here,err)
         endif
      ENDIF
*     
      if((first_time.or.g_test_rebook).and.g_ctp_test_filename.ne.' ') then
         file = g_ctp_test_filename
         call g_sub_run_number(file,gen_run_number)
         if(thload(file).ne.0) then
            ABORT = .true.
            if(err.ne.' ') then
               call g_append(err,' & '//file)
            else
               err = file
            endif
         endif
      endif
      
      write(6,'(a)') 'COMMAND LINE FLAGS'
      call engine_command_line(.true.) ! Reset CTP vars from command line
      
*     that was the last call to engine_command_line, the last time to input
*     ctp variables.  Set some here to avoid divide by zero errors if they
*     were not read in.
      
      if (hpcentral.le.0.001)  hpcentral = 0.5
      if (epcentral.le.0.001)  hpcentral = 0.01
      if (htheta_lab.le.0.001) htheta_lab = 90.
      if (etheta_lab.le.0.001) etheta_lab = 90.
      
      if((first_time.or.g_hist_rebook).and.g_ctp_hist_filename.ne.' ') then
         file = g_ctp_hist_filename
         call g_sub_run_number(file,gen_run_number)
         if(thload(file).ne.0) then
            ABORT = .true.
            if(err.ne.' ') then
               call g_append(err,' & '//file)
            else
               err = file
            endif
         endif
      endif
*     
      if(ABORT) then
         call g_add_path(here,err)
         return                 ! Don't try to proceed
      endif
      
*     
*     Load the report definitions
*     
      print *,'>>> loading the report definitions...'
      if((first_time.or.g_report_rebook)
     $     .and.g_report_template_filename.ne.' ') then
         file = g_report_template_filename
         print *,'>>> ',file
         call g_sub_run_number(file,gen_run_number)
         ierr = thload(file)
      endif
*     
      if((first_time.or.g_report_rebook)
     $     .and.g_stats_template_filename.ne.' ') then
         file = g_stats_template_filename
         print *,'>>> ',file
         call g_sub_run_number(file,gen_run_number)
         ierr = thload(file)
      endif
*     
      if((first_time.or.g_report_rebook)
     $     .and.h_report_template_filename.ne.' ') then
         file = h_report_template_filename
         print *,'>>> ',file
         call g_sub_run_number(file,gen_run_number)
         ierr = thload(file)
      endif
*     
      if((first_time.or.g_report_rebook)
     $     .and.e_report_template_filename.ne.' ') then
         file = e_report_template_filename
         print *,'>>> ',file
         call g_sub_run_number(file,gen_run_number)
         ierr = thload(file)
      endif
*
      if((first_time.or.g_report_rebook)
     $     .and.c_report_template_filename.ne.' ') then
         file = c_report_template_filename
         print *,'>>> ',file
         call g_sub_run_number(file,gen_run_number)
         ierr = thload(file)
         print *,'>>> ',file
      endif
*     
*     Call thbook if any new files have been loaded
*     
      if(first_time.or.g_parm_rebook.or.g_test_rebook
     $     .or.g_hist_rebook.or.g_report_rebook) then
         ierr = thbook()
*     
*     Recalculate all histogram id's of user (hard wired) histograms
*     
         print *,'>>> init histogram...'
         call g_init_histid(ABORT,err)
         call e_init_histid(ABORT,err)
         call h_init_histid(ABORT,err)
         call c_init_histid(ABORT,err)
*     
         if(g_alias_filename.ne.' ') then
            file = g_alias_filename
            call g_sub_run_number(file,gen_run_number)
            ierr = thwhalias(file)
            if (ierr.ne.0) print *,'called haliaswrite',ierr
         endif
      endif
*     
      call thtstclrg("default") ! Clear test flags
      call thtstclsg("default") ! Clear test scalers
*     
      call g_target_initialize(ABORT,err)
      
*     Open output file for charge scalers.
      if (g_charge_scaler_filename.ne.' ') then
         file=g_charge_scaler_filename
         call g_sub_run_number(file,gen_run_number)
         open(unit=G_LUN_CHARGE_SCALER,file=file,status='unknown')
         write(G_LUN_CHARGE_SCALER,*) 
     &        '!Charge scalers - Run #',gen_run_number
         write(G_LUN_CHARGE_SCALER,*) 
     &        '!event   time(sec)    Unser(uC)     BCM1(uC)     BCM2(uC)     mean(uC)'
      endif
      
*     Open output file for epics events.
      if (g_epics_output_filename.ne.' ') then
         file=g_epics_output_filename
         call g_sub_run_number(file,gen_run_number)
         open(unit=G_LUN_EPICS_OUTPUT,file=file,status='unknown')
      endif
      
*     - HES initialize
      print *,'>>> initializing hes...'
      call E_initialize(HES_ABORT,HES_err)
*     
*     - HKS initialize
      print *,'>>> initializeg HKS...'
      call H_initialize(HKS_ABORT,HKS_err)
*     
      ABORT= HES_ABORT .or. HKS_ABORT
      If(HES_ABORT .and. .NOT.HKS_ABORT) Then
         err= HES_err
      ElseIf(HKS_ABORT .and. .NOT.HES_ABORT) Then
         err= HKS_err
      ElseIf(HES_ABORT .and. HKS_ABORT) Then
         err= '&'//HKS_err
         call G_prepend(HES_err,err)
      EndIf
*     
      IF(.NOT.ABORT) THEN
*     
*     -COIN initialize
*     
         call C_initialize(ABORT,err)
*     
      ENDIF
*     
      print *,'>>> initilizing ntuple...'
      call g_ntuple_init(ABORT,err) 
*     
*     -force reset of all space of all working arrays
*     -(clear just zeros the index of each array)
      IF(.NOT.ABORT) THEN
         print *,'>>> reset event...'
         call G_reset_event(ABORT,err)
*     
      ENDIF
*     
      IF(ABORT .or. err.NE.' ') call G_add_path(here,err)
*     
      first_time = .false.
*     
      RETURN
      END
