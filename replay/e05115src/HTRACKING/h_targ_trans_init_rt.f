      subroutine h_targ_trans_init_rt(ABORT,err,istat)
*__________________________________________________________________
*     
*     Facility: CEBAF Hall-C software.
*     
*     Module:   h_targ_trans_init_rt
*     
*     Version:  0.1 (In development)
*     
*     Revision 1.4  1996/09/04 20:57:23  saw
*     (JRA) Add target x to track definition
*     
*     Revision 1.3  1996/01/17 18:56:58  cdaq
*     (JRA)
*     
*     Revision 1.2  1995/08/08 16:08:36  cdaq
*     (DD) Add detector and angular offsets
*     
*     Revision 1.1  1994/05/13  03:50:18  cdaq
*     Initial revision
*     
*     Abstract: Temporary routine to 
*     initialize HES reconstruction coefficients
*     from a datafile.
*     
*     Output arguments:
*     
*     istat   (integer) Status flag. Value returned indicates the following:
*     = 1      Normal return.
*     = 2      Datafile could not be opened.
*     = 4      Error reading datafile.
*     = 6      Datafile overflowed the internal arrays.
*     
*     Author:   David H. Potterveld, Argonne National Lab, Nov. 1993
*     Modified: D. F. Geesaman   Add Abort, err arguments
*     Use G_IO_CONTROL to get LUN
*______________________________________________________________________________
      
      implicit none
      
!     Argument definitions.
      logical ABORT
      character*(*) err
      
      integer         istat
      
!     Include files.
      
      include 'hks_recon_elements.cmn' !Recon coefficients.
      include 'gen_filenames.cmn'
      
!     Misc. variables.
      
      integer*4       i,j,chan
      
      character*132   line
      
!     ============================= Executable Code ====================
!     Reset flag, and zero arrays.
      err= ' '
      ABORT = .FALSE.
      h_recon_initted = 0
      do j = 1,hmax_recon_elements
         do i = 1,4
            h_recon_coeff(i,j) = 0.
            h_recon_expon(i,j) = 0.
         enddo
      enddo
      
      h_ang_slope_x=0.0
      h_ang_slope_y=0.0
      h_ang_offset_x=0.0
      h_ang_offset_y=0.0
      h_det_offset_x=0.0
      h_det_offset_y=0.0
      h_z_true_focus=0.0
     
      do i=1,4
         Nfactor_of_hfp(i)=0
         Center_of_hfp(i)=0
      enddo 
      istat = 1                 !Assume success.
!     Get an I/O unit to open datafiles.
c     call G_IO_control(chan,'ANY',ABORT,err) !"ASK"="ANY"
      chan = G_LUN_TEMP
      
!     Open and read in coefficients.
      
      open (unit=chan,status='old',file='matrices/hks_recon_coeff.dat',err=92)
      Write(*,*) 'hks_recon_coeff.dat is opened successfully.' 
      
!     Read header comments.
      
      line = '!'
      do while (line(1:1).eq.'!')
         read (chan,1001,err=94) line
      enddo
      
*     Read in focal plane rotation coefficients.
      do while (line(1:4).ne.' ---')
         if(line(1:13).eq.'h_ang_slope_x') 
     &        read(line,1201,err=94) h_ang_slope_x
         if(line(1:13).eq.'h_ang_slope_y') 
     &        read(line,1201,err=94) h_ang_slope_y
         if(line(1:14).eq.'h_ang_offset_x')read(line,1201,err=94)
     &        h_ang_offset_x
         if(line(1:14).eq.'h_ang_offset_y')read(line,1201,err=94)
     &        h_ang_offset_y
         if(line(1:14).eq.'h_det_offset_x')read(line,1201,err=94)
     &        h_det_offset_x
         if(line(1:14).eq.'h_det_offset_y')read(line,1201,err=94)
     &        h_det_offset_y
         if(line(1:14).eq.'h_z_true_focus')read(line,1201,err=94)
     &        h_z_true_focus
         if(line(1:13).eq.'Center_of_hxf')read(line,1201,err=94)
     &        Center_of_hfp(1)
         if(line(1:14).eq.'Center_of_hxpf')read(line,1201,err=94)
     &        Center_of_hfp(2)
         if(line(1:13).eq.'Center_of_hyf')read(line,1201,err=94)
     &        Center_of_hfp(3)
         if(line(1:14).eq.'Center_of_hypf')read(line,1201,err=94)
     &        Center_of_hfp(4)
         if(line(1:14).eq.'Nfactor_of_hxf')read(line,1201,err=94)
     &        Nfactor_of_hfp(1)
         if(line(1:15).eq.'Nfactor_of_hxpf')read(line,1201,err=94)
     &        Nfactor_of_hfp(2)
         if(line(1:14).eq.'Nfactor_of_hyf')read(line,1201,err=94)
     &        Nfactor_of_hfp(3)
         if(line(1:15).eq.'Nfactor_of_hypf')read(line,1201,err=94)
     &        Nfactor_of_hfp(4)
         read (chan,1001,err=94) line
      enddo
       
!     Read in coefficients and exponents.
      line =' '
      read (chan,1001,err=94) line
      h_num_recon_terms = 0
      do while (line(1:4).ne.' ---')
         h_num_recon_terms = h_num_recon_terms + 1
         if (h_num_recon_terms .gt. hmax_recon_elements) goto 96
         read (line,1200,err=94) 
     >        (h_recon_coeff(i,h_num_recon_terms),i=1,4),
     >        (h_recon_expon(j,h_num_recon_terms),j=1,4)
         read (chan,1001,err=94) line
      enddo
      
!     Data read in OK.
      
      h_recon_initted = 1
      goto 100
      
!     File reading or data processing errors.
      
 92   istat = 2                 !Error opening file.
*     If file does not exist, report err and then continue for development
      err = 'error opening file hks_recon_coeff_rt.dat'
      call g_rep_err(ABORT,err)
      goto 100
      
 94   istat = 4                 !Error reading or processing data.
      ABORT=.true.
      err = 'error processing file hks_recon_coeff_rt.dat'
      goto 100
      
 96   istat = 6                 !Too much data in file for arrays.
      ABORT=.true.
      err = 'too much data in file hks_recon_coeff_rt.dat'
      goto 100
      
!     Done with open file.
      
 100  close (unit=chan)
*     free lun
*      call G_IO_control(chan,'FREE',ABORT,err) !"FINISH"="FREE"
*
*     Read in target to collimator transfer matrix elements
      h_t2s_initted = 0
      do j = 1,hmax_t2s_elements
         do i = 1,2
            h_t2s_coeff(i,j) = 0.
         enddo
         do i=1,3
            h_t2s_expon(i,j) = 0.
         enddo   
      enddo
      chan = G_LUN_TEMP
      do i=1,4
         Nfactor_of_htar(i)=0
         Center_of_htar(i)=0
      enddo 
      
!     Open and read in coefficients.
      
      open (unit=chan,status='old',file='matrices/hks_t2s_coeff.dat',err=97)
      Write(*,*) 'hks_t2s_coeff.dat is opened successfully.' 
*
      line = '!'
      do while (line(1:1).eq.'!')
         read (chan,1001,err=98) line
      enddo
      
*     Read in focal plane rotation coefficients.
      do while (line(1:4).ne.' ---')
         if(line(1:15).eq.'Nfactor_of_hxpt')read(line,1201,err=98)
     &        Nfactor_of_htar(1)
         if(line(1:15).eq.'Nfactor_of_hypt')read(line,1201,err=98)
     &        Nfactor_of_htar(2)
         if(line(1:15).eq.'Nfactor_of_hmom')read(line,1201,err=98)
     &        Nfactor_of_htar(3)
         if(line(1:14).eq.'Center_of_hxpt')read(line,1201,err=98)
     &        Center_of_htar(1)
         if(line(1:14).eq.'Center_of_hypt')read(line,1201,err=98)
     &        Center_of_htar(2)
         if(line(1:14).eq.'Center_of_hmom')read(line,1201,err=98)
     &        Center_of_htar(3)
         read (chan,1001,err=98) line
      enddo
      

      line =' '
      read (chan,1001,err=98) line
      h_num_t2s_terms = 0
      do while (line(1:4).ne.' ---')
         h_num_t2s_terms = h_num_t2s_terms + 1
         if (h_num_t2s_terms .gt. hmax_t2s_elements) goto 97
         read (line,1202,err=98) 
     >        (h_t2s_coeff(i,h_num_t2s_terms),i=1,2),
     >        (h_t2s_expon(j,h_num_t2s_terms),j=1,3)
         read (chan,1001,err=98) line
      enddo
      h_t2s_initted = 1
      goto 105
*
 97   istat = 8                 !Error opening file.
*     If file does not exist, report err and then continue for development
      err = 'error opening file hks_t2s_coeff.dat'
      call g_rep_err(ABORT,err)
      goto 105
      
 98   istat = 9                 !Error reading or processing data.
      ABORT=.true.
      err = 'error processing file hks_t2s_coeff.dat'
      goto 105
 105  close(chan)

      return
!     ============================ Format Statements ===============================
      
 1001 format(a)
 1200 format(4f11.6,4i2)
 1201 format(18x,f4.2)
 1202 format(2f11.6,3i2)
c 1200 format(4g17.9,1x,4i1)
c 1201 format(17x,g16.9)
      
      end
      

