      subroutine e_targ_trans_init(ABORT,err,istat)
*__________________________________________________________________
*     
*     Facility: CEBAF Hall-C software.
*     
*     Module:   e_targ_trans_init
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
*     initialize ENGE reconstruction coefficients
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
      
      include 'hes_recon_elements.cmn' !Recon coefficients.
      include 'gen_filenames.cmn'
      
!     Misc. variables.
      
      integer*4       i,j,chan
      
      character*132   line
!     ============================= Executable Code ====================
!     Reset flag, and zero arrays.
      err= ' '
      ABORT = .FALSE.

      e_recon_initted = 0
      do j = 1,emax_recon_elements
         do i = 1,4
            e_recon_coeff(i,j) = 0.
            e_recon_expon(i,j) = 0.
         enddo
      enddo
         
      do i = 1,4
         Nfactor_of_efp(i) = 0.
         Center_of_efp(i) = 0.
      enddo
      
      e_ang_slope_x=0.0
      e_ang_slope_y=0.0
      e_ang_offset_x=0.0
      e_ang_offset_y=0.0
      e_det_offset_x=0.0
      e_det_offset_y=0.0
      e_z_true_focus=0.0
      
      istat = 1                 !Assume success.
!     Get an I/O unit to open datafiles.
c     call G_IO_control(chan,'ANY',ABORT,err) !"ASK"="ANY"
      chan = G_LUN_TEMP
      
!     Open and read in coefficients.
      
      open (unit=chan,status='old',file='matrices/hes_recon_coeff.dat',err=92)
      Write(*,*) 'hes_recon_coeff.dat is opened successfully.' 
      
!     Read header comments.
      
      line = '!'
      do while (line(1:1).eq.'!')
         read (chan,1001,err=94) line
      enddo
      
*     Read in focal plane rotation coefficients.
      do while (line(1:4).ne.' ---')
         if(line(1:13).eq.'e_ang_slope_x') 
     &        read(line,1201,err=94) e_ang_slope_x
         if(line(1:13).eq.'e_ang_slope_y') 
     &        read(line,1201,err=94) e_ang_slope_y
         if(line(1:14).eq.'e_ang_offset_x')read(line,1201,err=94)
     &            e_ang_offset_x
         if(line(1:14).eq.'e_ang_offset_y')read(line,1201,err=94)
     &            e_ang_offset_y
         if(line(1:14).eq.'e_det_offset_x')read(line,1201,err=94)
     &            e_det_offset_x
         if(line(1:14).eq.'e_det_offset_y')read(line,1201,err=94)
     &            e_det_offset_y
         if(line(1:14).eq.'e_z_true_focus')read(line,1201,err=94)
     &            e_z_true_focus
         if(line(1:14).eq.'Nfactor_of_exf')read(line,1201,err=94)
     &            Nfactor_of_efp(1)
         if(line(1:15).eq.'Nfactor_of_expf')read(line,1201,err=94)
     &            Nfactor_of_efp(2)
         if(line(1:14).eq.'Nfactor_of_eyf')read(line,1201,err=94)
     &            Nfactor_of_efp(3)
         if(line(1:15).eq.'Nfactor_of_eypf')read(line,1201,err=94)
     &            Nfactor_of_efp(4)
         if(line(1:13).eq.'Center_of_exf')read(line,1201,err=94)
     &            Center_of_efp(1)
         if(line(1:14).eq.'Center_of_expf')read(line,1201,err=94)
     &            Center_of_efp(2)
         if(line(1:13).eq.'Center_of_eyf')read(line,1201,err=94)
     &            Center_of_efp(3)
         if(line(1:14).eq.'Center_of_eypf')read(line,1201,err=94)
     &            Center_of_efp(4)
         read (chan,1001,err=94) line
      enddo
     
!     Read in coefficients and exponents.
      line =' '
      read (chan,1001,err=94) line
      e_num_recon_terms = 0
      do while (line(1:4).ne.' ---')
         e_num_recon_terms = e_num_recon_terms + 1
         if (e_num_recon_terms .gt. emax_recon_elements) goto 96
         read (line,1200,err=94) 
     >        (e_recon_coeff(i,e_num_recon_terms),i=1,4),
     >        (e_recon_expon(j,e_num_recon_terms),j=1,4)
         read (chan,1001,err=94) line
      enddo
!     Data read in OK.
      
      e_recon_initted = 1
      goto 100
      
!     File reading or data processing errors.
      
 92   istat = 2                 !Error opening file.
*     If file does not exist, report err and then continue for development
      err = 'error opening file hes_recon_coeff.dat'
      call g_rep_err(ABORT,err)
      goto 100
      
 94   istat = 4                 !Error reading or processing data.
      ABORT=.true.
      err = 'error processing file hes_recon_coeff.dat'
      goto 100
      
 96   istat = 6                 !Too much data in file for arrays.
      ABORT=.true.
      err = 'too much data in file hes_recon_coeff.dat'
      goto 100
      
!     Done with open file.
      
 100  close (unit=chan)
*     free lun
c      call G_IO_control(chan,'FREE',ABORT,err) !"FINISH"="FREE"
*
c     Read in target to collimator transfer matrix elements
      e_t2s_initted = 0
      do j = 1,e_num_t2s_terms
         do i = 1,2
            e_t2s_coeff(i,j) = 0.
         enddo
         do i=1,3
            e_t2s_expon(i,j) = 0.
         enddo   
      enddo
      chan = G_LUN_TEMP
      
      do i = 1,4
         Nfactor_of_etar(i) = 0.
         Center_of_etar(i) = 0.
      enddo
      
!     Open and read in coefficients.
      
      open (unit=chan,status='old',file='matrices/hes_t2s_coeff.dat',err=97)
      Write(*,*) 'hes_t2s_coeff.dat is opened successfully.' 
      
      line = '!'
      do while (line(1:1).eq.'!')
         read (chan,1001,err=94) line
      enddo
      
*     Read in focal plane rotation coefficients.
      do while (line(1:4).ne.' ---')
         if(line(1:15).eq.'Nfactor_of_expt')read(line,1201,err=98)
     &            Nfactor_of_etar(1)
         if(line(1:15).eq.'Nfactor_of_eypt')read(line,1201,err=98)
     &            Nfactor_of_etar(2)
         if(line(1:15).eq.'Nfactor_of_emom')read(line,1201,err=98)
     &            Nfactor_of_etar(3)
         if(line(1:14).eq.'Center_of_expt')read(line,1201,err=98)
     &            Center_of_etar(1)
         if(line(1:14).eq.'Center_of_eypt')read(line,1201,err=98)
     &            Center_of_etar(2)
         if(line(1:14).eq.'Center_of_emom')read(line,1201,err=98)
     &            Center_of_etar(3)
         read (chan,1001,err=98) line
      enddo
*
      line =' '
c      write(*,*) "nfac",Nfactor_of_etar
c      write(*,*) "cent",Center_of_etar
      read (chan,1001,err=98) line
      e_num_t2s_terms = 0
      do while (line(1:4).ne.' ---')
         e_num_t2s_terms = e_num_t2s_terms + 1
         if (e_num_t2s_terms .gt. emax_t2s_elements) goto 97
         read (line,1202,err=98) 
     >        (e_t2s_coeff(i,e_num_t2s_terms),i=1,2),
     >        (e_t2s_expon(i,e_num_t2s_terms),i=1,3)
         read (chan,1001,err=98) line
      enddo
      e_t2s_initted = 1
      goto 105
*
 97   istat = 8                 !Error opening file.
*     If file does not exist, report err and then continue for development
      err = 'error opening file hes_t2s_coeff.dat'
      call g_rep_err(ABORT,err)
      goto 105
      
 98   istat = 9                 !Error reading or processing data.
      ABORT=.true.
      err = 'error processing file hes_t2s_coeff.dat'
      goto 105
 105  close(chan)

      return
      
!     ============================ Format Statements ===============================
      
 1001 format(a)
 1200 format(4f11.6,4i2)
 1201 format(18x,f5.3)
 1202 format(2f11.6,3i2)
c 1200 format(4g17.9,1x,4i1)
c 1201 format(17x,g16.9)
c 1202 format(2g17.9,1x,3i1)
      end
      

