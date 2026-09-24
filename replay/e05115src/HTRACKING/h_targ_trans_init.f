	subroutine h_targ_trans_init(ABORT,err,istat)
c******************************************************************************
c       
c	Read reconstruction coefficients by ERIKA
c       
c******************************************************************************
	implicit none
	
c       Argument definitions.
	logical ABORT
	character*(*) err
	integer*4       i,j,chan,istat
	include 'gen_filenames.cmn'
	include 'hks_recon_elements.cmn'

	ABORT = .FALSE.
   	chan = G_LUN_TEMP
	
c       Open ERIKA.COE file
	
	open (unit=chan,status='old',file='matrices/hks_t2s_coeff.dat',err=92)
	
c       Read coefficients
	
	CALL GETCOE(chan)
	Write(*,*) 'hks_t2s_coeff.dat is opened successfully.'
	h_recon_initted = 1
	goto 100

 92	istat = 2		!Error opening file.
c       If file does not exist, report err and then continue for development
	err = 'error opening file hks_t2s_coeff.dat'
	call g_rep_err(ABORT,err)
	
 100	close (unit=chan)
*
c     Read in target to collimator transfer matrix elements
      h_t2s_initted = 0
      do j = 1,h_num_t2s_terms
         do i = 1,2
            h_t2s_coeff(i,j) = 0.
         enddo
         do i=1,3
            h_t2s_expon(i,j) = 0.
         enddo   
      enddo
      chan = G_LUN_TEMP
      
!     Open and read in coefficients.
      
      open (unit=chan,status='old',file='matrices/hks_t2s_coeff.dat',err=97)
      Write(*,*) 'hks_t2s_coeff.dat is opened successfully.' 
*
      do j=1,h_num_t2s_terms
         read (chan,1202,err=98) (h_t2s_coeff(i,j),i=1,2),(h_t2s_expon(i,j),i=1,3)
      enddo
      h_t2s_initted = 1
      goto 105
*
 97	istat = 8		!Error opening file.
*     If file does not exist, report err and then continue for development
      err = 'error opening file hks_t2s_coeff.dat'
      call g_rep_err(ABORT,err)
      goto 105
      
 98	istat = 9		!Error reading or processing data.
      ABORT=.true.
      err = 'error processing file hks_t2s_coeff.dat'
      goto 105
 105  close(chan)

 1202 format(2g17.9,1x,3i1)
	
	Return
	END
	
C*****************************************************
	SUBROUTINE GETCOE(local_lun)
C*****************************************************
C       
C       READ IN COEFICIENTS FROM ERIKA.COE FROM LUN:LUNERK
C       
C       FORMAT OF THE ERIKA.COE FILE HAS BEEN CHANGED AND
C       THE RECORD LENGTH IS NOW LESS THAN 80. 3/5/85
C       
C       Y(I,J):     COEFICIENT OF J-TH TERM IN CHEBYSCHEV EXPANSION
C       TO FIT FOR THE I-TH VARIABLE.
C       
C       IPOWER(I,J,K)= ORDER OF CHEBYSCHEV POLYNOMIAL FOR K-TH INDEPENDENT
C       VARIABLE (THE ETA'S) IN THE J-TH TERM IN THE CHEBY-
C       SCHEV FIT TO THE I-TH DEPENDENT VARIABLE.
C       
	integer*4 local_lun
	COMMON/ AVERAG /XAVEL(400)
	COMMON/ MAXMUM /XIMAXE(400)
	COMMON/ COEFIC /Y(400,400)
	COMMON/ POWERS /IPOWER(400,400,12)
	COMMON/ NUMBER /NCORRE,MAXLEV,NTERMS(400)
	COMMON/ MATRIX  /U(400,12)
	DIMENSION IPWR(12),IMAP(12)
	DATA U/4800*0.0/
	DATA XAVEL/400*0.0/
C       
C***************INCERK   *********************
C       
	COMMON/FILNAM/ FLERKN,FLERKP,FILERK
	COMMON/LUNERI/LNERKN,LNERKP,LUNOUT
	CHARACTER*80 FLERKN,FLERKP,FILERK
c	DATA LNERKN/35/
        DATA LNERKP/36/
C*************************************************

	LNERKN = local_lun
c       
 5015	FORMAT(1X, 25I3)
C       NOTE FOLLOWING FORMAT STATEMENTS HAVE BEEN MODIFIED 3/10/85
 6010	FORMAT(1x,8E15.8)
 6020	FORMAT(1H ,10F5.3)
 6031	FORMAT(5E20.8/)
 7010	FORMAT(4I3,G20.12,14I3)
	
	READ(LNERKN,5015) NCORRE
	READ(LNERKN,5015) (IMAP(N),N=1,NCORRE)
	Do N=1,12
	   IMAP(N)=N
	EndDo
	READ(LNERKN,6031) (XAVEL( IMAP(N) ), N=1,NCORRE)
	DO I = 1,NCORRE
	   READ(LNERKN,6010) (U(IMAP(J),I) , J=1,NCORRE)
	EndDo
	READ(LNERKN,6031) (XIMAXE(N), N=1,NCORRE)
	READ(LNERKN,7010) ID,MAXLEV
 190	NUM = 0
 200	READ(LNERKN,7010,END=1000)ID,IFIT,IA,IACC,COEF,(IPWR(K),K=1,12),KFITLP
	IF(ID.EQ.69) MAXLEV = IFIT
	IF(ID.EQ.69) GO TO 190
	NUM = NUM + 1
	Y(IFIT,NUM) = COEF
	DO 300 KP = 1,MAXLEV
	   IPOWER(IFIT,NUM,KP) = IPWR(KP)
 300	CONTINUE
	NTERMS(IFIT) = NUM
	GO TO 200
C       
 1000	continue
c 1000	WRITE(6,600) LNERKN
c 600	FORMAT(1H ,'CHEBYCHEB COEFICIENTS HAVE BEEN READ
c	1  FROM LUN:',I2)

c	CLOSE(LNERKN)

	RETURN
	END
