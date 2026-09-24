	SUBROUTINE H_TARG_TRANS(ABORT,err,istat)
c***************************************************************************
c       
c    Purpose and Methods :  Transforms tracks from SOS focal plane to 
c                           target.
c
c***************************************************************************
*     -      Required Input BANKS     HKS_FOCAL_PLANE
*-
*     -      Output BANKS             HKS_TARGET
*     -
*     -      Output: ABORT           - success or failure
*     -            : err             - reason for failure, if any
*     -      istat   (integer) Status flag. Value returned indicates 
*     -              following:
*     -           = 1      Normal return.
*     -           = 2      Matrix elements not initted correctly.
*__________________________________________________________________________
	
	IMPLICIT NONE
	SAVE
*       
	character*12 here
	parameter (here= 'h_targ_trans')
*       
	logical ABORT
	character*(*) err
	integer*4   istat
	integer*4   i,j,itrk
	Real*4 xyinpt(20),xyfit(40)                        

	INCLUDE 'gen_data_structures.cmn'
	INCLUDE 'hks_data_structures.cmn'
	INCLUDE 'gen_constants.par'
	INCLUDE 'gen_units.par'
	include 'hks_tracking.cmn'
	include 'hks_recon_elements.cmn'
	include 'hks_id_histid.cmn'
	include 'hks_physics_sing.cmn'
	include 'gen_event_info.cmn'
	
*============================ Executable Code ============================
	ABORT = .FALSE.
	err= ' '
	
	
*************************************************************
*       begin of debugging
*       !!! debug only !!!
*************************************************************
	if(h_tar_output_on .eq. 1) then ! tracking output exists.
	   if(h_kept_tar_index .eq. 0) then
	      Read(84,*,ERR=71) h_kept_tar_ID_number,h_kept_ntar
c       Write(*,*) h_kept_track_ID_number,h_kept_ntrack
	      h_kept_tar_index = 1
	   EndIF
	   if(h_kept_tar_ID_number .eq. gen_event_ID_number) then
	      if(h_kept_ntar .gt. 0) then
		 Do i=1,hntracks_fp
		    Read(84,*) hxp_tar(i),hyp_tar(i),hp_tar(i)
		    hdelta_tar(i) = (hp_tar(i)/hpcentral-1)*100
		 EndDo		! track loop
	      EndIF		! ntrack>0
	      h_kept_tar_index = 0
	   Else			! not match
c---    first ,kept=0 < genID=1
	      Do while(h_kept_tar_ID_number .lt. gen_event_ID_number)
c---    Here trackID < eventID.
c---    h_kept_track_index = 0 ; the next readout is track ID and ntrack.
		 if(h_kept_tar_index .eq. 0) then
		    Read(84,*) h_kept_tar_ID_number,h_kept_ntar
c       Write(*,*) h_kept_track_ID_number,h_kept_ntrack
		    h_kept_tar_index = 1
		 EndIF
		 if(h_kept_tar_ID_number .eq. gen_event_ID_number) then
		    if(h_kept_ntar .gt. 0) then
		       Do i=1,hntracks_fp
			  Read(84,*) hxp_tar(i),hyp_tar(i),hp_tar(i)
			  hdelta_tar(i) = (hp_tar(i)/hpcentral-1)*100
		       EndDo	! track loop
		    EndIF	! ntrack>0
		    h_kept_tar_index = 0
		 EndIF		! keptID=genID
	      EndDo		! keptID<genID
	   EndIf		! keptID=genID
	EndIF			! hdc_tot_hits=0

***************************************************************
*       end of debugging
***************************************************************

*       Check for correct initialization.

	if(h_tar_output_on .ne. 1) then
	   
	   if (h_recon_initted.ne.1) then
	      istat = 2
	      return
	   endif
	   istat = 1
	   
*       Loop over tracks.
	   
	   hntracks_tar = hntracks_fp
	   do itrk = 1, hntracks_fp
*       set link between target and focal plane track. Currenty 1 to 1
	      hlink_tar_fp(itrk) = itrk

c       xyinpt(1) =  hx_fp(itrk) ! xf
c       xyinpt(2) =  hxp_fp(itrk) ! xf'
c       xyinpt(3) =  hy_fp(itrk) ! yf
c       xyinpt(4) =  hyp_fp(itrk) ! yf'
c       --- analysis x,y,xp,yp (cm & rad)
c       --- ERIKA x,y,xp,yp (mm & mrad)
	      xyinpt(1) =  hx_fp(itrk) * 10.
	      xyinpt(2) =  hxp_fp(itrk) * 1000.
	      xyinpt(3) =  hy_fp(itrk) * 10.
	      xyinpt(4) =  hyp_fp(itrk) * 1000.
	      
c       xyinpt(5)=xe(11)  ! xt
c       xyinpt(6)=xe(12)  ! yt
	      
c       Write(*,*) 'xyinpt=',(xyinpt(j),j=1,4)
	      
	      call erkfit(XYINPT,XYFIT)
	      
c       Write(*,*) 'xyfit=',(xyfit(j),j=1,4)
	      
*       Load output values.
ccc	      hp_tar(itrk)  = XYFIT(1) / 1000.0 !Momentum in GeV
	      hxp_tar(itrk) = XYFIT(2) / 1000.0 ! Slope xp (tan)
	      hyp_tar(itrk) = XYFIT(3) / 1000.0 ! Slope yp (tan)
	      hx_tar(itrk)  = 0.0 ! ** No beam raster **
cc	      hy_tar(itrk)  = 0.0
	      hx_sv(itrk)  =  XYFIT(4) / 10 ! mm-> cm
	      hy_sv(itrk)  =  XYFIT(5) / 10 ! mm-> cm
	      hxp_sv(itrk)  = XYFIT(6) / 1000.0 ! Slope xp (tan)
	      hyp_sv(itrk)  = XYFIT(7) / 1000.0 ! Slope yp (tan)

c	      write(*,*) hxp_tar(itrk),hyp_tar(itrk),hp_tar(itrk)
c	      
c       --- if fitting is not good, hp_tar set 0.1.
c	      if(hp_tar(itrk) .lt. 0.5 .or. hp_tar(itrk) .gt. 2.0) then  
c		 hp_tar(itrk) = 0.1
c	      EndIF
	      
ccc	      hdelta_tar(itrk) = (hp_tar(itrk)/hpcentral-1)*100
	      
c       --- x is to hes (analysis) and to hks (ERIKAFIT). 
c       hxp_tar(itrk) = - hxp_tar(itrk)

	   enddo
	EndIf			! h_tar_output_on = 0 (not 1)

c--     Transfer to track quantities on HKS collimator plane
c	call h_coll_trans(ABORT,err)
c	if(ABORT) then
c	   call g_add_path(here,err)
c	endif

	goto 72
 71	Write(*,*) 'Error in readout of lun=84'
 72	continue

*       histogram target quantities
*       
        call h_fill_target_hist(ABORT,err)
        if(ABORT) then
           call g_add_path(here,err)
           return
        endif
	
	Return
	end

C*****************************************************************
      SUBROUTINE ERKFIT(XYINPT,XYFIT)
C*****************************************************************
C
	Real*4 XYINPT(20),XYFIT(40),ETA(12)
C
C POSITION ---> ETA IE. PRINCIPAL COMPONENTS
C
      CALL GETETA(XYINPT,ETA)
C
C CHEBYCHEV FITTING 
c
C   THE FOLLOWING LINES calculate the requested quantities using the
C   erika coefficients which have been read by the subroutine GETCOE
C   These lines have to be modified according to the format of calculated
C   erika coefficient.
C
      CALL GETFIT(1,ETA,XYFIT(1)) ! momentum
      CALL GETFIT(2,ETA,XYFIT(2)) ! xt'
      CALL GETFIT(3,ETA,XYFIT(3)) ! yt'
      CALL GETFIT(13,ETA,XYFIT(4))! x_sv
      CALL GETFIT(14,ETA,XYFIT(5))! y_sv
      CALL GETFIT(15,ETA,XYFIT(6))! x'_sv
      CALL GETFIT(16,ETA,XYFIT(7))! y'_sv

      RETURN
      END
C
C------ GETETA ------   WIRE POSITION ---> PRINCIPAL COMPONENT
C *********************************************
      SUBROUTINE GETETA(WIRES,ETA)
C *********************************************
C
C     THIS SUBROUTINE TAKES A MULTI-DIMENSIONAL ARRAY OF HIT POSITIONS
C     TRANSFORMS THEM INTO THE ARRAY ETA(12), USING THE TRANSFORMATION
C     MATRIX FOUND BY ERIKA IN THE PRINCIPAL COMPONENT ANALYSIS.
C     THE MATRIX IS ASSUMED TO BE RESIDE IN THE COMMON BLOCK MATRIX
C 
      COMMON/ AVERAG /XAVEL(400)
      COMMON/ MAXMUM /XIMAXE(400)
      COMMON/ COEFIC /Y(400,400)
      COMMON/ POWERS  /IPOWER(400,400,12)
      COMMON/ MATRIX  /U(400,12)
      COMMON/ NUMBER /NCORRE,MAXLEV,NTERMS(400)
      DIMENSION WIRES(12),XSI(12),ETA(12),WIRAV(12)
      DO 100 N = 1,NCORRE
      WIRAV(N) = WIRES(N) - XAVEL(N)
100   CONTINUE
C
      DO 300 N = 1,NCORRE
      XSI(N) = 0.0
      DO 200 K = 1,NCORRE
      XSI(N) = XSI(N) + U(K,N)*WIRAV(K)
200   CONTINUE
      ETA(N) = XSI(N)/XIMAXE(N)
300   CONTINUE
      RETURN
      END
C
C
C------ GETFIT ------
C
      SUBROUTINE GETFIT(IFIT,ETAWIR,YFIT)
C
C     THIS SUBROUTINE TAKES AN ARRAY 'ETAWIR' OF REDUCED WIRE #'S,
C     AND RETURNS THE FITTED QUANTITY YFIT, WHICH IS THE
C     ERIKA FIT FOR THE IFIT-TH VARIABLE.
C     THE FIT COEFICIENTS FOR THE CHEBYSCHEV EXPANSION ARE FOUND IN THE
C     ARRAY Y(6,50), WHICH IS PASSED TO THIS ROUTINE
C     THRU THE NAMED COMMON 'COEFIC'.
C
      COMMON/ AVERAG /XAVEL(400)
      COMMON/ MAXMUM /XIMAXE(400)
      COMMON/ COEFIC /Y(400,400)
      COMMON/ POWERS  /IPOWER(400,400,12)
      COMMON/ NUMBER /NCORRE,MAXLEV,NTERMS(400)
      COMMON/ MATRIX  /U(400,12)
      DOUBLE PRECISION ETAI,CHEBY
      DIMENSION ETAWIR(12)

      YFIT = 0
      DO 200 N = 1,NTERMS(IFIT)
      CHPROD = Y(IFIT,N)
      DO 100 I = 1,MAXLEV
      NORDER = IPOWER(IFIT,N,I)
      ETAI = ETAWIR(I)
      CH = CHEBY(ETAI,NORDER)
      CHPROD = CHPROD*CH
100   CONTINUE
      YFIT = YFIT + CHPROD
200   CONTINUE
      RETURN
      END
C
C------ CHEBY ------    CHEBYCHEV POLINOIMIAL
C
      DOUBLE PRECISION FUNCTION CHEBY(X,N)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IF (N.GT.0) GOTO 10
      CHEBY=1.
      GOTO 999
10    IF (N.GT.1) GOTO 20
      CHEBY=X
      GOTO 999
20    CONTINUE
      A1=1.
      A2=X
      XX=X+X
      DO 30 I=2,N
      T=XX*A2-A1
      A1=A2
      A2=T
30    CONTINUE
      CHEBY=T
999   CONTINUE
      RETURN
      END
