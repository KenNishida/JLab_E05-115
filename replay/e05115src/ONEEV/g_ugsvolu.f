	subroutine g_ugsvolu (name, shape, nmed, par, npar, ivolu)
*
* Simple interface to gsvolu which checks for error conditions on return
* See page GEOM 100-1 in GEANT manual
*
* 1992, Pat Welch, Oregon State University, tpw@physics.orst.edu
*
* $Log: g_ugsvolu.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:46  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1  2005/05/03 21:26:53  miyoshi
* newly added
*
* Revision 1.1  1995/03/14 21:27:25  cdaq
* Initial revision
*

	implicit none

	character*4 name	! Unique name for this volume
	character*4 shape	! Geometric shape for this volume
	integer*4 nmed		! Tracking medium
	integer*4 npar		! Number of user parameters for this vol
	real*4 par(npar)	! User parameters for this volume
	integer*4 ivolu		! system volume number returned

	call gsvolu (name, shape, nmed, par, npar, ivolu)

	if (ivolu .le. 0) then
	  write (*,*) ' ERROR: gsvolu returned an error!'
	  write (*,*) '        name ', name, ' shape ', shape
	  write (*,*) '        nmed ', nmed, ' npar ', npar, ' ivolu ', ivolu
	  if (npar .gt. 0) then
	    write (*,*)'         par(i) ', par
	  endif
	endif

	end

