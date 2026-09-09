	SUBROUTINE UP_shift(InPut)
*
* $Log: up_shift.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:46  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
* new dir
*
* Revision 1.1  1994/02/22 20:03:26  cdaq
* Initial revision
*
*
	character*(*) InPut
	integer tab
c
c      shifts strings to upper case, replaces nulls&tabs with spaces
c
	if(InPut.eq.' ') return 
	call NO_nulls(INPUT)
	call NO_tabs(INPUT)
	call UP_case(InPut)
	RETURN
	END
