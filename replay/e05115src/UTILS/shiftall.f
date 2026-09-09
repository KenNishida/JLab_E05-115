       SUBROUTINE SHIFTall(InPut,OUTPUT) 
*
* $Log: shiftall.f,v $
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
* Revision 1.1  1994/02/22 20:02:34  cdaq
* Initial revision
*
*
       character*(*) InPut,OUTPUT 
c
c      shifts strings to upper case, removes all tabs,nulls & leading blanks
c
	OutPut=InPut
	call UP_shift(OutPut)
	call NO_leading_blanks(OUTPUT)
	return
       end 
