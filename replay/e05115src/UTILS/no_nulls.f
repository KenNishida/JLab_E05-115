	SUBROUTINE NO_nulls(line)
*
* $Log: no_nulls.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:47  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
* new dir
*
* Revision 1.1  1994/02/22 20:01:49  cdaq
* Initial revision
*
*
	character*(*) line
	character*1 null
        integer i
	character*1 CHAR	!FUNCTION
c
c   replaces nulls with blanks
c
	null= CHAR(0)			!ASCII zero
	i= INDEX(line,null)
	DO WHILE (i.NE.0)
	  line(i:i)= ' '		!blank
	  i= INDEX(line,null)
	ENDDO
	RETURN
	END
