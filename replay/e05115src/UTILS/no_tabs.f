	SUBROUTINE NO_tabs(line)
*
* $Log: no_tabs.f,v $
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
* Revision 1.1  1994/02/22 20:05:11  cdaq
* Initial revision
*
*
	character*(*) line
	character*1 tab
        integer i
	character*1 CHAR	!FUNCTION
c
c   replaces tabs with blanks
c
	tab= CHAR(9)			!ASCII nine
	i= INDEX(line,tab)
	DO WHILE (i.NE.0)
	  line(i:i)= ' '		!blank
	  i= INDEX(line,tab)
	ENDDO
	RETURN
	END
