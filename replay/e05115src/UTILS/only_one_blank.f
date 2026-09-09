        SUBROUTINE only_one_blank(string) 
*
* $Log: only_one_blank.f,v $
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
* Revision 1.1  1994/02/22 20:02:18  cdaq
* Initial revision
*
*
        IMPLICIT NONE 
        CHARACTER*(*) string
        CHARACTER*1024 line 
        INTEGER i 
C		eliminate tabs,leading blanks, multiple blanks
        CALL NO_tabs(string)
        CALL NO_leading_blanks(string)
        i= INDEX(string,'  ')                !2 blanks
        DO WHILE (i.NE.0 .AND. string(max(i,1):).ne.' ')
          line= string(i+1:)              !skip 1st blank 
          string(i:)= line                !shift left 
          i= INDEX(string,'  ')           !look again 
        ENDDO 
        RETURN
        END 
