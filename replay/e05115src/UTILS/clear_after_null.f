        SUBROUTINE clear_after_null(string) 
*
* $Log: clear_after_null.f,v $
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
* Revision 1.1  1996/05/16 17:48:45  saw
* Initial revision
*
        IMPLICIT NONE 
        CHARACTER*(*) string
        CHARACTER*1024 line 
        INTEGER i
        CHARACTER*1 null

C		clear out the string after the first null character.
        null=char(0)   !null character.
        i= INDEX(string,null)                !2 blanks
        if (i.ne.0) string(i:)=' '

        RETURN
        END 
