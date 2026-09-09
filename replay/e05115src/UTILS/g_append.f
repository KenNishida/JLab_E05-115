      SUBROUTINE G_append(prefix,suffix)
*----------------------------------------------------------------------
*- 
*-   Purpose and Methods : Put "suffix" as a suffix on the message
*- 
*-   Inputs  : prefix	- prefix to add onto suffix
*-           : suffix   - suffix to be prepended
*-   Outputs : prefix	- suffixed prefix
*- 
*-   Created  17-May-1994   Kevin B. Beard, Hampton U. 
* $Log: g_append.f,v $
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
* Revision 1.1  1994/05/27 16:11:41  cdaq
* Initial revision
*
*----------------------------------------------------------------------
      IMPLICIT NONE
      SAVE 
      character*(*) prefix,suffix
*
      integer m
*
      INCLUDE 'gen_routines.dec'
*
*----------------------------------------------------------------------
*
        IF(prefix.EQ.' ') THEN
          prefix= suffix
          RETURN
        ELSEIF(suffix.EQ.' ') THEN
          RETURN
        ENDIF
*
	call no_leading_blanks(prefix)
	m= G_important_length(prefix)
        IF(LEN(prefix).GT.m) prefix(m+1:)= suffix
*
        RETURN 
        END
