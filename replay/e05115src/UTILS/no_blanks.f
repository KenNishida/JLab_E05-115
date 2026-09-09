       SUBROUTINE NO_blanks(string)
*
* $Log: no_blanks.f,v $
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
* Revision 1.1  1994/02/22 20:01:14  cdaq
* Initial revision
*
*
       character*(*) string
       integer nonblank_length
c
c      strips out blanks and tabs
c
       if(string.eq.' ') RETURN
       call squeeze(string,nonblank_length)
       return
       end
