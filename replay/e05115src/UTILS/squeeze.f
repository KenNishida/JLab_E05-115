       SUBROUTINE squeeze(line,nonblank)
*
* $Log: squeeze.f,v $
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
* Revision 1.2  1996/05/24 16:03:33  saw
* (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.1  1994/02/22 20:02:45  cdaq
* Initial revision
*
*
       character*(*) line
       integer nonblank
       character*1 tab
       data tab/'	'/
c
c      removes all blanks and tabs from a string
c       and return nonblank length
c
       call NO_nulls(line)			!nulls=>' '
       nonblank=0
       LEN_line= len(line) 
       DO i=1,LEN_line
           if(line(i:i).ne.' '.and.line(i:i).ne.tab) then
		nonblank=nonblank+1			!skip blanks&tabs
		line(nonblank:nonblank)=line(i:i)
           elseif(line(i:).eq.' ') then		!nonblank < LEN_line 
		line(nonblank+1:)=' '			!quick check
		return
           endif
       ENDDO
       if(nonblank.lt.LEN_line) line(nonblank+1:)=' '
       return
       end 
