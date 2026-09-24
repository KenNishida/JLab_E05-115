       SUBROUTINE NO_leading_blanks(string)
*
* $Log: no_leading_blanks.f,v $
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
* Revision 1.2  1996/05/24 16:02:54  saw
* (SAW) Relocate data statements for f2c compatibility
*
* Revision 1.1  1994/02/22 20:01:36  cdaq
* Initial revision
*
*
       character*(*) string
       integer skip
       character*1 tab
       data tab/'	'/ 
c
c      strips out leading blanks and tabs
c
	if(string.eq.' ') RETURN
	skip=0
	LEN_string= len(string)
	DO i=1,LEN_string
	  if(string(i:i).eq.' ' .or. string(i:i).eq.tab) then
	    skip=skip+1 
	  else				!not a tab or blank
	    if(skip.eq.0) RETURN
	    do k=skip+1,LEN_string
		string(k-skip:k-skip)=string(k:k)
	    enddo
	    string(LEN_string-skip+1:)=' '
	    RETURN
	  endif 
	ENDDO
	string=' '			!only tabs and blanks
	RETURN
	end 
