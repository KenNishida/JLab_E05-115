      integer function regparmstringarray(name, strings, size)
*
* $Log: regparmstringarray.f,v $
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
* Revision 1.1  1994/08/18 03:50:34  cdaq
* Initial revision
*
      character*(*) name, strings(*)
      integer*4 size
      
      regparmstringarray = 0
      return
      end
