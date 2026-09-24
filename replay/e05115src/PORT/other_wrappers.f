*
*     Wrapperse for G77 intrinsic functions that have different names from
*     other f77's
*
* $Log: other_wrappers.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:46  kawama
*
* e05115 src repository for software development
*
* Revision 1.1  2005/07/01 15:47:56  cdaq
* Import the PORT directory from the standard Hall C ENGINE CVS
*
* Revision 1.1  2000/11/30 14:24:44  saw
* JIDNNT function
*
*
* JIDNNT Return  nearest INT for a REAL*16 number
*
      integer*4 function jidnnt(f)
      real*8 f
*
      jidnnt = nint(f)
      return
      end

