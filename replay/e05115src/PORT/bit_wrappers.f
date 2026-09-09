*
*     Wrappers for F2C intrinsic functions that have different names
*     from standard f77s.
*
* $Log: bit_wrappers.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:46  kawama
*
* e05115 src repository for software development
*
* Revision 1.1  2005/07/01 15:47:53  cdaq
* Import the PORT directory from the standard Hall C ENGINE CVS
*
* Revision 1.3  1999/11/04 20:36:32  saw
* Linux/G77 compatibility fixes - Add jieor function
*
* Revision 1.2  1996/11/22 17:06:27  saw
* (SAW) Move trig routines to trig_wrappers.f
*
* Revision 1.1  1996/09/09 13:34:26  saw
* Initial revision
*
*
      integer*4 function jishft(a1,a2)
      integer*4 a1,a2
      if(a2.lt.0) then
         jishft = rshift(a1,-a2)         
      else
         jishft = lshift(a1,a2)
      endif
      return
      end

      integer*4 function jiand(a1,a2)
      integer*4 a1,a2
      jiand = and(a1,a2)
      return
      end

      integer*4 function jieor(a1,a2)
      integer*4 a1,a2
      jieor = xor(a1,a2)
      return
      end

      integer*4 function jibset(a1,a2)
      integer*4 a1,a2
      jibset = or(a1,lshift(1,a2))
      return
      end

      logical*4 function bjtest(a1,a2)
      integer*4 a1,a2
      bjtest = (and(a1,lshift(1,a2)).ne.0)
      return
      end

