      Real function localy(t)
      
      Integer t
      Integer c1,c2
      
      c1 = t/60/60/24/365       ! year
      c1 = (2+c1)/4             ! 366/year 
      c1 = t - c1 * 60 * 60 * 24 ! c1 * day
      localy = 1970 + c1 /60/60/24/365
      
      End
