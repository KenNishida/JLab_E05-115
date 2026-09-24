      Subroutine e_calc_trigtime(ABORT,err)
*--------------------------------------------------------
*     calculate trigger time.
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_calc_trigtime')
*     
      Logical ABORT
      Character*(*) err
      
      Include 'hes_data_structures.cmn'
      Include 'hks_data_structures.cmn'
      Include 'hes_scin_parms.cmn'
      Include 'gen_event_info.cmn'
      Include 'gen_run_info.cmn'
      
      Integer*4 i1,i2,ihit,jhit,la,co,tdcpos,tdcneg,hit1(29*10),hit2(29*10)
      Integer*4 tmax,tmin,codiff,tco,tla
      Real*4 timetemp,meantemp,ave

      ABORT= .FALSE.
      err= ' '
      
c     Re-timing by EHODO, not final version. 2005/6/24
c     Calculate estart_time by using mean time of EHODO 2005/7/4
      
c     --- write code to calculate estart_time here.

      timetemp = 70000

      i1 = 0
      i2 = 0
      tco = 0

      Do ihit = 1, escin_tot_hits
         if(escin_layer_num(ihit).eq.1) then
            i1 = i1 + 1
            hit1(i1) = ihit
         elseif(escin_layer_num(ihit).eq.2) then
            i2 = i2 + 1
            hit2(i2) = ihit
         endif
      enddo

** Find hits in 1 layer only      
      if(i2.eq.0.and.i1.gt.0) then
         do ihit = 1,i1
            if( escin_mean_time(hit1(ihit)).le.timetemp ) then
               timetemp = escin_mean_time(hit1(ihit))
               tco = escin_counter_num(hit1(ihit))
            endif
         enddo      
** Find hits in 2 layer only      
      elseif(i1.eq.0.and.i2.gt.0) then
         do ihit = 1,i2
            if( escin_mean_time(hit2(ihit)).le.timetemp ) then
               timetemp = escin_mean_time(hit2(ihit))
               tco = escin_counter_num(hit2(ihit))
            endif
         enddo
** Find hits in both layers
      else

         do ihit = 1, i1
            do jhit = 1, i2
               codiff = abs(escin_counter_num(hit1(ihit))-escin_counter_num(hit2(jhit)))
               if( codiff.le.1 ) then
                  ave = 0.5*(escin_mean_time(ihit)+escin_mean_time(jhit))
                  if( ave.le.timetemp ) then
                     timetemp = ave
                     tco = escin_counter_num(ihit)
                  endif
               endif
            enddo
         enddo

         if(timetemp.eq.70000) then
            do ihit = 1, i1
               do jhit = 1, i2
                  codiff = abs(escin_counter_num(hit1(ihit))-escin_counter_num(hit2(jhit)))
                  if( codiff.le.2 ) then
                     ave = 0.5*(escin_mean_time(ihit)+escin_mean_time(jhit))
                     if( ave.le.timetemp ) then
                        timetemp = ave
                        tco = escin_counter_num(ihit)
                     endif
                  endif
               enddo
            enddo
         endif

      endif
      
c      Do ihit = 1, escin_tot_hits
c         if(escin_layer_num(ihit).eq.1.and.
c     &        escin_good_hits(ihit).eq.1) then
c            if(escin_mean_time(ihit).le.timetemp) then
c               timetemp = escin_mean_time(ihit) 
c            endif
c         endif
c      EndDo
c      if(timetemp.eq.70000) then
c         Do ihit = 1, escin_tot_hits
c            if(escin_layer_num(ihit).eq.2.and.
c     &           escin_good_hits(ihit).eq.1) then
c               if(escin_mean_time(ihit).le.timetemp) then
c                  timetemp = escin_mean_time(ihit) 
c               endif
c            endif
c         enddo
c      endif
      
c      if( -10.lt.timetemp.and.timetemp.lt.30) then
c         estart_time = timetemp
c      else
c         estart_time = 10.
c      endif
      
* Off. Use pre_link by MIZUKI
c      estart_time = 0.

      Return
      end

