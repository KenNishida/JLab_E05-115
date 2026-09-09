      Subroutine h_calc_trigtime(ABORT,err)
*--------------------------------------------------------
*     calculate trigger time.
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='h_calc_trigtime')
*     
      Logical ABORT
      Character*(*) err
      
      Include 'hes_data_structures.cmn'
      Include 'hks_data_structures.cmn'
      Include 'hks_scin_parms.cmn'
      Include 'hks_scin_tof.cmn'
      Include 'gen_event_info.cmn'
      Include 'gen_run_info.cmn'
      Include 'hks_geometry.cmn'
      
      Integer*4 la,co,itdc,tdcpos,tdcneg,i
      Real*4 sumtdc,avetdc
      Real*4 avearray(12)
      Common /hdc_temp/avearray
      Real*4 zratio(12),hdz,hz1,tf,ttrans1,tf1(12)
      Real*4 t1ave,t2ave

c     user specify : htest_flag,htrigger_channel(2)
c     variables : hstart_time,htrigger_flag
      
      ABORT= .FALSE.
      err= ' '
      
c     --- write code to calculate estart_time here.
 
c     hstart_time is usually defined in h_trans_scin
      if(hscin_raw_tot_hits .le. 0) then
         if(gen_run_number .lt. 5999) 
     &        hstart_time = hstart_time_center
      Else
         if(gen_run_number .eq. 1058) then
            hdz=0
            hz1=0
            Do i=1,12
               zratio(i) = (hdc_zpos(i)-hz1)/hdz 
            EndDo
            sumtdc = 0.0
            avetdc = 0.0
            itdc = 0
            t1ave = -1
            t2ave = -1
            Do i=1,hscin_raw_tot_hits
               la = hscin_raw_layer_num(i)
               co = hscin_raw_counter_num(i)
               tdcpos = hscin_raw_tdc_pos(i)
               tdcneg = hscin_raw_tdc_neg(i)
               if(co .eq. 12) then
                  t1ave = (tdcpos+tdcneg)*0.5
               Else if(co .eq. 13) then
                  t2ave = (tdcpos+tdcneg)*0.5
               EndIf
               if(co .gt. 11 .and. co .lt. 14 .and. tdcpos .gt. 2 .and.
     &              tdcpos .le. 1000) then
                  sumtdc = sumtdc + Real(tdcpos)*0.05
                  itdc = itdc + 1
               EndIF
               if(co .ne. 11 .and. co .lt. 14 .and. tdcneg .gt. 0 .and.
     &              tdcneg .le. 1000) then
                  sumtdc = sumtdc + Real(tdcneg)*0.05
                  itdc = itdc + 1
               EndIF
            EndDo
            if(itdc .eq. 4) then
               tf = t1ave - t2ave
               ttrans1 = -t1ave
               Do i=1,12
                  tf1(i) = (1-zratio(i))*tf
                  avearray(i) = -(tf1(i)+ttrans1)
               EndDo
            EndIf

            avetdc = sumtdc/real(itdc)
            hstart_time = avetdc

c     Write(*,*) 'trigtime, 1058 hstarttime=',hstart_time
         EndIF                  ! run=1058
      EndIf




      Return
      end

