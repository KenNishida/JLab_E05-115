       subroutine h_select_good_combination(ABORT,err)
c----------------------------------------------------
c April 16,2010
c  Chunhua Chen
c---------------------
c To select good combination of the stubs from HDC1 and HDC2
c----------------------------------------------------

      implicit none
        
       include 'hks_data_structures.cmn'
       include 'hks_scin_parms.cmn'
       include 'hks_scin_tof.cmn'
       include 'hks_id_histid.cmn'
       include 'hks_tracking.cmn'

       INCLUDE 'gen_constants.par'
       INCLUDE 'hks_geometry.cmn'
       include 'gen_decode_common.cmn'
       include 'gen_rocid.cmn'
       include 'gen_f1tdc.cmn'
       include 'gen_run_info.cmn'
       include 'gen_event_info.cmn'
      

      external h_chamnum
      integer*4 h_chamnum

      character*12 here
      parameter (here= 'h_select_good_combination')
       integer*4 isp1,isp2,i,j
     
      real*4 y1,y2,x1,x2
      real*4 x1x,x2x
      
      real*4 dx1,dx2

      real*4 dx1min,dx2min
      real*4 dposx,dposy,dposxp,dposyp
      integer*4 ix1,ix2
      integer*4 x1index,x2index
      integer*4 hntracks_loop
      real*4 deltaz1,deltaz2,deltaz3,temp1,temp2,temp3,deltaz4
      real*4 dxisp1min(hmax_space_points,hmax_space_points)
      real*4 dxisp2min(hmax_space_points,hmax_space_points)
      integer*4 skipflagx1,skipflagx2
      integer*4 addflagx1,addflagx2
      logical ABORT
      character*(*) err

      ABORT= .FALSE.
      err=' '
      if(ntof.eq.0) return
      
      if(hsingle_stub .eq. 0 ) then   
         if(hnspace_points_tot.ge.2) then ! return if less than 2 space points
            do i=1,hnspace_points_tot
               do j=1,hnspace_points_tot
                  dxisp1min(i,j)=100000.0
                  dxisp2min(i,j)=100000.0
               enddo
            enddo
            deltaz1=hdc_1_zpos-hdc_2_zpos ! zhdc1-zhdc2
            deltaz2=hscin_zpos(1)-hdc_1_zpos !126.001    !z1x-zhdc1
            deltaz3=hscin_zpos(3)-hdc_1_zpos !z2x-zhdc1
            deltaz4=hscin_zpos(2)-hdc_1_zpos ! z1y-zhdc1
            temp1=deltaz2/deltaz1 
            temp2=deltaz3/deltaz1
            temp3=deltaz4/deltaz1
            hntracks_loop=0
            Do isp1 = 1,hnspace_points_tot-1
               do isp2 = isp1+1,hnspace_points_tot
                  if(h_chamnum(isp1) .ne. h_chamnum(isp2)) then
                     if(prelinkflag(isp1,isp2).ne.0) then
                        x1=hbeststub(isp1,1)+hdc_1_zpos*hbeststub(isp1,3)
                        x2=hbeststub(isp2,1)+hdc_2_zpos*hbeststub(isp2,3) 
                        dposx=hbeststub(isp1,1)-hbeststub(isp2,1)
                        dposxp=hbeststub(isp1,3)-hbeststub(isp2,3)
                        y1=hbeststub(isp1,2)+hdc_1_zpos*hbeststub(isp1,4)
                        y2=hbeststub(isp2,2)+hdc_2_zpos*hbeststub(isp2,4)
                        dposy=y1-y2
                        dposyp=hbeststub(isp1,4)-hbeststub(isp2,4)
          
                        if  (abs(dposx) .lt. hxt_track_criterion
     $                   .and. abs(dposy) .lt. hyt_track_criterion
     $                   .and. abs(dposxp).lt. hxpt_track_criterion    
     $                   .and. abs(dposyp).lt. hypt_track_criterion) then
                           x1index=0
                           x2index=0
                           dx1min=10000.0
                           dx2min=10000.0
                           x1x=(1+temp1)*x1-temp1*x2                
                           if(nhits1x.gt.0.and.nhits2x.gt.0) then
                              do ix1=1,nhits1x
                                 dx1=hscin_1x_center(hpro_counter_num(ix1,1))-x1x 
                                 if(abs(dx1).lt.abs(dx1min)) then
                                    dx1min=dx1
                                    x1index=ix1
                                 endif 
                              enddo   
                              x2x=(1+temp2)*x1-temp2*x2
                              do ix2=1,nhits2x  
                                 dx2=hscin_2x_center(hpro_counter_num(ix2,3))-x2x  
                                 if(abs(dx2).lt.abs(dx2min)) then
                                    dx2min=dx2
                                    x2index=ix2
                                 endif  
                              enddo
                              hntracks_loop=hntracks_loop+1
                              dxisp1min(isp1,isp2)=dx1min
                              dxisp2min(isp1,isp2)=dx2min
                           endif
                        endif
                     endif   
                  endif
               enddo
            enddo  
            skipflagx1=0
            addflagx1=0
            skipflagx2=0
            addflagx2=0

            if(hntracks_loop.ge.2) then
               do isp1 = 1,hnspace_points_tot-1
                  do isp2 = isp1+1,hnspace_points_tot
                     if(abs(dxisp1min(isp1,isp2)).lt.10000.0.and.
     +                abs(dxisp2min(isp1,isp2)).lt.10000.0) then
                        if(abs(dxisp1min(isp1,isp2)).gt.11.25) then
                           skipflagx1=1
                        endif
                        if(abs(dxisp1min(isp1,isp2)).le.10.0) then
                           addflagx1=1
                        endif    

                        if(abs(dxisp2min(isp1,isp2)).gt.14.25) then
                           skipflagx2=1
                        endif
                        if(abs(dxisp2min(isp1,isp2)).le.10.0) then
                           addflagx2=1
                        endif 
                     endif       
                  enddo
               enddo

               if(addflagx1.ne.0.and.skipflagx1.ne.0) then
                  do isp1 = 1,hnspace_points_tot-1
                     do isp2 = isp1+1,hnspace_points_tot
                        if(abs(dxisp1min(isp1,isp2)).lt.10000.0.and.
     +                   abs(dxisp2min(isp1,isp2)).lt.10000.0) then
                           if(abs(dxisp1min(isp1,isp2)).gt.11.25) then
                              prelinkflag(isp1,isp2)=0
                           endif
                        endif
                     enddo
                  enddo
               endif

               if(addflagx2.ne.0.and.skipflagx2.ne.0) then
                  do isp1 = 1,hnspace_points_tot-1
                     do isp2 = isp1+1,hnspace_points_tot
                        if(abs(dxisp1min(isp1,isp2)).lt.10000.0.and.
     +                   abs(dxisp2min(isp1,isp2)).lt.10000.0) then
                           if(abs(dxisp2min(isp1,isp2)).gt.14.25) then
                              prelinkflag(isp1,isp2)=0
                           endif
                        endif
                     enddo
                  enddo
               endif
            endif
      
      
         endif
      endif
      
      return
       
         end
