         subroutine h_track_selection_pre(ABORT,err)
c----------------------------------------------------
c April 07,2010
c  Chunhua Chen
c---------------------
cThis subroutine is in order to make correct tracks by the information from HDC1&2, timing counter and 
cCherenkov detectors
c---
c     Try to use the information only comes from HDC1&HDC2
c
       
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
      parameter (here= 'h_track_selection_pre')

      integer*4 isp1,isp2,i,j
     
      real*4 y1,y2,x1,x2
      real*4 x1x,x2x
      real*4 y1y,x1y
      
      real*4 dx1,dx2

      real*4 dx1min,dx2min
    
      integer*4 ix1,ix2
      integer*4 x1index,x2index,y1index
     
      real*4 deltaz1,deltaz2,deltaz3,temp1,temp2,temp3,deltaz4
  
      logical ABORT
      character*(*) err


   
       ABORT= .FALSE.
       err=' '
       
      

       if(ntof.eq.0) return

       if(hsingle_stub .eq. 0 ) then   
       if(hnspace_points_tot.ge.2) then ! return if less than 2 space points
           
          deltaz1=hdc_1_zpos-hdc_2_zpos ! zhdc1-zhdc2
          deltaz2=hscin_zpos(1)-hdc_1_zpos !126.001    !z1x-zhdc1
          deltaz3=hscin_zpos(3)-hdc_1_zpos !z2x-zhdc1
          deltaz4=hscin_zpos(2)-hdc_1_zpos ! z1y-zhdc1
          temp1=deltaz2/deltaz1 
          temp2=deltaz3/deltaz1
          temp3=deltaz4/deltaz1

        Do isp1 = 1,hnspace_points_tot-1
       
         do isp2 = isp1+1,hnspace_points_tot
   
           if(h_chamnum(isp1) .ne. h_chamnum(isp2)) then
            if(prelinkflag(isp1,isp2).ne.0) then

               x1=hbeststub(isp1,1)+hdc_1_zpos*hbeststub(isp1,3)
               x2=hbeststub(isp2,1)+hdc_2_zpos*hbeststub(isp2,3) 
             
               y1=hbeststub(isp1,2)+hdc_1_zpos*hbeststub(isp1,4)
               y2=hbeststub(isp2,2)+hdc_2_zpos*hbeststub(isp2,4)
             
                x1index=0
               x2index=0
               dx1min=10000.0
               dx2min=10000.0
            
               x1x=(1+temp1)*x1-temp1*x2                
              
             if(nhits1x.gt.0.and.nhits2x.gt.0) then
               do ix1=1,nhits1x
                 dx1=hscin_1x_center(hpro_counter_num(ix1,1))-x1x 
                
 
                 if(abs(dx1).le.26.25) then  !2.5*hscin_1x_width                         
                 if(abs(dx1).lt.abs(dx1min)) then
                   dx1min=dx1
                   x1index=ix1
                 endif 
                endif  
       
               enddo                 
             
                    
                x2x=(1+temp2)*x1-temp2*x2
               do ix2=1,nhits2x  
                dx2=hscin_2x_center(hpro_counter_num(ix2,3))-x2x  
              
                if(abs(dx2).le.33.25) then   ! 2.5*hscin_2x_width        
                 if(abs(dx2).lt.abs(dx2min)) then
                  dx2min=dx2
                  x2index=ix2
                endif  
               endif           
              enddo
             if(x1index.eq.0.or.x2index.eq.0) then
              prelinkflag(isp1,isp2)=0
             endif          
            endif

             
            
            
         
          
            endif   
           endif
          enddo
                      
         enddo    
        
       endif
  
       endif
         return
       
         end
