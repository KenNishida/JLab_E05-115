      Subroutine h_link_tracks(ABORT,err)
*--------------------------------------------------------
*     link tracking and Cherenkov data.
*     $Log: h_link_tracks.f,v $
*     Revision 1.1.1.1  2009/07/11 14:10:2 yez
*     Add Lucite info
*
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.3  2005/04/19 17:52:57  miyoshi
*     change minor one
*
*     Revision 1.2  2005/04/08 20:42:57  miyoshi
*     change minor ones
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     
*     03/24/04 Miyoshi 
*     
*     Input Banks    
*     Output Banks   HKS_TRACK_TESTS
*     
*     htrack_aer_num : index in HAER_TOT_HITS (1st hits)
*     htrack_aer_extra_num : index in  
*                            HAER_TOT_HITS (2nd hits if existing)
*     
*     htrack_wat_num: index in HWAT_TOT_HITS (1st hits)
*     htrack_extra_wat_num : index in 
*                            HWAT_TOT_HITS (2nd hits if existing)
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='h_link_tracks')
*     
      Logical ABORT
      Character*(*) err
      
      Include 'hks_data_structures.cmn'
      Include 'hks_scin_parms.cmn'
      Include 'hks_water_parms.cmn'
      Include 'hks_aero_parms.cmn'
      Include 'hks_lucite_parms.cmn'
      Include 'hks_id_histid.cmn'
      Include 'hks_physics_sing.cmn'

      Integer*4 i,j,k,itrk,ind1,ind2
      Real*4 x0,y0,xp0,yp0,x1,y1
      Real*4 x1_min,x1_max,x2_min,x2_max
      Real*4 y1_min,y1_max,y2_min,y2_max
      Integer*4 la,co,naer(3),nwat(2),nluc(1)
      !DK added to include T/B ADC/TDC of AC, WC and LC in ntuple 2010/1/14
      Integer*4 naer_pos(3),nwat_pos(2),nluc_pos(1)
      Integer*4 naer_neg(3),nwat_neg(2),nluc_neg(1)
      
      ABORT= .FALSE.
      err= ' '
      
      if(hntracks_fp .le. 0) Return
      
      Do itrk=1,hntracks_fp
         Do j=1,hnum_aer_layers
            htrk_naer(itrk,j) = 0
            htrk_aer_npe_sum(itrk,j) = 0.
            htrk_aer_npe_sum2(itrk,j) = 0.
            htrk_aer_time(itrk,j) = -1.
            htrk_aer_time2(itrk,j) = -1.
            htrk_aer_npe_pos(itrk,j) = 0.
            htrk_aer_npe_pos2(itrk,j) = 0.
            htrk_aer_time_pos(itrk,j) = -1.
            htrk_aer_time_pos2(itrk,j) = -1.
            htrk_aer_npe_neg(itrk,j) = 0.
            htrk_aer_npe_neg2(itrk,j) = 0.
            htrk_aer_time_neg(itrk,j) = -1.
            htrk_aer_time_neg2(itrk,j) = -1.
            htrk_aer_num(itrk,j) = 0.
         EndDo
         Do j=1,hnum_wat_layers
            htrk_nwat(itrk,j) = 0
            htrk_wat_npe_sum(itrk,j) = 0.
            htrk_wat_npe_sum2(itrk,j) = 0.
            htrk_wat_npe_sum_k_ratio(itrk,j) = 0.
            htrk_wat_npe_sum_k_ratio2(itrk,j) = 0.
            htrk_wat_time(itrk,j) = -1.
            htrk_wat_time2(itrk,j) = -1.
            htrk_wat_npe_pos(itrk,j) = 0.
            htrk_wat_npe_pos2(itrk,j) = 0.
            htrk_wat_time_pos(itrk,j) = -1.
            htrk_wat_time_pos2(itrk,j) = -1.
            htrk_wat_npe_neg(itrk,j) = 0.
            htrk_wat_npe_neg2(itrk,j) = 0.
            htrk_wat_time_neg(itrk,j) = -1.
            htrk_wat_time_neg2(itrk,j) = -1.
            htrk_wat_num(itrk,j) = 0.
         EndDo
         
         Do j=1,hnum_luc_layers
            htrk_nluc(itrk,j) = 0
            htrk_luc_npe_sum(itrk,j) = 0.
            htrk_luc_npe_sum2(itrk,j) = 0.
            htrk_luc_time(itrk,j) = -1.
            htrk_luc_time2(itrk,j) = -1.
            htrk_luc_npe_pos(itrk,j) = 0.
            htrk_luc_npe_pos2(itrk,j) = 0.
            htrk_luc_time_pos(itrk,j) = -1.
            htrk_luc_time_pos2(itrk,j) = -1.
            htrk_luc_npe_neg(itrk,j) = 0.
            htrk_luc_npe_neg2(itrk,j) = 0.
            htrk_luc_time_neg(itrk,j) = -1.
            htrk_luc_time_neg2(itrk,j) = -1.
            htrk_luc_num(itrk,j) = 0.
         EndDo

      EndDo
      
      Do itrk = 1,hntracks_fp
         x0 = hx_fp(itrk)
         y0 = hy_fp(itrk)
         xp0 = hxp_fp(itrk)
         yp0 = hyp_fp(itrk)
         
*     --- link AC
         if(haer_tot_hits .gt. 0) then
            Do j=1,hnum_aer_layers
               naer(j)=0
               naer_pos(j)=0
               naer_neg(j)=0
            EndDo
            Do j = 1,haer_tot_hits        
               la = haer_layer_num(j)
               co = haer_counter_num(j)
               x1 = x0 + xp0 * haer_box_zpos(la)
               y1 = y0 + yp0 * haer_box_zpos(la)
               x1_min = haer_box_xcenter(la,co) - 0.5*haer_width 
     &              - haer_slope
               x1_max = haer_box_xcenter(la,co) + 0.5*haer_width 
     &              + haer_slope
               y1_min = haer_box_ycenter(la,co) - 0.5*haer_height 
     &              - haer_slope
               y1_max = haer_box_ycenter(la,co) + 0.5*haer_height 
     &              + haer_slope
c     
c     when having multiple hits, two npes are summed.
c     
               if(x1 .gt. x1_min .and. x1 .lt. x1_max .and.
     &              y1 .gt. y1_min .and. y1 .lt. y1_max) then
                  htrk_naer(itrk,la) = htrk_naer(itrk,la) + 1
                  htrk_aer_num(itrk,la) = htrk_aer_num(itrk,la) 
     &                 + Real(co)
                  if(haer_npe_sum(j) .gt. haer_npe_min) then
                     htrk_aer_npe_sum(itrk,la) = 
     &                    htrk_aer_npe_sum(itrk,la)
     &                    + haer_npe_sum(j)
                     naer(la) = naer(la) + 1
                     if(naer(la).eq.1) then
                        if(haer_pos_time(j).gt.0.and.
     &                       haer_neg_time(j).gt.0) then
                           htrk_aer_time(itrk,la) = 
     &                          (haer_pos_time(j)+haer_neg_time(j))/2
                        else if(haer_pos_time(j).gt.0) then
                           htrk_aer_time(itrk,la) = haer_pos_time(j)
                        else if(haer_neg_time(j).gt.0) then
                           htrk_aer_time(itrk,la) = haer_neg_time(j)
                        endif
                     endif
                     if(naer(la) .eq. 2) then
                        htrk_aer_npe_sum2(itrk,la) = 
     &                       + haer_npe_sum(j)
                        if(haer_pos_time(j).gt.0.and.
     &                       haer_neg_time(j).gt.0) then
                           htrk_aer_time2(itrk,la) = 
     &                          (haer_pos_time(j)+haer_neg_time(j))/2
                        else if(haer_pos_time(j).gt.0) then
                           htrk_aer_time2(itrk,la) = haer_pos_time(j)
                        else if(haer_neg_time(j).gt.0) then
                           htrk_aer_time2(itrk,la) = haer_neg_time(j)
                        endif
                     endif
                  EndIF
!DK added to include T/B ADC/TDC of AC in ntuple 2010/1/14
!From here-----------------------------------------------------------
                  if(haer_pos_time(j) .gt. 0) then !For pos channel 
                     htrk_aer_npe_pos(itrk,la) = 
     &                    htrk_aer_npe_pos(itrk,la)
     &                    + haer_pos_npe(j)
                     naer_pos(la) = naer_pos(la) + 1
                     if(naer_pos(la).eq.1) then
                        htrk_aer_time_pos(itrk,la) = haer_pos_time(j)
                     else if(naer_pos(la) .eq. 2) then
                        htrk_aer_npe_pos2(itrk,la) = 
     &                       + haer_pos_npe(j)
                        htrk_aer_time_pos2(itrk,la) = haer_pos_time(j)
                     endif
                  EndIF
                  if(haer_neg_time(j) .gt. 0) then !For neg channel
                     htrk_aer_npe_neg(itrk,la) = 
     &                    htrk_aer_npe_neg(itrk,la)
     &                    + haer_neg_npe(j)
                     naer_neg(la) = naer_neg(la) + 1
                     if(naer_neg(la).eq.1) then
                        htrk_aer_time_neg(itrk,la) = haer_neg_time(j)
                     else if(naer_neg(la) .eq. 2) then
                        htrk_aer_npe_neg2(itrk,la) = 
     &                       + haer_neg_npe(j)
                        htrk_aer_time_neg2(itrk,la) = haer_neg_time(j)
                     endif
                  EndIF
!To here--------------------------------------------------------------
               EndIf
            EndDo               ! AC loop
            

            Do j=1,hnum_aer_layers
               if(htrk_naer(itrk,j) .gt. 0) then
                  htrk_aer_num(itrk,j) = htrk_aer_num(itrk,j)
     &                 /Real(htrk_naer(itrk,j))
               EndIf
            EndDo

         EndIf                  ! AC hits > 0
         
*     --- link WC
         
         if(hwat_tot_hits .gt. 0) then
            Do j=1,hnum_wat_layers
               nwat(j)=0
               nwat_pos(j)=0
               nwat_neg(j)=0
            EndDo
            Do j = 1,hwat_tot_hits            
               la = hwat_layer_num(j)
               co = hwat_counter_num(j)
               x1 = x0 + xp0 * hwat_box_zpos(la)
               y1 = y0 + yp0 * hwat_box_zpos(la)
               x1_min = hwat_box_xcenter(la,co) - 0.5*hwat_width 
     &              - hwat_slope
               x1_max = hwat_box_xcenter(la,co) + 0.5*hwat_width 
     &              + hwat_slope
               y1_min = hwat_box_ycenter(la,co) - 0.5*hwat_height 
     &              - hwat_slope
               y1_max = hwat_box_ycenter(la,co) + 0.5*hwat_height 
     &              + hwat_slope
               if(x1 .gt. x1_min .and. x1 .lt. x1_max .and.
     &              y1 .gt. y1_min .and. y1 .lt. y1_max) then
                  
c     
c     when having multiple hits, two npes are summed.
c     
                  htrk_nwat(itrk,la) = htrk_nwat(itrk,la) + 1
                  htrk_wat_num(itrk,la) = htrk_wat_num(itrk,la) 
     &                 + Real(co)

                  if(hwat_npe_sum(j) .gt. hwat_npe_min) then
                     htrk_wat_npe_sum(itrk,la) = 
     &                    htrk_wat_npe_sum(itrk,la)
     &                    + hwat_npe_sum(j)
c normalized kaon NPE
                     htrk_wat_npe_sum_k_ratio(itrk,la) = 
     &                    htrk_wat_npe_sum_k_ratio(itrk,la)
     &                    + hwat_npe_sum_k_ratio(j)
                     nwat(la) = nwat(la) + 1
                     htrk_wat_npe_pos(itrk,la) = 
     &                    htrk_wat_npe_pos(itrk,la)
     &                    + hwat_pos_npe(j)
                     htrk_wat_npe_neg(itrk,la) = 
     &                    htrk_wat_npe_neg(itrk,la)
     &                    + hwat_neg_npe(j)
                     if(nwat(la).eq.1) then
                        if(hwat_pos_time(j).gt.0..and.
     &                       hwat_neg_time(j).gt.0.) then
                           htrk_wat_time(itrk,la) = 
     &                          (hwat_pos_time(j)+hwat_neg_time(j))/2
                        else if(hwat_pos_time(j).gt.0.) then
                           htrk_wat_time(itrk,la) = hwat_pos_time(j)
                        else if(hwat_neg_time(j).gt.0.) then
                           htrk_wat_time(itrk,la) = hwat_neg_time(j)
                        endif
                     endif
                     if(nwat(la).eq.2) then
                        htrk_wat_npe_sum2(itrk,la) = 
     &                       + hwat_npe_sum(j)
c normalized kaon NPE
                        htrk_wat_npe_sum_k_ratio2(itrk,la) = 
     &                       + hwat_npe_sum_k_ratio(j)
                        htrk_wat_npe_pos2(itrk,la) = 
     &                       + hwat_pos_npe(j)
                        htrk_wat_npe_neg2(itrk,la) = 
     &                       + hwat_neg_npe(j)
                        if(hwat_pos_time(j).gt.0..and.
     &                       hwat_neg_time(j).gt.0.) then
                           htrk_wat_time2(itrk,la) = 
     &                          (hwat_pos_time(j)+hwat_neg_time(j))/2
                        else if(hwat_pos_time(j).gt.0.) then
                           htrk_wat_time2(itrk,la) = hwat_pos_time(j)
                        else if(hwat_neg_time(j).gt.0.) then
                           htrk_wat_time2(itrk,la) = hwat_neg_time(j)
                        endif
                     endif
                  EndIF
!DK added to include T/B ADC/TDC of WC in ntuple 2010/1/14
!From here-----------------------------------------------------------
c                  if(hwat_pos_time(j) .gt. 0) then !For pos channel
c                     htrk_wat_npe_pos(itrk,la) = 
c     &                    htrk_wat_npe_pos(itrk,la)
c     &                    + hwat_pos_npe(j)
c                     nwat_pos(la) = nwat_pos(la) + 1
c                     if(nwat_pos(la).eq.1) then
c                        htrk_wat_time_pos(itrk,la) = hwat_pos_time(j)
c                     else if(nwat_pos(la) .eq. 2) then
c                        htrk_wat_npe_pos2(itrk,la) = 
c     &                       + hwat_pos_npe(j)
c                        htrk_wat_time_pos2(itrk,la) = hwat_pos_time(j)
c                     endif
c                  EndIF
c                  if(hwat_neg_time(j) .gt. 0) then !For neg channel
c                     htrk_wat_npe_neg(itrk,la) = 
c     &                    htrk_wat_npe_neg(itrk,la)
c     &                    + hwat_neg_npe(j)
c                     nwat_neg(la) = nwat_neg(la) + 1
c                     if(nwat_neg(la).eq.1) then
c                        htrk_wat_time_neg(itrk,la) = hwat_neg_time(j)
c                     else if(nwat_neg(la) .eq. 2) then
c                        htrk_wat_npe_neg2(itrk,la) = 
c     &                       + hwat_neg_npe(j)
c                        htrk_wat_time_neg2(itrk,la) = hwat_neg_time(j)
c                     endif
c                  EndIF
!To here--------------------------------------------------------------
               EndIF            ! A track in WC
            EndDo               ! WC loop

            Do j=1,hnum_wat_layers
               if(htrk_nwat(itrk,j) .gt. 0) then
                  htrk_wat_num(itrk,j) = htrk_wat_num(itrk,j)
     &                 /Real(htrk_nwat(itrk,j))
               EndIf
            EndDo

         EndIf                  ! WC hits > 0
         
*     --- link LC
         
         if(hluc_tot_hits .gt. 0) then
            Do j=1,hnum_luc_layers
               nluc(j)=0
            EndDo
            Do j = 1,hluc_tot_hits            
               la = hluc_layer_num(j)
               co = hluc_counter_num(j)
cc
cc ----Correct different lucite bars's size: -- Z. Ye 09/07/2009
cc                   Width    Height   Thick
cc     Old(3~10):    13.52,   69.57,   3.20
cc     New(11~15):   13.61,   63.85,   2.20
cc     Sane(1,2,16):  6.10,   97.36,   3.85
cc
cc             ! Lucite from SANE
               if(co.eq.1.or.co.eq.2.or.co.eq.16) then 
                  hluc_width  =  6.10
                  hluc_height = 97.36
c              ! New built Lucite bars
               elseif(co.gt.10.and.co.lt.16) then
                  hluc_width  = 13.61
                  hluc_height = 63.85
               endif

               x1 = x0 + xp0 * hluc_box_zpos(la)
               y1 = y0 + yp0 * hluc_box_zpos(la)
               x1_min = hluc_box_xcenter(la,co) - 0.5*hluc_width 
     &              - hluc_slope
               x1_max = hluc_box_xcenter(la,co) + 0.5*hluc_width 
     &              + hluc_slope
               y1_min = hluc_box_ycenter(la,co) - 0.5*hluc_height 
     &              - hluc_slope
               y1_max = hluc_box_ycenter(la,co) + 0.5*hluc_height 
     &              + hluc_slope
               if(x1 .gt. x1_min .and. x1 .lt. x1_max .and.
     &              y1 .gt. y1_min .and. y1 .lt. y1_max) then
                  
c     
c     when having multiple hits, two npes are summed.
c     
                  htrk_nluc(itrk,la) = htrk_nluc(itrk,la) + 1
                  htrk_luc_num(itrk,la) = htrk_luc_num(itrk,la) 
     &                 + Real(co)

                  if(hluc_npe_sum(j) .gt. hluc_npe_min) then
                     htrk_luc_npe_sum(itrk,la) = 
     &                    htrk_luc_npe_sum(itrk,la)
     &                    + hluc_npe_sum(j)
                     nluc(la) = nluc(la) + 1
                     if(nluc(la).eq.1) then
                        if(hluc_pos_time(j).gt.0..and.
     &                       hluc_neg_time(j).gt.0.) then
                           htrk_luc_time(itrk,la) = 
     &                          (hluc_pos_time(j)+hluc_neg_time(j))/2
                        else if(hluc_pos_time(j).gt.0.) then
                           htrk_luc_time(itrk,la) = hluc_pos_time(j)
                        else if(hluc_neg_time(j).gt.0.) then
                           htrk_luc_time(itrk,la) = hluc_neg_time(j)
                        endif
                     endif
                     if(nluc(la).eq.2) then
                        htrk_luc_npe_sum2(itrk,la) = 
     &                       + hluc_npe_sum(j)
                        if(hluc_pos_time(j).gt.0..and.
     &                       hluc_neg_time(j).gt.0.) then
                           htrk_luc_time2(itrk,la) = 
     &                          (hluc_pos_time(j)+hluc_neg_time(j))/2
                        else if(hluc_pos_time(j).gt.0.) then
                           htrk_luc_time2(itrk,la) = hluc_pos_time(j)
                        else if(hluc_neg_time(j).gt.0.) then
                           htrk_luc_time2(itrk,la) = hluc_neg_time(j)
                        endif
                     endif
                  EndIF
               EndIF            ! A track in LC
            EndDo               ! LC loop

            Do j=1,hnum_luc_layers
               if(htrk_nluc(itrk,j) .gt. 0) then
                  htrk_luc_num(itrk,j) = htrk_luc_num(itrk,j)
     &                 /Real(htrk_nluc(itrk,j))
               EndIf
            EndDo

         EndIf                  ! LC hits > 0
         


c     --- fill histogram
         Do j=1,hnum_aer_layers
c     Write(*,*) '(link)naer=',htrk_naer(itrk,j)
            Call HF1(hidtrknaer(j),float(htrk_naer(itrk,j)),1.)
            if(htrk_naer(itrk,j) .gt. 0) then
               Call HF1(hidtrkaernum(j),htrk_aer_num(itrk,j),1.)
            EndIf
            Call HF1(hidtrkaernpesum(j),htrk_aer_npe_sum(itrk,j),1.)
         EndDo
         Do j=1,hnum_wat_layers
            Call HF1(hidtrknwat(j),float(htrk_nwat(itrk,j)),1.)
            if(htrk_nwat(itrk,j) .gt. 0) then
               Call HF1(hidtrkwatnum(j),htrk_wat_num(itrk,j),1.)
            endif
            Call HF1(hidtrkwatnpesum(j),htrk_wat_npe_sum(itrk,j),1.)
            Call HF1(hidtrkwatnpesumkratio(j),htrk_wat_npe_sum_k_ratio(itrk,j),1.)
         EndDo
         Do j=1,hnum_luc_layers
            Call HF1(hidtrknluc(j),float(htrk_nluc(itrk,j)),1.)
            if(htrk_nluc(itrk,j) .gt. 0) then
               Call HF1(hidtrklucnum(j),htrk_luc_num(itrk,j),1.)
            endif
            Call HF1(hidtrklucnpesum(j),htrk_luc_npe_sum(itrk,j),1.)
         EndDo

      EndDo                     ! hntracks_fp
      
      Return
      end
