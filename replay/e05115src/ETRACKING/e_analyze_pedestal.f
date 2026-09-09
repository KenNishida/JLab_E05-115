      Subroutine e_analyze_pedestal(ABORT,err)
*--------------------------------------------------------
* Hodoscope pedestals analysis
* 
* $Log: e_analyze_pedestal.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.2  2005/07/08 13:44:11  sumihama
* Mod. remove TDC-multihits by Joerg
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.5 2004/03/03 Miyoshi
* for E01-011
* Revision 1.4  2000/03/09 01:32:39  ysato
* Update in the production run Mar.8
*
* Revision 1.3  1999/12/23 19:59:23  ysato
* Compiled on Redhat Linux
*
*
*
* This routine will be called from g_analyze_pedestal
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE

      character*50 here
      parameter (here='e_analyze_pedestal')

      Logical ABORT
      Character*(*) err

      Include 'hes_data_structures.cmn'
      Include 'hes_pedestals.cmn'

      integer*4 ihit
      integer*4 pln,cnt
      integer*4 counter_hits(ENUM_SCIN_LAYERS,ENUM_SCIN_COUNTERS)
*--------------------------------------------------------
*
*
* HODOSCOPE PEDESTALS
*

*     clear hit counter
      do pln=1,ENUM_SCIN_LAYERS
         do cnt=1,ENUM_SCIN_COUNTERS
          counter_hits(pln,cnt)=0
       enddo
      enddo  

      do ihit = 1 , escin_raw_tot_hits
         pln = escin_raw_layer_num(ihit)
         cnt = escin_raw_counter_num(ihit)
         counter_hits(pln,cnt)=counter_hits(pln,cnt)+1
         if(counter_hits(pln,cnt).eq.1)then ! ingore multiple hits
            if (escin_rawadc_pos(ihit) 
     &           .le. escin_pos_ped_limit(pln,cnt)) then
               escin_neg_ped_sum2(pln,cnt) = 
     &              escin_neg_ped_sum2(pln,cnt) +
     &              escin_rawadc_neg(ihit)*escin_rawadc_neg(ihit)
               escin_neg_ped_sum(pln,cnt) = 
     &              escin_neg_ped_sum(pln,cnt) +
     &              escin_rawadc_neg(ihit)
               escin_neg_ped_num(pln,cnt) = 
     &              escin_neg_ped_num(pln,cnt) + 1
               if (escin_pos_ped_num(pln,cnt) 
     &              .eq. nint(escin_pos_min_peds/5.)) then
                  escin_pos_ped_limit(pln,cnt) = 100 +
     &                 escin_pos_ped_sum(pln,cnt) 
     &                 / escin_pos_ped_num(pln,cnt)
               endif
            endif
            if (escin_rawadc_neg(ihit) 
     &           .le. escin_neg_ped_limit(pln,cnt)) then
               escin_pos_ped_sum2(pln,cnt) = 
     &              escin_pos_ped_sum2(pln,cnt) +
     &              escin_rawadc_pos(ihit)*escin_rawadc_pos(ihit)
               escin_pos_ped_sum(pln,cnt) = 
     &              escin_pos_ped_sum(pln,cnt) +
     &              escin_rawadc_pos(ihit)
               escin_pos_ped_num(pln,cnt) = 
     &              escin_pos_ped_num(pln,cnt) + 1
               if (escin_neg_ped_num(pln,cnt) 
     &              .eq. nint(escin_neg_min_peds/5.)) then
                  escin_neg_ped_limit(pln,cnt) = 100 +
     &                 escin_neg_ped_sum(pln,cnt) 
     &                 / escin_neg_ped_num(pln,cnt)
               endif
            endif
         endif
      enddo

      Return
      End
