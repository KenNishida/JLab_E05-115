      subroutine h_dump_peds(ABORT,err)
*     
*     $Log: h_dump_peds.f,v $
*     Revision 1.1.1.1  2009/08/17 22:45:18  yez
*     Add Lucite sum channels
*     
*     Revision 1.1.1.1  2009/07/10 0:27:10  yez
*     Add Lucite info for E05-115
*     
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.2  2005/07/07 19:58:45  sumihama
*     Having 3 layer HTOF layers
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.2  2005/03/14 19:50:49  miyoshi
*     change variables names
*
*     Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
*     new dir
*
*     Revision 2.1 2004/03/02 Miyoshi
*     for E01-011
*     Revision 2.0  2000/03/05 11:52:45  jinghua
*     (JLiu)Added Aerogel and Lucite pedestals
*     
*     Revision 1.6  1999/01/29 17:34:58  saw
*     Add variables for second tubes on shower counter
*     
*     Revision 1.5  1996/11/07 19:50:44  saw
*     (JRA) ??
*     
*     Revision 1.4  1996/04/30 17:11:20  saw
*     (JRA) Cleanup
*     
*     Revision 1.3  1996/01/17 19:04:27  cdaq
*     (JRA)
*     
*     Revision 1.2  1995/10/09 20:18:31  cdaq
*     (JRA) Cleanup, add cerenkov pedestals
*     
*     Revision 1.1  1995/08/31 18:06:45  cdaq
*     Initial revision
*     
*     
      implicit none
      save
*     
      character*11 here
      parameter (here='h_dump_peds')
*     
      logical ABORT
      character*(*) err
*     
      integer*4 pln,cnt
      integer*4 blk
      integer*4 pmt
      character*132 file
      
      integer*4 HPAREID
      parameter (HPAREID=67)
*     
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'hks_pedestals.cmn'
      INCLUDE 'hks_scin_parms.cmn'
      INCLUDE 'hks_filenames.cmn'
      INCLUDE 'gen_run_info.cmn'
      
      if (h_pedestal_output_filename.ne.' ') then
         file=h_pedestal_output_filename
         call g_sub_run_number(file, gen_run_number)
         open(unit=HPAREID,file=file,status='unknown')
      else
         return
      endif
      
      write(HPAREID,*) 
     &     'These are the values that were used for the analysis'
      write(HPAREID,*) '      (from the param file or pedestal events)'
      write(HPAREID,*)
*     
*     
*     HODOSCOPE PEDESTALS
*     
      write(HPAREID,*) 'hscin_raw_ped_pos ='
      do cnt = 1 , hnum_scin_elements
         write(HPAREID,111) (hscin_raw_ped_pos(pln,cnt),pln=1,3)
      enddo
      write(HPAREID,*) 'hscin_new_ped_pos ='
      do cnt = 1 , hnum_scin_elements
         write(HPAREID,111) (hscin_new_ped_pos(pln,cnt),pln=1,3)
      enddo
      write(HPAREID,*) 'hscin_new_sig_pos ='
      do cnt = 1 , hnum_scin_elements
         write(HPAREID,111) (hscin_new_sig_pos(pln,cnt),pln=1,3)
      enddo
      write(HPAREID,*) 'hscin_new_threshold_pos ='
      do cnt = 1 , hnum_scin_elements
         write(HPAREID,111) (hscin_new_threshold_pos(pln,cnt),pln=1,3)
      enddo
      write(HPAREID,*) 'hscin_raw_ped_neg ='
      do cnt = 1 , hnum_scin_elements
         write(HPAREID,111) (hscin_raw_ped_neg(pln,cnt),pln=1,3)
      enddo
      write(HPAREID,*) 'hscin_new_ped_neg ='
      do cnt = 1 , hnum_scin_elements
         write(HPAREID,111) (hscin_new_ped_neg(pln,cnt),pln=1,3)
      enddo
      write(HPAREID,*) 'hscin_new_sig_neg ='
      do cnt = 1 , hnum_scin_elements
         write(HPAREID,111) (hscin_new_sig_neg(pln,cnt),pln=1,3)
      enddo
      write(HPAREID,*) 'hscin_new_threshold_neg ='
      do cnt = 1 , hnum_scin_elements
         write(HPAREID,111) 
     &        (hscin_new_threshold_neg(pln,cnt),pln=1,3)
      enddo
      
*     --- remember we have 3 layers of hodoscope.
 111  format(10x,2(f6.1,','),f6.1)

*****************************************************
*     
*     Aerogel Pedestals
*     
*****************************************************
      
      write(HPAREID,*) 'haer_pos_ped_mean ='
      do cnt = 1 , hnum_aer_counters
         write(HPAREID,112) 
     &        (haer_pos_ped_mean(pln,cnt),pln=1,HNUM_AER_LAYERS)
      enddo
      write(HPAREID,*) 'haer_new_ped_pos ='
      do cnt = 1 , hnum_aer_counters
         write(HPAREID,112) 
     &        (haer_new_ped_pos(pln,cnt),pln=1,HNUM_AER_LAYERS)
      enddo
      write(HPAREID,*) 'haer_new_sig_pos ='
      do cnt = 1 , hnum_aer_counters
         write(HPAREID,112) 
     &        (haer_new_sig_pos(pln,cnt),pln=1,HNUM_AER_LAYERS)
      enddo
      write(HPAREID,*) 'haer_new_threshold_pos ='
      do cnt = 1 , hnum_aer_counters
         write(HPAREID,112) 
     &        (haer_new_threshold_pos(pln,cnt),pln=1,HNUM_AER_LAYERS)
      enddo
      
      write(HPAREID,*) 'haer_neg_ped_mean ='
      do cnt = 1 , hnum_aer_counters
         write(HPAREID,112) 
     &        (haer_neg_ped_mean(pln,cnt),pln=1,HNUM_AER_LAYERS)
      enddo
      write(HPAREID,*) 'haer_new_ped_neg ='
      do cnt = 1 , hnum_aer_counters
         write(HPAREID,112) 
     &        (haer_new_ped_neg(pln,cnt),pln=1,HNUM_AER_LAYERS)
      enddo
      write(HPAREID,*) 'haer_new_sig_neg ='
      do cnt = 1 , hnum_aer_counters
         write(HPAREID,112) 
     &        (haer_new_sig_neg(pln,cnt),pln=1,HNUM_AER_LAYERS)
      enddo
      write(HPAREID,*) 'haer_new_threshold_neg ='
      do cnt = 1 , hnum_aer_counters
         write(HPAREID,112) 
     &        (haer_new_threshold_neg(pln,cnt),pln=1,HNUM_AER_LAYERS)
      enddo
      
*     --- remember we have 3 layers of aerogel
 112  format(10x,2(f6.1,','),f6.1)
      
*****************************************************
*     
*     Water Pedestals
*     
*****************************************************
      
      
      write(HPAREID,*) 'hwat_pos_ped_mean ='
      do cnt = 1 , hnum_wat_counters
         write(HPAREID,113) 
     &        (hwat_pos_ped_mean(pln,cnt),pln=1,HNUM_WAT_LAYERS)
      enddo
      write(HPAREID,*) 'hwat_new_ped_pos ='
      do cnt = 1 , hnum_wat_counters
         write(HPAREID,113) 
     &        (hwat_new_ped_pos(pln,cnt),pln=1,HNUM_WAT_LAYERS)
      enddo
      write(HPAREID,*) 'hwat_new_sig_pos ='
      do cnt = 1 , hnum_wat_counters
         write(HPAREID,113) 
     &        (hwat_new_sig_pos(pln,cnt),pln=1,HNUM_WAT_LAYERS)
      enddo
      write(HPAREID,*) 'hwat_new_threshold_pos ='
      do cnt = 1 , hnum_wat_counters
         write(HPAREID,113) 
     &        (hwat_new_threshold_pos(pln,cnt),pln=1,HNUM_WAT_LAYERS)
      enddo
      
      write(HPAREID,*) 'hwat_neg_ped_mean ='
      do cnt = 1 , hnum_wat_counters
         write(HPAREID,113) 
     &        (hwat_neg_ped_mean(pln,cnt),pln=1,HNUM_WAT_LAYERS)
      enddo
      write(HPAREID,*) 'hwat_new_ped_neg ='
      do cnt = 1 , hnum_wat_counters
         write(HPAREID,113) 
     &        (hwat_new_ped_neg(pln,cnt),pln=1,HNUM_WAT_LAYERS)
      enddo
      write(HPAREID,*) 'hwat_new_sig_neg ='
      do cnt = 1 , hnum_wat_counters
         write(HPAREID,113) 
     &        (hwat_new_sig_neg(pln,cnt),pln=1,HNUM_WAT_LAYERS)
      enddo
      write(HPAREID,*) 'hwat_new_threshold_neg ='
      do cnt = 1 , hnum_wat_counters
         write(HPAREID,113) 
     &        (hwat_new_threshold_neg(pln,cnt),pln=1,HNUM_WAT_LAYERS)
      enddo
      
*     --- remember we have 2 layers of water
 113  format(10x,f6.1,',',f6.1)
      
*     
*     Water Pedestals
*     
        
*****************************************************
*     
*    Lucite Pedestals
*     
*****************************************************
      
      
      write(HPAREID,*) 'hluc_pos_ped_mean ='
      do cnt = 1 , hnum_luc_counters
         write(HPAREID,114) 
     &        (hluc_pos_ped_mean(pln,cnt),pln=1,HNUM_LUC_LAYERS)
      enddo
      write(HPAREID,*) 'hluc_new_ped_pos ='
      do cnt = 1 , hnum_luc_counters
         write(HPAREID,114) 
     &        (hluc_new_ped_pos(pln,cnt),pln=1,HNUM_LUC_LAYERS)
      enddo
      write(HPAREID,*) 'hluc_new_sig_pos ='
      do cnt = 1 , hnum_luc_counters
         write(HPAREID,114) 
     &        (hluc_new_sig_pos(pln,cnt),pln=1,HNUM_LUC_LAYERS)
      enddo
      write(HPAREID,*) 'hluc_new_threshold_pos ='
      do cnt = 1 , hnum_luc_counters
         write(HPAREID,114) 
     &        (hluc_new_threshold_pos(pln,cnt),pln=1,HNUM_LUC_LAYERS)
      enddo
      
      write(HPAREID,*) 'hluc_neg_ped_mean ='
      do cnt = 1 , hnum_luc_counters
         write(HPAREID,114) 
     &        (hluc_neg_ped_mean(pln,cnt),pln=1,HNUM_LUC_LAYERS)
      enddo
      write(HPAREID,*) 'hluc_new_ped_neg ='
      do cnt = 1 , hnum_luc_counters
         write(HPAREID,114) 
     &        (hluc_new_ped_neg(pln,cnt),pln=1,HNUM_LUC_LAYERS)
      enddo
      write(HPAREID,*) 'hluc_new_sig_neg ='
      do cnt = 1 , hnum_luc_counters
         write(HPAREID,114) 
     &        (hluc_new_sig_neg(pln,cnt),pln=1,HNUM_LUC_LAYERS)
      enddo
      write(HPAREID,*) 'hluc_new_threshold_neg ='
      do cnt = 1 , hnum_luc_counters
         write(HPAREID,114) 
     &        (hluc_new_threshold_neg(pln,cnt),pln=1,HNUM_LUC_LAYERS)
      enddo
      
      write(HPAREID,*) 'hluc_tot_ped_mean ='
      do cnt = 1 , hnum_luc_counters
         write(HPAREID,114) 
     &        (hluc_tot_ped_mean(pln,cnt),pln=1,HNUM_LUC_LAYERS)
      enddo
      write(HPAREID,*) 'hluc_new_ped_tot ='
      do cnt = 1 , hnum_luc_counters
         write(HPAREID,114) 
     &        (hluc_new_ped_tot(pln,cnt),pln=1,HNUM_LUC_LAYERS)
      enddo
      write(HPAREID,*) 'hluc_new_sig_tot ='
      do cnt = 1 , hnum_luc_counters
         write(HPAREID,114) 
     &        (hluc_new_sig_tot(pln,cnt),pln=1,HNUM_LUC_LAYERS)
      enddo
      write(HPAREID,*) 'hluc_new_threshold_tot ='
      do cnt = 1 , hnum_luc_counters
         write(HPAREID,114) 
     &        (hluc_new_threshold_tot(pln,cnt),pln=1,HNUM_LUC_LAYERS)
      enddo
*     --- we only have 1 layer of lucite
 114  format(10x,f6.1)      
*     
*     Lucite Pedestals
*     
      
      
      close(HPAREID)
      
      return
      end







