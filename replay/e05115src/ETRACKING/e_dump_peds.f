      Subroutine e_dump_peds(ABORT,err)
*--------------------------------------------------------
* Hodoscope pedestals calculation
*
* $Log: e_dump_peds.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.2  2005/06/12 20:36:52  cdaq
* change dum size
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.1.1.1  2004/08/30 21:21:41  miyoshi
* new dir
*
* Revision 1.4 2004/03/02 Miyoshi
*
*
* Revision 1.3  1999/12/23 19:59:24  ysato
* Compiled on Redhat Linux
*
*
* This routine will be called from engine
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Logical ABORT
      Character*(*) err
*--------------------------------------------------------
      Include "hes_data_structures.cmn"
      Include "hes_pedestals.cmn"
      Include "hes_filenames.cmn"
      Include "hes_scin_parms.cmn"
      Include "gen_run_info.cmn"

      character*50 here
      parameter (here='e_dump_peds')
*
      integer*4 pln,cnt
      character*132 file

      integer*4 EPAREID
      parameter (EPAREID=67)
*     
      if (e_pedestal_output_filename.ne.' ') then
         file=e_pedestal_output_filename
         call g_sub_run_number(file, gen_run_number)
         open(unit=EPAREID,file=file,status='unknown')
      else
         return
      endif
      
      write(EPAREID,*) 
     &     'These are the values that were used for the analysis'
      write(EPAREID,*) '      (from the param file or pedestal events)'
      write(EPAREID,*)
*     
*     
*     HODOSCOPE PEDESTALS
*     
      
      write(EPAREID,*) 'escin_pos_ped_mean ='
      do cnt = 1 , enum_scin_counters
         write(EPAREID,111) (escin_pos_ped_mean(pln,cnt),
     &        pln=1,enum_scin_layers)
      enddo
      write(EPAREID,*) 'escin_new_ped_pos ='
      do cnt = 1 , enum_scin_counters
         write(EPAREID,111) (escin_new_ped_pos(pln,cnt),
     &        pln=1,enum_scin_layers)
      enddo
      write(EPAREID,*) 'escin_new_sig_pos ='
      do cnt = 1 , enum_scin_counters
         write(EPAREID,111) (escin_new_sig_pos(pln,cnt),
     &        pln=1,enum_scin_layers)
      enddo
      write(EPAREID,*) 'escin_new_threshold_pos ='
      do cnt = 1 , enum_scin_counters
         write(EPAREID,111) (escin_new_threshold_pos(pln,cnt),
     &        pln=1,enum_scin_layers)
      enddo
      
      
      write(EPAREID,*) 'escin_neg_ped_mean ='
      do cnt = 1 , enum_scin_counters
         write(EPAREID,111) (escin_neg_ped_mean(pln,cnt),
     &        pln=1,enum_scin_layers)
      enddo
      write(EPAREID,*) 'escin_new_ped_neg ='
      do cnt = 1 , enum_scin_counters
         write(EPAREID,111) (escin_new_ped_neg(pln,cnt),
     &        pln=1,enum_scin_layers)
      enddo
      write(EPAREID,*) 'escin_new_sig_neg ='
      do cnt = 1 , enum_scin_counters
         write(EPAREID,111) (escin_new_sig_neg(pln,cnt),
     &        pln=1,enum_scin_layers)
      enddo
      write(EPAREID,*) 'escin_new_threshold_neg ='
      do cnt = 1 , enum_scin_counters
         write(EPAREID,111) (escin_new_threshold_neg(pln,cnt),
     &        pln=1,enum_scin_layers)
      EndDo

*     --- we know we have 3 layers of hodoscope
 111  format (f5.1,',',f5.1,',',f5.1)
      
      close(EPAREID)

      Return
      End
