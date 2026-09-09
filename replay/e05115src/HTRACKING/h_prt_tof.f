      subroutine h_prt_tof(itrk)
*-------------------------------------------------------------------
*     author: John Arrington
*     created: 3/27/94
*     
*     s_prt_tof dumps the sos_scin_tof bank.
*     
*     modifications:
*     $Log: h_prt_tof.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.5  2005/04/19 17:51:45  miyoshi
*     change variable name
*
*     Revision 1.4  2005/01/11 00:44:10  sumihama
*     Mod HKS-TOF
*
*     Revision 1.3  2005/01/07 23:38:13  sumihama
*     Mod. htof
*
*     Revision 1.2  2004/12/24 21:38:11  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.3  1995/05/22 19:45:51  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.2  1994/11/23  13:57:39  cdaq
*     (SPB) Recopied from hms file and modified names for SOS
*     
*     Revision 1.1  1994/04/13  18:22:01  cdaq
*     Initial revision
*     
*-------------------------------------------------------------------
      
      implicit none
      
      include 'hks_data_structures.cmn'
      include 'hks_scin_parms.cmn'
      include 'hks_scin_tof.cmn'
      include 'hks_tracking.cmn'
      
      integer*4 ihit, itrk, pl, co
      
      save
      
      write(hluno,'(''              ***H_SCIN_TOF BANK***'')')
      write(hluno,'(''        TRACK NUMBER'',i3)') itrk
      write(hluno,'(''POSITION/CALIBRATION VARIABLES:'')')
      write(hluno,'(''  +coord  -coord '',
     &     '' pos_dt  neg_dt  +sigma  +sigma'')')
      do ihit=1,hscin_tot_hits
         pl = hscin_layer_num(ihit)
         co = hscin_counter_num(ihit)
         write(hluno,'(f8.3,f8.3,2f8.3,2f8.3)')
     &        hscin_pos_coord(ihit), hscin_neg_coord(ihit),
     &        hscin_neg_time_offset(pl,co), hscin_pos_time_offset(pl,co),
     &        hscin_pos_sigma(pl,co), hscin_neg_sigma(pl,co)
      enddo
      write(hluno,'(''HIT POSITION AND OTHER CALCULATED VARIABLES:'')')
      write(hluno,'(''  long_coord trans_coord    +time    -time'',
     &     '' scin_time scin_sig  on_track  time@fp'')')
      do ihit=1,hscin_tot_hits
         write(hluno,'(2f12.4,2f9.3,f10.3,f10.3)')
     &        hscin_long_coord(ihit), hscin_trans_coord(ihit),
     &        hscin_pos_time(ihit), hscin_neg_time(ihit),
     &        hscin_time(ihit), hscin_trk_fptime(itrk,1)
      enddo
      write(hluno,'(''  trk  beta     chisq_beta  fp_time '',
     &     ''num_scin_hit'')')
      write(hluno,'(i4,f8.4,f14.3,f9.3,i8)') itrk,
     &     htrk_beta(itrk), htrk_tof(itrk), htrk_time_atfp(itrk),
     &     hscin_on_track(itrk,1)
      write(hluno,*)
      
      return
      end
