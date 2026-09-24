      subroutine h_tof_init(abort,err)
      
*-------------------------------------------------------------------
*     author: John Arrington
*     created: 2/22/94
*     
*     s_tof_init sets up the track independant parameters
*     for fitting the tof of the particle.
*     
*     modifications: 31 Mar 1994    DFG  Check for 0 hits
*     $Log: h_tof_init.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.2  2004/12/24 21:35:57  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.6 2004/04 Miyoshi
*     for E01-011
*
*     Revision 1.5  1995/05/22 19:46:00  cdaq
*     (SAW) Split gen_data_data_structures into gen, 
*     hms, sos, and coin parts"
*     
*     Revision 1.4  1995/02/23  15:58:54  cdaq
*     (JRA)  Change shodo_center_coord to shodo_center.
*     Make minph variables into per pmt constants.
*     
*     Revision 1.3  1994/11/23  14:23:05  cdaq
*     (SPB) Recopied from hms file and modified names for SOS
*     
*     Revision 1.2  1994/06/01  15:40:08  cdaq
*     (SAW) Change declaration of err to *(*)
*     
*     Revision 1.1  1994/04/13  18:45:03  cdaq
*     Initial revision
*
*-------------------------------------------------------------------

      implicit none
      
      include 'hks_data_structures.cmn'
      include 'hks_scin_parms.cmn'
      include 'hks_scin_tof.cmn'
      
      logical abort
      character*(*) err
      character*20 here
      parameter (here = 'h_tof_init')
      
      integer*4 ihit,pl,co
      save
      
      if(hscin_tot_hits.gt.0) then
         do ihit = 1 , hscin_tot_hits
            
            pl = hscin_layer_num(ihit) !from s_raw_scin common block.
            co = hscin_counter_num(ihit)            
            hscin_center_coord(ihit) = hscin_center(pl,co)
            
            if (pl .eq. 1) then !1x
               hscin_zpos(ihit) = hscin_1x_zpos
               if (2*int(float(co)/2.) .eq. co) then !even tube, in back.
                  hscin_zpos(ihit) = hscin_zpos(ihit) + hscin_1x_dzpos
               endif
               hscin_pos_coord(ihit) = hscin_1x_left
               hscin_neg_coord(ihit) = hscin_1x_right
            else if (pl .eq. 2) then !1y
               hscin_zpos(ihit) = hscin_1y_zpos
               if (2*int(float(co)/2.) .eq. co) then !even tube, in back.
                  hscin_zpos(ihit) = hscin_zpos(ihit) + hscin_1y_dzpos
               endif
               hscin_pos_coord(ihit) = hscin_1y_bot
               hscin_neg_coord(ihit) = hscin_1y_top
            else if (pl .eq. 3) then !2x
               hscin_zpos(ihit) = hscin_2x_zpos
               if (2*int(float(co)/2.) .eq. co) then !even tube, in back.
                  hscin_zpos(ihit) = hscin_zpos(ihit) + hscin_2x_dzpos
               endif
               hscin_pos_coord(ihit) = hscin_2x_left
               hscin_neg_coord(ihit) = hscin_2x_right
            else                ! unknown layer
               abort = .true.
               write(err,*) 'Trying to init. hks hodoscope layer',pl
               call g_prepend(here,err)
               return
            endif
            
         enddo
      endif                     ! end test on zero hits
      return
      end
