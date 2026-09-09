      subroutine h_init_scin(ABORT,err)
      
*-------------------------------------------------------------------
*     author: John Arrington
*     created: 2/22/94
*     
*     s_init_scin initializes the corrections and parameters
*     for the scintillators.  Corrections are read from data files
*     or the database.  Arrays used by the tof fitting routines
*     are filled from the CTP variables input from the hks_positions
*     parameter file.
*     
*     modifications:
*     23 March 1993   DFG
*     Remove /nolist from include statement. UNIX doesn't like it.
*     $Log: h_init_scin.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.2  2004/12/24 21:33:07  miyoshi
*     change name plane to layer
*
*     Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
*     new dir
*
*     Revision 1.6  1996/04/30 17:32:20  saw
*     (JRA) Calculate expected particle velocity
*     
*     Revision 1.5  1995/05/22 19:45:41  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, hks, and coin parts"
*     
*     Revision 1.4  1995/02/23  13:36:31  cdaq
*     * (JRA) Remove _coord fro shodo_center array.  Edge coordinates replaced by
*     * center locations.
*     
*     Revision 1.3  1994/11/22  21:12:11  cdaq
*     (SPB) Recopied from hms file and modified names for HKS
*     
*     Revision 1.2  1994/06/01  15:37:05  cdaq
*     (SAW) Add Abort and err arguments
*     
*     Revision 1.1  1994/04/13  18:19:01  cdaq
*     Initial revision
*     
*-------------------------------------------------------------------
      
      implicit none
      
      include 'hks_data_structures.cmn'
      include 'hks_scin_parms.cmn'
      include 'hks_scin_tof.cmn'
      include 'hks_statistics.cmn'
      
      logical abort
      character*(*) err
      character*20 here
      parameter (here='h_init_scin')
      
      integer*4 layer,counter
      save
*     
*     
*     initialize some position parameters.
      hnum_scin_counters(1) = hscin_1x_nr
      hnum_scin_counters(2) = hscin_1y_nr
      hnum_scin_counters(3) = hscin_2x_nr
      
      hstat_numevents=0
      
      do layer = 1 , hnum_scin_layers
         do counter = 1 , hnum_scin_counters(layer)
            
*     initialize tof parameters.
            
            if (layer .eq. 1) then
               hscin_center(layer,counter) =
     1              hscin_1x_center(counter) + hscin_1x_offset
            else if (layer .eq. 2) then
               hscin_center(layer,counter) =
     1              hscin_1y_center(counter) + hscin_1y_offset
            else if (layer .eq. 3) then
               hscin_center(layer,counter) =
     1              hscin_2x_center(counter) + hscin_2x_offset
            else                ! Error in layer number
               abort = .true.
               write(err,*) 'Trying to init. hks hodoscope layer',layer
               call g_prepend(here,err)
               return
            endif
            
            hstat_trk(layer,counter)=0
            hstat_poshit(layer,counter)=0
            hstat_neghit(layer,counter)=0
            hstat_andhit(layer,counter)=0
            hstat_orhit(layer,counter)=0
            
         enddo                  !loop over counters
      enddo                     !loop over layers
      
*     need expected particle velocity for start time calculation.
      hbeta_pcent = hpcentral/sqrt(hpcentral*hpcentral+
     &     hpartmass*hpartmass)
      
      return
      end
