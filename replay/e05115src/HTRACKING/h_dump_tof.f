      SUBROUTINE H_DUMP_TOF(ABORT,errmsg)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods :Fill scintillator information
*     -
*     -      Required Input BANKS     HKS_SCIN_TOF
*     -                               HKS_DATA_STRUCTURES
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any
*     - 
*     author: M.Sumihama
*     created: 1/10/2005
*     
*     h_dump_tof writes out the raw timing information.
*     This data is analyzed by independent routines to fit 
*      the corrections for pulse height walk, time offset.
*--------------------------------------------------------
      IMPLICIT NONE
*     
      character*50 here
      parameter (here= 'H_DUMP_TOF')
*     
      logical ABORT
      character*(*) errmsg
*     
      INCLUDE 'hks_data_structures.cmn'
      include 'hks_scin_parms.cmn'
      include 'hks_scin_tof.cmn'
      integer*4 ihit, cnt, lay

      save
*     
*     Write out TOF fitting data.
*     
      if(hscin_tot_hits.le.0) return
      
      do ihit = 1, hscin_tot_hits
         if( hgood_pos(ihit).and.hgood_neg(ihit)) then
            lay = hscin_layer_num(ihit)
            cnt = hscin_counter_num(ihit)
            write(38,112) hntracks_fp, lay, cnt, 
     &           hscin_adc_pos(ihit),hscin_adc_neg(ihit),
     &           hscin_tdc_pos(ihit),hscin_tdc_neg(ihit)
 112        format(i2,1x,i2,1x,i3,1x,f7.1,1x,f7.1,f7.1,1x,f7.1)
         endif
      EndDo 

      RETURN
      END
