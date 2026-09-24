      subroutine g_bm_Nt_keep(ABORT,err)
*----------------------------------------------------------------------
*
*     Purpose : Add entry to the SOS Sieve slit Ntuple
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 28-Feb-2000 
*
* Revision 1.0  2000/02/28 12:20:04  jinghua
* Initial revision
*
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='g_bm_nt_keep')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'g_beam_ntuple.cmn'
      INCLUDE 'gen_epics.cmn'
      INCLUDE 'gen_scalers.cmn'
      INCLUDE 'gen_event_info.cmn'
*
      logical HEXIST                    !CERNLIB function
*
      integer m
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(.NOT.g_beam_Ntuple_exists) RETURN !nothing to do
*
************************************************
      m= 0
*  
      m= m+1
      g_beam_Ntuple_contents(m)= FLOAT(gen_event_ID_number) !1
      m= m+1
      g_beam_Ntuple_contents(m)= g_run_time 
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_xph00
      m= m+1                                                       
      g_beam_Ntuple_contents(m)= gepics_yph00
      m= m+1                                                       
      g_beam_Ntuple_contents(m)= gepics_xph01 !5
      m= m+1                                                       
      g_beam_Ntuple_contents(m)= gepics_yph01
      m= m+1                                                       
      g_beam_Ntuple_contents(m)= gepics_xph02a
      m= m+1                                                       
      g_beam_Ntuple_contents(m)= gepics_yph02a
      m= m+1                                                       
      g_beam_Ntuple_contents(m)= gepics_xph02b
      m= m+1                                                       
      g_beam_Ntuple_contents(m)= gepics_yph02b !10
      m= m+1                                                       
      g_beam_Ntuple_contents(m)= gepics_xph02c
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_yph02c
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_xph03a
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_yph03a
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_xph03b !15
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_yph03b
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_dbdl
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_diset
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_diread
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_d00v_bdl !20
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_d00v_s
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_d00vm
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_01h_bdl
      m= m+1                                                        
      g_beam_Ntuple_contents(m)= gepics_01h_s    
      m=m+1                                                         
      g_beam_Ntuple_contents(m)= gepics_01hm !25    
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_02v_bdl  
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_02v_s    
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_02vm     
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_03h_bdl  
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_03h_s !30 
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_03hm     
      m= m+1
      g_beam_Ntuple_contents(m)= gepics_04h_bdl  
      m=m+1                                     
      g_beam_Ntuple_contents(m)= gepics_04h_s    
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_04hm     
c     ARC BPMs
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc01 !35
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc01
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc02
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc02
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc03
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc03 !40
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc04
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc04
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc05
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc05
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc06 !45
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc06
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc07
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc07
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc08
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc08 !50
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc10
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc10
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc11
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc11
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc12 !55
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc12
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc14
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc14
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc16
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc16 !60
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc17
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc17
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc18
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc18
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc19 !65
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc19
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xc20
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yc20
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_xp4c00
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_yp4c00 !70
c     etc
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_pbeam
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_bcm1
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_bcm2
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_bcm3
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_bcm4 !75
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_9th_field
c     Tohoku's magnets
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_spl_field
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_spl_stat
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_spl_temp
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_kd_field   !80
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_kd_stat
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_kq1_field 
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_kq1_stat
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_kq1_temp
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_kq2_field  !85
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_kq2_stat
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_kq2_temp
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_ed_field
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_ed_stat
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_eq1_field  !90
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_eq1_stat
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_eq1_temp
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_eq2_field
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_eq2_stat
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_eq2_temp !95
c     Target and Sieve Slits
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_kss_pos
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_ess_pos
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_tgt_pos
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_tgt_id
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_tgt_pid  !100
c     Fast Feed Back
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_ffb_c_stat
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_ffb_c_use
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_ffb_c_on
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_dz01_bdl
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_fz03_bdl !105
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_sp04_bdl
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_co04v_bdl
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_dw04ah_bdl
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_dw04bh_bdl
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_ed_mps_iout !110
      m= m+1                                    
      g_beam_Ntuple_contents(m)= gepics_kd_mps_iout
      
      
*
************************************************
*
*
      ABORT= .NOT.HEXIST(g_beam_Ntuple_ID)
      IF(ABORT) THEN
        call G_build_note(':Ntuple ID#$ does not exist',
     &       '$',g_beam_Ntuple_ID,' ',0.,' ',err)
        call G_add_path(here,err)
      ELSE
        call HFN(g_beam_Ntuple_ID,g_beam_Ntuple_contents)
      ENDIF
*
      RETURN
      END
