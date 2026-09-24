      subroutine h_Ntuple_keep(ABORT,err)
*-----------------------------------------------------------------
*     
*     Purpose : Add entry to the SOS Ntuple
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
*     
*     Revision 2.2 2009/08/11 Z.Ye
*     for E05-115: Add Lucite info
*   
*     Revision 2.1 2004/03/03 Miyoshi
*     for E01-011
*     Revision 2.0  2000/03/03 11:50:51  jinghua
*     (JLiu) Added Lucite info
*     
*     Revision 1.7  1996/09/04 15:18:21  saw
*     (JRA) Modify ntuple contents
*     
*     Revision 1.6  1996/01/16 16:40:31  cdaq
*     (JRA) Modify ntuple contents
*     
*     Revision 1.5  1995/09/01 13:38:46  cdaq
*     (JRA) Add Cerenkov photoelectron count to ntuple
*     
*     Revision 1.4  1995/05/22  20:50:48  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, 
*     sos, and coin parts"
*     
*     Revision 1.3  1995/05/11  19:00:39  cdaq
*     (SAW) Change SSDEDXn vars to an array.
*     
*     Revision 1.2  1994/06/17  02:42:33  cdaq
*     (KBB) Upgrade
*     
*     Revision 1.1  1994/04/12  16:16:28  cdaq
*     Initial revision
*     
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='h_Ntuple_keep')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_constants.par'
      INCLUDE 'hks_data_structures.cmn'
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'hks_tracking.cmn'
      INCLUDE 'hks_physics_sing.cmn'
      INCLUDE 'hks_scin_parms.cmn'
      INCLUDE 'hks_scin_tof.cmn'
      include 'hks_track_histid.cmn'
      include 'hks_aero_parms.cmn'
*     
      logical HEXIST            !CERNLIB function
*     
      integer m,i,j,k,hitn
      real*4 ts(5)
      real*4 hmsq
      
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(.NOT.h_Ntuple_exists) RETURN !nothing to do
*     
      Do i=1,hnphysics
         m= 0
         m= m+1
         h_Ntuple_contents(m)= HSP(i) ! 1 
         m= m+1
         h_Ntuple_contents(m)= HSDELTA(i) ! 2 
c         m= m+1
c         h_Ntuple_contents(m)= HSTHETA(i) ! 3
c         m= m+1
c         h_Ntuple_contents(m)= HSPHI(i) ! 4
         m= m+1
         h_Ntuple_contents(m)= hphys_scin_hit(i,1) ! 3
         m= m+1
         h_Ntuple_contents(m)= hsna1(i) ! 4
         m= m+1
         h_Ntuple_contents(m)= hsna2(i) ! 5
         m= m+1
         h_Ntuple_contents(m)= hsna3(i) ! 6
         m= m+1
         h_Ntuple_contents(m)= hsnt1(i) ! 7
         m= m+1
         h_Ntuple_contents(m)= hsnt2(i) ! 8
         m= m+1
         h_Ntuple_contents(m)= hsnt3(i) ! 9
         m= m+1
         h_Ntuple_contents(m)= hsnco1(i) ! 10
         m= m+1
         h_Ntuple_contents(m)= hsnco2(i) ! 11
         m= m+1
         h_Ntuple_contents(m)= hsnco3(i) ! 12
         m= m+1
         h_Ntuple_contents(m)= HSBETA(i) ! 13
         m= m+1
         h_Ntuple_contents(m)= HSBETA1Y(i) ! 14
c      ----- calculate mass for HKS
         hmsq= (hsp(i)*hsp(i)*(1/hsbeta(i)/hsbeta(i)-1))
         m= m+1
         h_Ntuple_contents(m)= hmsq ! 15
         m= m+1
         h_Ntuple_contents(m)= hstof(i) ! 16
         m= m+1
         h_Ntuple_contents(m)= HSX_FP(i) ! 17 
         m= m+1
         h_Ntuple_contents(m)= HSY_FP(i) ! 18
         m= m+1
         h_Ntuple_contents(m)= HSXP_FP(i) ! 19
         m= m+1
         h_Ntuple_contents(m)= HSYP_FP(i) ! 20
         m= m+1
         h_Ntuple_contents(m)= hstime_at_fp(i) ! 21
         m= m+1
         h_Ntuple_contents(m)= hspathlength(i) ! 22
         m= m+1
         h_Ntuple_contents(m)= HSXP_TAR(i) ! 23
         m= m+1
         h_Ntuple_contents(m)= HSYP_TAR(i) ! 24

         m= m+1
         h_Ntuple_contents(m)= HX_SV(i) ! 25
         m= m+1
         h_Ntuple_contents(m)= HY_SV(i) ! 26

         m= m+1
         h_Ntuple_contents(m)= float(gen_event_ID_number) ! 27
         m= m+1
         h_Ntuple_contents(m)= float(gen_run_number) ! 28
         
c     --- number of hit aerogel layer
         hitn=0
         Do j=1,3
            if(htrk_naer(i,j) .gt.0) then
               hitn=hitn+1
            EndIf
         EndDo
         m= m+1
         h_Ntuple_contents(m)= float(hitn) ! 29

         m= m+1
         h_Ntuple_contents(m)= hsaer_num(i,1) ! 30
         m= m+1
         h_Ntuple_contents(m)= hsaer_num(i,2) ! 31
         m= m+1
         h_Ntuple_contents(m)= hsaer_num(i,3) ! 32
         m= m+1
         h_Ntuple_contents(m)= hsaer_npe(i,1) ! 33
         m= m+1
         h_Ntuple_contents(m)= hsaer_npe(i,2) ! 34
         m= m+1
         h_Ntuple_contents(m)= hsaer_npe(i,3) ! 35
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE2(i,1) ! 36 
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE2(i,2) ! 37
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE2(i,3) ! 38
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE_POS(i,1) ! 39
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE_POS(i,2) ! 40
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE_POS(i,3) ! 41
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE_POS2(i,1) ! 42 
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE_POS2(i,2) ! 43
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE_POS2(i,3) ! 44
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE_NEG(i,1) ! 45
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE_NEG(i,2) ! 46
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE_NEG(i,3) ! 47
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE_NEG2(i,1) ! 48 
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE_NEG2(i,2) ! 49
         m= m+1
         h_Ntuple_contents(m)= HSAER_NPE_NEG2(i,3) ! 50
         m= m+1
         h_Ntuple_contents(m)= hsaer_time(i,1) ! 51
         m= m+1
         h_Ntuple_contents(m)= hsaer_time(i,2) ! 52
         m= m+1
         h_Ntuple_contents(m)= hsaer_time(i,3) ! 53
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME2(i,1) ! 54 
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME2(i,2) ! 55
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME2(i,3) ! 56
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME_POS(i,1) ! 57
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME_POS(i,2) ! 58
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME_POS(i,3) ! 59
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME_POS2(i,1) ! 60
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME_POS2(i,2) ! 61 
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME_POS2(i,3) ! 62 
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME_NEG(i,1) ! 63
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME_NEG(i,2) ! 64
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME_NEG(i,3) ! 65
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME_NEG2(i,1) ! 66
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME_NEG2(i,2) ! 67
         m= m+1
         h_Ntuple_contents(m)= HSAER_TIME_NEG2(i,3) ! 68
         
c     --- number of hit water layer
         hitn=0
         Do j=1,2
            if(htrk_nwat(i,j) .gt.0) then
               hitn=hitn+1
            EndIf
         EndDo
         m= m+1
         h_Ntuple_contents(m)= float(hitn) ! 69

         m= m+1
         h_Ntuple_contents(m)= hswat_num(i,1) ! 70
         m= m+1
         h_Ntuple_contents(m)= hswat_num(i,2) ! 71

         m= m+1
         h_Ntuple_contents(m)= hswat_npe(i,1) ! 72
         m= m+1
         h_Ntuple_contents(m)= hswat_npe(i,2) ! 73
         m= m+1
         h_Ntuple_contents(m)= HSWAT_NPE2(i,1) ! 74
         m= m+1
         h_Ntuple_contents(m)= HSWAT_NPE2(i,2) ! 75
*add normalized kaon NPE 
         m= m+1
         h_Ntuple_contents(m)= hswat_npe_k_ratio(i,1) ! 76
         m= m+1
         h_Ntuple_contents(m)= hswat_npe_k_ratio(i,2) ! 77
         m= m+1
         h_Ntuple_contents(m)= HSWAT_NPE_K_RATIO2(i,1) ! 78
         m= m+1
         h_Ntuple_contents(m)= HSWAT_NPE_K_RATIO2(i,2) ! 79
*end
         m= m+1
         h_Ntuple_contents(m)= HSWAT_NPE_POS(i,1) ! 80
         m= m+1
         h_Ntuple_contents(m)= HSWAT_NPE_POS(i,2) ! 81
         m= m+1
         h_Ntuple_contents(m)= HSWAT_NPE_POS2(i,1) ! 82
         m= m+1
         h_Ntuple_contents(m)= HSWAT_NPE_POS2(i,2) ! 83
         m= m+1
         h_Ntuple_contents(m)= HSWAT_NPE_NEG(i,1) ! 84
         m= m+1
         h_Ntuple_contents(m)= HSWAT_NPE_NEG(i,2) ! 85
         m= m+1
         h_Ntuple_contents(m)= HSWAT_NPE_NEG2(i,1) ! 86
         m= m+1
         h_Ntuple_contents(m)= HSWAT_NPE_NEG2(i,2) ! 87
         
         m= m+1
         h_Ntuple_contents(m)= hswat_time(i,1) ! 88
         m= m+1
         h_Ntuple_contents(m)= hswat_time(i,2) ! 89
         m= m+1
         h_Ntuple_contents(m)= HSWAT_TIME2(i,1) ! 90
         m= m+1
         h_Ntuple_contents(m)= HSWAT_TIME2(i,2) ! 91
 
c     --- number of hit lucite layer
         hitn=0
         Do j=1,2
            if(htrk_nluc(i,j) .gt.0) then
               hitn=hitn+1
            EndIf
         EndDo
         m= m+1
         h_Ntuple_contents(m)= float(hitn) ! 92
         m= m+1
         h_Ntuple_contents(m)= hsluc_num(i,1) ! 93
         m= m+1
         h_Ntuple_contents(m)= hsluc_npe(i,1) ! 94
         
         m= m+1
         h_Ntuple_contents(m)= hschi2perdeg(i) ! 95
         m= m+1
         h_Ntuple_contents(m)= hsnfree_fp(i) ! 96
         do j = 1, hdc_num_layers
            m=m+1
            h_Ntuple_contents(m)=hdc_hits_per_layer(j)    
         enddo
         do j = 1, hdc_num_layers
            m=m+1
            h_Ntuple_contents(m)=hsdc_drift_distance(i,j)    
         enddo
         do j = 1, hdc_num_layers
            m=m+1
            h_Ntuple_contents(m)=hsdc_layer_drift_time(i,j)    
         enddo
c         do j = 1, hdc_num_layers
c            m=m+1
c            h_Ntuple_contents(m)=hsdc_layer_sigma(i,j)    
c         enddo
         do j = 1, hdc_num_layers
            m=m+1
            h_Ntuple_contents(m)=hsdc_sing_res(i,j)    
         enddo
         do j = 1 , hdc_num_layers
            m=m+1
            h_Ntuple_contents(m)=hsdc_track_coord(i,j) 
         enddo
         do j = 1, hdc_num_layers
            m=m+1
            h_Ntuple_contents(m)=hsdc_wire_center(i,j)
         enddo
c         do j = 1, hdc_num_layers
c            m=m+1
c            h_Ntuple_contents(m)=hsdc_wire_numb(i,j)
c         enddo
         do j = 1, hdc_num_layers
            m=m+1
            h_Ntuple_contents(m)=hsdc_wire_coord(i,j)
         enddo
         m= m+1
         h_Ntuple_contents(m)= hsdc_track_numb(i) ! 97
         m= m+1
         h_Ntuple_contents(m)= hsbeta_k(i) ! 97
         m= m+1
         h_Ntuple_contents(m)= hsbeta_pi(i) ! 98
         m= m+1
         h_Ntuple_contents(m)= hsbeta_pr(i) ! 99
         m = m+1
         h_Ntuple_contents(m)= hmisc_dec_data(65,1)*0.025 ! 100
         m = m+1
         h_Ntuple_contents(m)= hstime_at_tar(i) ! 101
         m = m+1 
         h_Ntuple_contents(m)= hnphysics ! 102
         m = m+1 
         h_Ntuple_contents(m)= hsrftime ! 103
         m = m+1 
         h_Ntuple_contents(m)= hsrfdiff ! 104
         m= m+1
         h_Ntuple_contents(m)= gfrx !GBEAM_X ! 105
         m= m+1
         h_Ntuple_contents(m)= gfry !GBEAM_Y ! 106
         do j=1,5
            ts(j)=0.
            if(gen_event_ts_flag(j)) ts(j)=1
         enddo
         m= m+1
         h_Ntuple_contents(m)= ts(1) ! 107
         m= m+1
         h_Ntuple_contents(m)= ts(2) ! 108
         m= m+1
         h_Ntuple_contents(m)= ts(3) ! 109
         m= m+1
         h_Ntuple_contents(m)= ts(5) ! 110
*     Add to analtyze grouping 0918/2010 chiba
         m= m+1
         h_Ntuple_contents(m)= gtrig_flag_hes ! 111
         m= m+1
         h_Ntuple_contents(m)= gtrig_flag_kaonsum ! 112
         m= m+1
         h_Ntuple_contents(m)= gtrig_flag_kaonseg1 ! 113
         m= m+1
         h_Ntuple_contents(m)= gtrig_flag_kaonseg2 ! 114
         m= m+1
         h_Ntuple_contents(m)= gtrig_flag_kaonseg3 ! 115
         m= m+1
         h_Ntuple_contents(m)= gtrig_flag_kaonseg4 ! 116
         m= m+1
         h_Ntuple_contents(m)= gtrig_flag_kaonseg5 ! 117
         m= m+1
         h_Ntuple_contents(m)= gtrig_flag_kaonseg6 ! 118
         
*     Experiment dependent entries start here.
         
         
*     Fill ntuple for this event
         ABORT= .NOT.HEXIST(h_Ntuple_ID)
         IF(ABORT) THEN
            call G_build_note(':Ntuple ID#$ does not exist',
     &           '$',h_Ntuple_ID,' ',0.,' ',err)
            call G_add_path(here,err)
         ELSE
            call HFN(h_Ntuple_ID,h_Ntuple_contents)
         ENDIF
      
      EndDo                     ! hnphysics loop

*
      RETURN
      END      
