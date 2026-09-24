      subroutine c_Ntuple_keep(ABORT,err)
*----------------------------------------------------------------------
*     
*     Purpose : Add entry to the COIN Ntuple
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*     Created: 11-Apr-1994  K.B.Beard, Hampton U.
*     Modified: 11-Oct-1999 Jinghua Liu for HNSS
*     
*     Revision 2.2 2009/08/11 Z. Ye
*     change for E05-115: Add Lucite to the end of the list
*
*     Revision 2.1 2004/03/02 Miyoshi
*     change for E01-011
*     
*     Revision 2.0  1999/10/11 17:20:09  jinghua
*     (JLiu) Change to HNSS
*     
*     Revision 1.9  1999/02/23 16:41:08  csa
*     Variable changes
*     
*     Revision 1.8  1996/09/04 15:30:17  saw
*     (JRA) Modify ntuple contents
*     
*     Revision 1.7  1996/04/29 18:44:04  saw
*     (JRA) Add aerogel photon count
*     
*     Revision 1.6  1996/01/22 15:06:41  saw
*     (JRA) Change ntuple contents
*     
*     Revision 1.5  1996/01/16 21:01:33  cdaq
*     (JRA) Add HSDELTA and SSDELTA
*     
*     Revision 1.4  1995/09/01 15:45:21  cdaq
*     (JRA) Add spectrometer kinematic vars to ntuple
*     
*     Revision 1.3  1995/05/22  20:50:43  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, 
*     and coin parts"
*     
*     Revision 1.2  1994/06/17  02:41:25  cdaq
*     (KBB) Upgrade
*     
*     Revision 1.1  1994/04/12  16:12:33  cdaq
*     Initial revision
*     
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='c_Ntuple_keep')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'c_ntuple.cmn'
      INCLUDE 'gen_data_structures.cmn'
      INCLUDE 'gen_run_info.cmn'
      INCLUDE 'coin_data_structures.cmn'
      INCLUDE 'hes_data_structures.cmn'
      INCLUDE 'hks_data_structures.cmn'
      Include 'hks_physics_sing.cmn'
      INCLUDE 'hks_scin_parms.cmn'
      INCLUDE 'hks_track_histid.cmn'
      INCLUDE 'hks_aero_parms.cmn'
      INCLUDE 'hks_water_parms.cmn'
      INCLUDE 'hks_lucite_parms.cmn'
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'hks_tracking.cmn'
      INCLUDE 'gen_event_info.cmn'
      INCLUDE 'gen_scalers.cmn'
      INCLUDE 'hks_scin_tof.cmn'
      Include 'gen_epics.cmn'
*     
      logical HEXIST            !CERNLIB function
*     
      integer*4 m,i,j,k,ih,ie,cn
      Real*4 avedepo
      Integer*4 avendepo,hitn
      real*4 ts(12),hmsq
      integer*4 layer
*     
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(.NOT.c_Ntuple_exists) RETURN !nothing to do
      
      IF(
     &     cnphysics.LE.0
     &     ) RETURN             ! No coincidence
      
**********begin insert description of contents of COIN tuple ******
      
cc      Write(*,*) '(cntkeep) cn=',cnphysics

      Do cn=1,CNPHYSICS
         ih=cnhks(cn) 
         ie=cnhes(cn)
         
*     --- put cut condition here

c         if(
c     &        esp(ie) .gt. 0 .and.
c     &        hsp(ih) .gt. 0
c     &        hsbeta(ih) .gt. 0 .and.
c     &        hsbeta(ih) .lt. 2.0
c     &        ) then
            
c     --- start ntuple contents
            m= 0
            m= m+1
            c_Ntuple_contents(m)= gepics_pbeam ! 0
            m= m+1
            c_Ntuple_contents(m)= hstime_at_fp(ih) ! 1 
            m= m+1
            c_Ntuple_contents(m)= hstime_at_tar(ih) ! 2
            m= m+1
            c_Ntuple_contents(m)= HSX_FP(ih) ! 3
            m= m+1
            c_Ntuple_contents(m)= HSY_FP(ih) ! 4
            m= m+1
            c_Ntuple_contents(m)= HSXP_FP(ih) ! 5
            m= m+1
            c_Ntuple_contents(m)= HSYP_FP(ih) ! 6
c            m= m+1
c            c_Ntuple_contents(m)= HSX_TAR(ih) ! 7
c            m= m+1
c            c_Ntuple_contents(m)= HSY_TAR(ih) ! 8
            m= m+1
            c_Ntuple_contents(m)= HSXP_TAR(ih) ! 9
            m= m+1
            c_Ntuple_contents(m)= HSYP_TAR(ih) ! 10
            m= m+1
            c_Ntuple_contents(m)= HSX_SV(ih) ! 11
            m= m+1
            c_Ntuple_contents(m)= HSY_SV(ih) ! 12
            m= m+1
            c_Ntuple_contents(m)= HSXP_SV(ih) ! 13
            m= m+1
            c_Ntuple_contents(m)= HSYP_SV(ih) ! 14
            m= m+1
            c_Ntuple_contents(m)= HSP(ih) ! 15
c            m= m+1
c            c_Ntuple_contents(m)= HSdelta(ih) ! 16
            m= m+1
            c_Ntuple_contents(m)= hsbeta(ih) ! 17
            m= m+1
            c_Ntuple_contents(m)= hsbeta1y(ih) ! 18
            m= m+1
            c_Ntuple_contents(m)= hsbeta_k(ih) ! 19
c     --- calculate mass for HKS             
            hmsq= (hsp(ih)*hsp(ih)*(1/hsbeta(ih)/hsbeta(ih)-1))
            m= m+1
            c_Ntuple_contents(m)= hmsq ! 20
c     --- number of hit aerogel layer
            hitn=0
            Do j=1,3
               if(htrk_naer(ih,j) .gt.0) then
                  hitn=hitn+1
               EndIf
            EndDo
            m= m+1
            c_Ntuple_contents(m)= float(hitn) ! 21
            m= m+1
            c_Ntuple_contents(m)= float(htrk_naer(ih,1)) ! 22 
            m= m+1
            c_Ntuple_contents(m)= float(htrk_naer(ih,2)) ! 23 
            m= m+1
            c_Ntuple_contents(m)= float(htrk_naer(ih,3)) ! 24 
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE(ih,1) ! 25
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE(ih,2) ! 26
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE(ih,3) ! 27
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE2(ih,1) ! 28 
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE2(ih,2) ! 29
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE2(ih,3) ! 30
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE_POS(ih,1) ! 31
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE_POS(ih,2) ! 32
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE_POS(ih,3) ! 33
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE_POS2(ih,1) ! 34 
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE_POS2(ih,2) ! 35
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE_POS2(ih,3) ! 36
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE_NEG(ih,1) ! 37
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE_NEG(ih,2) ! 38
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE_NEG(ih,3) ! 39
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE_NEG2(ih,1) ! 40 
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE_NEG2(ih,2) ! 41
            m= m+1
            c_Ntuple_contents(m)= HSAER_NPE_NEG2(ih,3) ! 42
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME(ih,1) ! 43
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME(ih,2) ! 44
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME(ih,3) ! 45
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME2(ih,1) ! 46 
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME2(ih,2) ! 47
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME2(ih,3) ! 48
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME_POS(ih,1) ! 49
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME_POS(ih,2) ! 50
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME_POS(ih,3) ! 51
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME_POS2(ih,1) ! 52 
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME_POS2(ih,2) ! 53
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME_POS2(ih,3) ! 54
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME_NEG(ih,1) ! 55
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME_NEG(ih,2) ! 56
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME_NEG(ih,3) ! 57
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME_NEG2(ih,1) ! 58 
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME_NEG2(ih,2) ! 59
            m= m+1
            c_Ntuple_contents(m)= HSAER_TIME_NEG2(ih,3) ! 60
            m= m+1
            c_Ntuple_contents(m)= HSAER_NUM(ih,1) ! 61 
            m= m+1
            c_Ntuple_contents(m)= HSAER_NUM(ih,2) ! 62
            m= m+1
            c_Ntuple_contents(m)= HSAER_NUM(ih,3) ! 63
c     --- number of hit water layer
            hitn=0
            Do j=1,2
               if(htrk_nwat(ih,j) .gt.0) then
                  hitn=hitn+1
               EndIf
            EndDo
            m= m+1
            c_Ntuple_contents(m)= float(hitn) ! 64
            m= m+1
            c_Ntuple_contents(m)= float(htrk_nwat(ih,1)) ! 65
            m= m+1
            c_Ntuple_contents(m)= float(htrk_nwat(ih,2)) ! 66
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE(ih,1) ! 67
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE(ih,2) ! 68
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE2(ih,1) ! 69
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE2(ih,2) ! 70
*add normalized kaon NPE  
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE_K_RATIO(ih,1) ! 71
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE_K_RATIO(ih,2) ! 72
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE_K_RATIO2(ih,1) ! 73
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE_K_RATIO2(ih,2) ! 74
*end
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE_POS(ih,1) ! 75
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE_POS(ih,2) ! 76
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE_POS2(ih,1) ! 77
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE_POS2(ih,2) ! 78
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE_NEG(ih,1) ! 79
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE_NEG(ih,2) ! 80
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE_NEG2(ih,1) ! 81
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NPE_NEG2(ih,2) ! 82
            m= m+1
            c_Ntuple_contents(m)= HSWAT_TIME(ih,1) ! 83
            m= m+1
            c_Ntuple_contents(m)= HSWAT_TIME(ih,2) ! 84
            m= m+1
            c_Ntuple_contents(m)= HSWAT_TIME2(ih,1) ! 85
            m= m+1
            c_Ntuple_contents(m)= HSWAT_TIME2(ih,2) ! 86
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NUM(ih,1) ! 87
            m= m+1
            c_Ntuple_contents(m)= HSWAT_NUM(ih,2) ! 88
            m= m+1
            c_Ntuple_contents(m)= hschi2perdeg(ih) ! 89
            m= m+1
            c_Ntuple_contents(m)= hsnfree_fp(ih) ! 90
            do j = 1, hdc_num_layers
               m=m+1
               c_Ntuple_contents(m)=hsdc_sing_res(ih,j)    
            enddo
            do j = 1, hdc_num_layers
               m=m+1
               c_Ntuple_contents(m)=hsdc_track_coord(ih,j) 
            enddo
c     --- number of scin hit
            m= m+1
            c_Ntuple_contents(m)= hphys_scin_hit(ih,1) ! 91
            m= m+1
            c_Ntuple_contents(m)= hsnco1(ih) ! 92
            m= m+1
            c_Ntuple_contents(m)= hsnco2(ih) ! 93
            m= m+1
            c_Ntuple_contents(m)= hsnco3(ih) ! 94
            m= m+1
            c_Ntuple_contents(m)= hsnt1(ih) ! 95
            m= m+1
            c_Ntuple_contents(m)= hsnt2(ih) ! 96
            m= m+1
            c_Ntuple_contents(m)= hsnt3(ih) ! 97
            m= m+1
            c_Ntuple_contents(m)= hsnt1m(ih) ! 98
            m= m+1
            c_Ntuple_contents(m)= hsnt2m(ih) ! 99
            m= m+1
            c_Ntuple_contents(m)= hsnt3m(ih) ! 100
            m= m+1
            c_Ntuple_contents(m)= hsna1(ih) ! 101
            m= m+1
            c_Ntuple_contents(m)= hsna2(ih) ! 102
            m= m+1
            c_Ntuple_contents(m)= hsna3(ih) ! 103
            m= m+1
            c_Ntuple_contents(m)= hsna1m(ih) ! 104
            m= m+1
            c_Ntuple_contents(m)= hsna2m(ih) ! 105
            m= m+1
            c_Ntuple_contents(m)= hsna3m(ih) ! 106
c     --- average scintillator deposit per good scintillator hits
            avedepo = 0
            avendepo = 0
            Do j=1,3
               if(hsdedx(ih,j) .gt. 1) then
                  avendepo = avendepo + 1
                  avedepo = avedepo + hsdedx(ih,j)
               EndIf
            EndDo 
            avedepo = avedepo/Real(avendepo)
c            m= m+1
c            c_Ntuple_contents(m)= avedepo ! 107
            m= m+1
            c_Ntuple_contents(m)= hsrftime ! 108
c            m= m+1
c            c_Ntuple_contents(m)= hsrfdiff ! 109
*     ---hes 
            m= m+1
            c_Ntuple_contents(m)= estime_at_fp(ie) ! 110
            m= m+1
            c_Ntuple_contents(m)= estime_at_tar(ie) ! 111
            m= m+1
            c_Ntuple_contents(m)= esx_fp(ie) ! 112
            m= m+1
            c_Ntuple_contents(m)= esy_fp(ie) ! 113
            m= m+1
            c_Ntuple_contents(m)= esxp_fp(ie) ! 114
            m= m+1
            c_Ntuple_contents(m)= esyp_fp(ie) ! 115
c            m= m+1
c            c_Ntuple_contents(m)= esx_tar(ie) ! 116
c            m= m+1
c            c_Ntuple_contents(m)= esy_tar(ie) ! 117
            m= m+1
            c_Ntuple_contents(m)= esxp_tar(ie) ! 118
            m= m+1
            c_Ntuple_contents(m)= esyp_tar(ie) ! 119
            m= m+1
            c_Ntuple_contents(m)= esx_sv(ie) ! 120
            m= m+1
            c_Ntuple_contents(m)= esy_sv(ie) ! 121
            m= m+1
            c_Ntuple_contents(m)= esxp_sv(ie) ! 122
            m= m+1
            c_Ntuple_contents(m)= esyp_sv(ie) ! 123
            m= m+1
            c_Ntuple_contents(m)= esp(ie) ! 124
c            m= m+1
c            c_Ntuple_contents(m)= esdelta(ie) ! 125
c     ==========EDC =======================
            m= m+1
            c_Ntuple_contents(m)= eschi2perdeg(ie) ! 126
            m= m+1
            c_Ntuple_contents(m)= esnfree_fp(ie) ! 84
            do j=1,16
               m= m+1
               c_Ntuple_contents(m)= esresidual(ie,j) ! 83
            enddo
            do j=1,16
               m= m+1
               c_Ntuple_contents(m) = edc_tc(ie,j) ! 
            enddo
            do j=1,16
               m= m+1
               c_Ntuple_contents(m) = edc_wc(ie,j) ! 
            enddo
c     =========HES HODOSCOPE (EHODO) =======
            m= m+1
            c_Ntuple_contents(m)= ephys_scin_hits(ie,1) ! 85 tot hits

c     --- hes hodoscope total hit
            m= m+1
            c_Ntuple_contents(m)= esnco1(ie) ! 129
            m= m+1
            c_Ntuple_contents(m)= esnco2(ie) ! 130
            m= m+1
            c_Ntuple_contents(m)= esnt1(ie) ! 131
            m= m+1
            c_Ntuple_contents(m)= esnt2(ie) ! 132
            m= m+1
            c_Ntuple_contents(m)= esnt1m(ie) ! 133
            m= m+1
            c_Ntuple_contents(m)= esnt2m(ie) ! 134
            m= m+1
            c_Ntuple_contents(m)= esna1(ie) ! 135
            m= m+1
            c_Ntuple_contents(m)= esna2(ie) ! 136
            m= m+1
            c_Ntuple_contents(m)= esna1m(ie) ! 137
            m= m+1
            c_Ntuple_contents(m)= esna2m(ie) ! 138
c     --- hes avarage scintillator deposit
c            m= m+1
c            c_Ntuple_contents(m)= esscin_depo(ie) ! 139
            m= m+1
            c_Ntuple_contents(m)= esrftime ! 140
c            m= m+1
c            c_Ntuple_contents(m)= esrfdiff ! 141
c     --- beam position
c     --- extracted from g_analyze_misc
            m= m+1
            c_Ntuple_contents(m)= (gfrx-0.63)/10. !GBEAM_X ! 142
            m= m+1
            c_Ntuple_contents(m)= (gfry-0.55)/2.5 !GBEAM_Y ! 143
c     --- SLI
            m= m+1
            c_Ntuple_contents(m)= gepics_sli_espreadm ! 144
c     --- FFB
c     --- fast feedback on flag (1:off,0:on)
c            m= m+1
c            c_Ntuple_contents(m)= gepics_ffb_c_stat ! 145
c     --- fast energy rock on flag(1:on,0:off) 
            m= m+1
            c_Ntuple_contents(m)= gepics_ffb_c_use ! 145
c     --- IPM3C*** read out in cm (used be Energy drift)30Mar07 Y.O.
            m= m+1
            c_Ntuple_contents(m)= gbpm_meanx(1) ! 146
            m= m+1
            c_Ntuple_contents(m)= gbpm_meany(1) ! 147
            m= m+1
            c_Ntuple_contents(m)= gbpm_meanx(2) ! 148
            m= m+1
            c_Ntuple_contents(m)= gbpm_meany(2) ! 149
            m= m+1
            c_Ntuple_contents(m)= gbpm_meanx(3) ! 150
            m= m+1
            c_Ntuple_contents(m)= gbpm_meany(3) ! 151
c     --- coin
            m= m+1
            c_Ntuple_contents(m)= FLOAT(ih) ! 152
            m= m+1
            c_Ntuple_contents(m)= FLOAT(ie) ! 153
            m= m+1
            c_Ntuple_contents(m)= cnphysics ! 154
            m= m+1
            c_Ntuple_contents(m)= FLOAT(gen_event_ID_number) ! 155
            m= m+1
            c_Ntuple_contents(m)= FLOAT(gen_run_number) ! 156
c            m= m+1
c            c_Ntuple_contents(m)= coinrf ! 157
            m= m+1
            c_Ntuple_contents(m)= ctime_coin_cor(cn) ! 158
            m= m+1
            c_Ntuple_contents(m)= gtrig_flag_hes ! 159
            m= m+1
            c_Ntuple_contents(m)= gtrig_flag_kaonsum ! 160
            m= m+1
            c_Ntuple_contents(m)= gtrig_flag_kaonseg1 ! 161
            m= m+1
            c_Ntuple_contents(m)= gtrig_flag_kaonseg2 ! 162
            m= m+1
            c_Ntuple_contents(m)= gtrig_flag_kaonseg3 ! 163
            m= m+1
            c_Ntuple_contents(m)= gtrig_flag_kaonseg4 ! 164
            m= m+1
            c_Ntuple_contents(m)= gtrig_flag_kaonseg5 ! 165
            m= m+1
            c_Ntuple_contents(m)= gtrig_flag_kaonseg6 ! 166
            do j=1,12
               ts(j)=0.
               if(gen_event_ts_flag(j)) ts(j)=1
            enddo
            m= m+1
            c_Ntuple_contents(m)= ts(1) ! 167    
            m= m+1
            c_Ntuple_contents(m)= ts(2) ! 168    
            m= m+1
            c_Ntuple_contents(m)= ts(3) ! 169    
            m= m+1
            c_Ntuple_contents(m)= ts(5) ! 170    
            m= m+1
            c_Ntuple_contents(m)= ts(11) ! 171
            m= m+1
            c_Ntuple_contents(m)= ts(12) ! 172
c            c_Ntuple_contents(m)= happex ! 136

c     --- number of hit lucite layer
            hitn=0
            Do j=1,2
               if(htrk_nluc(ih,j) .gt.0) then
                  hitn=hitn+1
               EndIf
            EndDo
            m= m+1
            c_Ntuple_contents(m)= float(hitn) ! 139
            m= m+1
            c_Ntuple_contents(m)= HSLUC_NPE(ih,1) ! 140
            m= m+1
            c_Ntuple_contents(m)= HSLUC_NPE2(ih,1) ! 141
            m= m+1
            c_Ntuple_contents(m)= HSLUC_TIME(ih,1) ! 142
            m= m+1
            c_Ntuple_contents(m)= HSLUC_TIME2(ih,1) ! 143
            m= m+1
            c_Ntuple_contents(m)= HSLUC_NUM(ih,1) ! 144
           

***********end insert description of contents of COIN tuple********
            ABORT= .NOT.HEXIST(c_Ntuple_ID)
            IF(ABORT) THEN
               call G_build_note(':Ntuple ID#$ does not exist',
     &              '$',c_Ntuple_ID,' ',0.,' ',err)
               call G_add_path(here,err)
            ELSE
               call HFN(c_Ntuple_ID,c_Ntuple_contents)
            ENDIF
      EndDo                     ! cnphysics loop
      RETURN
      END      
      
