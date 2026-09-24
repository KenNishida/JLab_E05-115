      Subroutine e_init_histid(ABORT,err)
*--------------------------------------------------------
*     $Log: e_init_histid.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.7  2005/07/06 19:20:57  sumihama
*     Mod.hrf/erf
*
*     Revision 1.6  2005/07/06 02:31:57  sumihama
*     Mod. hist
*
*     Revision 1.5  2005/07/03 04:18:38  sumihama
*     Mod. hbook sor s.s
*
*     Revision 1.4  2005/06/10 18:57:53  cdaq
*     add tul hist and ana
*
*     Revision 1.3  2005/05/30 22:29:18  miyoshi
*     add/delete histogram
*
*     Revision 1.2  2005/05/28 00:43:03  cdaq
*     change/add f1trigtime hist
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.12  2005/04/19 17:46:52  miyoshi
*     add new hist
*
*     Revision 1.11  2005/04/08 18:50:16  miyoshi
*     add hist
*
*     Revision 1.10  2005/03/14 19:48:51  miyoshi
*     change histgram fill condition
*
*     Revision 1.9  2005/03/10 16:48:23  miyoshi
*     change tracking variable name
*
*     Revision 1.8  2005/02/14 22:27:35  miyoshi
*     change minor one
*
*     Revision 1.7  2005/02/10 19:12:26  miyoshi
*     add hist for phys
*
*     Revision 1.6  2005/01/24 19:56:11  miyoshi
*     add/delete histogram
*
*     Revision 1.5  2005/01/18 23:38:53  miyoshi
*     add histogram for enge track check
*
*     Revision 1.4  2005/01/14 22:42:58  miyoshi
*     add histogram
*
*     Revision 1.3  2004/12/24 21:27:03  miyoshi
*     add some histogram
*
*     Revision 1.2  2004/10/13 22:14:52  miyoshi
*     update for f1tdc
*
*     Revision 1.1.1.1  2004/08/30 21:21:41  miyoshi
*     new dir
*
*     Revision 1.4  2000/03/09 01:32:40  ysato
*     Update in the production run Mar.8
*     
*     Revision 1.3  1999/12/23 19:59:25  ysato
*     Compiled on Redhat Linux
*     
*     Revision 1.2  1999/11/02 15:55:26  ysato
*     Upgrade for HNSS
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_init_histid')
*     
      Logical ABORT
      Character*(*) err
*--------------------------------------------------------
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"
      Include "gen_f1tdc.cmn"
      
      Integer*4 thgetid
      Character*32 histname
      Integer*4 i,j,k,l
      
*     e_fill_dc1_raw_hist
      eiddc1rawtothit = thgetid('edc1rawtothit')
      eiddc1rawtothitzoom = thgetid('edc1rawtothitzoom')
      eiddc1rawtdcall = thgetid('edc1rawtdcall')

c     --- F1 trigger time hist is here for ROC11
      gidf1trigtimehist(1,1,1) = thgetid('gf1trigtime1-1')

      Do i=1,EMAX_NUM_DC1_LAYERS
         if(i .le. 9) then
            Write(histname,'(14Hedc1rawhitpat0,I1)') i
            eiddc1rawhitpat(i) = thgetid(histname)
         Else
            Write(histname,'(13Hedc1rawhitpat,I2)') i
            eiddc1rawhitpat(i) = thgetid(histname)
         EndIf
      EndDo

      Do i=1,EMAX_NUM_DC1_LAYERS
         if(i .le. 9) then
            Write(histname,'(16Hedc1rawlayertdc0,I1)') i
            eiddc1rawlayertdc(i) = thgetid(histname)
         Else
            Write(histname,'(15Hedc1rawlayertdc,I2)') i
            eiddc1rawlayertdc(i) = thgetid(histname)
         EndIf
         if(i .le. 9) then
            Write(histname,'(16Hedc1rawlayerhit0,I1)') i
            eiddc1rawlayerhit(i) = thgetid(histname)
         Else
            Write(histname,'(15Hedc1rawlayerhit,I2)') i
            eiddc1rawlayerhit(i) = thgetid(histname)
         endif
      EndDo
      
      if(eturnon_dc1_raw_hist .eq. 1) then
         Do i=1,EMAX_NUM_DC1_SLOTS
            if(i .le. 9) then
               Write(histname,'(11Hedc1rawtdc0,I1)') i
               eiddc1rawtdc(i) = thgetid(histname)
            Else
               Write(histname,'(10Hedc1rawtdc,I2)') i
               eiddc1rawtdc(i) = thgetid(histname)
            EndIf
         EndDo
      EndIf
      
*     e_fill_dc2_raw_hist
      eiddc2rawtothit = thgetid('edc2rawtothit')
      eiddc2rawtothitzoom = thgetid('edc2rawtothitzoom')
      eiddc2rawtdcall = thgetid('edc2rawtdcall')

      Do i=1,EMAX_NUM_DC2_LAYERS
         if(i .le. 9) then
            Write(histname,'(14Hedc2rawhitpat0,I1)') i
            eiddc2rawhitpat(i) = thgetid(histname)
         EndIf
      EndDo

      Do i=1,EMAX_NUM_DC2_LAYERS
         if(i .le. 9) then
            Write(histname,'(16Hedc2rawlayertdc0,I1)') i
            eiddc2rawlayertdc(i) = thgetid(histname)
            Write(histname,'(16Hedc2rawlayerhit0,I1)') i
            eiddc2rawlayerhit(i) = thgetid(histname)
         EndIf
      EndDo
      
      if(eturnon_dc2_raw_hist .eq. 1) then
         Do i=1,EMAX_NUM_DC2_SLOTS
            if(i .le. 9) then
               Write(histname,'(11Hedc2rawtdc0,I1)') i
               eiddc2rawtdc(i) = thgetid(histname)
             else
             Write(histname,'(10Hedc2rawtdc,I2)') i
               eiddc2rawtdc(i) = thgetid(histname)
            EndIf
         EndDo
      EndIf

*     e_fill_dc_dec_hist
      eiddc1dectothit = thgetid('edc1dectothit')
      eiddc1dectothitzoom = thgetid('edc1dectothitzoom')
      
      Do i=1,EMAX_NUM_DC1_LAYERS
         if(i .le. 9) then
            Write(histname,'(15Hedc1wirecenter0,I1)') i
            eiddc1wirecenter(i) = thgetid(histname)
         Else
            Write(histname,'(14Hedc1wirecenter,I2)') i
            eiddc1wirecenter(i) = thgetid(histname)
         EndIf
      EndDo

      if(eturnon_dc1_dec_hist .eq. 1) then
         Do i=1,EMAX_NUM_DC1_SLOTS
            if(i .le. 9) then
               Write(histname,'(14Hedc1drifttime0,I1)') i
               eiddc1drifttime(i) = thgetid(histname)
            Else
               Write(histname,'(13Hedc1drifttime,I2)') i
               eiddc1drifttime(i) = thgetid(histname)
            EndIf
         EndDo
      EndIf

      Do i=1,EMAX_NUM_DC1_SLOTS
         if(i .le. 9) then
            Write(histname,'(11Hdtime_edc10,I1)') i
            eiddc1singdtime(i) = thgetid(histname)
         Else
            Write(histname,'(10Hdtime_edc1,I2)') i
            eiddc1singdtime(i) = thgetid(histname)
         EndIf
      EndDo

      Do i=1,EMAX_NUM_DC1_LAYERS
         if(i .le. 9) then
            Write(histname,'(13Hedc1layerhit0,I1)') i
            eiddc1layerhit(i) = thgetid(histname)
         Else
            Write(histname,'(12Hedc1layerhit,I2)') i
            eiddc1layerhit(i) = thgetid(histname)
         EndIf
      EndDo

*     e_fill_dc_pretrk_hist
      
      Do i=1,emax_num_dc1_layers
         if(i .le. 9) then
            Write(histname,'(13Hedc1driftdis0,i1)') i
            eiddc1driftdis(i) = thgetid(histname)
         Else
            Write(histname,'(12Hedc1driftdis,i2)') i
            eiddc1driftdis(i) = thgetid(histname)
         EndIf
         if(i .le. 9) then
            Write(histname,'(11Hedc1wcoord0,i1)') i
            eiddc1wcoord(i) = thgetid(histname)
         Else
            Write(histname,'(10Hedc1wcoord,i2)') i
            eiddc1wcoord(i) = thgetid(histname)
         EndIf
      EndDo

*     e_fill_dc1_sp_hist
      eiddc1nspacepoint=thgetid('edc1nspacepoint')
      eiddc1nspacepointzoom=thgetid('edc1nspacepointzoom')
      eiddc1nspacepointhits=thgetid('edc1nspacepointhits')
      


      if(eturnon_dc1_track_hist .eq. 1) then
*     e_fill_dc1_dec_hist  
         eiddc1earlymult = thgetid('edc1earlymult')
         eiddc1latemult = thgetid('edc1latemult')
         eiddc1extramult = thgetid('edc1extramult')
         
         Do i=1,EMAX_NUM_DC1_LAYERS
            if(i .le. 9) then
               Write(histname,'(12Hedc1cluster0,I1)') i
               eiddc1cluster(i) = thgetid(histname)
            Else
               Write(histname,'(11Hedc1cluster,I2)') i
               eiddc1cluster(i) = thgetid(histname)
            EndIf
         EndDo
         
*     in e_pattern_recognition
         eiddc1edistancetest=thgetid('edistancetest')
         
*     e_fill_dc1_pretrk_hist
         eidntrackspre = thgetid('entrackspre')
         eidntracksprezoom = thgetid('entracksprezoom')
         eiddc1ntrackspre = thgetid('edc1ntrackspre')
         eiddc1ntracksprezoom = thgetid('edc1ntracksprezoom')
         eiddc2ntrackspre = thgetid('edc2ntrackspre')
         eiddc2ntracksprezoom = thgetid('edc2ntracksprezoom')
         
*     e_fill_dc1_track_hist
         Do i=1,emax_num_dc1_layers
            if(i .le. 9) then
               Write(histname,'(16Hedc1residualpre0,i1)') i
               eiddc1residualpre(i) = thgetid(histname)
            Else
               Write(histname,'(15Hedc1residualpre,i2)') i
               eiddc1residualpre(i) = thgetid(histname)
            EndIf
         EndDo
         
         eidchi2perdofpre = thgetid('echi2perdofpre')
         eidchi2perdofprezoom = thgetid('echi2perdofprezoom')
         eiddc1chi2perdofpre = thgetid('edc1chi2perdofpre')
         eiddc1chi2perdofprezoom = thgetid('edc1chi2perdofprezoom')
         eiddc2chi2perdofpre = thgetid('edc2chi2perdofpre')
         eiddc2chi2perdofprezoom = thgetid('edc2chi2perdofprezoom')
         
         Do i=1,emax_num_dc1_layers
            if(i .le. 9) then
               Write(histname,'(12Hedc1distime0,i1)') i
               eiddc1distime(i) = thgetid(histname)
            Else
               Write(histname,'(11Hedc1distime,i2)') i
               eiddc1distime(i) = thgetid(histname)
            EndIf
         EndDo
         Do i=1,emax_num_dc2_layers
            Write(histname,'(12Hedc2distime0,i1)') i
            eiddc2distime(i) = thgetid(histname)
         EndDo
         

        Do i=1,EMAX_NUM_DC2_LAYERS
         if(i .le. 9) then
            Write(histname,'(15Hedc2wirecenter0,I1)') i
            eiddc2wirecenter(i) = thgetid(histname)
         Else
            Write(histname,'(14Hedc2wirecenter,I2)') i
            eiddc2wirecenter(i) = thgetid(histname)
         EndIf
       EndDo

      if(eturnon_dc2_dec_hist .eq. 1) then
         Do i=1,EMAX_NUM_DC2_SLOTS
            if(i .le. 9) then
               Write(histname,'(14Hedc2drifttime0,I1)') i
               eiddc2drifttime(i) = thgetid(histname)
            Else
               Write(histname,'(13Hedc2drifttime,I2)') i
               eiddc2drifttime(i) = thgetid(histname)
            EndIf
         EndDo
      EndIf

       Do i=1,EMAX_NUM_DC2_layers
         if(i .le. 9) then
            Write(histname,'(11Hdtime_edc20,I1)') i
            eiddc2singdtime(i) = thgetid(histname)
         Else
            Write(histname,'(10Hdtime_edc2,I2)') i
            eiddc2singdtime(i) = thgetid(histname)
         EndIf
      EndDo

       Do i=1,EMAX_NUM_DC2_LAYERS
         if(i .le. 9) then
            Write(histname,'(13Hedc2layerhit0,I1)') i
            eiddc2layerhit(i) = thgetid(histname)
         Else
            Write(histname,'(12Hedc2layerhit,I2)') i
            eiddc2layerhit(i) = thgetid(histname)
         EndIf
      EndDo

        Do i=1,emax_num_dc2_layers
         if(i .le. 9) then
            Write(histname,'(13Hedc2driftdis0,i1)') i
            eiddc2driftdis(i) = thgetid(histname)
         Else
            Write(histname,'(12Hedc2driftdis,i2)') i
            eiddc2driftdis(i) = thgetid(histname)
         EndIf
        EndDo

*     e_select_good_tracks.f
         eidtrkdx = thgetid('etrkdx')
         eidtrkdy = thgetid('etrkdy')
         eidtrkdxp = thgetid('etrkdxp')
         eidtrkdyp = thgetid('etrkdyp')
         eidtrkdiff = thgetid('etrkdiff')
      EndIf                     ! eturnon_dc1_track_hist
      
*     e_select_good_tracks.f
      eidntracksfp = thgetid('entracksfp')
      eidntracksfpzoom = thgetid('entracksfpzoom')
      eiddc1ntracksfp = thgetid('edc1ntracksfp')
      eiddc1ntracksfpzoom = thgetid('edc1ntracksfpzoom')
      eiddc2ntracksfp = thgetid('edc2ntracksfp')
      eiddc2ntracksfpzoom = thgetid('edc2ntracksfpzoom')
      eidchi2perdoffp = thgetid('echi2perdoffp')
      eidchi2perdoffpzoom = thgetid('echi2perdoffpzoom')
      eiddc1chi2perdoffp = thgetid('edc1chi2perdoffp')
      eiddc1chi2perdoffpzoom = thgetid('edc1chi2perdoffpzoom')
      eiddc2chi2perdoffp = thgetid('edc2chi2perdoffp')
      eiddc2chi2perdoffpzoom = thgetid('edc2chi2perdoffpzoom')
      Do i=1,emax_num_dc1_layers
         if(i .le. 9) then
            Write(histname,'(12Hedc1singres0,i1)') i
            eiddc1singleresidual(i) = thgetid(histname)
         Else
            Write(histname,'(11Hedc1singres,i2)') i
            eiddc1singleresidual(i) = thgetid(histname)
         EndIf
      EndDo
      Do i=1,emax_num_dc2_layers
         if(i .le. 9) then
            Write(histname,'(12Hedc2singres0,i1)') i
            eiddc2singleresidual(i) = thgetid(histname)
         Else
            Write(histname,'(11Hedc2singres,i2)') i
            eiddc2singleresidual(i) = thgetid(histname)
         EndIf
      EndDo
      eidfpx = thgetid('efpx')
      eidfpy = thgetid('efpy')
      eidfpxp = thgetid('efpxp')
      eidfpyp = thgetid('efpyp')
      eidfpxy = thgetid('efpxy')
      eidfpxpyp = thgetid('efpxpyp')
      eidfpxxp = thgetid('efpxxp')
      eidfpxyp = thgetid('efpxyp')
      eidfpyxp = thgetid('efpyxp')
      eidfpyyp = thgetid('efpyyp')
      eiddc1fpx = thgetid('edc1fpx')
      eiddc1fpy = thgetid('edc1fpy')
      eiddc1fpxp = thgetid('edc1fpxp')
      eiddc1fpyp = thgetid('edc1fpyp')
      eiddc1fpxy = thgetid('edc1fpxy')
      eiddc1fpxpyp = thgetid('edc1fpxpyp')
      eiddc1fpxxp = thgetid('edc1fpxxp')
      eiddc1fpxyp = thgetid('edc1fpxyp')
      eiddc1fpyxp = thgetid('edc1fpyxp')
      eiddc1fpyyp = thgetid('edc1fpyyp')
      eiddc2fpx = thgetid('edc2fpx')
      eiddc2fpy = thgetid('edc2fpy')
      eiddc2fpxp = thgetid('edc2fpxp')
      eiddc2fpyp = thgetid('edc2fpyp')
      eiddc2fpxy = thgetid('edc2fpxy')
      eiddc2fpxpyp = thgetid('edc2fpxpyp')
      eiddc2fpxxp = thgetid('edc2fpxxp')
      eiddc2fpxyp = thgetid('edc2fpxyp')
      eiddc2fpyxp = thgetid('edc2fpyxp')
      eiddc2fpyyp = thgetid('edc2fpyyp')

****************************
*     ehodo
****************************
      
*     e_fill_scin_raw_hist
      eidscinrawtothit = thgetid('escinrawtothit')
      eidscinrawtothitzoom = thgetid('escinrawtothitzoom')
      
      eidscinrawtothitpos(1) = thgetid('escinrawtothitpos1')
      eidscinrawtothitpos(2) = thgetid('escinrawtothitpos2')
      eidscinrawtothitneg(1) = thgetid('escinrawtothitneg1')
      eidscinrawtothitneg(2) = thgetid('escinrawtothitneg2')
      eidscinrawtothitboth(1) = thgetid('escinrawtothitboth1')
      eidscinrawtothitboth(2) = thgetid('escinrawtothitboth2')

      eidscinrawadchitpatpos(1) = thgetid('escinrawadchitpatpos1')
      eidscinrawadchitpatpos(2) = thgetid('escinrawadchitpatpos2')
      eidscinrawadchitpatneg(1) = thgetid('escinrawadchitpatneg1')
      eidscinrawadchitpatneg(2) = thgetid('escinrawadchitpatneg2')
      eidscinrawtdchitpatpos(1) = thgetid('escinrawtdchitpatpos1')
      eidscinrawtdchitpatpos(2) = thgetid('escinrawtdchitpatpos2')
      eidscinrawtdchitpatneg(1) = thgetid('escinrawtdchitpatneg1')
      eidscinrawtdchitpatneg(2) = thgetid('escinrawtdchitpatneg2')

      eidscinrawsumposadc(1) = thgetid('escinrawsumposadc1')
      eidscinrawsumnegadc(1) = thgetid('escinrawsumnegadc1')
      eidscinrawsumposadc(2) = thgetid('escinrawsumposadc2')
      eidscinrawsumnegadc(2) = thgetid('escinrawsumnegadc2')
      eidscinrawsumpostdc(1) = thgetid('escinrawsumpostdc1')
      eidscinrawsumnegtdc(1) = thgetid('escinrawsumnegtdc1')
      eidscinrawsumpostdc(2) = thgetid('escinrawsumpostdc2')
      eidscinrawsumnegtdc(2) = thgetid('escinrawsumnegtdc2')

c     --- F1 trigger time hist is here for ROC12
      gidf1trigtimehist(2,1,1) = thgetid('gf1trigtime2-1')
      
      if(eturnon_scin_raw_hist .eq. 1) then
c     --- RAWADC and RAWTDC
         Do i=1,ENUM_SCIN_LAYERS
            Do j=1,ENUM_SCIN_COUNTERS
               if(j .le. 9) then
                  Write(histname,'(14Hescinrawposadc,I1,2H_0,I1)') i,j
                  eidscinrawposadc(i,j) = thgetid(histname)
                  Write(histname,'(14Hescinrawnegadc,I1,2H_0,I1)') i,j
                  eidscinrawnegadc(i,j) = thgetid(histname)
               Else
                  Write(histname,'(14Hescinrawposadc,I1,1H_,I2)') i,j
                  eidscinrawposadc(i,j) = thgetid(histname)
                  Write(histname,'(14Hescinrawnegadc,I1,1H_,I2)') i,j
                  eidscinrawnegadc(i,j) = thgetid(histname)
               EndIf
            EndDo
         EndDo
         Do i=1,ENUM_SCIN_LAYERS
            Do j=1,ENUM_SCIN_COUNTERS
               if(j .le. 9) then
                  Write(histname,'(14Hescinrawpostdc,I1,2H_0,I1)') i,j
                  eidscinrawpostdc(i,j) = thgetid(histname)
                  Write(histname,'(14Hescinrawnegtdc,I1,2H_0,I1)') i,j
                  eidscinrawnegtdc(i,j) = thgetid(histname)
               Else
                  Write(histname,'(14Hescinrawpostdc,I1,1H_,I2)') i,j
                  eidscinrawpostdc(i,j) = thgetid(histname)
                  Write(histname,'(14Hescinrawnegtdc,I1,1H_,I2)') i,j
                  eidscinrawnegtdc(i,j) = thgetid(histname)
               EndIf
            EndDo
         EndDo
      EndIf                     ! eturnon_scin_raw_hist = 1
      
      
*     e_fill_scin_dec_hist
      eidscindectothit = thgetid('escindectothit')
      eidscindectothitzoom = thgetid('escindectothitzoom')
      
      eidscindectothitpos(1) = thgetid('escindectothitpos1')
      eidscindectothitpos(2) = thgetid('escindectothitpos2')
      eidscindectothitneg(1) = thgetid('escindectothitneg1')
      eidscindectothitneg(2) = thgetid('escindectothitneg2')
      eidscindectothitboth(1) = thgetid('escindectothitboth1')
      eidscindectothitboth(2) = thgetid('escindectothitboth2')
      
      eidscindechitpatpos(1) = thgetid('escindechitpatpos1')
      eidscindechitpatpos(2) = thgetid('escindechitpatpos2')
      eidscindechitpatneg(1) = thgetid('escindechitpatneg1')
      eidscindechitpatneg(2) = thgetid('escindechitpatneg2')
      eidscindechitpatboth(1) = thgetid('escindechitpatboth1')
      eidscindechitpatboth(2) = thgetid('escindechitpatboth2')
      
      if(eturnon_scin_dec_hist .eq. 1) then
         Do i=1,ENUM_SCIN_LAYERS
            Do j=1,ENUM_SCIN_COUNTERS
               if(j .le. 9) then
                  Write(histname,'(10Hescinphpos,I1,2H_0,I1)') i,j
                  eidscinphpos(i,j) = thgetid(histname)
                  Write(histname,'(10Hescinphneg,I1,2H_0,I1)') i,j
                  eidscinphneg(i,j) = thgetid(histname)
               Else
                  Write(histname,'(10Hescinphpos,I1,1H_,I2)') i,j
                  eidscinphpos(i,j) = thgetid(histname)
                  Write(histname,'(10Hescinphneg,I1,1H_,I2)') i,j
                  eidscinphneg(i,j) = thgetid(histname)
               EndIf
            EndDo
         EndDo
         Do i=1,ENUM_SCIN_LAYERS
            Do j=1,ENUM_SCIN_COUNTERS
               if(j .le. 9) then
                  Write(histname,'(12Hescintimepos,I1,2H_0,I1)') i,j
                  eidscintimepos(i,j) = thgetid(histname)
                  Write(histname,'(12Hescintimeneg,I1,2H_0,I1)') i,j
                  eidscintimeneg(i,j) = thgetid(histname)
               Else
                  Write(histname,'(12Hescintimepos,I1,1H_,I2)') i,j
                  eidscintimepos(i,j) = thgetid(histname)
                  Write(histname,'(12Hescintimeneg,I1,1H_,I2)') i,j
                  eidscintimeneg(i,j) = thgetid(histname)
               EndIf
            EndDo
         EndDo
      EndIF                     ! eturnon_scin_dec_hist = 1
      
      Do i=1,ENUM_SCIN_LAYERS
         Do j=1,ENUM_SCIN_COUNTERS
            if(j .le. 9) then
               Write(histname,'(13Hescinmeantime,I1,2H_0,I1)') i,j
               eidscinmeantime(i,j) = thgetid(histname)
            Else
               Write(histname,'(13Hescinmeantime,I1,1H_,I2)') i,j
               eidscinmeantime(i,j) = thgetid(histname)
            EndIf
         EndDo
      EndDo
      
      if(eturnon_scin_dt_hist .eq. 1) then
*     e_fill_scin_dec_hist --- time difference
         Do i=1,ENUM_SCIN_LAYERS
            Do j=2,ENUM_SCIN_COUNTERS
               if(j .le. 8) then
                  Write(histname,'(9Hescindtco,I1,2H_0,I1,4Hand0,I1)') 
     &                 i,j-1,j
                  eidscindtcounter(i,j) = thgetid(histname)
               Else if(j .eq. 9) then
                  Write(histname,'(9Hescindtco,I1,2H_0,I1,3Hand,I2)') 
     &                 i,j-1,j
                  eidscindtcounter(i,j) = thgetid(histname)
               Else
                  Write(histname,'(9Hescindtco,I1,1H_,I2,3Hand,I2)') 
     &                 i,j-1,j
                  eidscindtcounter(i,j) = thgetid(histname)
               EndIf
            EndDo               ! counter
         EndDo                  ! layer
         
         Do i=1,ENUM_SCIN_COUNTERS
            if(i .le. 9) then
               Write(histname,'(13Hescindtlayer0,I1)') i
               eidscindtlayer(i) = thgetid(histname)
            Else
               Write(histname,'(12Hescindtlayer,I2)') i
               eidscindtlayer(i) = thgetid(histname)
            EndIf
         EndDo                  ! counter
      EndIf

c     --- e_fill_dc1_target_hist.f
      eidxtar = thgetid('extar')
      eidytar = thgetid('eytar')
      eidxptar = thgetid('exptar')
      eidyptar = thgetid('eyptar')
      eiddeltatar = thgetid('edeltatar')
      eidptar = thgetid('eptar')
      eidtarxy = thgetid('etarxy')
      eidtarxpyp = thgetid('etarxpyp')
c Sieve slit
      eidxsv = thgetid('exsv')
      eidysv = thgetid('eysv')
      eidxysv = thgetid('esvxy')

c     --- e_fill_link_hist.f
      eidxla(1)=thgetid('exla1')
      eidxla(2)=thgetid('exla2')
      eidtrkhodohit=thgetid('etrkhodohit')
      eidtrkfptime=thgetid('etrkfptime')
      Do i=1,ENUM_SCIN_COUNTERS
         if(i .le. 9) then
            Write(histname,'(8Hexla1co0,I1)') i
            eidxla1co(i) = thgetid(histname)
         Else
            Write(histname,'(7Hexla1co,I2)') i
            eidxla1co(i) = thgetid(histname)
         EndIf
      EndDo                     ! counter
      
c     --- for RF TDC
      eidmisctdcs(1) = thgetid('emisctdcs1')
      eidmisctdcs(2) = thgetid('emisctdcs2')
c     --- e_fill_phys_hist.f
      
      eidnphysics=thgetid('enphysics')
      eidphysscinhits=thgetid('ephysscinhits')
      eidsxfp=thgetid('esxfp')
      eidsyfp=thgetid('esyfp')
      eidsxpfp=thgetid('esxpfp')
      eidsypfp=thgetid('esypfp')
      eidsxtar=thgetid('esxtar')
      eidsytar=thgetid('esytar')
      eidsxptar=thgetid('esxptar')
      eidsyptar=thgetid('esyptar')
      eidsdelta=thgetid('esdelta')
      eidstimeatfp=thgetid('estimeatfp')
      eidsp=thgetid('esp')
      eidsenergy=thgetid('esenergy')
      eidsxdc1layer1=thgetid('esxdc1layer1')
      eidsydc1layer1=thgetid('esydc1layer1')
      eidsxs1=thgetid('esxs1')
      eidsys1=thgetid('esys1')
      eidsxs2=thgetid('esxs2')
      eidsys2=thgetid('esys2')
      eidspathlength=thgetid('espathlength')
      eidstimeattar=thgetid('estimeattar')
      eidsrfdiff=thgetid('esrfdiff')
      eidsrftime=thgetid('esrftime')
      eidschi2perdeg=thgetid('eschi2perdeg')
      eidseloss=thgetid('eseloss')
      eidscorre=thgetid('escorre')
      eidscorrp=thgetid('escorrp')
      eidstheta=thgetid('estheta')
      eidskpvec1=thgetid('eskpvec1')
      eidskpvec2=thgetid('eskpvec2')
      eidskpvec3=thgetid('eskpvec3')
      eidskpvec4=thgetid('eskpvec4')
      eidsphi=thgetid('esphi')
      eidszbeam=thgetid('eszbeam')


c
c     ETUL
c
      eidtulrawtothits=thgetid('etulrawtothits')
      eidtulrawtotchannelnum=thgetid('etulrawtotchannelnum')
      eidtulrawtdc=thgetid('etulrawtdc')
      eidtultothits=thgetid('etultothits')
      eidtultotchannelnum=thgetid('etultotchannelnum')

CCC DK from here
      Do i=1,emax_num_dc1_layers+emax_num_dc2_layers
         Call HBOOK2(5000+i,'time vs drift dis(abs)',160,-1.,1.,
     &        180,-40.,140.,0.)
         Call HBOOK2(5100+i,'time vs drift dis',160,-1.,1.,
     &        180,-40.,140.,0.)
         Call HBOOK2(5120+i,'time vs drift dis(-)',160,-1.,1.,
     &        180,-40.,140.,0.)
         Call HBOOK2(5140+i,'time vs drift dis(+)',160,-1.,1.,
     &        180,-40.,140.,0.)
         Call HBOOK1(5200+i,'layer hits',2,0.,2.,0.)
         Call HBOOK1(5300+i,'layer distribution',200,-100.,100.,0.)
         Call HBOOK1(5400+i,'drift distance',200,-100.,100.,0.)
      EndDo
      Do i=1,emax_num_dc1_layers
         Call HBOOK1(5500+i,'dx',500,-0.5,0.5,0.)
         Call HBOOK1(5600+i,'dy',500,-0.5,0.5,0.)
         Call HBOOK1(5700+i,'ds',500,-0.5,0.5,0.)
c         Call HBOOK2(5720+i,'ds',200,-60.,60.,200,-0.5,0.5,0.)
c         Call HBOOK2(5720+i,'ds',500,-60.,60.,500,-0.5,0.5,0.)

         Call HBOOK2(5720+i,'ds',200,-10.,10.,200,-0.1,0.1,0.)
c         Call HBOOK2(5740+i,'dtvsx',200,-60.,60.,200,-50.,200.,0.)

      EndDo
      Do i=emax_num_dc1_layers+1,emax_num_dc1_layers+emax_num_dc2_layers
         Call HBOOK1(5500+i,'dx',200,-1.,1.,0.)
         Call HBOOK1(5600+i,'dy',200,-1.,1.,0.)
         Call HBOOK1(5700+i,'ds',500,-0.5,0.5,0.)

c         Call HBOOK2(5720+i,'ds',200,-60.,60.,200,-0.5,0.5,0.)

         Call HBOOK2(5720+i,'ds',200,-.5,.5,200,-0.1,0.1,0.)
c         Call HBOOK2(5740+i,'dtvsx',200,-60.,60.,200,-50.,200.,0.)

         !Call HBOOK1(5700+i,'ds',500,-0.5,0.5,0.)
      EndDo


      Return
      End

