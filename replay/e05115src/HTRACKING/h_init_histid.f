      subroutine h_init_histid(Abort,err)
*     
*     routine to get HBOOK histogram ID numbers for:
*     raw/decorded histogram for AC,WC
*     raw HTF1x,1y,2x
*     raw HDC
*     tracking results
*     TOF results
*     
*     skip TOF and tracking histogram
*     
*     Author:	D. F. Geesaman
*     Date:      9 April 1994     
*     Revision 1.1.1.1  2009/08/17 22:17:30  yez
*     Add Lucite sum channels
*
*     Revision 1.1.1.1  2009/07/11 13:36:04  yez
*     Add Lucite info
*
*     $Log: h_init_histid.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.10  2005/07/08 13:46:46  sumihama
*     Mod. AC hist
*
*     Revision 1.7  2005/07/03 04:18:11  sumihama
*     Mod. hbook sor s.s
*
*     Revision 1.6  2005/06/14 04:31:27  cdaq
*     Mod. rf histgram
*
*     Revision 1.5  2005/06/10 19:01:09  cdaq
*     add tul ana
*
*     Revision 1.4  2005/05/30 22:28:39  miyoshi
*     add/delete histogram
*
*     Revision 1.3  2005/05/30 17:40:17  miyoshi
*     change hist name for ac and wc
*
*     Revision 1.2  2005/05/28 00:42:20  cdaq
*     change f1trigtime hist
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.14  2005/04/19 17:50:45  miyoshi
*     add new hist
*
*     Revision 1.13  2005/04/08 20:46:14  miyoshi
*     add histogram
*
*     Revision 1.12  2005/03/14 19:52:17  miyoshi
*     change tracking variables and histogram names
*
*     Revision 1.11  2005/03/08 14:56:56  miyoshi
*     add link stub debug hist
*
*     Revision 1.10  2005/03/07 16:34:37  miyoshi
*     add new hist for hdc
*
*     Revision 1.9  2005/03/02 22:30:46  miyoshi
*     add new hist for hdc
*
*     Revision 1.8  2005/03/02 17:26:30  miyoshi
*     add definition
*
*     Revision 1.7  2005/03/01 23:52:31  miyoshi
*     add hdc raw hist
*
*     Revision 1.6  2005/02/11 18:53:12  miyoshi
*     clean up used/unused hist
*
*     Revision 1.5  2004/12/24 21:39:09  miyoshi
*     change common block
*
*     Revision 1.4  2004/12/24 19:50:13  miyoshi
*     change minor
*
*     Revision 1.3  2004/12/23 23:37:28  sumihama
*     small bug
*
*     Revision 1.2  2004/12/23 19:38:01  sumihama
*     Add and delete Hbooks, add h_fill_scin_dec_hist.f
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 2.1 2004/04 Miyoshi
*     for E01-011
*     
*     -
*--------------------------------------------------------
      IMPLICIT NONE
*     
      character*13 here
      parameter (here= 'h_init_histid')
*     
      logical ABORT
      character*(*) err
      external thgetid
      integer*4 thgetid,i
      integer*4 layer,counter,la,co
*     
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'hks_scin_parms.cmn'
      include 'hks_id_histid.cmn'
      include 'hks_phys_histid.cmn'
      Include 'gen_f1tdc.cmn'
      
      character*32 histname
      character*8  wiremap
      character*6 drifttime
      character*9 driftdis
      character*9 wirecent
      character*9 residual
      character*9 singres
      character*6 postdc,negtdc,posadc,negadc
      character*9 rawposadc,rawnegadc
      character*6 hdclayername(hmax_num_dc_layers)
      character*1 hscinlayernum(HNUM_SCIN_LAYERS)
      character*10 hscinlayer
      character*10 hscinrawlayer
      character*10 hscinrawpatpos
      character*10 hscinrawpatneg
      character*10 hscinrawpatboth
      character*7 hposadc,hnegadc,hpostdc,hnegtdc,postime,negtime
      character*7 hscinlayername(HNUM_SCIN_LAYERS)
      character*5 hwatname(HNUM_WAT_LAYERS)

      data wiremap/'_wiremap'/       
      data drifttime/'dtime_'/
      data driftdis /'_driftdis'/
      data wirecent/'_wirecent'/
      data residual/'_residual'/
      data singres/'_sing_res'/
      data rawposadc /'rawposadc'/
      data rawnegadc /'rawnegadc'/
      data posadc /'posadc'/
      data negadc /'negadc'/
      data postdc /'postdc'/
      data negtdc /'negtdc'/
      data postime /'postime'/
      data negtime /'negtime'/
      data hdclayername/'hdc1u1','hdc1u2','hdc1x1',
     &     'hdc1x2','hdc1v1'
     $     ,'hdc1v2','hdc2u1','hdc2u2','hdc2x1',
     &     'hdc2x2','hdc2v1','hdc2v2'/
      data hscinlayernum/'1','2','3','4'/
      data hscinlayer /'hscinlayer'/
      data hscinrawlayer /'hscinrawlayer'/
      data hscinrawpatpos /'hscinrawpatpos'/
      data hscinrawpatneg /'hscinrawpatneg'/
      data hscinrawpatboth /'hscinrawpatboth'/
      data hposadc /'hposadc'/
      data hnegadc /'hnegadc'/
      data hpostdc /'hpostdc'/
      data hnegtdc /'hnegtdc'/
      data hscinlayername/'hscin1x','hscin1y','hscin2x','hscin1y'/
*     
      SAVE
*--------------------------------------------------------
*     
      ABORT= .FALSE.
      err= ' '

      
****************************************
*     HDC
****************************************
*     h_fill_dc_raw_hist
      hiddcrawtothit = thgetid('hdcrawtothit')
      hiddcrawtothitzoom = thgetid('hdcrawtothitzoom')
      hiddcrawtdcall = thgetid('hdcrawtdcall')


      hidstarttime = thgetid('hstarttime')

      Do i=1,HMAX_NUM_DC_LAYERS
         if(i .le. 9) then
            Write(histname,'(13Hhdcrawhitpat0,I1)') i
            hiddcrawhitpat(i) = thgetid(histname)
         Else
            Write(histname,'(12Hhdcrawhitpat,I2)') i
            hiddcrawhitpat(i) = thgetid(histname)
         EndIf
      EndDo

      Do i=1,HMAX_NUM_DC_LAYERS
         if(i .le. 9) then
            Write(histname,'(15Hhdcrawlayertdc0,I1)') i
            hiddcrawlayertdc(i) = thgetid(histname)
         Else
            Write(histname,'(14Hhdcrawlayertdc,I2)') i
            hiddcrawlayertdc(i) = thgetid(histname)
         EndIf
      EndDo

      if(hturnon_dc_raw_hist .eq. 1) then
         Do i=1,HMAX_NUM_DC_SLOTS
            if(i .le. 9) then
               Write(histname,'(10Hhdcrawtdc0,I1)') i
               hiddcrawtdc(i) = thgetid(histname)
            Else
               Write(histname,'(9Hhdcrawtdc,I2)') i
               hiddcrawtdc(i) = thgetid(histname)
            EndIf
         EndDo
      EndIf
      
*     h_fill_dc_dec_hist
      hiddcdectothit = thgetid('hdcdectothit')
      hiddcdectothitzoom = thgetid('hdcdectothitzoom')
      hiddcdectdcall = thgetid('hdcdectdcall')

      Do i=1,HMAX_NUM_DC_LAYERS
         if(i .le. 9) then
            Write(histname,'(14Hhdcwirecenter0,I1)') i
            hiddcwirecenter(i) = thgetid(histname)
         Else
            Write(histname,'(13Hhdcwirecenter,I2)') i
            hiddcwirecenter(i) = thgetid(histname)
         EndIf
      EndDo

      if(hturnon_dc_dec_hist .eq. 1) then
         Do i=1,HMAX_NUM_DC_SLOTS
            if(i .le. 9) then
               Write(histname,'(15Hhdcdrifttimesl0,I1)') i
               hiddcdrifttimesl(i) = thgetid(histname)
            Else
               Write(histname,'(14Hhdcdrifttimesl,I2)') i
               hiddcdrifttimesl(i) = thgetid(histname)
            EndIf
         EndDo
      EndIf

c     --- drift time per layer
      do layer = 1, hdc_num_layers
         histname = drifttime//hdclayername(layer)
         hiddcdrifttime(layer) = thgetid(histname)
      enddo        
      
      Do i=1,HMAX_NUM_DC_LAYERS
         if(i .le. 9) then
            Write(histname,'(15Hhdcrawlayerhit0,I1)') i
            hiddcrawlayerhit(i) = thgetid(histname)
         Else
            Write(histname,'(14Hhdcrawlayerhit,I2)') i
            hiddcrawlayerhit(i) = thgetid(histname)
         EndIf
         if(i .le. 9) then
            Write(histname,'(12Hhdclayerhit0,I1)') i
            hiddclayerhit(i) = thgetid(histname)
         Else
            Write(histname,'(11Hhdclayerhit,I2)') i
            hiddclayerhit(i) = thgetid(histname)
         EndIf
      EndDo

*     debug
      hiddcearlymult = thgetid('hdcearlymult')
      hiddclatemult = thgetid('hdclatemult')
      hiddcextramult = thgetid('hdcextramult')
      
      Do i=1,HMAX_NUM_DC_LAYERS
         if(i .le. 9) then
            Write(histname,'(11Hhdccluster0,I1)') i
            hiddccluster(i) = thgetid(histname)
         Else
            Write(histname,'(10Hhdccluster,I2)') i
            hiddccluster(i) = thgetid(histname)
         EndIf
      EndDo

*     h_fill_dc_sp_hist
      hiddcnspacepoint=thgetid('hdcnspacepoint')
      hiddcnspacepointzoom=thgetid('hdcnspacepointzoom')
      hiddcnspacepointhits=thgetid('hdcnspacepointhits')

*     at h_find_space_points.f, defined in hist.HKS_decdebug
      hidsqdistance = thgetid('sqdistance')
      
*     at the end of h_left_right
*     --- fill hdc drift distance after solving left right.
      do layer = 1, hdc_num_layers
         histname = hdclayername(layer)//driftdis
         hiddcdriftdis(layer) = thgetid(histname)
      enddo 
*     in h_link_stub.f
      hidxtcriterion = thgetid('xtcriterion')
      hidytcriterion = thgetid('ytcriterion')
      hidxptcriterion = thgetid('xptcriterion')
      hidyptcriterion = thgetid('yptcriterion')

*     in the beginning of h_track_fit.f
      hidhntracksfp = thgetid('hntracksfp')
      hidhntracksfpzoom = thgetid('hntracksfpzoom')
      
*     h_fill_dc_track_hist
      Do i=1,hmax_num_dc_layers
         if(i .le. 9) then
            Write(histname,'(18Hhdcsingleresidual0,i1)') i
            hidresidualpre(i) = thgetid(histname)
         Else
            Write(histname,'(17Hhdcsingleresidual,i2)') i
            hidresidualpre(i) = thgetid(histname)
         EndIf
      EndDo
      Do i=1,hmax_num_dc_layers
         if(i .le. 9) then
            Write(histname,'(11Hhdcdistime0,i1)') i
            hiddcdistime(i) = thgetid(histname)
         Else
            Write(histname,'(10Hhdcdistime,i2)') i
            hiddcdistime(i) = thgetid(histname)
         EndIf
      EndDo
      
      hidchi2perdoffp = thgetid('hchi2perdoffp')
      hidchi2perdoffpzoom = thgetid('hchi2perdoffpzoom')
      hidhxfp = thgetid('hxfp')
      hidhyfp = thgetid('hyfp')
      hidhxpfp = thgetid('hxpfp')
      hidhypfp = thgetid('hypfp')
      hidfpxy = thgetid('hfpxy')
      hidfpxpyp = thgetid('hfpxpyp')
      hidfpxyp = thgetid('hfpxyp')
      hidfpyyp = thgetid('hfpyyp')
      hidfpxxp = thgetid('hfpxxp')
      hidfpyxp = thgetid('hfpyxp')

*     h_select_good_tracks.f
      hidtrkdx = thgetid('htrkdx')
      hidtrkdy = thgetid('htrkdy')
      hidtrkdxp = thgetid('htrkdxp')
      hidtrkdyp = thgetid('htrkdyp')
      hidtrkdiff = thgetid('htrkdiff')
      Do i=1,hmax_num_dc_layers
         if(i .le. 9) then
            Write(histname,'(14Hhsingresfinal0,i1)') i
            hidsingresidual(i) = thgetid(histname)
         Else
            Write(histname,'(13Hhsingresfinal,i2)') i
            hidsingresidual(i) = thgetid(histname)
         EndIf
      EndDo

*---  Histogram block hks_target
      hidhx_tar = thgetid('hxtar')
      hidhy_tar = thgetid('hytar')
      hidhxp_tar = thgetid('hxptar')
      hidhyp_tar = thgetid('hyptar')
      hidtarxpyp = thgetid('htarxpyp')
ccc      hidhz_tar = thgetid('hztar')
      hidhdelta_tar = thgetid('hdeltatar')
      hidhp_tar = thgetid('hptar')
cccc      hidhpwide = thgetid('hpwide')
c  Sieve slit
      hidxsv = thgetid('hxsv')
      hidysv = thgetid('hysv')
      hidxysv = thgetid('hsvxy')

c     do layer = 1, hdc_num_layers
c     histname = hdclayername(layer)//residual
c     hidres_fp(layer) = thgetid(histname)
c     histname = hdclayername(layer)//singres
c     hidsingres_fp(layer) = thgetid(histname)
c     enddo                     ! end loop over dc layers 

*************************************
*     Hhodo
*************************************
      
*     h_fill_scin_raw_hist.f
*     defined in hist.HKS_raw
      hidscinrawtothits = thgetid('hscinrawtothits')
      hidscinrawlayer = thgetid('hscinrawlayer')
      do layer = 1, HNUM_SCIN_LAYERS
         Write(histname,'(16Hhscinrawcounters,i1)') layer
         hidscinrawcounters(layer) = thgetid(histname)
      EndDo

c     --- F1 trigger time hist is here for ROC14
      gidf1trigtimehist(2,1,1) = thgetid('gf1trigtime4-1')

*---  HTF counter raw hit pattern

      do layer = 1, HNUM_SCIN_LAYERS
         histname = "hscinrawadchitpatpos"//hscinlayernum(layer)
         hidscinrawadchitpatpos(layer) = thgetid(histname)
         histname = "hscinrawadchitpatneg"//hscinlayernum(layer)
         hidscinrawadchitpatneg(layer) = thgetid(histname)
         histname = "hscinrawtdchitpatpos"//hscinlayernum(layer)
         hidscinrawtdchitpatpos(layer) = thgetid(histname)
         histname = "hscinrawtdchitpatneg"//hscinlayernum(layer)
         hidscinrawtdchitpatneg(layer) = thgetid(histname)
      EndDo

      do layer = 1, HNUM_SCIN_LAYERS
         histname = "hscinsumpostdc"//hscinlayernum(layer)
         hidscinsumpostdc(layer) = thgetid(histname)
         histname = "hscinsumnegtdc"//hscinlayernum(layer)
         hidscinsumnegtdc(layer) = thgetid(histname)
         histname = "hscinsumposadc"//hscinlayernum(layer)
         hidscinsumposadc(layer) = thgetid(histname)
         histname = "hscinsumnegadc"//hscinlayernum(layer)
         hidscinsumnegadc(layer) = thgetid(histname)
      EndDo
      hnum_scin_counters(1) = hscin_1x_nr
      hnum_scin_counters(2) = hscin_1y_nr
      hnum_scin_counters(3) = hscin_2x_nr
      do layer = 1, HNUM_SCIN_LAYERS
c     --- define in hist.HKS_hodoraw
         if(hturnon_scin_raw_hist .ne. 0) then
            do counter = 1,hnum_scin_counters(layer)
*     this is probably very awkward character manipulation

               if(counter.lt.10) then
                  write(histname,'(14Hhscinrawposped,i1,2H_0,i1)')
     &               layer,counter
*                  write(histname,'(a7,i1,a9)') 
*     &                 hscinlayername(layer),counter,rawposped
               else
                  write(histname,'(14Hhscinrawposped,i1,1H_,i2)')
     &               layer,counter
*                  write(histname,'(a7,i2,a9)') 
*     &                 hscinlayername(layer),counter,rawposped
               endif
               hidscinrawposped(layer,counter) = thgetid(histname)

               if(counter.lt.10) then
                  write(histname,'(14Hhscinrawnegped,i1,2H_0,i1)')
     &               layer,counter
*                  write(histname,'(a7,i1,a9)') 
*     &                 hscinlayername(layer),counter,rawnegped
               else
                  write(histname,'(14Hhscinrawnegped,i1,1H_,i2)')
     &               layer,counter
*                  write(histname,'(a7,i2,a9)') 
*     &                 hscinlayername(layer),counter,rawnegped
               endif
               hidscinrawnegped(layer,counter) = thgetid(histname)



               if(counter.lt.10) then
                  write(histname,'(14Hhscinrawposadc,i1,2H_0,i1)')
     &               layer,counter
*                  write(histname,'(a7,i1,a9)') 
*     &                 hscinlayername(layer),counter,rawposadc
               else
                  write(histname,'(14Hhscinrawposadc,i1,1H_,i2)')
     &               layer,counter
*                  write(histname,'(a7,i2,a9)') 
*     &                 hscinlayername(layer),counter,rawposadc
               endif
               hidscinrawposadc(layer,counter) = thgetid(histname)

               if(counter.lt.10) then
                  write(histname,'(14Hhscinrawnegadc,i1,2H_0,i1)')
     &               layer,counter
*                  write(histname,'(a7,i1,a9)') 
*     &                 hscinlayername(layer),counter,rawnegadc
               else
                  write(histname,'(14Hhscinrawnegadc,i1,1H_,i2)')
     &               layer,counter
*                  write(histname,'(a7,i2,a9)') 
*     &                 hscinlayername(layer),counter,rawnegadc
               endif
               hidscinrawnegadc(layer,counter) = thgetid(histname)

               if(counter.lt.10) then
                  write(histname,'(14Hhscinrawpostdc,i1,2H_0,i1)')
     &               layer,counter
*                  write(histname,'(a7,i1,a6)') 
*     &                 hscinlayername(layer),counter,postdc
               else
                  write(histname,'(14Hhscinrawpostdc,i1,1H_,i2)')
     &               layer,counter
*                  write(histname,'(a7,i2,a6)') 
*     &                 hscinlayername(layer),counter,postdc
               endif
               hidscinrawpostdc(layer,counter) = thgetid(histname)
               if(counter.lt.10) then
                  write(histname,'(14Hhscinrawnegtdc,i1,2H_0,i1)')
     &               layer,counter
*                  write(histname,'(a7,i1,a6)') 
*     &                 hscinlayername(layer),counter,negtdc
               else
                  write(histname,'(14Hhscinrawnegtdc,i1,1H_,i2)')
     &               layer,counter
*                  write(histname,'(a7,i2,a6)') 
*     &                 hscinlayername(layer),counter,negtdc
               endif
               hidscinrawnegtdc(layer,counter) = thgetid(histname)     
            enddo               ! end loop over scintillator counters
         endIf
      enddo                     ! end loop over scintillator layer 

*     h_fill_scin_dec_hist
      hidscindectothits = thgetid('hscindectothits')
      
*---  HTF counter dec hit pattern
      do layer = 1, HNUM_SCIN_LAYERS
         write(histname,'(14Hhscinpattdcpos,i1)') layer
         hidscinpattdcpos(layer) = thgetid(histname)
         write(histname,'(14Hhscinpattdcneg,i1)') layer
         hidscinpattdcneg(layer) = thgetid(histname)
         write(histname,'(14Hhscinpatadcpos,i1)') layer
         hidscinpatadcpos(layer) = thgetid(histname)
         write(histname,'(14Hhscinpatadcneg,i1)') layer
         hidscinpatadcneg(layer) = thgetid(histname)
         write(histname,'(15Hhscinpattdcboth,i1)') layer
         hidscinpattdcboth(layer) = thgetid(histname)
         write(histname,'(15Hhscinpatadcboth,i1)') layer
         hidscinpatadcboth(layer) = thgetid(histname)
      EndDo
      
      hidscinlayer = thgetid('hscinlayer')
      
      do layer = 1, HNUM_SCIN_LAYERS
         Write(histname,'(13Hhscincounters,i1)') layer
         hidscincounters(layer) = thgetid(histname)
         Write(histname,'(17Hhscindeclayerhits,i1)') layer
         hidscindeclayerhits(layer) = thgetid(histname)
      EndDo
      
      do layer = 1, HNUM_SCIN_LAYERS
         if(hturnon_scin_dec_hist .ne. 0) then
*     this is defined in hist.HKS_hododec
            do counter = 1,hnum_scin_counters(layer)
               if(counter.lt.10) then
                  write(histname,'(14Hhscindecposadc,i1,2H_0,i1)')
     &               layer,counter
               else
                  write(histname,'(14Hhscindecposadc,i1,1H_,i2)')
     &               layer,counter
               endif
               hidscinposadc(layer,counter) = thgetid(histname)
               if(counter.lt.10) then
                  write(histname,'(14Hhscindecnegadc,i1,2H_0,i1)')
     &               layer,counter
               else
                  write(histname,'(14Hhscindecnegadc,i1,1H_,i2)')
     &               layer,counter
               endif
               hidscinnegadc(layer,counter) = thgetid(histname)
               if(counter.lt.10) then
                  write(histname,'(14Hhscindecpostdc,i1,2H_0,i1)')
     &               layer,counter
               else
                  write(histname,'(14Hhscindecpostdc,i1,1H_,i2)')
     &               layer,counter
               endif
               hidscinpostime(layer,counter) = thgetid(histname)
               if(counter.lt.10) then
                  write(histname,'(14Hhscindecnegtdc,i1,2H_0,i1)')
     &               layer,counter
               else
                  write(histname,'(14Hhscindecnegtdc,i1,1H_,i2)')
     &               layer,counter
               endif
               hidscinnegtime(layer,counter) = thgetid(histname)     
            enddo               ! end loop over scintillator counters
         endIf                  ! turn on dec hist
      enddo                     ! end loop over scintillator layer 
      
      hidscinalltime = thgetid('hscinalltime')
      hidscinfptime = thgetid('hscinfptime')

*     --- h_tof.f

      hidtrktof = thgetid('htrktof')
      hidtrkleng = thgetid('htrkleng')
      hidtrkp = thgetid('htrkp')
      hidtrkbetap = thgetid('htrkbetap')
      hidtrkbetaall = thgetid('htrkbetaall')
      hidtrkbeta(1) = thgetid('htrkbeta1')
      hidtrkbeta(2) = thgetid('htrkbeta2')
      hidtrkdepo = thgetid('htrkdepo')
      hidtrkid = thgetid('htrkid')
      hidtrkdeltabeta = thgetid('htrkdeltabeta')
      hidscintrktime(1) = thgetid('hscintrktime1')
      hidscintrktime(2) = thgetid('hscintrktime2')
      hidscintrktime(3) = thgetid('hscintrktime3')
      hidscintrkfptime(1) = thgetid('hscintrkfptime1')
      hidscintrkfptime(2) = thgetid('hscintrkfptime2')
      hidscintrkfptime(3) = thgetid('hscintrkfptime3')
      hidtrktimeatfpall = thgetid('htrktimeatfpall')
      hidtrktimeatfp(1) = thgetid('htrktimeatfp1')
      hidtrktimeatfp(2) = thgetid('htrktimeatfp2')

*---  ???
      hiddcdposx = thgetid('hdcdposx')
      hiddcdposy = thgetid('hdcdposy')
      hiddcdposxp = thgetid('hdcdposxp')
      hiddcdposyp = thgetid('hdcdposyp') 
      
      do layer = 1, HNUM_SCIN_LAYERS
         histname = "hscindpos"//hscinlayernum(layer)
         hidscindpos(layer) = thgetid(histname)
         histname = "hscindpos_pid"//hscinlayernum(layer)
         hidscindpos_pid(layer) = thgetid(histname)
      EndDo
*     
*     HKS Aerogel histogram
*     skip ADC spectra and add npe spectra to save memory.
*     
*     --- h_fill_ac_raw_hist.f
      
      hidaerrawtothits =thgetid('haerrawtothits')
      hidaerrawlayer = thgetid('haerrawlayer')
      Do la=1,HNUM_AER_LAYERS
         write(histname,'(13Hhaerrawhitpat,i1)') la
         hidaerrawhitpat(la) = thgetid(histname)
      EndDo
      do layer = 1, HNUM_AER_LAYERS
         histname = "haerrawadchitpatpos"//hscinlayernum(layer)
         hidaerrawadchitpatpos(layer) = thgetid(histname)
         histname = "haerrawadchitpatneg"//hscinlayernum(layer)
         hidaerrawadchitpatneg(layer) = thgetid(histname)
         histname = "haerrawtdchitpatpos"//hscinlayernum(layer)
         hidaerrawtdchitpatpos(layer) = thgetid(histname)
         histname = "haerrawtdchitpatneg"//hscinlayernum(layer)
         hidaerrawtdchitpatneg(layer) = thgetid(histname)
      EndDo

*---  HAC pos&neg raw ADC
      hidaersumposadc(1) = thgetid('haersumposadc1')
      hidaersumposadc(2) = thgetid('haersumposadc2')
      hidaersumposadc(3) = thgetid('haersumposadc3')
      hidaersumnegadc(1) = thgetid('haersumnegadc1')
      hidaersumnegadc(2) = thgetid('haersumnegadc2')
      hidaersumnegadc(3) = thgetid('haersumnegadc3')
*---  HAC pos&neg raw TDC
      hidaersumpostdc(1) = thgetid('haersumpostdc1')
      hidaersumpostdc(2) = thgetid('haersumpostdc2')
      hidaersumpostdc(3) = thgetid('haersumpostdc3')
      hidaersumnegtdc(1) = thgetid('haersumnegtdc1')
      hidaersumnegtdc(2) = thgetid('haersumnegtdc2')
      hidaersumnegtdc(3) = thgetid('haersumnegtdc3')
      
      if(hturnon_ac_raw_hist .eq. 1) then
         Do la=1,HNUM_AER_LAYERS
            Do co=1,HNUM_AER_COUNTERS
               write(histname,'(13Hhaerrawposped,i1,1H_,i1)') la,co
               hidaerrawposped(la,co) = thgetid(histname)
            EndDo
            Do co=1,HNUM_AER_COUNTERS
               write(histname,'(13Hhaerrawnegped,i1,1H_,i1)') la,co
               hidaerrawnegped(la,co) = thgetid(histname)
            EndDo
         EndDo
         Do la=1,HNUM_AER_LAYERS
            Do co=1,HNUM_AER_COUNTERS
               write(histname,'(13Hhaerrawposadc,i1,1H_,i1)') la,co
               hidaerrawposadc(la,co) = thgetid(histname)
            EndDo
            Do co=1,HNUM_AER_COUNTERS
               write(histname,'(13Hhaerrawnegadc,i1,1H_,i1)') la,co
               hidaerrawnegadc(la,co) = thgetid(histname)
            EndDo
         EndDo
         Do la=1,HNUM_AER_LAYERS
            Do co=1,HNUM_AER_COUNTERS
               write(histname,'(13Hhaerrawpostdc,i1,1H_,i1)') la,co
               hidaerrawpostdc(la,co) = thgetid(histname)
            EndDo
            Do co=1,HNUM_AER_COUNTERS
               write(histname,'(13Hhaerrawnegtdc,i1,1H_,i1)') la,co
               hidaerrawnegtdc(la,co) = thgetid(histname)
            EndDo
         EndDo
      EndIF
      
c     --- h_fill_aero_hist.f
      
      hidaertothits = thgetid("haertothits")
      hidaerlayer = thgetid("haerlayer")
      Do la=1,HNUM_AER_LAYERS
         write(histname,'(10Hhaerhitpat,i1)') la
         hidaerhitpat(la) = thgetid(histname)
      EndDo
      Do la=1,HNUM_AER_LAYERS
         write(histname,'(13Hhaerlayerhits,i1)') la
         hidaerlayerhits(la) = thgetid(histname)
      EndDo


      Do la=1,HNUM_AER_LAYERS
         Do co=1,HNUM_AER_COUNTERS
            write(histname,'(10Hhaerposnpe,i1,1H_,i1)') la,co
            hidaerposnpe(la,co) = thgetid(histname)
         EndDo
         Do co=1,HNUM_AER_COUNTERS
            write(histname,'(10Hhaernegnpe,i1,1H_,i1)') la,co
            hidaernegnpe(la,co) = thgetid(histname)
         EndDo
         Do co=1,HNUM_AER_COUNTERS
            write(histname,'(10Hhaersumnpe,i1,1H_,i1)') la,co
            hidaersumnpe(la,co) = thgetid(histname)
         EndDo
      EndDo
      
      Do la=1,HNUM_AER_LAYERS
         Do co=1,HNUM_AER_COUNTERS
            write(histname,'(11Hhaerpostime,i1,1H_,i1)') la,co
            hidaerpostime(la,co) = thgetid(histname)
         EndDo
         Do co=1,HNUM_AER_COUNTERS
            write(histname,'(11Hhaernegtime,i1,1H_,i1)') la,co
            hidaernegtime(la,co) = thgetid(histname)
         EndDo
      EndDo
      
      Do la=1,HNUM_AER_LAYERS
         write(histname,'(10Hhaernpesum,i1)') la
         hidaernpesum(la) = thgetid(histname)
      EndDo     
      
*     
*     HKS Water histogram
*     skip ADC spectra and add npe spectra to save memory
*     
      hidwatrawtothits =thgetid('hwatrawtothits')
      hidwatrawlayer = thgetid('hwatrawlayer')
      Do la=1,HNUM_WAT_LAYERS
         write(histname,'(13Hhwatrawhitpat,i1)') la
         hidwatrawhitpat(la) = thgetid(histname)
      EndDo

      do layer = 1, HNUM_WAT_LAYERS
         histname = "hwatrawadchitpatpos"//hscinlayernum(layer)
         hidwatrawadchitpatpos(layer) = thgetid(histname)
         histname = "hwatrawadchitpatneg"//hscinlayernum(layer)
         hidwatrawadchitpatneg(layer) = thgetid(histname)
         histname = "hwatrawtdchitpatpos"//hscinlayernum(layer)
         hidwatrawtdchitpatpos(layer) = thgetid(histname)
         histname = "hwatrawtdchitpatneg"//hscinlayernum(layer)
         hidwatrawtdchitpatneg(layer) = thgetid(histname)
      EndDo

*---  HWC pos&neg raw ADC
      hidwatsumposadc(1) = thgetid('hwatsumposadc1')
      hidwatsumposadc(2) = thgetid('hwatsumposadc2')
      hidwatsumnegadc(1) = thgetid('hwatsumnegadc1')
      hidwatsumnegadc(2) = thgetid('hwatsumnegadc2')
*---  HWC pos&neg raw TDC
      hidwatsumpostdc(1) = thgetid('hwatsumpostdc1')
      hidwatsumpostdc(2) = thgetid('hwatsumpostdc2')
      hidwatsumnegtdc(1) = thgetid('hwatsumnegtdc1')
      hidwatsumnegtdc(2) = thgetid('hwatsumnegtdc2')

      if(hturnon_wc_raw_hist .eq. 1) then
         Do la=1,HNUM_WAT_LAYERS
            Do co=1,HNUM_WAT_COUNTERS
               if(co .lt. 10) then
                  write(histname,'(13Hhwatrawposped,i1,2H_0,i1)') la,co
                  hidwatrawposped(la,co) = thgetid(histname)
                  write(histname,'(13Hhwatrawnegped,i1,2H_0,i1)') la,co
                  hidwatrawnegped(la,co) = thgetid(histname)
               Else
                  write(histname,'(13Hhwatrawposped,i1,1H_,i2)') la,co
                  hidwatrawposped(la,co) = thgetid(histname)
                  write(histname,'(13Hhwatrawnegped,i1,1H_,i2)') la,co
                  hidwatrawnegped(la,co) = thgetid(histname)
               EndIF
            EndDo
         EndDo
         Do la=1,HNUM_WAT_LAYERS
            Do co=1,HNUM_WAT_COUNTERS
               if(co .lt. 10) then
                  write(histname,'(13Hhwatrawposadc,i1,2H_0,i1)') la,co
                  hidwatrawposadc(la,co) = thgetid(histname)
                  write(histname,'(13Hhwatrawnegadc,i1,2H_0,i1)') la,co
                  hidwatrawnegadc(la,co) = thgetid(histname)
               Else
                  write(histname,'(13Hhwatrawposadc,i1,1H_,i2)') la,co
                  hidwatrawposadc(la,co) = thgetid(histname)
                  write(histname,'(13Hhwatrawnegadc,i1,1H_,i2)') la,co
                  hidwatrawnegadc(la,co) = thgetid(histname)
               EndIF
            EndDo
         EndDo
         Do la=1,HNUM_WAT_LAYERS
            Do co=1,HNUM_WAT_COUNTERS
               if(co .lt. 10) then
                  write(histname,'(13Hhwatrawpostdc,i1,2H_0,i1)') la,co
                  hidwatrawpostdc(la,co) = thgetid(histname)
                  write(histname,'(13Hhwatrawnegtdc,i1,2H_0,i1)') la,co
                  hidwatrawnegtdc(la,co) = thgetid(histname)
               Else
                  write(histname,'(13Hhwatrawpostdc,i1,1H_,i2)') la,co
                  hidwatrawpostdc(la,co) = thgetid(histname)
                  write(histname,'(13Hhwatrawnegtdc,i1,1H_,i2)') la,co
                  hidwatrawnegtdc(la,co) = thgetid(histname)
               EndIF
            EndDo
         EndDo
      EndIF

c     --- h_fill_water_hist.f

      hidwattothits = thgetid("hwattothits")
      hidwatlayer = thgetid("hwatlayer")
      
      Do la=1,HNUM_WAT_LAYERS
         write(histname,'(10Hhwathitpat,i1)') la
         hidwathitpat(la) = thgetid(histname)
      EndDo

      Do la=1,HNUM_WAT_LAYERS
         write(histname,'(13Hhwatlayerhits,i1)') la
         hidwatlayerhits(la) = thgetid(histname)
      EndDo
      
      Do la=1,HNUM_WAT_LAYERS
         Do co=1,HNUM_WAT_COUNTERS
            if(co .lt. 10) then
               write(histname,'(10Hhwatposnpe,i1,2H_0,i1)') la,co
               hidwatposnpe(la,co) = thgetid(histname)
               write(histname,'(10Hhwatnegnpe,i1,2H_0,i1)') la,co
               hidwatnegnpe(la,co) = thgetid(histname)
            Else
               write(histname,'(10Hhwatposnpe,i1,1H_,i2)') la,co
               hidwatposnpe(la,co) = thgetid(histname)
               write(histname,'(10Hhwatnegnpe,i1,1H_,i2)') la,co
               hidwatnegnpe(la,co) = thgetid(histname)
            EndIF
         EndDo
      EndDo

      if(hturnon_wc_dec_hist .eq. 1) then
         Do la=1,HNUM_WAT_LAYERS
            Do co=1,HNUM_WAT_COUNTERS
               if(co .lt. 10) then
                  write(histname,'(11Hhwatpostime,i1,2H_0,i1)') la,co
                  hidwatpostime(la,co) = thgetid(histname)
                  write(histname,'(11Hhwatnegtime,i1,2H_0,i1)') la,co
                  hidwatnegtime(la,co) = thgetid(histname)
               Else
                  write(histname,'(11Hhwatpostime,i1,1H_,i2)') la,co
                  hidwatpostime(la,co) = thgetid(histname)
                  write(histname,'(11Hhwatnegtime,i1,1H_,i2)') la,co
                  hidwatnegtime(la,co) = thgetid(histname)   
               EndIF
            EndDo
         EndDo
      EndIf

      Do la=1,HNUM_WAT_LAYERS
         write(histname,'(10Hhwatnpesum,i1)') la
         hidwatnpesum(la) = thgetid(histname)
      EndDo 
    
*     
*     HKS Lucite histogram
*     skip ADC spectra and add npe spectra to save memory
*     
      hidlucrawtothits =thgetid('hlucrawtothits')
      hidlucrawlayer = thgetid('hlucrawlayer')
      Do la=1,HNUM_LUC_LAYERS
         write(histname,'(13Hhlucrawhitpat,i1)') la
         hidlucrawhitpat(la) = thgetid(histname)
      EndDo

      do layer = 1, HNUM_LUC_LAYERS
         histname = "hlucrawadchitpatpos"//hscinlayernum(layer)
         hidlucrawadchitpatpos(layer) = thgetid(histname)
         histname = "hlucrawadchitpatneg"//hscinlayernum(layer)
         hidlucrawadchitpatneg(layer) = thgetid(histname)
         histname = "hlucrawadchitpattot"//hscinlayernum(layer)
         hidlucrawadchitpattot(layer) = thgetid(histname)
         histname = "hlucrawtdchitpatpos"//hscinlayernum(layer)
         hidlucrawtdchitpatpos(layer) = thgetid(histname)
         histname = "hlucrawtdchitpatneg"//hscinlayernum(layer)
         hidlucrawtdchitpatneg(layer) = thgetid(histname)
         histname = "hlucrawtdchitpattot"//hscinlayernum(layer)
         hidlucrawtdchitpattot(layer) = thgetid(histname)
      EndDo

*---  HLC pos&neg raw ADC
      hidlucsumposadc(1) = thgetid('hlucsumposadc1')
      hidlucsumnegadc(1) = thgetid('hlucsumnegadc1')
      hidlucsumtotadc(1) = thgetid('hlucsumtotadc1')

*---  HLC pos&neg raw TDC
      hidlucsumpostdc(1) = thgetid('hlucsumpostdc1')
      hidlucsumnegtdc(1) = thgetid('hlucsumnegtdc1')
      hidlucsumtottdc(1) = thgetid('hlucsumtottdc1')


      if(hturnon_lc_raw_hist .eq. 1) then
         Do la=1,HNUM_LUC_LAYERS
            Do co=1,HNUM_LUC_COUNTERS
               if(co .lt. 10) then
                  write(histname,'(13Hhlucrawposped,i1,2H_0,i1)') la,co
                  hidlucrawposped(la,co) = thgetid(histname)
                  write(histname,'(13Hhlucrawnegped,i1,2H_0,i1)') la,co
                  hidlucrawnegped(la,co) = thgetid(histname)
                  write(histname,'(13Hhlucrawtotped,i1,2H_0,i1)') la,co
                  hidlucrawtotped(la,co) = thgetid(histname)
               Else
                  write(histname,'(13Hhlucrawposped,i1,1H_,i2)') la,co
                  hidlucrawposped(la,co) = thgetid(histname)
                  write(histname,'(13Hhlucrawnegped,i1,1H_,i2)') la,co
                  hidlucrawnegped(la,co) = thgetid(histname)
                  write(histname,'(13Hhlucrawtotped,i1,1H_,i2)') la,co
                  hidlucrawtotped(la,co) = thgetid(histname)
               EndIF
            EndDo
         EndDo
         Do la=1,HNUM_LUC_LAYERS
            Do co=1,HNUM_LUC_COUNTERS
               if(co .lt. 10) then
                  write(histname,'(13Hhlucrawposadc,i1,2H_0,i1)') la,co
                  hidlucrawposadc(la,co) = thgetid(histname)
                  write(histname,'(13Hhlucrawnegadc,i1,2H_0,i1)') la,co
                  hidlucrawnegadc(la,co) = thgetid(histname)
                  write(histname,'(13Hhlucrawtotadc,i1,2H_0,i1)') la,co
                  hidlucrawtotadc(la,co) = thgetid(histname)
               Else
                  write(histname,'(13Hhlucrawposadc,i1,1H_,i2)') la,co
                  hidlucrawposadc(la,co) = thgetid(histname)
                  write(histname,'(13Hhlucrawnegadc,i1,1H_,i2)') la,co
                  hidlucrawnegadc(la,co) = thgetid(histname)
                  write(histname,'(13Hhlucrawtotadc,i1,1H_,i2)') la,co
                  hidlucrawtotadc(la,co) = thgetid(histname)
               EndIF
            EndDo
         EndDo
         Do la=1,HNUM_LUC_LAYERS
            Do co=1,HNUM_LUC_COUNTERS
               if(co .lt. 10) then
                  write(histname,'(13Hhlucrawpostdc,i1,2H_0,i1)') la,co
                  hidlucrawpostdc(la,co) = thgetid(histname)
                  write(histname,'(13Hhlucrawnegtdc,i1,2H_0,i1)') la,co
                  hidlucrawnegtdc(la,co) = thgetid(histname)
                  write(histname,'(13Hhlucrawtottdc,i1,2H_0,i1)') la,co
                  hidlucrawtottdc(la,co) = thgetid(histname)
               Else
                  write(histname,'(13Hhlucrawpostdc,i1,1H_,i2)') la,co
                  hidlucrawpostdc(la,co) = thgetid(histname)
                  write(histname,'(13Hhlucrawnegtdc,i1,1H_,i2)') la,co
                  hidlucrawnegtdc(la,co) = thgetid(histname)
                  write(histname,'(13Hhlucrawtottdc,i1,1H_,i2)') la,co
                  hidlucrawtottdc(la,co) = thgetid(histname)
               EndIF
            EndDo
         EndDo
      EndIF

c     --- h_fill_lucite_hist.f

      hidluctothits = thgetid("hluctothits")
      hidluclayer = thgetid("hluclayer")
      
      Do la=1,HNUM_LUC_LAYERS
         write(histname,'(10Hhluchitpat,i1)') la
         hidluchitpat(la) = thgetid(histname)
      EndDo

      Do la=1,HNUM_LUC_LAYERS
         write(histname,'(13Hhluclayerhits,i1)') la
         hidluclayerhits(la) = thgetid(histname)
      EndDo
      
      Do la=1,HNUM_LUC_LAYERS
         Do co=1,HNUM_LUC_COUNTERS
            if(co .lt. 10) then
               write(histname,'(10Hhlucposnpe,i1,2H_0,i1)') la,co
               hidlucposnpe(la,co) = thgetid(histname)
               write(histname,'(10Hhlucnegnpe,i1,2H_0,i1)') la,co
               hidlucnegnpe(la,co) = thgetid(histname)
               write(histname,'(10Hhlucsumnpe,i1,2H_0,i1)') la,co
               hidlucsumnpe(la,co) = thgetid(histname)
               write(histname,'(10Hhluctotnpe,i1,2H_0,i1)') la,co
               hidluctotnpe(la,co) = thgetid(histname)
            Else
               write(histname,'(10Hhlucposnpe,i1,1H_,i2)') la,co
               hidlucposnpe(la,co) = thgetid(histname)
               write(histname,'(10Hhlucnegnpe,i1,1H_,i2)') la,co
               hidlucnegnpe(la,co) = thgetid(histname)
               write(histname,'(10Hhlucsumnpe,i1,1H_,i2)') la,co
               hidlucsumnpe(la,co) = thgetid(histname)
               write(histname,'(10Hhluctotnpe,i1,1H_,i2)') la,co
               hidluctotnpe(la,co) = thgetid(histname)
            EndIF
         EndDo
      EndDo

      if(hturnon_lc_dec_hist .eq. 1) then
         Do la=1,HNUM_LUC_LAYERS
            Do co=1,HNUM_LUC_COUNTERS
               if(co .lt. 10) then
                  write(histname,'(11Hhlucpostime,i1,2H_0,i1)') la,co
                  hidlucpostime(la,co) = thgetid(histname)
                  write(histname,'(11Hhlucnegtime,i1,2H_0,i1)') la,co
                  hidlucnegtime(la,co) = thgetid(histname)
                  write(histname,'(11Hhluctottime,i1,2H_0,i1)') la,co
                  hidluctottime(la,co) = thgetid(histname)
               Else
                  write(histname,'(11Hhlucpostime,i1,1H_,i2)') la,co
                  hidlucpostime(la,co) = thgetid(histname)
                  write(histname,'(11Hhlucnegtime,i1,1H_,i2)') la,co
                  hidlucnegtime(la,co) = thgetid(histname)   
                  write(histname,'(11Hhluctottime,i1,1H_,i2)') la,co
                  hidluctottime(la,co) = thgetid(histname)   
               EndIF
            EndDo
         EndDo
      EndIf

      Do la=1,HNUM_LUC_LAYERS
         write(histname,'(10Hhlucnpesum,i1)') la
         hidlucnpesum(la) = thgetid(histname)
      EndDo     
cc--------------End of Lucite--------------------------

c     --- h_link_tracks
      hidtrknaer(1) = thgetid('htrknaer1')
      hidtrknaer(2) = thgetid('htrknaer2')
      hidtrknaer(3) = thgetid('htrknaer3')
      hidtrkaernum(1) = thgetid('htrkaernum1')
      hidtrkaernum(2) = thgetid('htrkaernum2')
      hidtrkaernum(3) = thgetid('htrkaernum3')
      hidtrkaernpesum(1) = thgetid('htrkaernpesum1')
      hidtrkaernpesum(2) = thgetid('htrkaernpesum2')
      hidtrkaernpesum(3) = thgetid('htrkaernpesum3')
      hidtrknwat(1) = thgetid('htrknwat1')
      hidtrknwat(2) = thgetid('htrknwat2')
      hidtrkwatnum(1) = thgetid('htrkwatnum1')
      hidtrkwatnum(2) = thgetid('htrkwatnum2')
      hidtrkwatnpesum(1) = thgetid('htrkwatnpesum1')
      hidtrkwatnpesum(2) = thgetid('htrkwatnpesum2')
      hidtrkwatnpesumkratio(1) = thgetid('htrkwatnpesumkratio1')
      hidtrkwatnpesumkratio(2) = thgetid('htrkwatnpesumkratio2')
      
      hidtrknluc(1) = thgetid('htrknluc')
      hidtrklucnum(1) = thgetid('htrklucnum')
      hidtrklucnpesum(1) = thgetid('htrklucnpesum')
*     
*     MISC DATA
*     
      
      hidmisctdcs(1) = thgetid('hmisctdcs1')
      hidmisctdcs(2) = thgetid('hmisctdcs2')
      hidmisctdcs(3) = thgetid('hmisctdcs3')
      hidmisctdcs(4) = thgetid('hmisctdcs4')
      hidmisctdcs(5) = thgetid('hmisctdcs5')
      hidmisctdcs(6) = thgetid('hmisctdcs6')

c     --- h_fill_phys_hist.f
      
      hidnphysics=thgetid('hnphysics')
      hidphysscinhit=thgetid('hphysscinhit')
      hidsxfp=thgetid('hsxfp')
      hidsyfp=thgetid('hsyfp')
      hidsxpfp=thgetid('hsxpfp')
      hidsypfp=thgetid('hsypfp')
      hidsxtar=thgetid('hsxtar')
      hidsytar=thgetid('hsytar')
      hidsxptar=thgetid('hsxptar')
      hidsyptar=thgetid('hsyptar')
      hidsdelta=thgetid('hsdelta')
      hidstimeatfp=thgetid('hstimeatfp')
      hidsbeta=thgetid('hsbeta')
      hidstof=thgetid('hstof')
      hidsp=thgetid('hsp')
      hidsenergy=thgetid('hsenergy')
c     --- x=y plot at detectors
      hidsdc1_xy=thgetid('hsdc1xy')
      hidsdc2_xy=thgetid('hsdc2xy')
      hids1x_xy=thgetid('hs1xxy')
      hids1y_xy=thgetid('hs1yxy')
      hids2x_xy=thgetid('hs2xxy')
      hidsac1_xy=thgetid('hsac1xy')
      hidsac2_xy=thgetid('hsac2xy')
      hidsac3_xy=thgetid('hsac3xy')
      hidswc1_xy=thgetid('hswc1xy')
      hidswc2_xy=thgetid('hswc2xy')
      hidslc_xy=thgetid('hslcxy')
c     --- cherenkov
      hidsac1npe = thgetid('hsac1npe')
      hidsac2npe = thgetid('hsac2npe')
      hidsac3npe = thgetid('hsac3npe')
      hidswc1npe = thgetid('hswc1npe')
      hidswc2npe = thgetid('hswc2npe')
      hidswc1npekratio = thgetid('hswc1npekratio')
      hidswc2npekratio = thgetid('hswc2npekratio')
      hidswc1ac1 = thgetid('hswc1ac1')
      hidsac1s1x = thgetid('hsac1s1x')
      hidswc1s1x = thgetid('hswc1s1x')
      hidslcnpe = thgetid('hslcnpe')



c     --- betak,e,pi,pr
      hidsbetak = thgetid('hsbetak')
      hidsbetae = thgetid('hsbetae')
      hidsbetapi = thgetid('hsbetapi')
      hidsbetapr = thgetid('hsbetapr')

      hidsdeltabetak = thgetid('hsdeltabetak')

      hidspathlength=thgetid('hspathlength')
      hidstimeattar=thgetid('hstimeattar')
      hidsrfdiff=thgetid('hsrfdiff')
      hidsrftime=thgetid('hsrftime')
      hidschi2perdeg=thgetid('hschi2perdeg')
      hidseloss=thgetid('hseloss')
      hidscorre=thgetid('hscorre')
      hidscorrp=thgetid('hscorrp')
      hidspz=thgetid('hspz')
      hidstheta=thgetid('hstheta')
      hidskpvec1=thgetid('hskpvec1')
      hidskpvec2=thgetid('hskpvec2')
      hidskpvec3=thgetid('hskpvec3')
      hidskpvec4=thgetid('hskpvec4')
      hidsphi=thgetid('hsphi')
      hidszbeam=thgetid('hszbeam')

c
c     HTUL
c
      hidtulrawtothits=thgetid('htulrawtothits')
      hidtulrawtotchannelnum=thgetid('htulrawtotchannelnum')
      hidtulrawtdc=thgetid('htulrawtdc')
      hidtultothits=thgetid('htultothits')
      hidtultotchannelnum=thgetid('htultotchannelnum')

cccccc DK added              
      call hbook1(7001,'test1',100.,-100.,250.,0.)
      call hbook1(7002,'test2',100.,-100.,250.,0.)
      call hbook1(7003,'test3',100.,-100.,250.,0.)
      call hbook1(7004,'test4',100.,-100.,250.,0.)
      call hbook1(7005,'test5',100.,-100.,250.,0.)
      call hbook1(7006,'test6',100.,-100.,250.,0.)
      call hbook1(7007,'test7',100.,-100.,250.,0.)
      call hbook1(7008,'test8',100.,-100.,250.,0.)
      call hbook1(7009,'test9',100.,-100.,250.,0.)
      call hbook1(7010,'test10',100.,-100.,250.,0.)
      call hbook1(7011,'test11',100.,-100.,250.,0.)
      call hbook1(7012,'test12',100.,-100.,250.,0.)

      RETURN
      END

