      Subroutine g_init_histid(ABORT,err)
*--------------------------------------------------------
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='g_init_histid')
*     
      Logical ABORT
      Character*(*) err
*--------------------------------------------------------
      Include "gen_data_structures.cmn"
      
      Integer*4 thgetid
      Character*32 histname
      Integer*4 i,j,k
      
      gidtrigrawtothits=thgetid('gtrigrawtothits')
      gidtrigrawchannels=thgetid('gtrigrawchannels')
      gidtrigrawtdc = thgetid('gtrigrawtdc')
      gidtrigtothits=thgetid('gtrigtothits')
      gidtrigchannels=thgetid('gtrigchannels')
      gidtrigtdc1=thgetid('gtrigtdc1')
      gidtrigtdc2=thgetid('gtrigtdc2')
      gidtrigtdc3=thgetid('gtrigtdc3')
      gidtrigtdc4=thgetid('gtrigtdc4')
      gidtrigtdc5=thgetid('gtrigtdc5')
      gidtrigtdc6=thgetid('gtrigtdc6')
      gidtrigtdc7=thgetid('gtrigtdc7')
      gidtrigtdc8=thgetid('gtrigtdc8')
      gidtrigcoin=thgetid('gtrigcoin')
CC Probe
      gidprobe=thgetid('gprobe')
      
      gidmisctdcs(1) = thgetid('gmisctdcs1')
      gidmisctdcs(2) = thgetid('gmisctdcs2')
      gidmisctdcs(3) = thgetid('gmisctdcs3')
      gidmisctdcs(4) = thgetid('gmisctdcs4')
      gidmisctdcs(5) = thgetid('gmisctdcs5')
      gidmisctdcs(6) = thgetid('gmisctdcs6')
      
      gidbpmadcs(1) = thgetid('gbpm3c07xp')
      gidbpmadcs(2) = thgetid('gbpm3c07xm')
      gidbpmadcs(3) = thgetid('gbpm3c07yp')
      gidbpmadcs(4) = thgetid('gbpm3c07ym')
      gidbpmadcs(5) = thgetid('gbpm3c08xp')
      gidbpmadcs(6) = thgetid('gbpm3c08xm')
      gidbpmadcs(7) = thgetid('gbpm3c08yp')
      gidbpmadcs(8) = thgetid('gbpm3c08ym')
      gidbpmadcs(9) = thgetid('gbpm3c12xp')
      gidbpmadcs(10) = thgetid('gbpm3c12xm')
      gidbpmadcs(11) = thgetid('gbpm3c12yp')
      gidbpmadcs(12) = thgetid('gbpm3c12ym')
      gidbpmadcs(13) = thgetid('gFR1')
      gidbpmadcs(14) = thgetid('gFR2')
      gidbpmadcs(15) = thgetid('gFR3')
      gidbpmadcs(16) = thgetid('gFR4')

      Return
      End

