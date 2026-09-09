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

      Return
      End

