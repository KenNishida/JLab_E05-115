      Subroutine c_init_histid(ABORT,err)
*--------------------------------------------------------
*
*
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='c_init_histid')
*     
      Logical ABORT
      Character*(*) err
*--------------------------------------------------------
      Include "coin_data_structures.cmn"
      Include "coin_id_histid.cmn"
      
      Integer*4 thgetid
      Character*32 histname
      Integer*4 i,j,k
      
*     c_physics.f
      cidnphys = thgetid('cnphys')
      cidmissmassh = thgetid('cmissmassh')
      cidmissmassc = thgetid('cmissmassc')
      cidmissmassczoom = thgetid('cmissmassczoom')

      cidpeppk = thgetid('cpeppk')

      cidhksmulti = thgetid('chksmulti')
      cidhesmulti = thgetid('chesmulti')

      cidcointime = thgetid('ccointime')
      cidcointimezoom = thgetid('ccointimezoom')

      Return
      End

