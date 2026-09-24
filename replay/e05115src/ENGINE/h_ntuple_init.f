      subroutine h_Ntuple_init(ABORT,err)
*------------------------------------------------------------------
*     
*     Creates an HKS Ntuple
*     
*     Purpose : Books an HKS Ntuple; defines structure of it
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*     Created: 8-Apr-1994  K.B.Beard, Hampton Univ.
*     
*     Revision 2.2 2009/08/11 Z.Ye
*     for E05-115: Add Lucite info
*   
*     Revision 2.1 2004/03/03 Miyoshi
*     for E01-011 
*     
*     Revision 2.0  2000/03/03 12:11:08  jinghua
*     (JLiu) Added Lucite info
*     
*     Revision 1.7  1996/09/04 15:18:02  saw
*     (JRA) Modify ntuple contents
*     
*     Revision 1.6  1996/01/16 16:41:14  cdaq
*     (JRA) Modify ntuple contents
*     
*     Revision 1.5  1995/09/01 13:38:59  cdaq
*     (JRA) Add Cerenkov photoelectron count to ntuple
*     
*     Revision 1.4  1995/07/27  19:00:31  cdaq
*     (SAW) Relocate data statements for f2c compatibility
*     
*     Revision 1.3  1995/05/11  19:00:02  cdaq
*     (SAW) Allow %d for run number in filenames
*     
*     Revision 1.2  1994/06/17  02:36:00  cdaq
*     (KBB) Upgrade
*     
*     Revision 1.1  1994/04/12  16:16:18  cdaq
*     Initial revision
*     
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='h_Ntuple_init')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'h_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
*     
      character*80 default_name
      parameter (default_name= 'HKSntuple')
      integer default_bank,default_recL
      parameter (default_bank= 8000) !4 bytes/word
c      parameter (default_recL= 1024) !record length
      parameter (default_recL= 4096) !record length
      character*80 title
      character*80 directory,name
      character*256 file
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)
*     
      logical HEXIST            !CERNLIB function
*     
      INCLUDE 'h_ntuple.dte'
*     
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(h_Ntuple_exists) THEN
         call h_Ntuple_shutdown(ABORT,err)
         If(ABORT) Then
            call G_add_path(here,err)
            RETURN
         EndIf
      ENDIF
*     
      call NO_nulls(h_Ntuple_file) !replace null characters with blanks
*     
*     -if name blank, just forget it
      IF(h_Ntuple_file.EQ.' ') RETURN !do nothing
*     
*     - get any free IO channel
*     
      call g_IO_control(h_Ntuple_IOchannel,'ANY',ABORT,err)
      io= h_Ntuple_IOchannel
      h_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
         call G_add_path(here,err)
         RETURN
      ENDIF
*     
      h_Ntuple_ID= default_h_Ntuple_ID
      id= h_Ntuple_ID
*     
      ABORT= HEXIST(id)
      IF(ABORT) THEN
         call g_IO_control(h_Ntuple_IOchannel,'FREE',ABORT,err)
         call G_build_note(':HBOOK id#$ already in use',
     &        '$',id,' ',rv,' ',err)
         call G_add_path(here,err)
         RETURN
      ENDIF
*     
      CALL HCDIR(directory,'R') !CERNLIB read current directory
*     
      h_Ntuple_name= default_name
*     
      id= h_Ntuple_ID
      name= h_Ntuple_name
      
      file= h_Ntuple_file
      call g_sub_run_number(file,gen_run_number)
      
      recL= default_recL
*     
*     -open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status) !CERNLIB
*     !directory set to "//TUPLE"
      ABORT= status.NE.0
      IF(ABORT) THEN
         call g_IO_control(h_Ntuple_IOchannel,'FREE',ABORT,err)
         iv(1)= status
         iv(2)= io
         pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
         call G_build_note(pat,'$',iv,' ',rv,' ',err)
         call G_add_path(here,err)
         RETURN
      ENDIF
      h_Ntuple_file = file
*     
      m= 0
      m= m+1
      h_Ntuple_tag(m)= 'hsp'    ! 1
      m= m+1
      h_Ntuple_tag(m)= 'hsdelta' ! 2
c      m= m+1
c      h_Ntuple_tag(m)= 'hstheta' ! 3
c      m= m+1
c      h_Ntuple_tag(m)= 'hsphi'  ! 4
      m= m+1
      h_Ntuple_tag(m)= 'hscnhit' ! 3
      m= m+1
      h_Ntuple_tag(m)= 'hsna1' ! 4
      m= m+1
      h_Ntuple_tag(m)= 'hsna2' ! 5
      m= m+1
      h_Ntuple_tag(m)= 'hsna3' ! 6
      m= m+1
      h_Ntuple_tag(m)= 'hsnt1' ! 7
      m= m+1
      h_Ntuple_tag(m)= 'hsnt2' ! 8
      m= m+1
      h_Ntuple_tag(m)= 'hsnt3' ! 9
      m= m+1
      h_Ntuple_tag(m)= 'hsnco1' ! 10
      m= m+1
      h_Ntuple_tag(m)= 'hsnco2' ! 11
      m= m+1
      h_Ntuple_tag(m)= 'hsnco3' ! 12
      m= m+1
      h_Ntuple_tag(m)= 'hsbeta' ! 13
      m= m+1
      h_Ntuple_tag(m)= 'hsbeta1y' ! 14
      m=m+1
      h_Ntuple_tag(m)= 'hmsq' ! 15
      m= m+1
      h_Ntuple_tag(m)= 'hstof' ! 16
      m= m+1
      h_Ntuple_tag(m)= 'hsxfp'  ! 17 
      m= m+1
      h_Ntuple_tag(m)= 'hsyfp'  ! 18
      m= m+1
      h_Ntuple_tag(m)= 'hsxpfp' ! 19
      m= m+1
      h_Ntuple_tag(m)= 'hsypfp' ! 20
      m= m+1
      h_Ntuple_tag(m)= 'hstimefp' ! 21
      m= m+1
      h_Ntuple_tag(m)= 'hspathl' ! 22
      m= m+1
      h_Ntuple_tag(m)= 'hsxptar' ! 23
      m= m+1
      h_Ntuple_tag(m)= 'hsyptar' ! 24
      m= m+1
      h_Ntuple_tag(m)= 'hsxsv' ! 25
      m= m+1
      h_Ntuple_tag(m)= 'hsysv' ! 26
      m= m+1
      h_Ntuple_tag(m)= 'eventid' ! 27
      m = m+1
      h_Ntuple_tag(m)= 'runnum' ! 28
      m = m+1
      h_Ntuple_tag(m)= 'haernhit' ! 29
      m = m+1
      h_Ntuple_tag(m)= 'haernum1' ! 30
      m = m+1
      h_Ntuple_tag(m)= 'haernum2' ! 31
      m = m+1
      h_Ntuple_tag(m)= 'haernum3' ! 32
      m= m+1
      h_Ntuple_tag(m)= 'haernpe1' ! 33
      m= m+1
      h_Ntuple_tag(m)= 'haernpe2' ! 34
      m= m+1
      h_Ntuple_tag(m)= 'haernpe3' ! 35
      m= m+1
      h_Ntuple_tag(m)= 'hacnpe1m' ! 36
      m= m+1
      h_Ntuple_tag(m)= 'hacnpe2m' ! 37
      m= m+1
      h_Ntuple_tag(m)= 'hacnpe3m' ! 38
      m= m+1
      h_Ntuple_tag(m)= 'haern1p' ! 39
      m= m+1
      h_Ntuple_tag(m)= 'haern2p' ! 40
      m= m+1
      h_Ntuple_tag(m)= 'haern3p' ! 41
      m= m+1
      h_Ntuple_tag(m)= 'hacn1pm' ! 42
      m= m+1
      h_Ntuple_tag(m)= 'hacn2pm' ! 43
      m= m+1
      h_Ntuple_tag(m)= 'hacn3pm' ! 44
      m= m+1
      h_Ntuple_tag(m)= 'haern1n' ! 45
      m= m+1
      h_Ntuple_tag(m)= 'haern2n' ! 46
      m= m+1
      h_Ntuple_tag(m)= 'haern3n' ! 47
      m= m+1
      h_Ntuple_tag(m)= 'hacn1nm' ! 48
      m= m+1
      h_Ntuple_tag(m)= 'hacn2nm' ! 49
      m= m+1
      h_Ntuple_tag(m)= 'hacn3nm' ! 50
      m= m+1
      h_Ntuple_tag(m)= 'haert1' ! 51
      m= m+1
      h_Ntuple_tag(m)= 'haert2' ! 52
      m= m+1
      h_Ntuple_tag(m)= 'haert3' ! 53
      m= m+1
      h_Ntuple_tag(m)= 'hact1m' ! 54
      m= m+1
      h_Ntuple_tag(m)= 'hact2m' ! 55
      m= m+1
      h_Ntuple_tag(m)= 'hact3m' ! 56
      m= m+1
      h_Ntuple_tag(m)= 'hact1p' ! 57
      m= m+1
      h_Ntuple_tag(m)= 'hact2p' ! 58
      m= m+1
      h_Ntuple_tag(m)= 'hact3p' ! 59
      m= m+1
      h_Ntuple_tag(m)= 'hact1pm' ! 60
      m= m+1
      h_Ntuple_tag(m)= 'hact2pm' ! 61
      m= m+1
      h_Ntuple_tag(m)= 'hact3pm' ! 62
      m= m+1
      h_Ntuple_tag(m)= 'hact1n' ! 63
      m= m+1
      h_Ntuple_tag(m)= 'hact2n' ! 64
      m= m+1
      h_Ntuple_tag(m)= 'hact3n' ! 65
      m= m+1
      h_Ntuple_tag(m)= 'hact1nm' ! 66
      m= m+1
      h_Ntuple_tag(m)= 'hact2nm' ! 67
      m= m+1
      h_Ntuple_tag(m)= 'hact3nm' ! 68
       m= m+1
      h_Ntuple_tag(m)= 'hwatnhit' ! 69
       m= m+1
      h_Ntuple_tag(m)= 'hwatnum1' ! 70
       m= m+1
      h_Ntuple_tag(m)= 'hwatnum2' ! 71
      m= m+1
      h_Ntuple_tag(m)= 'hwatnpe1' ! 72
      m= m+1
      h_Ntuple_tag(m)= 'hwatnpe2' ! 73
      m= m+1
      h_Ntuple_tag(m)= 'hwcnpe1m' ! 74
      m= m+1
      h_Ntuple_tag(m)= 'hwcnpe2m' ! 75
*add normalized kaon NPE  
      m= m+1
      h_Ntuple_tag(m)= 'hwatnkn1' ! 76
      m= m+1
      h_Ntuple_tag(m)= 'hwatnkn2' ! 77
      m= m+1
      h_Ntuple_tag(m)= 'hwcnkn1m' ! 78
      m= m+1
      h_Ntuple_tag(m)= 'hwcnkn2m' ! 79
*end
      m= m+1
      h_Ntuple_tag(m)= 'hwatn1p' ! 80
      m= m+1
      h_Ntuple_tag(m)= 'hwatn2p' ! 81
      m= m+1
      h_Ntuple_tag(m)= 'hwcn1pm' ! 82
      m= m+1
      h_Ntuple_tag(m)= 'hwcn2pm' ! 83
      m= m+1
      h_Ntuple_tag(m)= 'hwatn1n' ! 84
      m= m+1
      h_Ntuple_tag(m)= 'hwatn2n' ! 85
      m= m+1
      h_Ntuple_tag(m)= 'hwcn1nm' ! 86
      m= m+1
      h_Ntuple_tag(m)= 'hwcn2nm' ! 87
      m= m+1
      h_Ntuple_tag(m)= 'hwatt1' ! 88
      m= m+1
      h_Ntuple_tag(m)= 'hwatt2' ! 89
      m= m+1
      h_Ntuple_tag(m)= 'hwct1m' ! 90
      m= m+1
      h_Ntuple_tag(m)= 'hwct2m' ! 91
       m= m+1
      h_Ntuple_tag(m)= 'hlucnhit' ! 92
      m= m+1
      h_Ntuple_tag(m)= 'hlucnum' ! 93
       m= m+1
      h_Ntuple_tag(m)= 'hlucnpe' ! 94
      m= m+1
      h_Ntuple_tag(m)= 'trkchi2' ! 95
      m= m+1
      h_Ntuple_tag(m)= 'trkndf' ! 96
      m= m+1
      h_Ntuple_tag(m)= 'multi1' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'multi2' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'multi3' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'multi4' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'multi5' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'multi6' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'multi7' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'multi8' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'multi9' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'multi10' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'multi11' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'multi12' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'ddist1' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'ddist2' ! 98
      m= m+1
      h_Ntuple_tag(m)= 'ddist3' ! 99
      m= m+1
      h_Ntuple_tag(m)= 'ddist4' ! 100
      m= m+1
      h_Ntuple_tag(m)= 'ddist5' ! 101
      m= m+1
      h_Ntuple_tag(m)= 'ddist6' ! 102
      m= m+1
      h_Ntuple_tag(m)= 'ddist7' ! 103
      m= m+1
      h_Ntuple_tag(m)= 'ddist8' ! 104
      m= m+1
      h_Ntuple_tag(m)= 'ddist9' ! 105
      m= m+1
      h_Ntuple_tag(m)= 'ddist10' ! 106
      m= m+1
      h_Ntuple_tag(m)= 'ddist11' ! 107
      m= m+1
      h_Ntuple_tag(m)= 'ddist12' ! 108
      m= m+1
      h_Ntuple_tag(m)= 'dtime1' ! 97
      m= m+1
      h_Ntuple_tag(m)= 'dtime2' ! 98
      m= m+1
      h_Ntuple_tag(m)= 'dtime3' ! 99
      m= m+1
      h_Ntuple_tag(m)= 'dtime4' ! 100
      m= m+1
      h_Ntuple_tag(m)= 'dtime5' ! 101
      m= m+1
      h_Ntuple_tag(m)= 'dtime6' ! 102
      m= m+1
      h_Ntuple_tag(m)= 'dtime7' ! 103
      m= m+1
      h_Ntuple_tag(m)= 'dtime8' ! 104
      m= m+1
      h_Ntuple_tag(m)= 'dtime9' ! 105
      m= m+1
      h_Ntuple_tag(m)= 'dtime10' ! 106
      m= m+1
      h_Ntuple_tag(m)= 'dtime11' ! 107
      m= m+1
      h_Ntuple_tag(m)= 'dtime12' ! 108
c      m= m+1
c      h_Ntuple_tag(m)= 'lsigma1' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'lsigma2' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'lsigma3' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'lsigma4' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'lsigma5' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'lsigma6' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'lsigma7' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'lsigma8' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'lsigma9' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'lsigma10' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'lsigma11' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'lsigma12' ! 96
      m= m+1
      h_Ntuple_tag(m)= 'sngres1' ! 109
      m= m+1
      h_Ntuple_tag(m)= 'sngres2' ! 110
      m= m+1
      h_Ntuple_tag(m)= 'sngres3' ! 111
      m= m+1
      h_Ntuple_tag(m)= 'sngres4' ! 112
      m= m+1
      h_Ntuple_tag(m)= 'sngres5' ! 113
      m= m+1
      h_Ntuple_tag(m)= 'sngres6' ! 114
      m= m+1
      h_Ntuple_tag(m)= 'sngres7' ! 115
      m= m+1
      h_Ntuple_tag(m)= 'sngres8' ! 116
      m= m+1
      h_Ntuple_tag(m)= 'sngres9' ! 117
      m= m+1
      h_Ntuple_tag(m)= 'sngres10' ! 118
      m= m+1
      h_Ntuple_tag(m)= 'sngres11' ! 119
      m= m+1
      h_Ntuple_tag(m)= 'sngres12' ! 120
      m= m+1
      h_Ntuple_tag(m)= 'htcoor1' ! 121
      m= m+1
      h_Ntuple_tag(m)= 'htcoor2' ! 122
      m= m+1
      h_Ntuple_tag(m)= 'htcoor3' ! 123
      m= m+1
      h_Ntuple_tag(m)= 'htcoor4' ! 124
      m= m+1
      h_Ntuple_tag(m)= 'htcoor5' ! 125
      m= m+1
      h_Ntuple_tag(m)= 'htcoor6' ! 126
      m= m+1
      h_Ntuple_tag(m)= 'htcoor7' ! 127
      m= m+1
      h_Ntuple_tag(m)= 'htcoor8' ! 128
      m= m+1
      h_Ntuple_tag(m)= 'htcoor9' ! 129
      m= m+1
      h_Ntuple_tag(m)= 'htcoor10' ! 130
      m= m+1
      h_Ntuple_tag(m)= 'htcoor11' ! 131
      m= m+1
      h_Ntuple_tag(m)= 'htcoor12' ! 132
      m= m+1
      h_Ntuple_tag(m)= 'hwcent1' ! 121
      m= m+1
      h_Ntuple_tag(m)= 'hwcent2' ! 122
      m= m+1
      h_Ntuple_tag(m)= 'hwcent3' ! 123
      m= m+1
      h_Ntuple_tag(m)= 'hwcent4' ! 124
      m= m+1
      h_Ntuple_tag(m)= 'hwcent5' ! 125
      m= m+1
      h_Ntuple_tag(m)= 'hwcent6' ! 126
      m= m+1
      h_Ntuple_tag(m)= 'hwcent7' ! 127
      m= m+1
      h_Ntuple_tag(m)= 'hwcent8' ! 128
      m= m+1
      h_Ntuple_tag(m)= 'hwcent9' ! 129
      m= m+1
      h_Ntuple_tag(m)= 'hwcent10' ! 130
      m= m+1
      h_Ntuple_tag(m)= 'hwcent11' ! 131
      m= m+1
      h_Ntuple_tag(m)= 'hwcent12' ! 132
c      m= m+1
c      h_Ntuple_tag(m)= 'hwire1' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'hwire2' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'hwire3' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'hwire4' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'hwire5' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'hwire6' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'hwire7' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'hwire8' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'hwire9' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'hwire10' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'hwire11' ! 96
c      m= m+1
c      h_Ntuple_tag(m)= 'hwire12' ! 96
      m= m+1
      h_Ntuple_tag(m)= 'hwcoor1' ! 133
      m= m+1
      h_Ntuple_tag(m)= 'hwcoor2' ! 133
      m= m+1
      h_Ntuple_tag(m)= 'hwcoor3' ! 134
      m= m+1
      h_Ntuple_tag(m)= 'hwcoor4' ! 135
      m= m+1
      h_Ntuple_tag(m)= 'hwcoor5' ! 136
      m= m+1
      h_Ntuple_tag(m)= 'hwcoor6' ! 137
      m= m+1
      h_Ntuple_tag(m)= 'hwcoor7' ! 138
      m= m+1
      h_Ntuple_tag(m)= 'hwcoor8' ! 139
      m= m+1
      h_Ntuple_tag(m)= 'hwcoor9' ! 140
      m= m+1
      h_Ntuple_tag(m)= 'hwcoor10' ! 141
      m= m+1
      h_Ntuple_tag(m)= 'hwcoor11' ! 142
      m= m+1
      h_Ntuple_tag(m)= 'hwcoor12' ! 143
      m= m+1
      h_Ntuple_tag(m)= 'trackid' ! 144
      m= m+1
      h_Ntuple_tag(m)= 'betak'  ! 145
      m= m+1
      h_Ntuple_tag(m)= 'betapi'  ! 146
      m= m+1
      h_Ntuple_tag(m)= 'betapr'  ! 147
      m= m+1
      h_Ntuple_tag(m)= 'hsrftime' ! 148
      m= m+1
      h_Ntuple_tag(m)= 'htimetar' ! 149
      m= m+1
      h_Ntuple_tag(m)= 'hnphys' ! 150
      m= m+1
      h_Ntuple_tag(m)= 'hrf' ! 151
      m= m+1
      h_Ntuple_tag(m)= 'hrfdiff' ! 152
      m= m+1
      h_Ntuple_tag(m)= 'gbeamx' ! 153
      m= m+1
      h_Ntuple_tag(m)= 'gbeamy' ! 154
      m=m+1
      h_ntuple_tag(m)= 'ts_hks'  ! 155
      m=m+1
      h_ntuple_tag(m)= 'ts_hes'  ! 156
      m=m+1
      h_ntuple_tag(m)= 'ts_coin'  ! 157
      m=m+1
      h_ntuple_tag(m)= 'ts_cp0'  ! 158
*     Add 0918/2010 chiba
      m=m+1
      h_ntuple_tag(m)= 'tg_hes'  ! 111
      m=m+1
      h_ntuple_tag(m)= 'tg_hks'  ! 112
      m=m+1
      h_ntuple_tag(m)= 'tg_g1'  ! 113
      m=m+1
      h_ntuple_tag(m)= 'tg_g2'  ! 114
      m=m+1
      h_ntuple_tag(m)= 'tg_g3'  ! 115
      m=m+1
      h_ntuple_tag(m)= 'tg_g4'  ! 116
      m=m+1
      h_ntuple_tag(m)= 'tg_g5'  ! 117
      m=m+1
      h_ntuple_tag(m)= 'tg_g6'  ! 118
     
*     Experiment dependent entries start here.
      
*     Open ntuple.
*     
      h_Ntuple_size= m          !total size
*     
      title= h_Ntuple_title
      IF(title.EQ.' ') THEN
         msg= name//' '//h_Ntuple_file
         call only_one_blank(msg)
         title= msg   
         h_Ntuple_title= title
      ENDIF
*     
      id= h_Ntuple_ID
      title= h_Ntuple_title
      size= h_Ntuple_size
      file= h_Ntuple_file
      bank= default_bank

      call HBOOKN(id,title,size,name,bank,h_Ntuple_tag) !create Ntuple
      Write(*,*) h_ntuple_file,' is opened.'

*     
      call HCDIR(h_Ntuple_directory,'R') !record Ntuple directory
*     
      CALL HCDIR(directory,' ') !reset CERNLIB directory
*     
      h_Ntuple_exists= HEXIST(h_Ntuple_ID)
      ABORT= .NOT.h_Ntuple_exists
*     
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // h_Ntuple_directory // '/]' // 
     &     name // ' IO#$ "' // h_Ntuple_file // '"'
      call G_build_note(pat,'$',iv,' ',rv,' ',msg)
      call sub_string(msg,' /]','/]')
*     
      IF(ABORT) THEN
         err= ':unable to create '//msg
         call G_add_path(here,err)
c     ELSE
c     pat= ':created '//msg
c     call G_add_path(here,pat)
c     call G_log_message('INFO: '//pat)
      ENDIF
*     
      RETURN
      END  
