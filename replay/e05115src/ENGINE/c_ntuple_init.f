      subroutine c_Ntuple_init(ABORT,err)
*------------------------------------------------------------
*     
*     Creates an COIN Ntuple
*     
*     Purpose : Books an COIN Ntuple; defines structure of it
*     
*     Output: ABORT      - success or failure
*     : err        - reason for failure, if any
*     
*     Created: 8-Apr-1994  K.B.Beard, Hampton Univ.
*     Modified: 11-Oct-1999 Jinghua Liu for HNSS
* 
*     Revision 2.2  2009/08/11 Z.Ye
*     change for E05-115 by adding Lucite info
*      
*     Revision 2.1  2004/03/02 Miyoshi
*     change for E01-011
*     
*     Revision 2.0  1999/10/11 10:35:56  jinghua
*     (JLiu) Change to HNSS
*     
*     Revision 1.9  1999/02/23 16:40:37  csa
*     Variable changes
*     
*     Revision 1.8  1996/09/04 15:29:57  saw
*     (JRA) Modify ntuple contents
*
*     Revision 1.7  1996/01/22 15:06:25  saw
*     (JRA) Change ntuple contents
*     
*     Revision 1.6  1996/01/16 21:01:12  cdaq
*     (JRA) Add HSDELTA and SSDELTA
*     
*     Revision 1.5  1995/08/08 16:09:40  cdaq
*     (DD) Change ntuple list
*     
*     Revision 1.4  1995/07/27  18:59:48  cdaq
*     (SAW) Relocate data statements for f2c compatibility
*     
*     Revision 1.3  1995/05/11  13:55:27  cdaq
*     (SAW) Allow %d for run number in filenames
*     
*     Revision 1.2  1994/06/17  02:32:24  cdaq
*     (KBB) Upgrade
*     
*     Revision 1.1  1994/04/12  16:11:34  cdaq
*     Initial revision
*     
*     
*----------------------------------------------------------------------
      implicit none
      save
*     
      character*13 here
      parameter (here='c_Ntuple_init')
*     
      logical ABORT
      character*(*) err
*     
      INCLUDE 'c_ntuple.cmn'
      INCLUDE 'gen_routines.dec'
      include 'gen_run_info.cmn'
*     
      character*80 default_name
      parameter (default_name= 'COINntuple')
      integer default_bank,default_recL
      parameter (default_bank= 8000) !4 bytes/word
c      parameter (default_recL= 512) !record length
      parameter (default_recL= 4096) !record length
      character*200 title
      character*80 file
      character*80 directory,name
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
      real rv(10)
      integer*4 layer
*     
      logical HEXIST            !CERNLIB function
*     
      INCLUDE 'c_ntuple.dte'
*     
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*     
      IF(c_Ntuple_exists) THEN
         call c_Ntuple_shutdown(ABORT,err)
         If(ABORT) Then
            call G_add_path(here,err)
            RETURN
         EndIf
      ENDIF
*     
      call NO_nulls(c_Ntuple_file) !replace null characters with blanks
*     
*     -if name blank, just forget it
      IF(c_Ntuple_file.EQ.' ') RETURN !do nothing
*     
*     - get any free IO channel
*     
      call g_IO_control(io,'ANY',ABORT,err)
      c_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
         call G_add_path(here,err)
         RETURN
      ENDIF
      c_Ntuple_IOchannel= io
*     
      c_Ntuple_ID= default_c_Ntuple_ID
      id= c_Ntuple_ID
*     
      ABORT= HEXIST(id)
      IF(ABORT) THEN
         call g_IO_control(c_Ntuple_IOchannel,'FREE',ABORT,err)
         call G_build_note(':HBOOK id#$ already in use',
     &        '$',id,' ',rv,' ',err)
         call G_add_path(here,err)
         RETURN
      ENDIF
*     
      CALL HCDIR(directory,'R') !CERNLIB read current directory
*     
      c_Ntuple_name= default_name
*     
      id= c_Ntuple_ID
      name= c_Ntuple_name
      
      file= c_Ntuple_file
      call g_sub_run_number(file,gen_run_number)
      
      recL= default_recL
      io= c_Ntuple_IOchannel
*     
*     -open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status) !CERNLIB
*     !directory set to "//TUPLE"
      io= c_Ntuple_IOchannel
      ABORT= status.NE.0
      IF(ABORT) THEN
         call g_IO_control(c_Ntuple_IOchannel,'FREE',ABORT,err)
         iv(1)= status
         iv(2)= io
         pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
         call G_build_note(pat,'$',iv,' ',rv,' ',err)
         call G_add_path(here,err)
         RETURN
      ENDIF
      c_Ntuple_file= file
*     
**********begin insert description of contents of COIN tuple ******
      m= 0
      m=m+1
      c_Ntuple_tag(m)= 'hallcp' ! 0
      m=m+1
      c_Ntuple_tag(m)= 'hstimefp' ! 1
      m=m+1
      c_Ntuple_tag(m)= 'htimetar' ! 2
      m= m+1
      c_Ntuple_tag(m)= 'hsxfp'  ! 3
      m= m+1
      c_Ntuple_tag(m)= 'hsyfp'  ! 4
      m= m+1
      c_Ntuple_tag(m)= 'hsxpfp' ! 5
      m= m+1
      c_Ntuple_tag(m)= 'hsypfp' ! 6
c      m= m+1
c      c_Ntuple_tag(m)= 'hsxtar' ! 7
c      m= m+1
c      c_Ntuple_tag(m)= 'hsytar' ! 8
      m= m+1
      c_Ntuple_tag(m)= 'hsxptar' ! 9
      m= m+1
      c_Ntuple_tag(m)= 'hsyptar' ! 10 
      m= m+1
      c_Ntuple_tag(m)= 'hsxsv' ! 11
      m= m+1
      c_Ntuple_tag(m)= 'hsysv' ! 12
      m= m+1
      c_Ntuple_tag(m)= 'hsxpsv' ! 13
      m= m+1
      c_Ntuple_tag(m)= 'hsypsv' ! 14
      m= m+1
      c_Ntuple_tag(m)= 'hsp'    ! 15
c      m= m+1
c      c_Ntuple_tag(m)= 'hsdelta'    ! 16
      m= m+1
      c_Ntuple_tag(m)= 'hsbeta' ! 17
      m= m+1
      c_Ntuple_tag(m)= 'hsbeta1y' ! 17
      m= m+1
      c_Ntuple_tag(m)= 'hsbeta_k' ! 18   
      m= m+1
      c_Ntuple_tag(m)= 'hmsq' ! 18   
      m= m+1
      c_Ntuple_tag(m)= 'haernhit' ! 19
      m= m+1
      c_Ntuple_tag(m)= 'haernh1' ! 19
      m= m+1
      c_Ntuple_tag(m)= 'haernh2' ! 19
      m= m+1
      c_Ntuple_tag(m)= 'haernh3' ! 19
      m= m+1
      c_Ntuple_tag(m)= 'haernpe1' ! 20
      m= m+1
      c_Ntuple_tag(m)= 'haernpe2' ! 21
      m= m+1
      c_Ntuple_tag(m)= 'haernpe3' ! 22
      m= m+1
      c_Ntuple_tag(m)= 'hacnpe1m' ! 23
      m= m+1
      c_Ntuple_tag(m)= 'hacnpe2m' ! 24
      m= m+1
      c_Ntuple_tag(m)= 'hacnpe3m' ! 25
      m= m+1
      c_Ntuple_tag(m)= 'haern1p' ! 20
      m= m+1
      c_Ntuple_tag(m)= 'haern2p' ! 21
      m= m+1
      c_Ntuple_tag(m)= 'haern3p' ! 22
      m= m+1
      c_Ntuple_tag(m)= 'hacn1pm' ! 23
      m= m+1
      c_Ntuple_tag(m)= 'hacn2pm' ! 24
      m= m+1
      c_Ntuple_tag(m)= 'hacn3pm' ! 25
      m= m+1
      c_Ntuple_tag(m)= 'haern1n' ! 20
      m= m+1
      c_Ntuple_tag(m)= 'haern2n' ! 21
      m= m+1
      c_Ntuple_tag(m)= 'haern3n' ! 22
      m= m+1
      c_Ntuple_tag(m)= 'hacn1nm' ! 23
      m= m+1
      c_Ntuple_tag(m)= 'hacn2nm' ! 24
      m= m+1
      c_Ntuple_tag(m)= 'hacn3nm' ! 25
      m= m+1
      c_Ntuple_tag(m)= 'hact1' ! 26
      m= m+1
      c_Ntuple_tag(m)= 'hact2' ! 27
      m= m+1
      c_Ntuple_tag(m)= 'hact3' ! 28
      m= m+1
      c_Ntuple_tag(m)= 'hact1m' ! 29
      m= m+1
      c_Ntuple_tag(m)= 'hact2m' ! 30
      m= m+1
      c_Ntuple_tag(m)= 'hact3m' ! 31
      m= m+1
      c_Ntuple_tag(m)= 'hact1p' ! 26
      m= m+1
      c_Ntuple_tag(m)= 'hact2p' ! 27
      m= m+1
      c_Ntuple_tag(m)= 'hact3p' ! 28
      m= m+1
      c_Ntuple_tag(m)= 'hact1pm' ! 29
      m= m+1
      c_Ntuple_tag(m)= 'hact2pm' ! 30
      m= m+1
      c_Ntuple_tag(m)= 'hact3pm' ! 31
      m= m+1
      c_Ntuple_tag(m)= 'hact1n' ! 26
      m= m+1
      c_Ntuple_tag(m)= 'hact2n' ! 27
      m= m+1
      c_Ntuple_tag(m)= 'hact3n' ! 28
      m= m+1
      c_Ntuple_tag(m)= 'hact1nm' ! 29
      m= m+1
      c_Ntuple_tag(m)= 'hact2nm' ! 30
      m= m+1
      c_Ntuple_tag(m)= 'hact3nm' ! 31
      m= m+1
      c_Ntuple_tag(m)= 'haernum1' ! 32
      m= m+1
      c_Ntuple_tag(m)= 'haernum2' ! 33
      m= m+1
      c_Ntuple_tag(m)= 'haernum3' ! 34
      m= m+1
      c_Ntuple_tag(m)= 'hwatnhit' ! 35
      m= m+1
      c_Ntuple_tag(m)= 'hwatnh1' ! 35
      m= m+1
      c_Ntuple_tag(m)= 'hwatnh2' ! 35
      m= m+1
      c_Ntuple_tag(m)= 'hwatnpe1' ! 36
      m= m+1
      c_Ntuple_tag(m)= 'hwatnpe2' ! 37
      m= m+1
      c_Ntuple_tag(m)= 'hwcnpe1m' ! 38
      m= m+1
      c_Ntuple_tag(m)= 'hwcnpe2m' ! 39
*add normalized kaon NPE 
      m= m+1
      c_Ntuple_tag(m)= 'hwatnkn1' ! 36
      m= m+1
      c_Ntuple_tag(m)= 'hwatnkn2' ! 37
      m= m+1
      c_Ntuple_tag(m)= 'hwcnkn1m' ! 38
      m= m+1
      c_Ntuple_tag(m)= 'hwcnkn2m' ! 39
*end
      m= m+1
      c_Ntuple_tag(m)= 'hwatn1p' ! 36
      m= m+1
      c_Ntuple_tag(m)= 'hwatn2p' ! 37
      m= m+1
      c_Ntuple_tag(m)= 'hwcn1pm' ! 38
      m= m+1
      c_Ntuple_tag(m)= 'hwcn2pm' ! 39
      m= m+1
      c_Ntuple_tag(m)= 'hwatn1n' ! 36
      m= m+1
      c_Ntuple_tag(m)= 'hwatn2n' ! 37
      m= m+1
      c_Ntuple_tag(m)= 'hwcn1nm' ! 38
      m= m+1
      c_Ntuple_tag(m)= 'hwcn2nm' ! 39
      m= m+1
      c_Ntuple_tag(m)= 'hwct1' ! 40
      m= m+1
      c_Ntuple_tag(m)= 'hwct2' ! 41
      m= m+1
      c_Ntuple_tag(m)= 'hwct1m' ! 42
      m= m+1
      c_Ntuple_tag(m)= 'hwct2m' ! 43
      m= m+1
      c_Ntuple_tag(m)= 'hwatnum1' ! 44
      m= m+1
      c_Ntuple_tag(m)= 'hwatnum2' ! 45
      m= m+1
      c_Ntuple_tag(m)= 'htrkchi2' ! 46
      m= m+1
      c_Ntuple_tag(m)= 'htrkndf' ! 47
      m= m+1
      c_Ntuple_tag(m)= 'hsgres1' ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsgres2' ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsgres3' ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsgres4' ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsgres5' ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsgres6' ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsgres7' ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsgres8' ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsgres9' ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsgres10' ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsgres11' ! 
      m= m+1
      c_Ntuple_tag(m)= 'hsgres12' ! 
      m= m+1
      c_Ntuple_tag(m)= 'htcoor1' ! 
      m= m+1
      c_Ntuple_tag(m)= 'htcoor2' ! 
      m= m+1
      c_Ntuple_tag(m)= 'htcoor3' ! 
      m= m+1
      c_Ntuple_tag(m)= 'htcoor4' ! 
      m= m+1
      c_Ntuple_tag(m)= 'htcoor5' ! 
      m= m+1
      c_Ntuple_tag(m)= 'htcoor6' ! 
      m= m+1
      c_Ntuple_tag(m)= 'htcoor7' ! 
      m= m+1
      c_Ntuple_tag(m)= 'htcoor8' ! 
      m= m+1
      c_Ntuple_tag(m)= 'htcoor9' ! 
      m= m+1
      c_Ntuple_tag(m)= 'htcoor10' ! 
      m= m+1
      c_Ntuple_tag(m)= 'htcoor11' ! 
      m= m+1
      c_Ntuple_tag(m)= 'htcoor12' ! 
      m= m+1
      c_Ntuple_tag(m)= 'hscnhit' ! 48
      m= m+1
      c_Ntuple_tag(m)= 'hsnco1' ! 49
      m= m+1
      c_Ntuple_tag(m)= 'hsnco2' ! 50
      m= m+1
      c_Ntuple_tag(m)= 'hsnco3' ! 51
      m= m+1
      c_Ntuple_tag(m)= 'hsnt1'  ! 52
      m= m+1
      c_Ntuple_tag(m)= 'hsnt2'  ! 53
      m= m+1
      c_Ntuple_tag(m)= 'hsnt3'  ! 54
      m= m+1
      c_Ntuple_tag(m)= 'hsnt1m'  ! 55
      m= m+1
      c_Ntuple_tag(m)= 'hsnt2m'  ! 56
      m= m+1
      c_Ntuple_tag(m)= 'hsnt3m'  ! 57
      m= m+1
      c_Ntuple_tag(m)= 'hsna1'  ! 58
      m= m+1
      c_Ntuple_tag(m)= 'hsna2'  ! 59
      m= m+1
      c_Ntuple_tag(m)= 'hsna3'  ! 60
      m= m+1
      c_Ntuple_tag(m)= 'hsna1m'  ! 61
      m= m+1
      c_Ntuple_tag(m)= 'hsna2m'  ! 62
      m= m+1
      c_Ntuple_tag(m)= 'hsna3m'  ! 63
c      m= m+1
c      c_Ntuple_tag(m)= 'hscdepo' ! 64
      m= m+1
      c_Ntuple_tag(m)= 'hrftime' ! 65
c      m= m+1
c      c_Ntuple_tag(m)= 'hrfdiff' ! 66
c     --- ENGE
      m= m+1
      c_Ntuple_tag(m)= 'estimefp' ! 67
      m= m+1
      c_Ntuple_tag(m)= 'etimetar' ! 68
      m= m+1
      c_Ntuple_tag(m)= 'esxfp'  ! 69
      m= m+1
      c_Ntuple_tag(m)= 'esyfp'  ! 70
      m= m+1
      c_Ntuple_tag(m)= 'esxpfp' ! 71 
      m=m+1
      c_Ntuple_tag(m)= 'esypfp' ! 72
c      m=m+1
c      c_Ntuple_tag(m)= 'esxtar' ! 73
c      m=m+1
c      c_Ntuple_tag(m)= 'esytar' ! 74
      m=m+1
      c_Ntuple_tag(m)= 'esxptar' ! 75
      m=m+1
      c_Ntuple_tag(m)= 'esyptar'    ! 76
      m=m+1
      c_Ntuple_tag(m)= 'esxsv' ! 77
      m=m+1
      c_Ntuple_tag(m)= 'esysv' ! 78
      m=m+1
      c_Ntuple_tag(m)= 'esxpsv' ! 79
      m=m+1
      c_Ntuple_tag(m)= 'esypsv' ! 80
      m=m+1
      c_Ntuple_tag(m)= 'esp'    ! 81
c      m=m+1
c      c_Ntuple_tag(m)= 'esdelta' ! 82
      m=m+1
      c_ntuple_tag(m)= 'etrkchi2' ! 83
c<<<<<<< .mine
c=======
c      m=m+1
c      c_ntuple_tag(m)= 'eres1' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres2' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres3' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres4' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres5' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres6' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres7' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres8' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres9' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres10' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres11' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres12' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres13' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres14' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres15' ! 83
c      m=m+1
c      c_ntuple_tag(m)= 'eres16' ! 83
c>>>>>>> .r459
      m= m+1
      c_Ntuple_tag(m)= 'etrkndf' ! 84
c     ==========EDC RESIDUAL===============
      m= m+1
      c_Ntuple_tag(m) = 'esgres1'
      m= m+1
      c_Ntuple_tag(m) = 'esgres2'
      m= m+1
      c_Ntuple_tag(m) = 'esgres3'
      m= m+1
      c_Ntuple_tag(m) = 'esgres4'
      m= m+1
      c_Ntuple_tag(m) = 'esgres5'
      m= m+1
      c_Ntuple_tag(m) = 'esgres6'
      m= m+1
      c_Ntuple_tag(m) = 'esgres7'
      m= m+1
      c_Ntuple_tag(m) = 'esgres8'
      m= m+1
      c_Ntuple_tag(m) = 'esgres9'
      m= m+1
      c_Ntuple_tag(m) = 'esgres10'
      m= m+1
      c_Ntuple_tag(m) = 'esgres11'
      m= m+1
      c_Ntuple_tag(m) = 'esgres12'
      m= m+1
      c_Ntuple_tag(m) = 'esgres13'
      m= m+1
      c_Ntuple_tag(m) = 'esgres14'
      m= m+1
      c_Ntuple_tag(m) = 'esgres15'
      m= m+1
      c_Ntuple_tag(m) = 'esgres16'
c     ==========EDC Tracking coordinate===============
      m= m+1
      c_Ntuple_tag(m) = 'etcoor1'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor2'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor3'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor4'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor5'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor6'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor7'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor8'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor9'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor10'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor11'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor12'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor13'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor14'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor15'
      m= m+1
      c_Ntuple_tag(m) = 'etcoor16'
c     ==========EDC center+drift distance===============
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor1'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor2'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor3'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor4'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor5'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor6'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor7'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor8'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor9'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor10'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor11'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor12'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor13'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor14'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor15'
      m= m+1
      c_Ntuple_tag(m) = 'ewcoor16'
      m= m+1
      c_Ntuple_tag(m)= 'escnhit' ! 85
      m= m+1
      c_Ntuple_tag(m)= 'esnco1' ! 86
      m= m+1
      c_Ntuple_tag(m)= 'esnco2' ! 87
      m= m+1
      c_Ntuple_tag(m)= 'esnt1'  ! 88
      m= m+1
      c_Ntuple_tag(m)= 'esnt2'  ! 89
      m= m+1
      c_Ntuple_tag(m)= 'esnt1m'  ! 90
      m= m+1
      c_Ntuple_tag(m)= 'esnt2m'  ! 91
      m= m+1
      c_Ntuple_tag(m)= 'esna1'  ! 92
      m= m+1
      c_Ntuple_tag(m)= 'esna2'  ! 93
      m= m+1
      c_Ntuple_tag(m)= 'esna1m'  ! 94
      m= m+1
      c_Ntuple_tag(m)= 'esna2m'  ! 95
c      m= m+1
c      c_Ntuple_tag(m)= 'escdepo' ! 96
      m= m+1
      c_Ntuple_tag(m)= 'erftime' ! 97
c      m= m+1
c      c_Ntuple_tag(m)= 'erfdiff' ! 98
c     --- beam position
      m= m+1
      c_Ntuple_tag(m)= 'gbeamx' ! 99
      m= m+1
      c_Ntuple_tag(m)= 'gbeamy' ! 100
c     --- sli ---
      m= m+1
      c_Ntuple_tag(m)= 'gespread' ! 101
c     --- FFB
c     --- fast feedback on flag (1:off,0:on)
c     This flag was obtained by epics signal. 
c     It should be replaced to the signal from TS in E05-115exp.
c     DK did the replacement (2011.Jan.28)
c      m= m+1
c      c_Ntuple_tag(m)= 'gfbon' ! 102 
c     --- fast energy rock on flag(1:on,0:off) 
      m= m+1
      c_Ntuple_tag(m)= 'gferon' ! 103
c     --- IPM3C*** read out (used be energy drift) ---
      m= m+1
      c_Ntuple_tag(m)= 'bpm3c07x' ! 104
      m= m+1
      c_Ntuple_tag(m)= 'bpm3c07y' ! 105
      m= m+1
      c_Ntuple_tag(m)= 'bpm3c08x' ! 106
      m= m+1
      c_Ntuple_tag(m)= 'bpm3c08y' ! 107
      m= m+1
      c_Ntuple_tag(m)= 'bpm3c12x' ! 110
      m= m+1
      c_Ntuple_tag(m)= 'bpm3c12y' ! 111
c     --- coin ---
      m=m+1
      c_Ntuple_tag(m)= 'hnphys' ! 122
      m=m+1
      c_Ntuple_tag(m)= 'enphys' ! 123
      m=m+1
      c_Ntuple_tag(m)= 'cnphys' ! 124
      m=m+1
      c_ntuple_tag(m)= 'eventid' ! 125
      m=m+1
      c_ntuple_tag(m)= 'runnum'  ! 126
c      m=m+1
c      c_ntuple_tag(m)= 'coinrf' ! 127
      m=m+1
      c_ntuple_tag(m)= 'cointime' ! 128
      m=m+1
      c_ntuple_tag(m)= 'tg_hes'  ! 129
      m=m+1
      c_ntuple_tag(m)= 'tg_hks'  ! 130
      m=m+1
      c_ntuple_tag(m)= 'tg_g1'  ! 131
      m=m+1
      c_ntuple_tag(m)= 'tg_g2'  ! 132
      m=m+1
      c_ntuple_tag(m)= 'tg_g3'  ! 133
      m=m+1
      c_ntuple_tag(m)= 'tg_g4'  ! 134
      m=m+1
      c_ntuple_tag(m)= 'tg_g5'  ! 135
      m=m+1
      c_ntuple_tag(m)= 'tg_g6'  ! 136
      m=m+1
      c_ntuple_tag(m)= 'ts_hks'  ! 138
      m=m+1
      c_ntuple_tag(m)= 'ts_hes'  ! 138
      m=m+1
      c_ntuple_tag(m)= 'ts_coin'  ! 138
      m=m+1
      c_ntuple_tag(m)= 'ts_cp0'  ! 138
      m=m+1
      c_ntuple_tag(m)= 'gfbon'  ! 138
      m=m+1
      c_ntuple_tag(m)= 'happex'  ! 138
c--   Lucite     
      m=m+1
      c_Ntuple_tag(m)= 'hlucnhit' ! 139
      m= m+1
      c_Ntuple_tag(m)= 'hlucnpe1' ! 140
      m= m+1
      c_Ntuple_tag(m)= 'hlcnpe1m' ! 141
      m= m+1
      c_Ntuple_tag(m)= 'hlct1'  ! 142
      m= m+1
      c_Ntuple_tag(m)= 'hlct1m' ! 143
      m= m+1
      c_Ntuple_tag(m)= 'hlucnum1' ! 144
      c_Ntuple_size= m
***********end insert description of contents of COIN tuple********
*
      Call g_get_compileinfo(title)
c      title= c_Ntuple_title
      IF(title.EQ.' ') THEN
        msg= name//' '//c_Ntuple_file
        call only_one_blank(msg)
        title= msg   
        c_Ntuple_title= title
      ENDIF
*
      id= c_Ntuple_ID
      io= c_Ntuple_IOchannel
      name= c_Ntuple_name
c      title= c_Ntuple_title
      size= c_Ntuple_size
      file= c_Ntuple_file
      bank= default_bank
      call HBOOKN(id,title,size,name,bank,c_Ntuple_tag)      !create Ntuple
      Write(*,*) c_ntuple_file,' is opened.'
      Write(*,*) "Title is ",title
*
      call HCDIR(c_Ntuple_directory,'R')      !record Ntuple directory
*
      CALL HCDIR(directory,' ')       !reset CERNLIB directory
*
      c_Ntuple_exists= HEXIST(c_Ntuple_ID)
      ABORT= .NOT.c_Ntuple_exists
*
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // c_Ntuple_directory // '/]' // 
     &                         name // ' IO#$ "' // file // '"'
      call G_build_note(pat,'$',iv,' ',rv,' ',msg)
      call sub_string(msg,' /]','/]')
*
      IF(ABORT) THEN
        err= ':unable to create '//msg
        call G_add_path(here,err)
c      ELSE
c        pat= ':created '//msg
c        call G_add_path(here,pat)
c        call G_log_message('INFO: '//pat)
      ENDIF
*
      RETURN
      END  
