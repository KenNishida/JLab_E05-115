      subroutine g_bm_Nt_init(ABORT,err)
*----------------------------------------------------------------------
*
*     Creates an GEN beam slit Ntuple
*
*     Purpose : Books an beam Ntuple; defines structure of it
*
*     Output: ABORT      - success or failure
*           : err        - reason for failure, if any
*
*     Created: 28-Feb-2000  
*
* Revision 1.0  2000/02/28 12:28:20  jinghua
* Initial revision
*----------------------------------------------------------------------
      implicit none
      save
*
      character*13 here
      parameter (here='g_bm_Nt_init')
*
      logical ABORT
      character*(*) err
*
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_run_info.cmn'
*
      character*80 default_name
      parameter (default_name= 'gbeamntuple')
      character*80 default_title
      parameter (default_title= 'gBeamMonitor')   
      integer default_bank,default_recL
      parameter (default_bank= 8000)    !4 bytes/word
      parameter (default_recL= 1024)    !record length
      character*80 title,file
      character*80 directory,name
      character*1000 pat,msg
      integer status,size,io,id,bank,recL,iv(10),m
*      parameter (id = 1)
      real rv(10)
*
      logical HEXIST           !CERNLIB function
      INCLUDE 'g_beam_ntuple.cmn'
      INCLUDE 'g_beam_ntuple.dte'
*
*--------------------------------------------------------
      err= ' '
      ABORT = .FALSE.
*
      IF(g_beam_Ntuple_exists) THEN    
        call g_bm_Nt_shutdown(ABORT,err)
        If(ABORT) Then
          call G_add_path(here,err)
          RETURN
        EndIf
      ENDIF
*

      g_beam_Ntuple_ID= default_g_beam_Ntuple_ID
      g_beam_Ntuple_name= default_name
      g_beam_Ntuple_title= default_title

      call NO_nulls(g_beam_Ntuple_file)     !replace null characters with blanks
*
*-if name blank, just forget it
      IF(g_beam_Ntuple_file.EQ.' ') RETURN   !do nothing
*
*- get any free IO channel
*
      call g_IO_control(io,'ANY',ABORT,err)
      g_beam_Ntuple_exists= .NOT.ABORT
      IF(ABORT) THEN
        call G_add_path(here,err)
        RETURN
      ENDIF
      g_beam_Ntuple_IOchannel= io
*
      id= g_beam_Ntuple_ID
*

      ABORT= HEXIST(id)
      IF(ABORT) THEN
        call g_IO_control(g_beam_Ntuple_IOchannel,'FREE',ABORT,err)
        call G_build_note(':HBOOK id#$ already in use',
     &                                 '$',id,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
*

      CALL HCDIR(directory,'R')       !CERNLIB read current directory
 
*
*
      id= g_beam_Ntuple_ID
      name= g_beam_Ntuple_name

      file= g_beam_Ntuple_file
      call g_sub_run_number(file,gen_run_number)

      recL= default_recL
      io= g_beam_Ntuple_IOchannel
*
*-open New *.rzdat file-
      call HROPEN(io,name,file,'N',recL,status)       !CERNLIB
*                                       !directory set to "//TUPLE"
      io= g_beam_Ntuple_IOchannel
      ABORT= status.NE.0
      IF(ABORT) THEN
        call g_IO_control(g_beam_Ntuple_IOchannel,'FREE',ABORT,err)
        iv(1)= status
        iv(2)= io
        pat= ':HROPEN error#$ opening IO#$ "'//file//'"'
        call G_build_note(pat,'$',iv,' ',rv,' ',err)
        call G_add_path(here,err)
        RETURN
      ENDIF
      g_beam_Ntuple_file= file
*
**********begin insert description of contents of BEAM tuple ******
      m= 0
*  
      m=m+1
      g_beam_ntuple_tag(m)= 'eventid' !1
      m=m+1
      g_beam_ntuple_tag(m)= 'time'     
      m=m+1
      g_beam_Ntuple_tag(m)= 'xph00'		! X focal plane position 
      m= m+1
      g_beam_Ntuple_tag(m)= 'yph00'
      m= m+1
      g_beam_Ntuple_tag(m)= 'xph01' !5
      m= m+1
      g_beam_Ntuple_tag(m)= 'yph01'
      m= m+1
      g_beam_Ntuple_tag(m)= 'xph02a'
      m= m+1
      g_beam_Ntuple_tag(m)= 'yph02a'
      m= m+1
      g_beam_Ntuple_tag(m)= 'xph02b'
      m= m+1
      g_beam_Ntuple_tag(m)= 'yph02b' !10
      m= m+1
      g_beam_Ntuple_tag(m)= 'xph02c'
      m=m+1
      g_beam_ntuple_tag(m)= 'yph02c'
      m=m+1
      g_beam_ntuple_tag(m)= 'xph03a'
      m=m+1
      g_beam_ntuple_tag(m)= 'yph03a'
      m=m+1
      g_beam_ntuple_tag(m)= 'xph03b' !15
      m=m+1
      g_beam_ntuple_tag(m)= 'yph03b'
      m=m+1
      g_beam_ntuple_tag(m)= 'dbdl'
      m=m+1
      g_beam_ntuple_tag(m)= 'diset'
      m=m+1
      g_beam_ntuple_tag(m)= 'diread'
      m=m+1
      g_beam_ntuple_tag(m)= 'd00v_bdl'  !20
      m=m+1
      g_beam_ntuple_tag(m)= 'd00v_s'
      m=m+1
      g_beam_ntuple_tag(m)= 'd00vm'
      m=m+1
      g_beam_ntuple_tag(m)= '01h_bdl'
      m=m+1
      g_beam_ntuple_tag(m)= '01h_s'
      m=m+1
      g_beam_ntuple_tag(m)= '01hm' !25     
      m=m+1
      g_beam_ntuple_tag(m)= '02v_bdl'
      m=m+1
      g_beam_ntuple_tag(m)= '02v_s'     
      m=m+1
      g_beam_ntuple_tag(m)= '02vm'     
      m=m+1
      g_beam_ntuple_tag(m)= '03h_bdl'     
      m=m+1
      g_beam_ntuple_tag(m)= '03h_s'   !30   
      m=m+1
      g_beam_ntuple_tag(m)= '03hm'     
      m=m+1
      g_beam_ntuple_tag(m)= '04h_bdl'     
      m=m+1
      g_beam_ntuple_tag(m)= '04h_s'     
      m=m+1
      g_beam_ntuple_tag(m)= '04hm'     
c     ARC BPMs
      m=m+1                  
      g_beam_ntuple_tag(m)= 'xc01'  !35
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc01'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc02'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc02'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc03'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc03' !40
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc04'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc04'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc05'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc05'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc06'  !45
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc06'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc07'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc07'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc08'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc08'  !50
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc10'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc10'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc11'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc11'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc12' !55
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc12' 
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc14'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc14'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc16'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc16' !60
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc17'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc17'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc18'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc18'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc19' !65
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc19'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xc20'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yc20'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'xp4c00'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'yp4c00' !70
      m=m+1                       
      g_beam_ntuple_tag(m)= 'pbeam'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'bcm1'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'bcm2'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'bcm3'
      m=m+1                       
      g_beam_ntuple_tag(m)= 'bcm4' !75
      m=m+1                       
      g_beam_ntuple_tag(m)= '9th_field' 
c     Tohoku's magnets
      m=m+1
      g_beam_ntuple_tag(m)= 'splfield'
      m=m+1
      g_beam_ntuple_tag(m)= 'splstat'
      m=m+1
      g_beam_ntuple_tag(m)= 'spltemp'
      m=m+1
      g_beam_ntuple_tag(m)= 'kdfield'  !80
      m=m+1
      g_beam_ntuple_tag(m)= 'kdstat'
      m=m+1
      g_beam_ntuple_tag(m)= 'kq1field'
      m=m+1
      g_beam_ntuple_tag(m)= 'kq1stat'
      m=m+1
      g_beam_ntuple_tag(m)= 'kq1temp'
      m=m+1
      g_beam_ntuple_tag(m)= 'kq2field' !85
      m=m+1
      g_beam_ntuple_tag(m)= 'kq2stat'
      m=m+1
      g_beam_ntuple_tag(m)= 'kq2temp'
      m=m+1
      g_beam_ntuple_tag(m)= 'edfield'
      m=m+1
      g_beam_ntuple_tag(m)= 'edstat' 
      m=m+1
      g_beam_ntuple_tag(m)= 'eq1field'  !90
      m=m+1
      g_beam_ntuple_tag(m)= 'eq1stat'
      m=m+1
      g_beam_ntuple_tag(m)= 'eq1temp'
      m=m+1
      g_beam_ntuple_tag(m)= 'eq2field'
      m=m+1
      g_beam_ntuple_tag(m)= 'eq2stat' 
      m=m+1
      g_beam_ntuple_tag(m)= 'eq2temp' !95
c     Target and Sieve Slits
      m=m+1
      g_beam_ntuple_tag(m)= 'kss_pos'
      m=m+1
      g_beam_ntuple_tag(m)= 'ess_pos'
      m=m+1
      g_beam_ntuple_tag(m)= 'tgt_pos'
      m=m+1
      g_beam_ntuple_tag(m)= 'tgt_id' 
      m=m+1
      g_beam_ntuple_tag(m)= 'tgt_pid' ! target folder id   !100
c     Fast Feed Back
      m=m+1
      g_beam_ntuple_tag(m)= 'ffb_c_st'
      m=m+1
      g_beam_ntuple_tag(m)= 'ffb_c_us'
      m=m+1
      g_beam_ntuple_tag(m)= 'ffb_c_on'
c     Beamline magnets in the HallC
      m=m+1
      g_beam_ntuple_tag(m)= 'dz1_bdl' 
      m=m+1
      g_beam_ntuple_tag(m)= 'fz3_bdl' !105
      m=m+1
      g_beam_ntuple_tag(m)= 'sp4_bdl'
      m=m+1
      g_beam_ntuple_tag(m)= 'co4v_bdl'
      m=m+1
      g_beam_ntuple_tag(m)= 'dw4ah_bdl'
      m=m+1
      g_beam_ntuple_tag(m)= 'dw4bh_bdl'
      m=m+1
      g_beam_ntuple_tag(m)= 'ediout' !110
      m=m+1
      g_beam_ntuple_tag(m)= 'kdiout'

*
      g_beam_Ntuple_size= m     !total size
***********end insert description of contents of BEAM tuple********
*
      title= g_beam_Ntuple_title
      IF(title.EQ.' ') THEN
        msg= name//' '//g_beam_Ntuple_file
        call only_one_blank(msg)
        title= msg   
        g_beam_Ntuple_title= title
      ENDIF
*

      id= g_beam_Ntuple_ID
      io= g_beam_Ntuple_IOchannel
      name= g_beam_Ntuple_name
      title= g_beam_Ntuple_title
      size= g_beam_Ntuple_size
      file= g_beam_Ntuple_file
      bank= default_bank
 
      call HBOOKN(id,title,size,name,bank,g_beam_Ntuple_tag)      !create Ntuple
*
      call HCDIR(g_beam_Ntuple_directory,'R')      !record Ntuple directory
*

      CALL HCDIR(directory,' ')       !reset CERNLIB directory

*
      g_beam_Ntuple_exists= HEXIST(g_beam_Ntuple_ID)
      ABORT= .NOT.g_beam_Ntuple_exists
*
      iv(1)= id
      iv(2)= io
      pat= 'Ntuple id#$ [' // g_beam_Ntuple_directory // '/]' // 
     &                         name // ' IO#$ "' // file // '"'
      call G_build_note(pat,'$',iv,' ',rv,' ',msg)
      call sub_string(msg,' /]','/]')
*
      IF(ABORT) THEN
        err= ':unable to create '//msg
        call G_add_path(here,err)
C      ELSE
C        pat= ':created '//msg
C        call G_add_path(here,pat)
C        call G_log_message('INFO: '//pat)
      ENDIF
*
      RETURN
      END  
