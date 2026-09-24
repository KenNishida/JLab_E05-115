      subroutine g_extract_epics_info
*
* Subroutine to extract EPICS information (mostly beam related) 
* and put into the common block /g_epics_events/
*
* Revision 2.0 February 28, 2000  jinghua
* (JLiu) This is a brand new version and is totally different from
* the original g_extract_bpm_info.f by P.Gueye and J.Reinhold.
*
*--------------------------------------------------------
      implicit none
      save

      character		buffer*12000
      equivalence	(craw(5), buffer)
      integer		find_firstchar
      integer		i,j,evlen,i_blank,linelen
      integer		sli_espreadm_flag
      integer		g_important_length
      real*4		value
      character    char_name*4000

      include		'gen_craw.cmn'
      include		'gen_epics.cmn'
      include		'gen_event_info.cmn'
      Include           'gen_data_structures.cmn'
      Include           'gen_run_info.cmn'
*--------------------------------------------------------

c
c ==> Break up the text stored in the event array into individual lines.
c
c ==> Check length of buffer
c
      evlen=g_important_length(buffer(1:4*(craw(3)-1)))
c
c ==> Main loop: while lower than buffer length
c
      sli_espreadm_flag=0
      i = 1
      do while (i.lt.evlen)
       j = find_firstchar(buffer,evlen, i, 10)

       if (j.le.i) goto 20
       if(j-i.lt.4000) then
         linelen=j-i
         char_name=buffer(i:j-1)
       else
         linelen=3999
         char_name=buffer(i:i+3998)
       endif
       value = 0.
       i_blank=find_firstchar(char_name,linelen,1,9)

c       if(i_blank.lt.j-i-1) then
       if(i_blank.lt.j-i) then ! matsu 2006/05/29
          read (char_name(i_blank+1:j-i),*,err=20) value

c          write(*,*) "char_name:",char_name(1:30)
c     ==> Target BPM position information
c    
c    BPM@Hall

          if (char_name(1:12).eq.'IPM3H00.XPOS') then
             gepics_xph00=value
c             write(*,*) "IPM3H00.XPOS=",gepics_xph00
          else if (char_name(1:12).eq.'IPM3H00.YPOS')  then
             gepics_yph00=value
c             write(*,*) "IPM3H00.YPOS=",gepics_yph00
          else if (char_name(1:12).eq.'IPM3H00.XALP') then
             gepics_xah00=value
c             write(*,*) "IPM3H00.XALP=", gepics_xah00
          else if (char_name(1:12).eq.'IPM3H00.YALP') then
             gepics_yah00=value
c             write(*,*) "IPM3H00.YALP=", gepics_yah00
          else if (char_name(1:7).eq.'IPM3H00') then
             gepics_h00=value
c             write(*,*) "IPM3H00=",gepics_h00
          endif

          if (char_name(1:12).eq.'IPM3H01.XPOS') then
             gepics_xph01=value
c             write(*,*) "IPM3H01.XPOS=",gepics_xph01
          else if (char_name(1:12).eq.'IPM3H01.YPOS')  then
             gepics_yph01=value
c             write(*,*) "IPM3H01.YPOS=",gepics_yph01
          else if (char_name(1:12).eq.'IPM3H01.XRAW') then
             gepics_xrh01=value
c             write(*,*) "IPM3H01.XRAW=", gepics_xrh01
          else if (char_name(1:12).eq.'IPM3H01.YRAW') then
             gepics_yrh01=value
c             write(*,*) "IPM3H01.YRAW=", gepics_yrh01
          else if (char_name(1:12).eq.'IPM3H01.XALP') then
             gepics_xah01=value
c             write(*,*) "IPM3H01.XALP=", gepics_xah01
          else if (char_name(1:12).eq.'IPM3H01.YALP') then
             gepics_yah01=value
c             write(*,*) "IPM3H01.YALP=", gepics_yah01
          endif

          if (char_name(1:13).eq.'IPM3H02A.XPOS') then
             gepics_xph02a=value
c             write(*,*) "IPM3H02A.XPOS=",gepics_xph02a
          else if (char_name(1:13).eq.'IPM3H02A.YPOS')  then
             gepics_yph02a=value
c             write(*,*) "IPM3H02A.YPOS=",gepics_yph02a
          else if (char_name(1:13).eq.'IPM3H02A.XRAW') then
             gepics_xrh02a=value
c             write(*,*) "IPM3H02A.XRAW=",gepics_xrh02a
          else if (char_name(1:13).eq.'IPM3H02A.YRAW') then
             gepics_yrh02a=value
c             write(*,*) "IPM3H02A.YRAW=",gepics_yrh02a
          else if (char_name(1:13).eq.'IPM3H02A.XALP') then
             gepics_xah02a=value
c             write(*,*) "IPM3H02A.XALP=",gepics_xah02a
          else if (char_name(1:13).eq.'IPM3H02A.YALP') then
             gepics_yah02a=value
c             write(*,*) "IPM3H02A.YALP=",gepics_yah02a
          endif

          if (char_name(1:13).eq.'IPM3H02B.XPOS')  then
             gepics_xph02b=value
c             write(*,*) "IPM3H02B.XPOS=",gepics_xph02b
          else if (char_name(1:13).eq.'IPM3H02B.YPOS')  then
             gepics_yph02b=value
c             write(*,*) "IPM3H02B.YPOS=",gepics_yph02b
          else if (char_name(1:13).eq.'IPM3H02B.XRAW') then 
             gepics_xrh02b=value
c             write(*,*) "IPM3H02B.XRAW=",gepics_xrh02b
          else if (char_name(1:13).eq.'IPM3H02B.YRAW') then 
             gepics_yrh02b=value
c             write(*,*) "IPM3H02B.YRAW=",gepics_yrh02b
          else if (char_name(1:13).eq.'IPM3H02B.XALP') then 
             gepics_xah02b=value
c             write(*,*) "IPM3H02B.XALP=",gepics_xah02b
          else if (char_name(1:13).eq.'IPM3H02B.YALP') then 
             gepics_yah02b=value
c             write(*,*) "IPM3H02B.YALP=",gepics_yah02b
          end if
          
          if (char_name(1:13).eq.'IPM3H02C.XPOS')  then
             gepics_xph02c=value
c             write(*,*) "IPM3H02C.XPOS=",gepics_xph02c
          else if (char_name(1:13).eq.'IPM3H02C.YPOS')  then
             gepics_yph02c=value
c             write(*,*) "IPM3H02C.YPOS=",gepics_yph02c
          else if (char_name(1:13).eq.'IPM3H02C.XRAW') then 
             gepics_xrh02c=value
c             write(*,*) "IPM3H02C.XRAW=", gepics_xrh02c
          else if (char_name(1:13).eq.'IPM3H02C.YRAW') then 
             gepics_yrh02c=value
c             write(*,*) "IPM3H02C.YRAW=", gepics_yrh02c
          else if (char_name(1:13).eq.'IPM3H02C.XALP') then 
             gepics_xah02c=value
c             write(*,*) "IPM3H02C.XALP=", gepics_xah02c
          else if (char_name(1:13).eq.'IPM3H02C.YALP') then 
             gepics_yah02c=value
c             write(*,*) "IPM3H02C.YALP=", gepics_yah02c
          end if
          
          if (char_name(1:13).eq.'IPM3H03A.XPOS') then
             gepics_xph03a=value
c             write(*,*) "IPM3H03A.XPOS=",gepics_xph03a
          else if (char_name(1:13).eq.'IPM3H03A.YPOS')  then
             gepics_yph03a=value
c             write(*,*) "IPM3H03A.YPOS=",gepics_yph03a
          else if (char_name(1:13).eq.'IPM3H03A.XRAW') then
             gepics_xrh03a=value
c             write(*,*) "IPM3H03A.XRAW=",gepics_xrh03a
          else if (char_name(1:13).eq.'IPM3H03A.YRAW') then
             gepics_yrh03a=value
c             write(*,*) "IPM3H03A.YRAW=",gepics_yrh03a
          else if (char_name(1:13).eq.'IPM3H03A.XALP') then
             gepics_xah03a=value
c             write(*,*) "IPM3H03A.XALP=",gepics_xah03a
          else if (char_name(1:13).eq.'IPM3H03A.YALP') then
             gepics_yah03a=value
c             write(*,*) "IPM3H03A.YALP=",gepics_yah03a
          endif

          if (char_name(1:13).eq.'IPM3H03B.XPOS') then
             gepics_xph03b=value
c             write(*,*) "IPM3H03B.XPOS=",gepics_xph03b
          else if (char_name(1:13).eq.'IPM3H03B.YPOS')  then
             gepics_yph03b=value
c             write(*,*) "IPM3H03B.YPOS=",gepics_yph03b
          else if (char_name(1:13).eq.'IPM3H03B.XRAW') then
             gepics_xrh03b=value
c             write(*,*) "IPM3H03B.XRAW=",gepics_xrh03b
          else if (char_name(1:13).eq.'IPM3H03B.YRAW') then
             gepics_yrh03b=value
c             write(*,*) "IPM3H03B.YRAW=",gepics_yrh03b
          else if (char_name(1:13).eq.'IPM3H03B.XALP') then
             gepics_xah03b=value
c             write(*,*) "IPM3H03B.XALP=",gepics_xah03b
          else if (char_name(1:13).eq.'IPM3H03B.YALP') then
             gepics_yah03b=value
c             write(*,*) "IPM3H03B.YALP=",gepics_yah03b
          endif


c          if (char_name(1:13).eq.'IPM3H03A.XPOS') 
c     &         gepics_xph03a=value
c          
c          if (char_name(1:13).eq.'IPM3H03A.YPOS') 
c     &         gepics_yph03a=value
c          
c          if (char_name(1:13).eq.'IPM3H03B.XPOS') 
c     &         gepics_xph03b=value
c
c          if (char_name(1:13).eq.'IPM3H03B.YPOS') 
c     &         gepics_yph03b=value
c          
c     ==> Magnet information
c     
c     ARC magnets
          if (char_name(1:10).eq.'MBSY3C.BDL') then
             gepics_dbdl=value
c             write(*,*) "MBSY3C.BDL=", gepics_dbdl
          else if (char_name(1:8).eq.'MBSY3C.S')  then
             gepics_diset=value
c             write(*,*) "MBSY3C.S=", gepics_diset
          else if (char_name(1:7).eq.'MBSY3CM')  then
             gepics_diread=value
c             write(*,*) "MBY3CM=", gepics_diread
          end if
          
          if (char_name(1:12).eq.'MBD3C00V.BDL') then
             gepics_d00v_bdl=value
c             write(*,*) "MBD3C00V.BDL=", gepics_d00v_bdl
          else if (char_name(1:10).eq.'MBD3C00V.S') then
             gepics_d00v_s=value
c             write(*,*) "MBD3C00V.S=",gepics_d00v_s
          else if (char_name(1:9).eq.'MBD3C00VM') then
             gepics_d00vm=value
c             write(*,*) "MBD3C00VM=",gepics_d00vm
          endif
             
          if (char_name(1:12).eq.'MBC3C01H.BDL') then
             gepics_01h_bdl=value
c             write(*,*) "MBC3C01H.BDL=", gepics_01h_bdl
          else if (char_name(1:10).eq.'MBC3C01H.S') then
             gepics_01h_s=value
c             write(*,*) "MBC3C01H.S=",gepics_01h_s
          else if (char_name(1:9).eq.'MBC3C01HM') then
             gepics_01hm=value
c             write(*,*) "MBC3C01HM=",gepics_01hm
          endif
             
          if (char_name(1:12).eq.'MBC3C02V.BDL') then
             gepics_02v_bdl=value
c             write(*,*) "MBC3C02V.BDL=", gepics_02v_bdl
          else if (char_name(1:10).eq.'MBC3C02V.S') then
             gepics_02v_s=value
c             write(*,*) "MBC3C02V.S=",gepics_02v_s
          else if (char_name(1:9).eq.'MBC3C02VM') then
             gepics_02vm=value
c             write(*,*) "MBC3C02VM=",gepics_02vm
          endif
             
          if (char_name(1:12).eq.'MBC3C03H.BDL') then
             gepics_03h_bdl=value
c             write(*,*) "MBC3C03H.BDL=", gepics_03h_bdl
          else if (char_name(1:10).eq.'MBC3C03H.S') then
             gepics_03h_s=value
c             write(*,*) "MBC3C03H.S=",gepics_03h_s
          else if (char_name(1:9).eq.'MBC3C03HM') then
             gepics_03hm=value
c             write(*,*) "MBC3C03HM=",gepics_03hm
          endif
             
          if (char_name(1:12).eq.'MBC3C04H.BDL') then
             gepics_04h_bdl=value
c             write(*,*) "MBC3C04H.BDL=", gepics_04h_bdl
          else if (char_name(1:10).eq.'MBC3C04H.S') then
             gepics_04h_s=value
c             write(*,*) "MBC3C04H.S=",gepics_04h_s
          else if (char_name(1:9).eq.'MBC3C04HM') then
             gepics_04hm=value
c             write(*,*) "MBC3C04HM=",gepics_04hm
          endif
             
          if (char_name(1:12).eq.'MBC3C05V.BDL') then
             gepics_05v_bdl=value
c             write(*,*) "MBC3C05V.BDL=", gepics_05v_bdl
          else if (char_name(1:10).eq.'MBC3C05V.S') then
             gepics_05v_s=value
c             write(*,*) "MBC3C05V.S=",gepics_05v_s
          else if (char_name(1:9).eq.'MBC3C05VM') then
             gepics_05vm=value
c             write(*,*) "MBC3C05VM=",gepics_05vm
          endif
             
          if (char_name(1:12).eq.'MBC3C06H.BDL') then
             gepics_06h_bdl=value
c             write(*,*) "MBC3C06H.BDL=", gepics_06h_bdl
          else if (char_name(1:10).eq.'MBC3C06H.S') then
             gepics_06h_s=value
c             write(*,*) "MBC3C06H.S=",gepics_06h_s
          else if (char_name(1:9).eq.'MBC3C06HM') then
             gepics_06hm=value
c             write(*,*) "MBC3C06HM=",gepics_06hm
          endif
             
          if (char_name(1:12).eq.'MBC3C07H.BDL') then
             gepics_07h_bdl=value
c             write(*,*) "MBC3C07H.BDL=", gepics_07h_bdl
          else if (char_name(1:10).eq.'MBC3C07H.S') then
             gepics_07h_s=value
c             write(*,*) "MBC3C07H.S=",gepics_07h_s
          else if (char_name(1:9).eq.'MBC3C07HM') then
             gepics_07hm=value
c             write(*,*) "MBC3C07HM=",gepics_07hm
          endif

          if (char_name(1:12).eq.'MBC3C07V.BDL') then
             gepics_07v_bdl=value
c             write(*,*) "MBC3C07V.BDL=", gepics_07v_bdl
          else if (char_name(1:10).eq.'MBC3C07V.S') then
             gepics_07v_s=value
c             write(*,*) "MBC3C07V.S=",gepics_07v_s
          else if (char_name(1:9).eq.'MBC3C07VM') then
             gepics_07vm=value
c             write(*,*) "MBC3C07VM=",gepics_07vm
          endif
             
          if (char_name(1:12).eq.'MBC3C09V.BDL') then
             gepics_09v_bdl=value
c             write(*,*) "MBC3C09V.BDL=", gepics_09v_bdl
          else if (char_name(1:10).eq.'MBC3C09V.S') then
             gepics_09v_s=value
c             write(*,*) "MBC3C09V.S=",gepics_09v_s
          else if (char_name(1:9).eq.'MBC3C09VM') then
             gepics_09vm=value
c             write(*,*) "MBC3C09VM=",gepics_09vm
          endif
             
          if (char_name(1:12).eq.'MBC3C11V.BDL') then
             gepics_11v_bdl=value
c             write(*,*) "MBC3C11V.BDL=", gepics_11v_bdl
          else if (char_name(1:10).eq.'MBC3C11V.S') then
             gepics_11v_s=value
c             write(*,*) "MBC3C11V.S=",gepics_11v_s
          else if (char_name(1:9).eq.'MBC3C11VM') then
             gepics_11vm=value
c             write(*,*) "MBC3C11VM=",gepics_11vm
          endif
             
          if (char_name(1:12).eq.'MBC3C13V.BDL') then
             gepics_13v_bdl=value
c             write(*,*) "MBC3C13V.BDL=", gepics_13v_bdl
          else if (char_name(1:10).eq.'MBC3C13V.S') then
             gepics_13v_s=value
c             write(*,*) "MBC3C13V.S=",gepics_13v_s
          else if (char_name(1:9).eq.'MBC3C13VM') then
             gepics_13vm=value
c             write(*,*) "MBC3C13VM=",gepics_13vm
          endif
             
          if (char_name(1:12).eq.'MBC3C15V.BDL') then
             gepics_15v_bdl=value
c             write(*,*) "MBC3C15V.BDL=", gepics_15v_bdl
          else if (char_name(1:10).eq.'MBC3C15V.S') then
             gepics_15v_s=value
c             write(*,*) "MBC3C15V.S=",gepics_15v_s
          else if (char_name(1:9).eq.'MBC3C15VM') then
             gepics_15vm=value
c             write(*,*) "MBC3C15VM=",gepics_15vm
          endif

          if (char_name(1:12).eq.'MBC3C17V.BDL') then
             gepics_17v_bdl=value
c             write(*,*) "MBC3C17V.BDL=", gepics_17v_bdl
          else if (char_name(1:10).eq.'MBC3C17V.S') then
             gepics_17v_s=value
c             write(*,*) "MBC3C17V.S=",gepics_17v_s
          else if (char_name(1:9).eq.'MBC3C17VM') then
             gepics_17vm=value
c             write(*,*) "MBC3C17VM=",gepics_17vm
          endif

          if (char_name(1:12).eq.'MBC3C18H.BDL') then
             gepics_18h_bdl=value
c             write(*,*) "MBC3C18H.BDL=", gepics_18h_bdl
          else if (char_name(1:10).eq.'MBC3C18H.S') then
             gepics_18h_s=value
c             write(*,*) "MBC3C18H.S=",gepics_18h_s
          else if (char_name(1:9).eq.'MBC3C18HM') then
             gepics_18hm=value
c             write(*,*) "MBC3C18HM=",gepics_18hm
          endif
             
          if (char_name(1:12).eq.'MBC3C18V.BDL') then
             gepics_18v_bdl=value
c             write(*,*) "MBC3C18V.BDL=", gepics_18v_bdl
          else if (char_name(1:10).eq.'MBC3C18V.S') then
             gepics_18v_s=value
c             write(*,*) "MBC3C18V.S=",gepics_18v_s
          else if (char_name(1:9).eq.'MBC3C18VM') then
             gepics_18vm=value
c             write(*,*) "MBC3C18VM=",gepics_18vm
          endif
             
          if (char_name(1:12).eq.'MBC3C20H.BDL') then
             gepics_20h_bdl=value
c             write(*,*) "MBC3C20H.BDL=", gepics_20h_bdl
          else if (char_name(1:10).eq.'MBC3C20H.S') then
             gepics_20h_s=value
c             write(*,*) "MBC3C20H.S=",gepics_20h_s
          else if (char_name(1:9).eq.'MBC3C20HM') then
             gepics_20hm=value
c             write(*,*) "MBC3C20HM=",gepics_20hm
          endif
             
          if (char_name(1:12).eq.'MBC3C20V.BDL') then
             gepics_20v_bdl=value
c             write(*,*) "MBC3C20V.BDL=", gepics_20v_bdl
          else if (char_name(1:10).eq.'MBC3C20V.S') then
             gepics_20v_s=value
c             write(*,*) "MBC3C20V.S=",gepics_20v_s
          else if (char_name(1:9).eq.'MBC3C20VM') then
             gepics_20vm=value
c             write(*,*) "MBC3C20VM=",gepics_20vm
          endif

          if (char_name(1:11).eq.'MQA3C01.BDL') then
             gepics_q01_bdl=value
c             write(*,*) "MQA3C01.BDL=", gepics_q01_bdl
          else if (char_name(1:9).eq.'MQA3C01.S') then
             gepics_q01_s=value
c             write(*,*) "MQA3C01.S=",gepics_q01_s
          else if (char_name(1:8).eq.'MQA3C01M') then
             gepics_q01m=value
c             write(*,*) "MQA3C01M=",gepics_q01m
          endif

          if (char_name(1:11).eq.'MQA3C02.BDL') then
             gepics_q02_bdl=value
c             write(*,*) "MQA3C02.BDL=", gepics_q02_bdl
          else if (char_name(1:9).eq.'MQA3C02.S') then
             gepics_q02_s=value
c             write(*,*) "MQA3C02.S=",gepics_q02_s
          else if (char_name(1:8).eq.'MQA3C02M') then
             gepics_q02m=value
c             write(*,*) "MQA3C02M=",gepics_q02m
          endif

          if (char_name(1:11).eq.'MQA3C03.BDL') then
             gepics_q03_bdl=value
c             write(*,*) "MQA3C03.BDL=", gepics_q03_bdl
          else if (char_name(1:9).eq.'MQA3C03.S') then
             gepics_q03_s=value
c             write(*,*) "MQA3C03.S=",gepics_q03_s
          else if (char_name(1:8).eq.'MQA3C03M') then
             gepics_q03m=value
c             write(*,*) "MQA3C03M=",gepics_q03m
          endif

          if (char_name(1:11).eq.'MQA3C04.BDL') then
             gepics_q04_bdl=value
c             write(*,*) "MQA3C04.BDL=", gepics_q04_bdl
          else if (char_name(1:9).eq.'MQA3C04.S') then
             gepics_q04_s=value
c             write(*,*) "MQA3C04.S=",gepics_q04_s
          else if (char_name(1:8).eq.'MQA3C04M') then
             gepics_q04m=value
c             write(*,*) "MQA3C04M=",gepics_q04m
          endif
          if (char_name(1:11).eq.'MQA3C05.BDL') then
             gepics_q05_bdl=value
c             write(*,*) "MQA3C05.BDL=", gepics_q05_bdl
          else if (char_name(1:9).eq.'MQA3C05.S') then
             gepics_q05_s=value
c             write(*,*) "MQA3C05.S=",gepics_q05_s
          else if (char_name(1:8).eq.'MQA3C05M') then
             gepics_q05m=value
c             write(*,*) "MQA3C05M=",gepics_q05m
          endif

          if (char_name(1:11).eq.'MQA3C06.BDL') then
             gepics_q06_bdl=value
c             write(*,*) "MQA3C06.BDL=", gepics_q06_bdl
          else if (char_name(1:9).eq.'MQA3C06.S') then
             gepics_q06_s=value
c             write(*,*) "MQA3C06.S=",gepics_q06_s
          else if (char_name(1:8).eq.'MQA3C06M') then
             gepics_q06m=value
c             write(*,*) "MQA3C06M=",gepics_q06m
          endif

          if (char_name(1:11).eq.'MQA3C07.BDL') then
             gepics_q07_bdl=value
c             write(*,*) "MQA3C07.BDL=", gepics_q07_bdl
          else if (char_name(1:9).eq.'MQA3C07.S') then
             gepics_q07_s=value
c             write(*,*) "MQA3C07.S=",gepics_q07_s
          else if (char_name(1:8).eq.'MQA3C07M') then
             gepics_q07m=value
c             write(*,*) "MQA3C07M=",gepics_q07m
          endif

          if (char_name(1:11).eq.'MQA3C08.BDL') then
             gepics_q08_bdl=value
c             write(*,*) "MQA3C08.BDL=",gepics_q08_bdl
          else if (char_name(1:9).eq.'MQA3C08.S') then
             gepics_q08_s=value
c             write(*,*) "MQA3C08.S=",gepics_q08_s
          else if (char_name(1:8).eq.'MQA3C08M') then
             gepics_q08m=value
c             write(*,*) "MQA3C08M=",gepics_q08m
          endif

          if (char_name(1:11).eq.'MQA3C11.BDL') then
             gepics_q11_bdl=value
c             write(*,*) "MQA3C11.BDL=",gepics_q11_bdl
          else if (char_name(1:9).eq.'MQA3C11.S') then
             gepics_q11_s=value
c             write(*,*) "MQA3C11.S=",gepics_q11_s
          else if (char_name(1:8).eq.'MQA3C11M') then
             gepics_q11m=value
c             write(*,*) "MQA3C11M=",gepics_q11m
          endif
             
          if (char_name(1:11).eq.'MQA3C12.BDL') then
             gepics_q12_bdl=value
c             write(*,*) "MQA3C12.BDL=",gepics_q12_bdl
          else if (char_name(1:9).eq.'MQA3C12.S') then
             gepics_q12_s=value
c             write(*,*) "MQA3C12.S=",gepics_q12_s
          else if (char_name(1:8).eq.'MQA3C12M') then
             gepics_q12m=value
c             write(*,*) "MQA3C12M=",gepics_q12m
          endif
             
          if (char_name(1:11).eq.'MQA3C13.BDL') then
             gepics_q13_bdl=value
c             write(*,*) "MQA3C13.BDL=",gepics_q13_bdl
          else if (char_name(1:9).eq.'MQA3C13.S') then
             gepics_q13_s=value
c             write(*,*) "MQA3C13.S=",gepics_q13_s
          else if (char_name(1:8).eq.'MQA3C13M') then
             gepics_q13m=value
c             write(*,*) "MQA3C13M=",gepics_q13m
          endif
             
          if (char_name(1:11).eq.'MQA3C16.BDL') then
             gepics_q16_bdl=value
c             write(*,*) "MQA3C16.BDL=",gepics_q16_bdl
          else if (char_name(1:9).eq.'MQA3C16.S') then
             gepics_q16_s=value
c             write(*,*) "MQA3C16.S=",gepics_q16_s
          else if (char_name(1:8).eq.'MQA3C16M') then
             gepics_q16m=value
c             write(*,*) "MQA3C16M=",gepics_q16m
          endif
             
          if (char_name(1:11).eq.'MQA3C17.BDL') then
             gepics_q17_bdl=value
c             write(*,*) "MQA3C17.BDL=",gepics_q17_bdl
          else if (char_name(1:9).eq.'MQA3C17.S') then
             gepics_q17_s=value
c             write(*,*) "MQA3C17.S=",gepics_q17_s
          else if (char_name(1:8).eq.'MQA3C17M') then
             gepics_q17m=value
c             write(*,*) "MQA3C17M=",gepics_q17m
          endif
             
          if (char_name(1:11).eq.'MQA3C18.BDL') then
             gepics_q18_bdl=value
c             write(*,*) "MQA3C18.BDL=",gepics_q18_bdl
          else if (char_name(1:9).eq.'MQA3C18.S') then
             gepics_q18_s=value
c             write(*,*) "MQA3C18.S=",gepics_q18_s
          else if (char_name(1:8).eq.'MQA3C18M') then
             gepics_q18m=value
c             write(*,*) "MQA3C18M=",gepics_q18m
          endif
             
          if (char_name(1:11).eq.'MQA3C19.BDL') then
             gepics_q19_bdl=value
c             write(*,*) "MQA3C19.BDL=",gepics_q19_bdl
          else if (char_name(1:9).eq.'MQA3C19.S') then
             gepics_q19_s=value
c             write(*,*) "MQA3C19.S=",gepics_q19_s
          else if (char_name(1:8).eq.'MQA3C19M') then
             gepics_q19m=value
c             write(*,*) "MQA3C19M=",gepics_q19m
          endif
             
          if (char_name(1:12).eq.'MQA3C20A.BDL') then
             gepics_q20a_bdl=value
c             write(*,*) "MQA3C20A.BDL=",gepics_q20a_bdl
          else if (char_name(1:10).eq.'MQA3C20A.S') then
             gepics_q20a_s=value
c             write(*,*) "MQA3C20A.S=",gepics_q20a_s
          else if (char_name(1:9).eq.'MQA3C20AM') then
             gepics_q20am=value
c             write(*,*) "MQA3C20AM=",gepics_q20am
          endif
             
          if (char_name(1:9).eq.'HCNMR:SIG') then !9th dipole
             gepics_9th_field=value
c             write(*,*) "HNMR:SIG=", gepics_9th_field
          end if

c     HallC magnets

          if (char_name(1:11).eq.'MDZ3H01.BDL') then
             gepics_dz01_bdl=value
c             write(*,*) "MDZ3H01.BDL=",gepics_dz01_bdl
          else if (char_name(1:10).eq.'MDZ3H01.B2') then
             gepics_dz01_b2=value
c             write(*,*) "MDZ3H01.B2=",gepics_dz01_b2
          else if (char_name(1:8).eq.'MDZ3H01M') then
             gepics_dz01m=value
c             write(*,*) "MDZ3H01M=",gepics_dz01m
          else if (char_name(1:7).eq.'MDZ3H01') then
             gepics_dz01=value
c             write(*,*) "MDZ3H01=",gepics_dz01
          endif

          if (char_name(1:11).eq.'MFZ3H03.BDL') then
             gepics_fz03_bdl=value
c             write(*,*) "MFZ3H03.BDL=",gepics_fz03_bdl
          else if (char_name(1:10).eq.'MFZ3H03.B2') then
             gepics_fz03_b2=value
c             write(*,*) "MFZ3H03.B2=",gepics_fz03_b2
          else if (char_name(1:8).eq.'MFZ3H03M') then
             gepics_fz03m=value
c             write(*,*) "MFZ3H03M=",gepics_fz03m
          else if (char_name(1:7).eq.'MFZ3H03') then
             gepics_fz03=value
c             write(*,*) "MFZ3H03=",gepics_fz03
          endif

          if (char_name(1:11).eq.'MSP3H04.BDL') then
             gepics_sp04_bdl=value
c             write(*,*) "MSP3H04.BDL=",gepics_sp04_bdl
          else if (char_name(1:10).eq.'MSP3H04.B2') then
             gepics_sp04_b2=value
c             write(*,*) "MSP3H04.B2=",gepics_sp04_b2
          endif

          if (char_name(1:12).eq.'MCO3H04V.BDL') then
             gepics_co04v_bdl=value
c             write(*,*) "MCO3H04V.BDL=",gepics_co04v_bdl
          else if (char_name(1:9).eq.'MCO3H04VM') then
             gepics_co04vm=value
c             write(*,*) "MCO3H04VM=",gepics_co04vm
          else if (char_name(1:8).eq.'MCO3H04V') then
             gepics_co04v=value
c             write(*,*) "MCO3H04V=",gepics_co04v
          endif

          if (char_name(1:13).eq.'MDW3H04AH.BDL') then
             gepics_dw04ah_bdl=value
c             write(*,*) "MDW3H04AH.BDL=",gepics_dw04ah_bdl
          else if (char_name(1:12).eq.'MDW3H04AH.B2') then
             gepics_dw04ah_b2=value
c             write(*,*) "MDW3H04AH.B2=",gepics_dw04ah_b2
          else if (char_name(1:10).eq.'MDW3H04AHM') then
             gepics_dw04ahm=value
c             write(*,*) "MDW3H04AHM=",gepics_dw04ahm
          else if (char_name(1:9).eq.'MDW3H04AH') then
             gepics_dw04ah=value
c             write(*,*) "MDW3H04AH=",gepics_dw04ah
          endif

          if (char_name(1:13).eq.'MDW3H04BH.BDL') then
             gepics_dw04bh_bdl=value
c             write(*,*) "MDW3H04BH.BDL=",gepics_dw04bh_bdl
          else if (char_name(1:14).eq.'MDW3H04BH.SETI') then
             gepics_dw04bh_seti=value
c             write(*,*) "MDW3H04BH.SETI=",gepics_dw04bh_seti
          else if (char_name(1:17).eq.'MDW3H04BHreadcalc') then
             gepics_dw04bh_readcalc=value
c             write(*,*) "MDW3H04BHreadcalc=",gepics_dw04bh_readcalc
          endif

c     Tohoku's magnets

          if (char_name(1:16).eq.'hks:spl_hp_field') then
             gepics_spl_field=value
c             write(*,*) "hks:spl_hp_field=", gepics_spl_field
          end if
          
          if (char_name(1:15).eq.'hks:spl_hp_temp') then
             gepics_spl_temp=value
c             write(*,*) "hks:spl_hp_temp=", gepics_spl_temp
          end if
          
          if (char_name(1:18).eq.'hks:spl_hp_dfilter') then
             gepics_spl_dfil=value
c             write(*,*) "hks:spl_hp_dfilter=", gepics_spl_dfil
          end if
          
          if (char_name(1:16).eq.'hks:spl_hp_range') then
             gepics_spl_range=value
c             write(*,*) "hks:spl_hp_range=", gepics_spl_range
          end if
          
          if (char_name(1:17).eq.'hks:spl_hp_status') then
             gepics_kq1_stat=value
c             write(*,*) "hks:kq1_hp_status=", gepics_kq1_stat
          end if
          
          if (char_name(1:16).eq.'hks:kd_nmr_field') then
             gepics_kd_field=value
c             write(*,*) "hks:kd_nmr_field=", gepics_kd_field
          end if

          if (char_name(1:17).eq.'hks:kd_nmr_status') then
             gepics_kd_stat=value
c             write(*,*) "hks:kd_nmr_status=", gepics_kd_stat
          end if

          if (char_name(1:16).eq.'hks:kq1_hp_field') then
             gepics_kq1_field=value
c             write(*,*) "hks:kq1_hp_field=", gepics_kq1_field
          end if
          
          if (char_name(1:15).eq.'hks:kq1_hp_temp') then
             gepics_kq1_temp=value
c             write(*,*) "hks:kq1_hp_temp=", gepics_kq1_temp
          end if
          
          if (char_name(1:18).eq.'hks:kq1_hp_dfilter') then
             gepics_kq1_dfil=value
c             write(*,*) "hks:kq1_hp_dfilter=", gepics_kq1_dfil
          end if
          
          if (char_name(1:16).eq.'hks:kq1_hp_range') then
             gepics_kq1_range=value
c             write(*,*) "hks:kq1_hp_range=", gepics_kq1_range
          end if
          
          if (char_name(1:17).eq.'hks:kq1_hp_status') then
             gepics_kq1_stat=value
c             write(*,*) "hks:kq1_hp_status=", gepics_kq1_stat
          end if
          
          if (char_name(1:16).eq.'hks:kq2_hp_field') then
             gepics_kq2_field=value
c             write(*,*) "hks:kq2_hp_field=", gepics_kq2_field
          end if
          
          if (char_name(1:15).eq.'hks:kq2_hp_temp') then
             gepics_kq2_temp=value
c             write(*,*) "hks:kq2_hp_temp=", gepics_kq2_temp
          end if
          
          if (char_name(1:18).eq.'hks:kq2_hp_dfilter') then
             gepics_kq2_dfil=value
c             write(*,*) "hks:kq2_hp_dfilter=", gepics_kq2_dfil
          end if
          
          if (char_name(1:16).eq.'hks:kq2_hp_range') then
             gepics_kq2_range=value
c             write(*,*) "hks:kq2_hp_range=", gepics_kq2_range
          end if
          
          if (char_name(1:17).eq.'hks:kq2_hp_status') then
             gepics_kq2_stat=value
c             write(*,*) "hks:kq2_hp_status=", gepics_kq2_stat
          end if

          if (char_name(1:16).eq.'hks:ed_nmr_field') then
             gepics_ed_field=value
c             write(*,*) "hks:ed_nmr_field=", gepics_ed_field
          end if
          
          if (char_name(1:17).eq.'hks:ed_nmr_status') then
             gepics_ed_stat=value
c             write(*,*) "hks:ed_nmr_status=", gepics_ed_stat
          end if

          if (char_name(1:16).eq.'hks:eq1_hp_field') then
             gepics_eq1_field=value
c             write(*,*) "hks:eq1_hp_field=", gepics_eq1_field
          end if
          
          if (char_name(1:15).eq.'hks:eq1_hp_temp') then
             gepics_eq1_temp=value
c             write(*,*) "hks:eq1_hp_temp=", gepics_eq1_temp
          end if
          
          if (char_name(1:18).eq.'hks:eq1_hp_dfilter') then
             gepics_eq1_dfil=value
c             write(*,*) "hks:eq1_hp_dfilter=", gepics_eq1_dfil
          end if
          
          if (char_name(1:16).eq.'hks:eq1_hp_range') then
             gepics_eq1_range=value
c             write(*,*) "hks:eq1_hp_range=", gepics_eq1_range
          end if
          
          if (char_name(1:17).eq.'hks:eq1_hp_status') then
             gepics_eq1_stat=value
c             write(*,*) "hks:eq1_hp_status=", gepics_eq1_stat
          end if

          if (char_name(1:16).eq.'hks:eq2_hp_field') then
             gepics_eq2_field=value
c             write(*,*) "hks:eq2_hp_field=", gepics_eq2_field
          end if
          
          if (char_name(1:15).eq.'hks:eq2_hp_temp') then
             gepics_eq2_temp=value
c             write(*,*) "hks:eq2_hp_temp=", gepics_eq2_temp
          end if
          
          if (char_name(1:18).eq.'hks:eq2_hp_dfilter') then
             gepics_eq2_dfil=value
c             write(*,*) "hks:eq2_hp_dfilter=", gepics_eq2_dfil
          end if
          
          if (char_name(1:16).eq.'hks:eq2_hp_range') then
             gepics_eq2_range=value
c             write(*,*) "hks:eq2_hp_range=", gepics_eq2_range
          end if
          
          if (char_name(1:17).eq.'hks:eq2_hp_status') then
             gepics_eq2_stat=value
c             write(*,*) "hks:eq2_hp_status=", gepics_eq2_stat
          end if

c     ==> Sieve Slit and Target information
          if (char_name(1:16).eq.'hks:hks_sst_posi') then
             gepics_kss_pos=value
c             write(*,*) "hks:hks_sst_posi=", gepics_kss_pos
          end if
          
          if (char_name(1:16).eq.'hks:hes_sst_posi') then
             gepics_ess_pos=value
c             write(*,*) "hks:hes_sst_posi=", gepics_ess_pos
          end if
          
          if (char_name(1:16).eq.'hks:tgt_sst_posi') then
             gepics_tgt_pos=value
c             write(*,*) "hks:tgt_sst_posi=", gepics_tgt_pos
          end if
          
          if (char_name(1:14).eq.'hks:tgt_sst_id') then
             gepics_tgt_id=value
c             write(*,*) "hks:tgt_sst_id=", gepics_tgt_id
          end if
          
          if (gen_run_number.lt.76309) then
             gepics_tgt_pid=1
          else if (gen_run_number.ge.76309 
     &            .and. gen_run_number.lt.76739) then
             gepics_tgt_pid=2
          else
             gepics_tgt_pid=3
          endif

          if (gepics_tgt_id.eq.0.and.gepics_tgt_pid.ge.1
     &         .and.gepics_tgt_pos.ge.70.) then
             gepics_tgt=1 ! water cell
          else if (gepics_tgt_id.eq.1.and.gepics_tgt_pid.ge.1) then
             gepics_tgt=2 ! BeO
          else if (gepics_tgt_id.eq.2.and.gepics_tgt_pid.ge.1) then
             gepics_tgt=3 ! 52Cr
          else if (gepics_tgt_id.eq.3.and.gepics_tgt_pid.eq.1) then
             gepics_tgt=4 ! 10B
          else if (gepics_tgt_id.eq.4.and.gepics_tgt_pid.eq.1) then
             gepics_tgt=5 ! 11B
          else if ((gepics_tgt_id.eq.5.and.gepics_tgt_pid.eq.1).or.
     &             (gepics_tgt_id.eq.6.and.gepics_tgt_pid.eq.3)) then
             gepics_tgt=6 ! 12C
          else if (gepics_tgt_id.eq.6.and.gepics_tgt_pid.eq.1) then
             gepics_tgt=7 ! 7Li
          else if (gepics_tgt_id.ge.3.and.gepics_tgt_id.le.5.and.
     &             gepics_tgt_pid.ge.2) then
             gepics_tgt=8 ! CH2
          else if (gepics_tgt_id.eq.6.and.gepics_tgt_pid.eq.2) then
             gepics_tgt=9 ! 9Be
          else
             if(gen_run_number.eq.76322.or.gen_run_number.eq.76323.or.
     &          gen_run_number.eq.76324.or.gen_run_number.eq.76813) then
                gepics_tgt=8 ! CH2
             else if(gen_run_number.eq.76840.or.gen_run_number.eq.76842
     &               .or.gen_run_number.eq.76843) then
                gepics_tgt=6 ! 12C
             else 
                gepics_tgt=-1
             end if
          end if

          if (char_name(1:16).eq.'hks:ed_mps_i_out') then
             gepics_ed_mps_iout=value
c             write(*,*) "hks:ed_mps_i_out=", gepics_ed_mps_iout
          end if

          if (char_name(1:16).eq.'hks:kd_mps_i_out') then
             gepics_kd_mps_iout=value
c             write(*,*) "hks:kd_mps_i_out=", gepics_kd_mps_iout
          end if

c          write(*,*) "grun=",gen_run_number,", gepics_tgt_pid=",gepics_tgt_pid

cc          if (char_name(1:15).eq.'HLCNMRFIELDRBCK') 
cc     &         gepics_nmr_field=value
          
cc          if (char_name(1:14).eq.'HLCNMRLOCKSTAT') 
cc     &         gepics_nmr_lock=value
          
cc          if (char_name(1:8).eq.'ecarc7t1') 
cc     &         gepics_mag_temp1=value
          
cc          if (char_name(1:8).eq.'ecarc7t2') 
cc     &         gepics_mag_temp2=value
          
cc          if (char_name(1:8).eq.'ecarc7t3') 
cc     &         gepics_mag_temp3=value
          
cc          if (char_name(1:8).eq.'ecarc7t4') 
cc     &         gepics_mag_temp4=value
          
cc          if (char_name(1:8).eq.'ecarc7t5') 
cc     &         gepics_mag_temp5=value
          
c     ==> OTR information
c     
cc          if (char_name(1:15).eq.'hlc:MV_AI_DXPOS') 
cc     &         gepics_otr_x=value
          
cc          if (char_name(1:15).eq.'hlc:MV_AI_DYPOS') 
cc     &         gepics_otr_y=value
          
cc          if (char_name(1:15).eq.'hlc:MV_AI_DXWID') 
cc     &         gepics_otr_widx=value
          
cc          if (char_name(1:15).eq.'hlc:MV_AI_DYWID') 
cc     &         gepics_otr_widy=value
          
cc          if (char_name(1:16).eq.'hlc:MV_LI_MAXPIX') 
cc     &         gepics_otr_maxpix=value
          
cc          if (char_name(1:13).eq.'hlc:MV_LI_SAT') 
cc     &         gepics_otr_sat=value
          
          
c     ==> Fast feedback information
c     
          if (char_name(1:20).eq.'FB_C:status:mbbi2.B7') then
             gepics_ffb_c_stat=value
c             write(*,*) "FB_C:status:mbbi2.B7=",gepics_ffb_c_stat
          else if (char_name(1:11).eq.'FB_C:use_RF') then
             gepics_ffb_c_use=value
c             write(*,*) "FB_C:use_RF=",gepics_ffb_c_use
          else if (char_name(1:10).eq.'FB_C:FB_On') then
             gepics_ffb_c_on=value
c             write(*,*) "FB_C:FB_On=",gepics_ffb_c_on
           endif
          
          if (char_name(1:20).eq.'FB_A:status:mbbi2.B7') then
             gepics_ffb_a_stat=value
c             write(*,*) "FB_A:status:mbbi2.B7=",gepics_ffb_a_stat
          else if (char_name(1:11).eq.'FB_A:use_RF') then
             gepics_ffb_a_use=value
c             write(*,*) "FB_A:use_RF=",gepics_ffb_a_use
          else if (char_name(1:10).eq.'FB_A:FB_On') then
             gepics_ffb_a_on=value
c             write(*,*) "FB_A:FB_On=",gepics_ffb_a_on
           endif
          
c     ==> Energy information
c     
cc          if (char_name(1:9).eq.'hallc_dpp') 
cc     &         gepics_dpp=value
          
cc          if (char_name(1:9).eq.'hallc_MeV') 
cc     &         gepics_ebeam=value
          
          if (char_name(1:7).eq.'HALLC:p') then
             gepics_pbeam=value
c             write(*,*) "HALLC:p=",gepics_pbeam
          end if
          
c     
c     ==> ARC BPM position info
c     
          if (char_name(1:12).eq.'IPM3C01.XPOS') then
             gepics_xc01=value
c             write(*,*) "IPM3C01.XPOS=",gepics_xc01
          else if (char_name(1:12).eq.'IPM3C01.YPOS') then
             gepics_yc01=value
c             write(*,*) "IPM3C01.YPOS=",gepics_yc01
          else if (char_name(1:7).eq.'IPM3C01') then
             gepics_c01=value
c             write(*,*) "IPM3C01=",gepics_c01
          end if
          if (char_name(1:12).eq.'IPM3C02.XPOS') then
             gepics_xc02=value
c             write(*,*) "IPM3C02.XPOS=",gepics_xc02
          else if (char_name(1:12).eq.'IPM3C02.YPOS') then
             gepics_yc02=value
c             write(*,*) "IPM3C02.YPOS=",gepics_yc02
          else if (char_name(1:7).eq.'IPM3C02') then
             gepics_c02=value
c             write(*,*) "IPM3C02=",gepics_c02
          end if
          if (char_name(1:12).eq.'IPM3C03.XPOS') then
             gepics_xc03=value
c             write(*,*) "IPM3C03.XPOS=",gepics_xc03
          else if (char_name(1:12).eq.'IPM3C03.YPOS') then
             gepics_yc03=value
c             write(*,*) "IPM3C03.YPOS=",gepics_yc03
          else if (char_name(1:7).eq.'IPM3C03') then
             gepics_c03=value
c             write(*,*) "IPM3C03=",gepics_c03
          end if
          if (char_name(1:12).eq.'IPM3C04.XPOS') then
             gepics_xc04=value
c             write(*,*) "IPM3C04.XPOS=",gepics_xc04             
          else if (char_name(1:12).eq.'IPM3C04.YPOS') then
             gepics_yc04=value
c             write(*,*) "IPM3C04.YPOS=",gepics_yc04             
          else if (char_name(1:7).eq.'IPM3C04') then
             gepics_c04=value
c             write(*,*) "IPM3C04=",gepics_c04
          end if
          if (char_name(1:12).eq.'IPM3C05.XPOS') then
             gepics_xc05=value
c             write(*,*) "IPM3C05.XPOS=",gepics_xc05
          else if (char_name(1:12).eq.'IPM3C05.YPOS') then
             gepics_yc05=value
c             write(*,*) "IPM3C05.YPOS=",gepics_yc05
          else if (char_name(1:7).eq.'IPM3C05') then
             gepics_c05=value
c             write(*,*) "IPM3C05=",gepics_c05
          end if

          if (char_name(1:12).eq.'IPM3C06.XPOS') then
             gepics_xc06=value
c             write(*,*) "IPM3C06.XPOS=",gepics_xc06
          else if (char_name(1:12).eq.'IPM3C06.YPOS') then
             gepics_yc06=value
c             write(*,*) "IPM3C06.YPOS=",gepics_yc06
          else if (char_name(1:7).eq.'IPM3C06') then
             gepics_c06=value
c             write(*,*) "IPM3C06=",gepics_c06
          end if
          if (char_name(1:12).eq.'IPM3C07.XPOS') then
             gepics_xc07=value
c             write(*,*) "IPM3C07.XPOS=",gepics_xc07
          else if (char_name(1:12).eq.'IPM3C07.YPOS') then
             gepics_yc07=value
c             write(*,*) "IPM3C07.YPOS=",gepics_yc07
          else if (char_name(1:7).eq.'IPM3C07') then
             gepics_c07=value
c             write(*,*) "IPM3C07=",gepics_c07
          end if

          if (char_name(1:12).eq.'IPM3C08.XPOS') then
             gepics_xc08=value
c             write(*,*) "IPM3C08.XPOS=",gepics_xc08
          else if (char_name(1:12).eq.'IPM3C08.YPOS') then
             gepics_yc08=value
c             write(*,*) "IPM3C08.YPOS=",gepics_yc08
          else if (char_name(1:7).eq.'IPM3C08') then
             gepics_c08=value
c             write(*,*) "IPM3C08=",gepics_c08
          end if
          if (char_name(1:12).eq.'IPM3C10.XPOS') then
             gepics_xc10=value
c             write(*,*) "IPM3C10.XPOS=",gepics_xc10
          else if (char_name(1:12).eq.'IPM3C10.YPOS') then
             gepics_yc10=value
c             write(*,*) "IPM3C10.YPOS=",gepics_yc10
          else if (char_name(1:7).eq.'IPM3C10') then
             gepics_c10=value
c             write(*,*) "IPM3C10=",gepics_c10
          end if
          if (char_name(1:12).eq.'IPM3C11.XPOS') then
             gepics_xc11=value
c             write(*,*) "IPM3C11.XPOS=",gepics_xc11
          else if (char_name(1:12).eq.'IPM3C11.YPOS') then
             gepics_yc11=value
c             write(*,*) "IPM3C11.YPOS=",gepics_yc11
          else if (char_name(1:7).eq.'IPM3C11') then
             gepics_c11=value
c             write(*,*) "IPM3C11=",gepics_c11
          end if

          if (char_name(1:12).eq.'IPM3C12.XPOS') then
             gepics_xc12=value
c             write(*,*) "IPM3C12.XPOS=",gepics_xc12
          else if (char_name(1:12).eq.'IPM3C12.YPOS') then
             gepics_yc12=value
c             write(*,*) "IPM3C12.YPOS=",gepics_yc12
          else if (char_name(1:7).eq.'IPM3C12') then
             gepics_c12=value
c             write(*,*) "IPM3C12=",gepics_c12
          end if

          if (char_name(1:12).eq.'IPM3C14.XPOS') then
             gepics_xc14=value
c             write(*,*) "IPM3C14.XPOS=",gepics_xc14
          else if (char_name(1:12).eq.'IPM3C14.YPOS') then
             gepics_yc14=value
c             write(*,*) "IPM3C14.YPOS=",gepics_yc14
          else if (char_name(1:7).eq.'IPM3C14') then
             gepics_c14=value
c             write(*,*) "IPM3C14=",gepics_c14
          end if

          if (char_name(1:12).eq.'IPM3C16.XPOS') then
             gepics_xc16=value
c             write(*,*) "IPM3C16.XPOS=",gepics_xc16
          else if (char_name(1:12).eq.'IPM3C16.YPOS') then
             gepics_yc16=value
c             write(*,*) "IPM3C16.YPOS=",gepics_yc16
          else if (char_name(1:7).eq.'IPM3C16') then
             gepics_c16=value
c             write(*,*) "IPM3C16=",gepics_c16
          end if

          if (char_name(1:12).eq.'IPM3C17.XPOS') then
             gepics_xc17=value
c             write(*,*) "IPM3C17.XPOS=",gepics_xc17
          else if (char_name(1:12).eq.'IPM3C17.YPOS') then
             gepics_yc17=value
c             write(*,*) "IPM3C17.YPOS=",gepics_yc17
          else if (char_name(1:7).eq.'IPM3C17') then
             gepics_c17=value
c             write(*,*) "IPM3C17=",gepics_c17
          end if

          if (char_name(1:12).eq.'IPM3C18.XPOS') then
             gepics_xc18=value
c             write(*,*) "IPM3C18.XPOS=",gepics_xc18
          else if (char_name(1:12).eq.'IPM3C18.YPOS') then
             gepics_yc18=value
c             write(*,*) "IPM3C18.YPOS=",gepics_yc18
          else if (char_name(1:7).eq.'IPM3C18') then
             gepics_c18=value
c             write(*,*) "IPM3C18=",gepics_c18
          end if
          if (char_name(1:12).eq.'IPM3C19.XPOS') then
             gepics_xc19=value
c             write(*,*) "IPM3C19.XPOS=",gepics_xc19
          else if (char_name(1:12).eq.'IPM3C19.YPOS') then
             gepics_yc19=value
c             write(*,*) "IPM3C19.YPOS=",gepics_yc19
          else if (char_name(1:7).eq.'IPM3C19') then
             gepics_c19=value
c             write(*,*) "IPM3C19=",gepics_c19
          end if
          if (char_name(1:12).eq.'IPM3C20.XPOS') then
             gepics_xc20=value
c             write(*,*) "IPM3C20.XPOS=",gepics_xc20
          else if (char_name(1:12).eq.'IPM3C20.YPOS') then
             gepics_yc20=value
c             write(*,*) "IPM3C20.YPOS=",gepics_yc20
          else if (char_name(1:7).eq.'IPM3C20') then
             gepics_c20=value
c             write(*,*) "IPM3C20=",gepics_c20
          end if
          if (char_name(1:12).eq.'IPM4C00.XPOS') then
             gepics_xp4c00=value
c             write(*,*) "IPM4C00.XPOS=",gepics_xp4c00
          else if (char_name(1:12).eq.'IPM4C00.YPOS') then
             gepics_yp4c00=value
c             write(*,*) "IPM4C00.YPOS=",gepics_yp4c00
          else if (char_name(1:12).eq.'IPM4C00.XRAW') then
             gepics_xr4c00=value
c             write(*,*) "IPM4C00.XRAW=",gepics_xr4c00
          else if (char_name(1:12).eq.'IPM4C00.YRAW') then
             gepics_yr4c00=value
c             write(*,*) "IPM4C00.YRAW=",gepics_yr4c00
          else if (char_name(1:7).eq.'IPM4C00') then
             gepics_4c00=value
c             write(*,*) "IPM4C00=",gepics_4c00
          end if
          
C==   > BCM information, here I use ibcm1,2,3. There are also hallc:bcm1,2,3
C     Add ibcm4 (T.M. @ 1/6/2010)
          if (char_name(1:5).eq.'ibcm1') then
             gepics_bcm1=value
c             write(*,*) "ibcm1=",gepics_bcm1
          end if
          
          if (char_name(1:5).eq.'ibcm2') then
             gepics_bcm2=value
c             write(*,*) "ibcm2=",gepics_bcm2
          end if
         
          if (char_name(1:5).eq.'ibcm3') then
             gepics_bcm3=value
c             write(*,*) "ibcm3=",gepics_bcm2
          end if

          if (char_name(1:5).eq.'ibcm4') then
             gepics_bcm4=value
c             write(*,*) "ibcm4=",gepics_bcm4
          end if
          
C==   > SLI information 
          if(char_name(1:17) .eq. 'slic:MV_AO_DYWIDS') then
!     Visual estimate of the X beam size directly from the interference picture
             gepics_sli_dywids = value  
c             write(*,*) "slic:MV_AO_DYWIDS=",gepics_sli_dywids
          else if(char_name(1:17) .eq. 'slic:MV_AO_DYWIDM') then
!     X beam size calculated with the use of the SLI model
             gepics_sli_dywidm = value  ! directly from 
c             write(*,*) "slic:MV_AO_DYWIDM=",gepics_sli_dywidm
          else if(char_name(1:18) .eq. 'slic:MV_AO_DYWIDME') then
!     Model beam size error
             gepics_sli_dywidme = value
c             write(*,*) "slic:MV_AO_DYWIDME=",gepics_sli_dywidme
          else if(char_name(1:18) .eq. 'slic:MV_AO_SPREADS') then
!     Visual estimate of the energy spread
             gepics_sli_spreads = value
c             write(*,*) "slic:MV_AO_SPREADS=",gepics_sli_spreads
          else if(char_name(1:19).eq.'slic:MV_AO_ESPREADM') then
!     Model-based energy spread
             gepics_sli_espreadm = value
c             write(*,*) "slic:MV_AO_ESPREADM=",gepics_sli_espreadm
          else if(char_name(1:20).eq.'slic:MV_AO_ESPREADME') then
!     Model-based energy spread error
             gepics_sli_espreadme = value
c             write(*,*)"slic:MV_AO_ESPREADME=",gepics_sli_espreadme
          else if(char_name(1:17).eq.'slic:MV_BO_YVALID') then
!     Beam size and enrgy spread VALID flag
             gepics_sli_yvalid = value
c             write(*,*)"slic:MV_BO_YVALID=",gepics_sli_yvalid
          endif

          if(char_name(1:17) .eq. 'slia:MV_AO_DYWIDS') then
!     Visual estimate of the X beam size directly from the interference picture
             gepics_slia_dywids = value  
c             write(*,*) "slia:MV_AO_DYWIDS=",gepics_slia_dywids
          else if(char_name(1:17) .eq. 'slia:MV_AO_DYWIDM') then
!     X beam size calculated with the use of the SLI model
             gepics_slia_dywidm = value  ! directly from 
c             write(*,*) "slia:MV_AO_DYWIDM=",gepics_slia_dywidm
          else if(char_name(1:18) .eq. 'slia:MV_AO_DYWIDME') then
!     Model beam size error
             gepics_slia_dywidme = value
c             write(*,*) "slia:MV_AO_DYWIDME=",gepics_slia_dywidme
          else if(char_name(1:18) .eq. 'slia:MV_AO_SPREADS') then
!     Visual estimate of the energy spread
             gepics_slia_spreads = value
c             write(*,*) "slia:MV_AO_SPREADS=",gepics_slia_spreads
          else if(char_name(1:19).eq.'slia:MV_AO_ESPREADM') then
!     Model-based energy spread
             gepics_slia_espreadm = value
c             write(*,*) "slia:MV_AO_ESPREADM=",gepics_slia_espreadm
          else if(char_name(1:20).eq.'slia:MV_AO_ESPREADME') then
!     Model-based energy spread error
             gepics_slia_espreadme = value
c             write(*,*)"slia:MV_AO_ESPREADME=",gepics_slia_espreadme
          else if(char_name(1:17).eq.'slia:MV_BO_YVALID') then
!     Beam size and enrgy spread VALID flag
             gepics_slia_yvalid = value
c             write(*,*)"slia:MV_BO_YVALID=",gepics_slia_yvalid
          endif

cccc matsu 2006/05/29
          if(char_name(1:19) .eq. 'slic:MV_AO_ESPREADM') then 
             if(sli_espreadm_flag.eq.0) then
                gepics_sli_espreadm = value
                sli_espreadm_flag=1
             endif
          endif
          if(char_name(1:20) .eq. 'slic:MV_AO_ESPREADME')
     &         gepics_sli_espreadme = value
          
       endif
       
 20    i = j + 1
      enddo
      
      return
      end
      
c     
c     Find the offset of first character "char_num" in text string "string"
c     
      integer function find_firstchar (string, strlen, i, char_num)
      character*(*) string
      integer strlen, i, j, char_num
      j = i
      do while ((j.le.strlen).and.(ichar(string(j:j)).ne.char_num)
     &     .and.(ichar(string(j:j)).ne.0))
         j = j + 1
      enddo

      find_firstchar = j
      return
      end

