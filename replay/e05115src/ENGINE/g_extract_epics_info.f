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

c     ==> Target BPM position information
c     
          if (char_name(1:13).eq.'IPM3H00A.XPOS') 
     &         gepics_xh00a=value
          
          if (char_name(1:13).eq.'IPM3H00A.YPOS') 
     &         gepics_yh00a=value
          
          if (char_name(1:13).eq.'IPM3H00B.XPOS') 
     &         gepics_xh00b=value

          if (char_name(1:13).eq.'IPM3H00B.YPOS') 
     &         gepics_yh00b=value
          
          if (char_name(1:13).eq.'IPM3H00C.XPOS') 
     &         gepics_xh00c=value
          
          if (char_name(1:13).eq.'IPM3H00C.YPOS') 
     &         gepics_yh00c=value
          
          
c     ==> Magnet information
c     
          if (char_name(1:10).eq.'MBSY3C.BDL') 
     &         gepics_dbdl=value
          
          if (char_name(1:8).eq.'MBSY3C.S') 
     &         gepics_diset=value
          
          if (char_name(1:7).eq.'MBSY3CM') 
     &         gepics_diread=value
          
          if (char_name(1:15).eq.'HLCNMRFIELDRBCK') 
     &         gepics_nmr_field=value
          
          if (char_name(1:14).eq.'HLCNMRLOCKSTAT') 
     &         gepics_nmr_lock=value
          
          if (char_name(1:8).eq.'ecarc7t1') 
     &         gepics_mag_temp1=value
          
          if (char_name(1:8).eq.'ecarc7t2') 
     &         gepics_mag_temp2=value
          
          if (char_name(1:8).eq.'ecarc7t3') 
     &         gepics_mag_temp3=value
          
          if (char_name(1:8).eq.'ecarc7t4') 
     &         gepics_mag_temp4=value
          
          if (char_name(1:8).eq.'ecarc7t5') 
     &         gepics_mag_temp5=value
          
c     ==> OTR information
c     
          if (char_name(1:15).eq.'hlc:MV_AI_DXPOS') 
     &         gepics_otr_x=value
          
          if (char_name(1:15).eq.'hlc:MV_AI_DYPOS') 
     &         gepics_otr_y=value
          
          if (char_name(1:15).eq.'hlc:MV_AI_DXWID') 
     &         gepics_otr_widx=value
          
          if (char_name(1:15).eq.'hlc:MV_AI_DYWID') 
     &         gepics_otr_widy=value
          
          if (char_name(1:16).eq.'hlc:MV_LI_MAXPIX') 
     &         gepics_otr_maxpix=value
          
          if (char_name(1:13).eq.'hlc:MV_LI_SAT') 
     &         gepics_otr_sat=value
          
          
c     ==> Fast feedback information
c     
          if (char_name(1:20).eq.'FB_C:status:mbbi2.B7') 
     &         gepics_ffb_c_stat=value
          
          if (char_name(1:11).eq.'FB_C:use_RF') 
     &         gepics_ffb_c_use=value

c     ==> Energy information
c     
          if (char_name(1:9).eq.'hallc_dpp') 
     &         gepics_dpp=value
          
          if (char_name(1:9).eq.'hallc_MeV') 
     &         gepics_ebeam=value
          
          if (char_name(1:7).eq.'HALLC:p') 
     &         gepics_pbeam=value
          
c     
c     ==> ARC BPM position info
c     
          if (char_name(1:12).eq.'IPM3C07.XPOS') 
     &         gepics_xc07=value
          if (char_name(1:12).eq.'IPM3C07.YPOS') 
     &         gepics_yc07=value
          if (char_name(1:12).eq.'IPM3C08.XPOS') 
     &         gepics_xc08=value
          if (char_name(1:12).eq.'IPM3C08.YPOS') 
     &         gepics_yc08=value
          if (char_name(1:12).eq.'IPM3C11.XPOS') 
     &         gepics_xc11=value
          if (char_name(1:12).eq.'IPM3C11.YPOS') 
     &         gepics_yc11=value
          if (char_name(1:12).eq.'IPM3C12.XPOS') 
     &         gepics_xc12=value
          if (char_name(1:12).eq.'IPM3C12.YPOS') 
     &         gepics_yc12=value
          if (char_name(1:12).eq.'IPM3C14.XPOS') 
     &         gepics_xc14=value
          if (char_name(1:12).eq.'IPM3C14.YPOS') 
     &         gepics_yc14=value
          if (char_name(1:12).eq.'IPM3C16.XPOS') 
     &         gepics_xc16=value
          if (char_name(1:12).eq.'IPM3C16.YPOS') 
     &         gepics_yc16=value
          if (char_name(1:12).eq.'IPM3C17.XPOS') 
     &         gepics_xc17=value
          if (char_name(1:12).eq.'IPM3C17.YPOS') 
     &         gepics_yc17=value
          if (char_name(1:12).eq.'IPM3C18.XPOS') 
     &         gepics_xc18=value
          if (char_name(1:12).eq.'IPM3C18.YPOS') 
     &         gepics_yc18=value
          if (char_name(1:12).eq.'IPM3C20.XPOS') 
     &         gepics_xc20=value
          if (char_name(1:12).eq.'IPM3C20.YPOS') 
     &         gepics_yc20=value
          
C==   > BCM information, here I use ibcm1,2,3. There are also hallc:bcm1,2,3
C     
          if (char_name(1:5).eq.'ibcm1') 
     &         gepics_bcm1=value
          
          if (char_name(1:5).eq.'ibcm2') 
     &         gepics_bcm2=value
          
          if (char_name(1:5).eq.'ibcm3') 
     &         gepics_bcm3=value
          
C==   > SLI information 
          if(char_name(1:17) .eq. 'slic:MV_AO_DYWIDS')
     &         gepics_sli_dywids = value
          if(char_name(1:17) .eq. 'slic:MV_AO_DYWIDM')
     &         gepics_sli_dywidm = value
          if(char_name(1:18) .eq. 'slic:MV_AO_DYWIDME')
     &         gepics_sli_dywidme = value
          if(char_name(1:18) .eq. 'slic:MV_AO_SPREADS')
     &         gepics_sli_spreads = value
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

