      Subroutine e_prt_dec_scin(ABORT,err)
*--------------------------------------------------------
*     Print hnss_decoded_scin Bank
*     
*     $Log: e_prt_dec_scin.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.2  2005/01/05 18:41:00  sumihama
*     enge-scinti mod.
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.3 03/20/2004 Miyoshi
*     for E01-011
*     
*     Revision 1.2  2000/03/09 01:32:40  ysato
*     Update in the production run Mar.8
*     
*     Revision 1.1  1999/12/23 19:59:26  ysato
*     Compiled on Redhat Linux
*
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_prt_dec_scin')
*     
      Logical ABORT
      Character*(*) err
      
      Include 'hes_data_structures.cmn'
      
      Integer*4 j
      
      Integer*4 elunno_prt
      Parameter(elunno_prt=6)
      
      ABORT= .FALSE.
      err= ' '
      
      
      
      write(elunno_prt,*) 
     &     '        ***ENGE_DECODED_SCIN BANKS***'
      write(elunno_prt,'(20H     escin_tot_hits=,I4)') 
     &     escin_tot_hits
      if(escin_tot_hits.GT.0) then
         write(elunno_prt,*) ' Num  layer  counter',
     &        '       PH(+,-)     TIME(+,-)'
         Do j=1,escin_tot_hits
            write(elunno_prt,
     &           '(1x,i4,1x,i2,1x,i3,1x,2f7.1,1x,2f8.3)')
     &           j,ESCIN_layer_NUM(j),
     &           ESCIN_counter_NUM(j),
     &           ESCIN_adc_pos(j),escin_adc_neg(j),
     &           ESCIN_TIME_pos(j),escin_time_neg(j)
         EndDo
      endif
      
      return
      end
