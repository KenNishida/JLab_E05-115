      Subroutine e_fill_scin_dec_hist(ABORT,err)
*--------------------------------------------------------
* Fill HNSS decoded data histograms
* Feburuary 29, 2000        Y.Sato        Initial version
* 
* $Log: e_fill_scin_dec_hist.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.5  2005/04/08 18:49:13  miyoshi
* add condition
*
* Revision 1.4  2005/01/24 19:54:26  miyoshi
* correct/add histogram
*
* Revision 1.3  2005/01/14 22:42:58  miyoshi
* add histogram
*
* Revision 1.2  2005/01/05 18:41:00  sumihama
* hes-scinti mod.
*
* Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
* new dir
*
* Revision 1.2 03/20/2004 Miyoshi
* for E01-011
*
* Revision 1.1  2000/03/09 02:05:45  ysato
* Update in the production run Mar.8
*
*
* This routine is called from k_trans_scin.f
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Logical ABORT
      Character*(*) err
*--------------------------------------------------------
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"

      character*15 here
      parameter (here='e_fill_scin_dec_hist')

      Integer*4 i,n,iscin,la,co,j,la2,co2
      Real*4 scin_time,t1,t2
      Real*4 timemin,timemax
      Integer*4 chits(6)
      Parameter(timemin=-2000.)
      Parameter(timemax=2000.)
      
*     --- Decoded data ---
      Call Hf1(eidscindectothit,Float(escin_tot_hits),1.)
      Call Hf1(eidscindectothitzoom,Float(escin_tot_hits),1.)
      
      if(escin_tot_hits .le. 0) return
      
      Do i=1,6
         chits(i) = 0
      EndDo
      
      Do i=1,escin_tot_hits
         la = escin_layer_num(i)
         co = escin_counter_num(i)
         t1 = escin_mean_time(i)
         if(escin_time_pos(i) .gt. timemin .and.
     &        escin_time_pos(i) .lt. timemax) then
            Call Hf1(eidscindechitpatpos(la),Float(co),1.)
            if(la .eq. 1) then
               chits(1) = chits(1) + 1
            Else if (la .eq. 2) then
               chits(3) = chits(3) + 1
            EndIf 
         EndIf
         if(escin_time_neg(i) .gt. timemin .and.
     &        escin_time_neg(i) .lt. timemax) then
            Call Hf1(eidscindechitpatneg(la),Float(co),1.)
            if(la .eq. 1) then
               chits(2) = chits(2) + 1
            Else
               chits(4) = chits(4) + 1
            EndIf
         EndIf
         if(escin_good_hits(i) .gt. 0) then
            Call Hf1(eidscindechitpatboth(la),Float(co),1.)
            if(la .eq. 1) then
               chits(5) = chits(5) + 1
            Else
               chits(6) = chits(6) + 1
            EndIf
         EndIf

         if(eturnon_scin_dec_hist .eq. 1) then

c     Write(*,*) '(escindechist)',escin_time_pos(i),
c     &           eidscintimepos(la,co),la,co


            Call Hf1(eidscinphpos(la,co),
     &           escin_adc_pos(i),1.)
            Call Hf1(eidscinphneg(la,co),
     &           escin_adc_neg(i),1.)
            Call Hf1(eidscintimepos(la,co),
     &           escin_time_pos(i),1.)
            Call Hf1(eidscintimeneg(la,co),
     &           escin_time_neg(i),1.)
         EndIf

         Call Hf1(eidscinmeantime(la,co),
     &        escin_mean_time(i),1.)
         
c----------------------------------------------
c     time difference check routine below.
c----------------------------------------------
         if(eturnon_scin_dt_hist .eq. 1) then
            if(escin_good_hits(i) .gt. 0) then
               Do j=1,i-1
                  la2 = escin_layer_num(j)
                  co2 = escin_counter_num(j)
                  t2 = escin_mean_time(j)
                  if(la2 .eq. la .and. co2 .eq. co-1) then
                     Call Hf1(eidscindtcounter(la,co),t1-t2,1.)
                  EndIf
               EndDo
               Do j=1,i-1
                  la2 = escin_layer_num(j)
                  co2 = escin_counter_num(j)
                  t2 = escin_mean_time(j)
                  if(la .eq. 2 .and. la2 .eq. 1 .and. 
     &                 co .eq. co2 .or. co-1 .eq. co2) then
                     Call Hf1(eidscindtlayer(co2),t1-t2,1.)
                  EndIf
               EndDo
            EndIf
         EndIf
c------end of time difference check ------------


      EndDo

c     --- fill dec hits for each groups with good timing.
      
c     Write(*,*) eidscindectothitpos,eidscindechitpatpos

      Call Hf1(eidscindectothitpos(1),float(chits(1)),1.)
      Call Hf1(eidscindectothitneg(1),float(chits(2)),1.)
      Call Hf1(eidscindectothitpos(2),float(chits(3)),1.)
      Call Hf1(eidscindectothitneg(2),float(chits(4)),1.)
      Call Hf1(eidscindectothitboth(1),float(chits(5)),1.)
      Call Hf1(eidscindectothitboth(2),float(chits(6)),1.)
      
      Return
      End

