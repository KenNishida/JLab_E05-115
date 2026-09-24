      Subroutine h_fill_phys_hist(ABORT,err)
*--------------------------------------------------------
*
* This routine is called from h_physics.f
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Logical ABORT
      Character*(*) err
*--------------------------------------------------------

      Include "hks_data_structures.cmn"
      Include "hks_physics_sing.cmn"
      Include "hks_phys_histid.cmn"
      Include "gen_constants.par"
      include 'hks_id_histid.cmn'

      character*15 here
      parameter (here='h_fill_phys_hist')

      Integer*4 i,n,iscin,la,co,j,la2,co2

c     Write(*,*) '(hfillphys)',hidnphysics,hnphysics,hntracks_fp

      Call HF1(hidnphysics,float(hnphysics),1.)

      if(hnphysics .le. 0) Return
      
      Do i=1,hnphysics
         Call HF1(hidphysscinhit,float(hphys_scin_hit(i,1)),1.)
         Call HF1(hidsdelta,hsdelta(i),1.)
         Call HF1(hidsxfp,hsx_fp(i),1.)
         Call HF1(hidsyfp,hsy_fp(i),1.)
         Call HF1(hidsxpfp,hsxp_fp(i),1.)
         Call HF1(hidsypfp,hsyp_fp(i),1.)
         Call HF1(hidsxtar,hsx_tar(i),1.)
         Call HF1(hidsytar,hsy_tar(i),1.)
         Call HF1(hidsxptar,hsxp_tar(i),1.)
         Call HF1(hidsyptar,hsyp_tar(i),1.)
         Call HF1(hidstimeatfp,hstime_at_fp(i),1.)
         Call HF1(hidsbeta,hsbeta(i),1.)
         Call HF1(hidstof,hstof(i),1.)
c         hidsdedx is not defined DK
c         Do j=1,hnum_scin_layers
c            Call HF1(hidsdedx(j),hsdedx(i,j),1.)
c         EndDo
         Call HF1(hidsp,hsp(i),1.)
         Call HF1(hidsenergy,hsenergy(i),1.)
         
c     --- x-y plot at detectors
         Call HF2(hidsdc1_xy,hsx_dc1(i),hsy_dc1(i),1.)
         Call HF2(hidsdc2_xy,hsx_dc2(i),hsy_dc2(i),1.)
         Call HF2(hids1x_xy,hsx_s1x(i),hsy_s1x(i),1.)
         Call HF2(hids1y_xy,hsx_s1y(i),hsy_s1y(i),1.)
         Call HF2(hids2x_xy,hsx_s2x(i),hsy_s2x(i),1.)
c     Write(*,*) '(hfillphys)',hsx_aer(i,1),hsy_aer(i,1)

         Call HF2(hidsac1_xy,hsx_aer(i,1),hsy_aer(i,1),1.)
         Call HF2(hidsac2_xy,hsx_aer(i,2),hsy_aer(i,2),1.)
         Call HF2(hidsac3_xy,hsx_aer(i,3),hsy_aer(i,3),1.)
         Call HF2(hidswc1_xy,hsx_wat(i,1),hsy_wat(i,1),1.)
         Call HF2(hidswc2_xy,hsx_wat(i,2),hsy_wat(i,2),1.)
c      --Lucite
         Call HF2(hidslc_xy, hsx_luc(i,1),hsy_luc(i,1),1.)
c     cherenkov and/or hof counters
         Call HF1(hidsac1npe,hsaer_npe(i,1),1.)
         Call HF1(hidsac2npe,hsaer_npe(i,2),1.)
         Call HF1(hidsac3npe,hsaer_npe(i,3),1.)
         Call HF1(hidswc1npe,hswat_npe(i,1),1.)
         Call HF1(hidswc2npe,hswat_npe(i,2),1.)
         Call HF1(hidswc1npekratio,hswat_npe_k_ratio(i,1),1.)
         Call HF1(hidswc2npekratio,hswat_npe_k_ratio(i,2),1.)
c        --lucite
         Call HF1(hidslcnpe, hsluc_npe(i,1),1.)
         
         Call HF2(hidswc1ac1,hsaer_npe(i,1),hswat_npe(i,1),1.)
         Call HF2(hidsac1s1x,hsdedx(i,1),hsaer_npe(i,1),1.)
         Call HF2(hidswc1s1x,hsdedx(i,1),hswat_npe(i,1),1.)
         
         Call HF1(hidsbetak,hsbeta(i)-hsbeta_k(i),1.)
         Call HF1(hidsbetae,hsbeta(i)-1.0,1.)
         Call HF1(hidsbetapi,hsbeta(i)-hsbeta_pi(i),1.)
         Call HF1(hidsbetapr,hsbeta(i)-hsbeta_pr(i),1.)
         
         Call HF2(hidsdeltabetak,hsbeta(i)-hsbeta_k(i),hsdelta(i),1.)
         
         Call HF1(hidspathlength,hspathlength(i),1.)
         Call HF1(hidstimeattar,hstime_at_tar(i),1.)
         Call HF1(hidschi2perdeg,hschi2perdeg(i),1.)
         Call HF1(hidseloss,hseloss(i),1.)
         Call HF1(hidscorre,hscorre(i),1.)
         Call HF1(hidscorrp,hscorrp(i),1.)
         Call HF1(hidstheta,hstheta(i)/degree,1.)
         Call HF1(hidskpvec1,hs_kpvec(i,1),1.)
         Call HF1(hidskpvec2,hs_kpvec(i,2),1.)
         Call HF1(hidskpvec3,hs_kpvec(i,3),1.)
         Call HF1(hidskpvec4,hs_kpvec(i,4),1.)
         Call HF1(hidsphi,hsphi(i),1.)
         Call HF1(hidszbeam,hszbeam(i),1.)

      EndDo

      Return
      End

