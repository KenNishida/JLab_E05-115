      Subroutine e_fill_phys_hist(ABORT,err)
*--------------------------------------------------------
*
* This routine is called from e_physics.f
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      Logical ABORT
      Character*(*) err
*--------------------------------------------------------
      Include "hes_data_structures.cmn"
      Include "hes_id_histid.cmn"
      Include "hes_physics_sing.cmn"
      Include 'gen_constants.par'

      character*15 here
      parameter (here='e_fill_phys_hist')

      Integer*4 i,n,iscin,la,co,j,la2,co2

      Call HF1(eidnphysics,float(enphysics),1.)

      if(enphysics .le. 0) Return
      
      Do i=1,enphysics
         Call HF1(eidphysscinhits,float(ephys_scin_hits(i,1)),1.)
         Call HF1(eidsxfp,esx_fp(i),1.)
         Call HF1(eidsyfp,esy_fp(i),1.)
         Call HF1(eidsxpfp,esxp_fp(i),1.)
         Call HF1(eidsypfp,esyp_fp(i),1.)
         Call HF1(eidsdelta,esdelta(i),1.)
         Call HF1(eidsxtar,esx_tar(i),1.)
         Call HF1(eidsytar,esy_tar(i),1.)
         Call HF1(eidsxptar,esxp_tar(i),1.)
         Call HF1(eidsyptar,esyp_tar(i),1.)
         Call HF1(eidstimeatfp,estime_at_fp(i),1.)
         Call HF1(eidsp,esp(i),1.)
         Call HF1(eidsenergy,esenergy(i),1.)
         Call HF1(eidsxdc1layer1,esx_dc1_layer1(i),1.)
         Call HF1(eidsydc1layer1,esy_dc1_layer1(i),1.)
         Call HF1(eidsxs1,esx_s1(i),1.)
         Call HF1(eidsys1,esy_s1(i),1.)
         Call HF1(eidsxs2,esx_s2(i),1.)
         Call HF1(eidsys2,esy_s2(i),1.)
         Call HF1(eidspathlength,espathlength(i),1.)
         Call HF1(eidstimeattar,estime_at_tar(i),1.)
         Call HF1(eidschi2perdeg,eschi2perdeg(i),1.)
         Call HF1(eidseloss,eseloss(i),1.)
         Call HF1(eidscorre,escorre(i),1.)
         Call HF1(eidscorrp,escorrp(i),1.)
         Call HF1(eidstheta,estheta(i)/degree,1.)
         Call HF1(eidskpvec1,es_kpvec(i,1),1.)
         Call HF1(eidskpvec2,es_kpvec(i,2),1.)
         Call HF1(eidskpvec3,es_kpvec(i,3),1.)
         Call HF1(eidskpvec4,es_kpvec(i,4),1.)
         Call HF1(eidsphi,esphi(i),1.)
         Call HF1(eidszbeam,eszbeam(i),1.)
      EndDo

      Return
      End

