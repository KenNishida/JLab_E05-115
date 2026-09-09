      Subroutine e_dc1_layer_eff(ABORT,err)
*--------------------------------------------------------
*     calculate EDC layer efficiency
*     
*     T. Miyoshi 1st draft
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*50 here
      Parameter (here='e_dc1_layer_eff')
*     
      Logical ABORT
      Character*(*) err
      
      Include "hes_data_structures.cmn"
      Include "hes_geometry.cmn"
      Include "hes_tracking.cmn"
      Include "hes_statistics.cmn"   
      Include "gen_event_info.cmn"
      
      Integer*4 i,j,k,l,goodtrack,itrk,la
      Integer*4 goodhit(emax_num_dc1_layers)
      Real*4 dp,goodchi2_limit,dp_limit
      Real*4 x0(entracks_pre_max,emax_num_dc1_layers)
      Real*4 y0(entracks_pre_max,emax_num_dc1_layers)
      Real*4 xp0(entracks_pre_max,emax_num_dc1_layers)
      Real*4 yp0(entracks_pre_max,emax_num_dc1_layers)
      Real*4 x1,y1,phi
      Real*4 aa1,aa2,aa3
      
      ABORT= .FALSE.
      err= ' '
      
c     Write(*,*) 'layer eff=?'
      
      goodchi2_limit = 3.0
*      dp_limit = 0.6
      dp_limit = 300.
      itrk = edc1bestchi2_pre_index(enum_fitting)
      if(edc1eff_flag(itrk).ne.55) goto 1215
         goodtrack = 0
         Do i=1,emax_num_dc1_layers
*            x0 = ex_fp_pre(itrk,enum_fitting)
*            y0 = ey_fp_pre(itrk,enum_fitting)
*            xp0 = exp_fp_pre(itrk,enum_fitting)
*            yp0 = eyp_fp_pre(itrk,enum_fitting)
            x0(itrk,i) = edc1effx_fp_pre(itrk,i,enum_fitting)
            y0(itrk,i) = edc1effy_fp_pre(itrk,i,enum_fitting)
            xp0(itrk,i) = edc1effx_fp_pre(itrk,i,enum_fitting)
            yp0(itrk,i) = edc1effy_fp_pre(itrk,i,enum_fitting)
         enddo
      if(edc1chi2perdof_pre(itrk,enum_fitting) .lt. goodchi2_limit) then
         goodtrack = 1
         edc1eff_good_trig = edc1eff_good_trig + 1
      EndIf
      
      if(goodtrack .eq. 1) then
         dp = -10000.
         Do i=1,emax_num_dc1_layers
            goodhit(i) = 0
         EndDo
         Do i=1,edc1_tot_hits
            la = edc1_layer_num(i)
            x1 = x0(itrk,la) + edc1_zpos(la) * xp0(itrk,la)
            y1 = y0(itrk,la) + edc1_zpos(la) * yp0(itrk,la)
            phi = x1 * sin(edc1_alpha_angle(la)) 
     &           + y1 * cos(edc1_alpha_angle(la))
            dp = abs(phi - edc1_wire_coord(i))
            dp = min(dp-edc1_drift_dis(i),dp+edc1_drift_dis(i))
            if(abs(dp) .lt. dp_limit) then
               goodhit(la) = 1
            EndIf
         EndDo
         Do i=1,emax_num_dc1_layers
            if(goodhit(i) .eq. 1) then
               edc1eff_did_trig(i) = edc1eff_did_trig(i) + 1
            EndIf
         EndDo
      EndIf
      
      if(edc1eff_good_trig .gt. 1) then
         Do i=1,emax_num_dc1_layers
            edc1_layer_eff(i) = real(edc1eff_did_trig(i))
     &           /Real(edc1eff_good_trig)
            if(edc1eff_did_trig(i) .gt. 0) then
               aa1=1.0/sqrt(real(edc1eff_did_trig(i)))
            Else
               aa1=0.0
            EndIf
            aa2=1.0/sqrt(real(edc1eff_good_trig))
            aa3=sqrt(aa1*aa1+aa2*aa2)
            edc1_layer_eff_err(i) = edc1_layer_eff(i) * aa3
*      Write(*,'(3Hev=,I8,12H dc1 la eff=,f9.4,5H err=,f9.4)') 
*     &           gen_event_id_number,edc1_layer_eff(i),
*     &           edc1_layer_eff_err(i)
         EndDo
      EndIF
 1215 continue

      return
      end
