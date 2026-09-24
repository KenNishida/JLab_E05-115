      Subroutine e_physics(ABORT,err)
*----------------------------------------------------------------
*     Select best tracks and store information to data banks
*     
*     $Log: e_physics.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.8  2005/07/08 13:44:29  sumihama
*     Mod. epathelength
*
*     Revision 1.7  2005/07/06 19:20:51  sumihama
*     Mod.hrf/erf
*
*     Revision 1.6  2005/07/03 11:12:38  sumihama
*     Bug fix and add EHODO info.
*
*     Revision 1.5  2005/07/03 04:19:25  sumihama
*     Mod. hbook sor s.s
*
*     Revision 1.4  2005/07/02 20:29:42  sumihama
*     Mod pathlength for enge
*
*     Revision 1.3  2005/07/02 19:38:54  sumihama
*     Change coeff for pathlength
*
*     Revision 1.2  2005/06/10 17:09:51  cdaq
*     Add constant term of path
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.10  2005/04/21 23:24:40  miyoshi
*     change eptar
*
*     Revision 1.9  2005/04/19 17:47:27  miyoshi
*     delete unused variables
*
*     Revision 1.8  2005/04/08 18:56:51  miyoshi
*     add histogram routine
*
*     Revision 1.7  2005/02/14 22:28:03  miyoshi
*     add loose condition
*
*     Revision 1.6  2005/02/11 23:01:12  sumihama
*     Mod. e-ntuple, Mod. etracking
*
*     Revision 1.5  2005/02/10 21:40:48  miyoshi
*     fix minor change
*
*     Revision 1.4  2005/02/10 21:23:42  miyoshi
*     add path length correction parameters
*
*     Revision 1.3  2005/02/10 19:15:43  miyoshi
*     change condition
*
*     Revision 1.2  2005/01/24 19:57:47  miyoshi
*     change some variables definition
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     
*     Revision 1.5 03/26/04 Miyoshi
*     for E01-011
*     
*     Revision 1.4  1999/12/23 19:59:25  ysato
*     Compiled on Redhat Linux
*     
*     Revision 1.3  1999/11/03 16:42:49  ysato
*     Internal update
*     
*     Revision 1.2  1999/11/02 15:55:27  ysato
*     Upgrade for HNSS
*     
*     October 20, 1999         Y.Sato     Add raw data information
*     July 12, 1999            Y.Fujii    First working draft
*     February 24, 1999        Y.Fujii    A first draft
*     
*     Input Banks    ENGE_GROUPS
*     
*     Output Banks   ENGE_PHYSICS
*----------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      Character*10 here
      Parameter (here='e_physics')
*     
      Logical ABORT
      Character*(*) err
      
      include 'gen_data_structures.cmn'
      Include "hes_data_structures.cmn"
      Include "hks_data_structures.cmn"
      Include "hks_scin_parms.cmn"
      Include "hes_geometry.cmn"
      Include "hes_scin_parms.cmn"
      Include "hes_physics_sing.cmn"
      INCLUDE 'gen_routines.dec'
      INCLUDE 'gen_constants.par'
      INCLUDE 'gen_units.par'
      Include 'gen_event_info.cmn'
      include 'hes_tracking.cmn'
      include 'hes_bypass_swiches.cmn' !Added by Gogami (3Feb2011)
 
      Integer*4 i,j,k,itrk,hit,ep,eg,ierr
      Real*4 dp, dt,x,y,z,dis
      
      real*4 cosestheta,sinestheta
c     real*4 p_nonzero
c     real*4 xdist,ydist,dist(12),res(12)
c     real*4 tmp,W2
      real*4 esp_z
c     real*4 Wvec(4)
      real*4 estheta_1st
c     real*4 scalar,mink
      real*4 time1,time2,slop
      real*4 xyz
      Real*4 adcdep
      Integer*4 gh
      integer*4 layer

*--------------------------------------------------------
      ABORT=.FALSE.
      err=' '
      
      ierr=0
      ephi_lab=0.0
      
      enphysics = 0
      ep = 0

      if(escin_tot_hits .le. 0 .or. entracks_fp .le. 0) Return
c     if(entracks_fp .le. 0) Return
      
c     =======OUTPUT CHAMBER INFROMATION=====================
c     ============Added by Gogami (3Feb2011)================
      if(edc_hitinfo_flag.eq.1 .and.
     &     edc_hitinfo_flag2.eq.1 )then
         open(24,file='hes_cham.dat',access='append')
         write(24,*)entracks_fp
         close(24)
      endif
c     ======================================================
c      write(*,*)"ENREACKS_FP=",entracks_fp
      Do i=1,entracks_fp
c         write(*,*)"entracks_fp",entracks_fp
*     --- put cut condition here
         if(
*DK     &        abs(etrk_fptime(i)) .le. 300 .and.
     &        enphysics .lt. enphysics_max
     &        ) then
            enphysics = enphysics + 1
            ep = enphysics
            
            ephys_scin_hits(ep,1) = etrk_hodo_hit(i,1)
            ephys_ntrack(ep) = i
            hit = ephys_scin_hits(ep,1)
            esscin_depo(ep) = 0
            adcdep = 0
            gh = 0
            esnco1(ep)=0
            esnco2(ep)=0
            esnco3(ep)=0
            esnt1(ep)=-1000
            esnt2(ep)=-2000
            esnt3(ep)=-3000
            esnt1m(ep)=-1000
            esnt2m(ep)=-2000
            esna1(ep)=0
            esna2(ep)=0
            esna3(ep)=0
            esna1m(ep)=0
            esna2m(ep)=0
            do j=1,emax_num_dc1_layers
               esresidual(ep,j) = 100000
               edc_tc(ep,j) = 100000
               edc_wc(ep,j) = 100000
               edc_wn(ep,j) = 100000
            enddo
            do j=1,emax_num_dc2_layers
               esresidual(ep,j+emax_num_dc1_layers) = 100000
               edc_tc(ep,j+emax_num_dc1_layers) = 100000
               edc_wc(ep,j+emax_num_dc1_layers) = 100000
               edc_wn(ep,j+emax_num_dc1_layers) = 100000
            enddo
            
            Do j=1,hit
               ephys_scin_hits(ep,j+1) = etrk_hodo_hit(i,j+1)
               if(escin_good_hits(etrk_hodo_hit(i,j+1)).eq.1) then
                  adcdep = sqrt(escin_adc_pos(etrk_hodo_hit(i,j+1))
     &                 *escin_adc_neg(etrk_hodo_hit(i,j+1)))
                  gh = gh + 1
               EndIf
               esscin_depo(ep) = esscin_depo(ep) + adcdep
c               write(*,*)"escin_layer_num(etrk_hodo_hit(i,j+1)=",
c     &              escin_layer_num(etrk_hodo_hit(i,j+1))
               if(escin_layer_num(etrk_hodo_hit(i,j+1)).eq.1) then
                  if(esnco1(ep).eq.0) then
                     esnco1(ep) = 
     &                    real(escin_counter_num(etrk_hodo_hit(i,j+1)))
                     esnt1(ep)  = escin_mean_time(etrk_hodo_hit(i,j+1))
                     if(escin_good_hits(etrk_hodo_hit(i,j+1)).eq.1) then
                        esna1(ep) = 
     &                       sqrt(escin_adc_pos(etrk_hodo_hit(i,j+1))
     &                       *escin_adc_neg(etrk_hodo_hit(i,j+1)))
                     endif
                  else
                     esnco1(ep) = 
     &                    (esnco1(ep) +
     &                    real(escin_counter_num(etrk_hodo_hit(i,j+1))))
     &                    /2
                     esnt1m(ep)  = escin_mean_time(etrk_hodo_hit(i,j+1))
                     if(escin_good_hits(etrk_hodo_hit(i,j+1)).eq.1) then
                        esna1m(ep) = 
     &                       sqrt(escin_adc_pos(etrk_hodo_hit(i,j+1))
     &                       *escin_adc_neg(etrk_hodo_hit(i,j+1)))
                     endif
                  endif
               elseif(escin_layer_num(etrk_hodo_hit(i,j+1)).eq.2) then
                  
                  if(esnco2(ep).eq.0) then
                     esnco2(ep) = 
     &                    real(escin_counter_num(etrk_hodo_hit(i,j+1)))
                     esnt2(ep) = escin_mean_time(etrk_hodo_hit(i,j+1))
                     if(escin_good_hits(etrk_hodo_hit(i,j+1)).eq.1) then
                        esna2(ep) = 
     &                       sqrt(escin_adc_pos(etrk_hodo_hit(i,j+1))
     &                       *escin_adc_neg(etrk_hodo_hit(i,j+1)))
                     endif
                  else
                     esnco2(ep) = 
     &                    (esnco2(ep)+
     &                    real(escin_counter_num(etrk_hodo_hit(i,j+1))))
     $                    /2
                     esnt2m(ep) = escin_mean_time(etrk_hodo_hit(i,j+1))
                     if(escin_good_hits(etrk_hodo_hit(i,j+1)).eq.1) then
                        esna2m(ep) = 
     &                       sqrt(escin_adc_pos(etrk_hodo_hit(i,j+1))
     &                       *escin_adc_neg(etrk_hodo_hit(i,j+1)))
                     endif
                  endif
               elseif(escin_layer_num(etrk_hodo_hit(i,j+1)).eq.3) then
                  if(esnco3(ep).eq.0) then
                     esnco3(ep) = 
     &                    real(escin_counter_num(etrk_hodo_hit(i,j+1)))
                     esnt3(ep) = escin_mean_time(etrk_hodo_hit(i,j+1))
                     if(escin_good_hits(etrk_hodo_hit(i,j+1)).eq.1) then
                        esna3(ep) = 
     &                       sqrt(escin_adc_pos(etrk_hodo_hit(i,j+1))
     &                       *escin_adc_neg(etrk_hodo_hit(i,j+1)))
                     endif
                  endif
               endif
            EndDo
            esscin_depo(ep) = esscin_depo(ep)/Real(gh)
            
*     --- focal plane quantities. z=0 is focal plane.
            esx_fp(ep) = ex_fp(i)
            esy_fp(ep) = ey_fp(i)
            esxp_fp(ep) = exp_fp(i)
            esyp_fp(ep) = eyp_fp(i)
            
c     =======OUTPUT CHAMBER INFROMATION=====================
c     ============Added by Gogami (3Feb2011)================
            if(edc_hitinfo_flag.eq.1 .and.
     &           edc_hitinfo_flag2.eq.1 )then
               open(24,file='hes_cham.dat',access='append')
               write(24,*)esx_fp(ep),esy_fp(ep),esxp_fp(ep),esyp_fp(ep)
               close(24)
            endif
c            write(*,*)esx_fp(ep),esy_fp(ep),esxp_fp(ep),esyp_fp(ep)
c     ======================================================
            
*     --- note that entracks_fp = entracks_tar
            esdelta(ep)  = edelta_tar(i)
            esx_tar(ep)  = ex_tar(i)
            esy_tar(ep)  = ey_tar(i)
            esxp_tar(ep) = exp_tar(i)
            esyp_tar(ep) = eyp_tar(i)
            
            esx_sv(ep)  = ex_sv(i)
            esy_sv(ep)  = ey_sv(i)

            estime_at_fp(ep) = etrk_fptime(i)
            
            espathlength(ep) = etrk_pathlength(i)
            
            estime_at_tar(ep) = estime_at_fp(ep)
     &           - espathlength(ep)/speed_of_light
            
*     Correct delta (this must be called AFTER filling 
*     focal plane quantites).
c     call h_satcorr(ABORT,err)
*     esp is momentum in GeV.
            
            esp(ep) = ep_tar(i)

            if(esp(ep).le.0.) RETURN
            esenergy(ep) = sqrt(esp(ep)*esp(ep)+epartmass*epartmass)        
            

c            espathlength(ep) = epathlength_central + speed_of_light
c     &           *(estime_at_fp(ep)-estime_at_tar(ep)+20.2)
            
c            esrftime(ep) = hmisc_dec_data(65,1)*0.025
c     &           - estime_at_tar(ep)
            
            eschi2perdeg(ep)  = echi2perdof_fp(i) 
            esnfree_fp(ep)    = enfree_fp(i)

            do j=1,emax_num_dc1_layers
               if(i.gt.1500 .or. j.gt.1500) then
                  write(*,*)"h_physics.f : (",i,",",j,")"
               endif
               esresidual(ep,j) 
     &              = edc1_single_residual(i,j)
               edc_tc(ep,j) = edc1_tcoord(i,j)
               edc_wc(ep,j) = edc1_wcoord(i,j)
               edc_wn(ep,j) = edc1_wnum(i,j)
            enddo
            do j=1,emax_num_dc2_layers
               esresidual(ep,j+emax_num_dc1_layers)
     &              =edc2_single_residual(i,j)
               edc_tc(ep,j+emax_num_dc1_layers) = edc2_tcoord(i,j)
               edc_wc(ep,j+emax_num_dc1_layers) = edc2_wcoord(i,j)
               edc_wn(ep,j+emax_num_dc1_layers) = edc2_wnum(i,j)
            enddo
c     =======OUTPUT CHAMBER INFROMATION=====================
c     ============Added by Gogami (5Feb2011)================
            if(edc_hitinfo_flag.eq.1 .and.
     &           edc_hitinfo_flag2.eq.1 )then
               open(24,file='hes_cham.dat',access='append')
               write(24,*)edc_wn(ep,1),edc_wn(ep,2),edc_wn(ep,3),
     &              edc_wn(ep,4),edc_wn(ep,5),edc_wn(ep,6),edc_wn(ep,7),
     &              edc_wn(ep,8),edc_wn(ep,9),edc_wn(ep,10),
     &              edc_wn(ep,11),edc_wn(ep,12),edc_wn(ep,13),
     &              edc_wn(ep,14),edc_wn(ep,15),edc_wn(ep,16)
               write(24,*)edc_tc(ep,1),edc_tc(ep,2),edc_tc(ep,3),
     &              edc_tc(ep,4),edc_tc(ep,5),edc_tc(ep,6),edc_tc(ep,7),
     &              edc_tc(ep,8),edc_tc(ep,9),edc_tc(ep,10),
     &              edc_tc(ep,11),edc_tc(ep,12),edc_tc(ep,13),
     &              edc_tc(ep,14),edc_tc(ep,15),edc_tc(ep,16)
               write(24,*)edc_wc(ep,1),edc_wc(ep,2),edc_wc(ep,3),
     &              edc_wc(ep,4),edc_wc(ep,5),edc_wc(ep,6),edc_wc(ep,7),
     &              edc_wc(ep,8),edc_wc(ep,9),edc_wc(ep,10),
     &              edc_wc(ep,11),edc_wc(ep,12),edc_wc(ep,13),
     &              edc_wc(ep,14),edc_wc(ep,15),edc_wc(ep,16)
               write(24,*)esresidual(ep,1),esresidual(ep,2),
     &              esresidual(ep,3),esresidual(ep,4),esresidual(ep,5),
     &              esresidual(ep,6),esresidual(ep,7),esresidual(ep,8),
     &              esresidual(ep,9),esresidual(ep,10),
     &              esresidual(ep,11),esresidual(ep,12),
     &              esresidual(ep,13),esresidual(ep,14),
     &              esresidual(ep,15),esresidual(ep,16)
               write(24,*)eschi2perdeg(ep)
               write(24,*)esnco1(ep),esnco2(ep)
               write(24,*)esp(ep)
               close(24)
            endif
c     ======================================================
            
*     Do energy loss, which is particle specific
            estheta_1st = etheta_lab*TT/180. + atan(esyp_tar(ep)) 
*     rough scat angle
            
            if (gtarg_z(gtarg_num).gt.0.) then
               call g_total_eloss(2,.true.,gtarg_z(gtarg_num),
     &              gtarg_a(gtarg_num),
     $              gtarg_thick(gtarg_num),gtarg_dens(gtarg_num),
     $              estheta_1st,gtarg_theta,1.0,eseloss(ep))
            else
               eseloss(ep)=0.
            endif
            
*     Correct esenergy and esp for eloss at the target
            
            escorre(ep) = esenergy(ep) + eseloss(ep)
            escorrp(ep) = sqrt(escorre(ep)**2-epartmass**2)
            
*     Angles for Scattered particle. Theta and phi are conventional
*     polar/azimuthal angles defined w.r.t. coordinate system defined
*     above. In rad, of course. Note that phi is around -pi/2 for HMS,
*     +pi/2 for HKS.
            
            xyz = 1. + esxp_tar(ep)**2 + esyp_tar(ep)**2
            if (abs(xyz) .ge. 1.) then
               if(esxp_tar(ep) .gt. 0) then
                  estheta(ep) = acos(1/sqrt(xyz))
               Else
                  estheta(ep) = -10.
               EndIf
            else
               estheta(ep) = -10.
            endif
            
*     Begin Kinematic stuff
*     coordinate system :
*     z points downstream along beam
*     x points downward 
*     y points toward beam left (away from HMS)
*     This coordinate system is a just a simple rotation away from the
*     TRANSPORT coordinate system used in the spectrometers
            
c     P*cos(theta) = Pz
c     --- use corrp or hsp?
            esp_z = esp(ep)/sqrt(xyz)
            
*     Initial Electron
            es_kvec(ep,1) = gebeam ! after energy loss in target
            es_kvec(ep,2) = 0
            es_kvec(ep,3) = 0
            es_kvec(ep,4) = gebeam 
            
*     Scattered Electron (not meaningful if hadron is in HKS!)
*     calculation without small angle approximation - gaw 98/10/5 csa
*     12/21/98 -- notice assumption of no out-of-plane offset
            
c     energy? or ecorre? 
            es_kpvec(ep,1) = esenergy(ep) 
            es_kpvec(ep,2) = esxp_tar(ep) * esp_z
            
C***  JLiu modified for HNSS
C***  changed the signs of following two terms containing "sinsthetas"
c---  Miyoshi modified for NewHNSS, sinhthetas --> -sinhthetas
c     --- no lotation.
            
            es_kpvec(ep,3) = esyp_tar(ep) * esp_z
            es_kpvec(ep,4) = esp_z
            
            esphi(ep) =atan2(es_kpvec(ep,3), es_kpvec(ep,2))      

C     --- local variables

            sinestheta = sin(estheta(ep))
            cosestheta = cos(estheta(ep))
            
            esphi(ep) = ephi_lab + esphi(ep)
            
            if (esphi(ep) .lt. 0.) esphi(ep) = esphi(ep) + 2*tt
            
*     eszbeam is the intersection of the beam ray with the
*     spectrometer as measured along the z axis.
            
            if( sinestheta .eq. 0.) then
               eszbeam(ep) = 0.
            else
               eszbeam(ep) = sin(esphi(ep)) 
     &              * ( -esy_tar(ep) + gbeam_y * cosestheta) /
     $              sinestheta 
            endif               ! end test on sinsstheta=0
            
*     Target particle 4-momentum
            
            es_tvec(ep,1) = gtarg_mass(gtarg_num)*m_amu
            es_tvec(ep,2) = 0.
            es_tvec(ep,3) = 0.
            es_tvec(ep,4) = 0.
            
            if (.false.) then
*     if (.true.) then
               write(6,*)' ***** i=',ep,'****************'
               write(6,*)' e_phys: (theta,phi)lab =',etheta_lab,ephi_lab
               write(6,*)' e_phys: esdelta        =',esdelta(ep)
               write(6,*)' e_phys: (x,y)tar       =',
     &              esx_tar(ep),esy_tar(ep)
               write(6,*)' e_phys: (xp,yp)tar     =',
     &              esxp_tar(ep),esyp_tar(ep)
               write(6,*)' e_phys: esenergy, esp  =',
     &              esenergy(ep),esp(ep)
               write(6,*)' e_phys: eseloss        =',
     &              eseloss(ep)
               write(6,*)' e_phys: escorre,p      =',
     &              escorre(ep),escorrp(ep)
               write(6,*)' e_phys: estheta_1st    =',
     &              estheta_1st
               write(6,*)' e_phys: esp_z          =',
     &              esp_z
               write(6,*)' e_phys: es_kvec        =',
     &              (es_kvec(ep,j),j=1,4)
               write(6,*)' e_phys: cos/sinethetae =',
     &              cosethetae,sinethetae
               write(6,*)' e_phys: es_kpvec       =',
     &              (es_kpvec(ep,j),j=1,4)
               write(6,*)' e_phys: es_tvec        =',
     &              (es_tvec(ep,j),j=1,4)
c     write(6,*)' e_phys: es_qvec        =',es_qvec(ep)
c     write(6,*)' e_phys: Wvec           =',Wvec(ep)
c     write(6,*)' e_phys: esq3           =',esq3(ep)
c     write(6,*)' e_phys: esbigq2, W2    =',esbigq2(ep),W2(ep)
               write(6,*)' e_phys: estheta, esphi =',
     &              estheta(ep),esphi(ep)
            endif            
         EndIf                  ! if data is ok.
      EndDo                     ! group loop.
      
c     ======= Calculation efficiency =========
      call e_dc_eff_count(abort,err,ep) ! EDC efficiency (T.Gogami,3Mar2011)

c     =======OUTPUT CHAMBER INFROMATION=====================
c     ============Added by Gogami (5Feb2011)================      
      if(enphysics.gt.0 .and.
     &     edc_hitinfo_flag.eq.1 .and. edc_hitinfo_flag2.eq.1 )then
         open(24,file='hes_cham.dat',access='append')
         write(24,*)"data"
         close(24)
      endif
c     ======================================================
      
*     --- Histogram for phys bank ---
      call e_fill_phys_hist(abort,err)
      if (abort) then
         call g_prepend(here,err)
         return
      endif


*     --- Calculate physics statistics and efficencies. ---
      call e_physics_stat(ABORT,err)
      IF(ABORT) THEN
         call G_add_path(here,err)
      ENDIF
      
*     --- Debug print ---
      If (edumpphysics.ne.0) call e_prt_physics(ABORT,err)
      
      Return
      End
      
