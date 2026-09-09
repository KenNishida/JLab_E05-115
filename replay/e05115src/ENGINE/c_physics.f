      SUBROUTINE C_PHYSICS(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods : Compute coincident quantities
*     -
*     -   Output: ABORT           - always success
*     -         : err             - never failure
*     - 
*     $Log: c_physics.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:46  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.6  2005/09/20 15:03:07  sumihama
*     Remove and add some ntuple variables. Looze the cut conditions
*
*     Revision 1.5  2005/08/23 17:04:40  cdaq
*     Mod. coinrf
*
*     Revision 1.4  2005/07/09 02:12:53  cdaq
*     Add. coin rf calc.
*
*     Revision 1.3  2005/07/07 20:30:29  sumihama
*     Change direction of ENGEangle for MM calc.
*
*     Revision 1.2  2005/06/03 19:33:39  cdaq
*     add missing mass calc with carbon kinem
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.3  2005/04/21 23:17:36  miyoshi
*     add debug comments
*
*     Revision 1.2  2005/04/19 17:46:00  miyoshi
*     correct missing mass calculation
*
*     Revision 1.1.1.1  2004/08/30 21:21:38  miyoshi
*     new dir
*
*     Revision 1.2  2000/03/28  11:12:35  jinghua
*     (JLiu) minor modification
*     
*     Revision 1.1  2000/03/09  18:56:27  jinghua
*     (JLiu) change from focal plane time to target time
*     
*     Revision 1.0  1999/10/05  15:17:36  jinghua 
*!!!  1) We assume that the pulse height correction and flight path length
*!!!  correction has been done in the HNSS_RECONSTRUCTION part.
*!!!  2) coincidence time = HNSS target time - SOS target time
      
*     Initial revision
*     
*--------------------------------------------------------
      IMPLICIT NONE
      SAVE
*
      character*9 here
      parameter (here= 'C_PHYSICS')
*     
      logical ABORT
      character*(*) err
*     
      include 'gen_data_structures.cmn'
      include 'hes_data_structures.cmn'
      include 'hes_physics_sing.cmn'
      include 'hks_data_structures.cmn'
      include 'coin_data_structures.cmn'
      include 'coin_id_histid.cmn'
      include 'gen_constants.par'
      include 'hks_scin_tof.cmn'
      include 'hks_physics_sing.cmn'
      include 'hks_scin_parms.cmn'
      include 'gen_event_info.cmn'
      include 'gen_run_info.cmn'

*     
*     local variables
*     
      real*4 cq
      real*4 delta_ctime
      integer*4 i,j,cn,ih,ie,num1,num2
      Real*4 be_h,be_c,be_t,tempp1,tempp2
      Integer*4 filter(cnphysics_max),temp_phys

      Real*4 et_t,et_c,et_h     ! beam energy + target energy
      Real*4 Plamsq
      Real*4 Mlamsq_h,Elam_h    ! missing mass quantities
      Real*4 Mlamsq_t,Elam_t
      Real*4 Mlamsq_c,Elam_c    ! missing mass quantities

      Real*4 Pep,Eep,Pe0,Pk,Ek
      Real*4 coseep,cosek,cosepk ! cos(angles)
      Real*4 det,a1,a2
      Real*4 aa1(3),aa2(3),xyz

ccc FOR RF
      real peak(13),peak2(13),peak3(13),peak4(13),peak5(13),prerf
      data peak/-0.40809, 7.609455, 15.627, 23.7015,
     +     31.776, 40.199, 47.918, 56.426, 64.061, 
     +     72.136, 80.211, 88.278, 96.344/  
      
      data peak2/35.303, 43.606, 51.231, 59.6, 67.238,
     +     75.71, 83.236, 91.73, 99.221, 107.6, 115.2, 123.6, 131.28 /

      data peak3/41.395, 49.89, 57.398, 65.84, 73.381,
     +     81.80, 89.376, 97.82, 105.37, 113.8, 121.37, 129.7, 137.3 /

      data peak4/41.492, 49.98, 57.502, 65.89, 73.503,
     +     81.94, 89.462, 97.91, 105.49, 113.9, 121.45, 129.9, 137.3 /

      data peak5/28.281, 36.99, 44.299, 52.75, 60.446,
     +     69.03, 76.454, 85.04, 92.521, 101.0, 108.57, 116.14, 123.71 /
      
c     --- target or core nuclear mass
      Real*4 local_mp,local_mc,local_mb
      Parameter(local_mp=0.93827231)
      Parameter(local_mc=11.17495)
      Parameter(local_mb=10.2526)
      
c     gen_contants.tar, m_kaon, 
c     mass_electron, mass_nucleon are defined.

      ABORT = .FALSE.
      err = ' '

      coinrf = -100
      
      if(esrftime.gt.-70000) then 
         prerf = esrftime-hsrftime+3700
         do i = 1,13
            
            if((gen_run_number.ge.56524.and.gen_run_number.le.56568).or.
     >           (gen_run_number.ge.56648.and.gen_run_number.le.59694.)) then
               if( prerf.lt.peak2(i)+2.and.prerf.gt.peak2(i)-2) then
                  coinrf = prerf-peak2(i)
               endif
            elseif(gen_run_number.ge.59700.and.gen_run_number.le.59830.) then
               if( prerf.lt.peak4(i)+2.and.prerf.gt.peak4(i)-2) then
                  coinrf = prerf-peak4(i)
               endif
            elseif(gen_run_number.ge.59831.and.gen_run_number.le.59944.) then
               if( prerf.lt.peak3(i)+2.and.prerf.gt.peak3(i)-2) then
                  coinrf = prerf-peak3(i)
               endif
            elseif(gen_run_number.ge.59945.and.gen_run_number.le.70000.) then
               if( prerf.lt.peak5(i)+2.and.prerf.gt.peak5(i)-2) then
                  coinrf = prerf-peak5(i)
               endif
            else
               if( prerf.lt.peak(i)+2.and.prerf.gt.peak(i)-2) then
                  coinrf = prerf-peak(i)
               endif
            endif

         enddo
      endif
      
      cnphysics = 0
      
c      if(c_cointime_high .eq. 0 .and. c_cointime_low .eq. 0) then
c         c_cointime_high = 5000.
c         c_cointime_low = -5000.
c      EndIf
      
      if(enphysics.le.0.or.hnphysics.le.0) Return
      
*     Need to select the e' within the coincidence window
*!!!  We assume that the pulse height correction and flight path length
*!!!  correction has been done in the ENGE_RECONSTRUCTION part. If not,
*!!!  we will have to add them here.
*     
      Do ih=1,hnphysics
         Do ie=1,enphysics
            delta_ctime = estime_at_tar(ie) - hstime_at_tar(ih) - coinrf
            Call HF1(cidcointime,delta_ctime,1.)

*     --- put cuts what you want
            if(
c     &           hsp(ih) .gt. 0 .and.
c     &           esp(ie) .gt. 0 .and.
     &           hsbeta(ih) .gt. 0 .and.
     &           hsbeta(ih) .lt. 2.0
c     &           delta_ctime .le. c_cointime_high .and.
c     &           delta_ctime .ge. c_cointime_low
     &           ) then

               if(cnphysics .eq. cnphysics_max) then
                  Write(*,*) ' too many multiplicity! --- c_physics'
               Else             ! less than multiplicity limit
                  cnphysics = cnphysics + 1
                  cn=cnphysics
                  cmissmass_tar(cn) = 0
                  cmissmass_h(cn) = 0
                  cmissmass_c(cn) = 0

                  cnhks(cn) = ih
                  cnhes(cn) = ie
                  
c     --- calculation of missing mass
c     --- hsenergy(ih) w/o target energy loss
c     --- hscorre(ih) w target energy loss
                  
c     --- confirm px,py,pz
                  xyz=1+esxp_tar(ie)*esxp_tar(ie)
     &                 +esyp_tar(ie)*esyp_tar(ie)
                  aa1(3) = esp(ie) / sqrt(xyz)
                  aa1(1) = esxp_tar(ie)*aa1(3)
                  aa1(2) = esyp_tar(ie)*aa1(3)

                  xyz=1+hsxp_tar(ih)*hsxp_tar(ie)
     &                 +hsyp_tar(ih)*hsyp_tar(ie)
                  aa2(3) = hsp(ih) / sqrt(xyz)
                  aa2(1) = hsxp_tar(ih)*aa2(3)
                  aa2(2) = hsyp_tar(ih)*aa2(3)

c     Write(*,*) 'ev,pe,pk=',gen_event_id_number,
c     &                 (aa1(j),j=1,3),(aa2(j),j=1,3)

                  Pe0 = gpbeam

                  et_t = 0
                  et_h = 0
                  et_c = 0
                  
                  et_t = Pe0 * Pe0 + mass_electron * mass_electron
                  et_t = sqrt(et_t)
                  
c     local_m? is nuclear mass.

                  et_h = et_t + local_mp
                  et_c = et_t + local_mc
c     --- do not change the order.
                  et_t = et_t + gtarg_nucl_mass(gtarg_num)
                  
c     Write(*,*) '(cphys) pe0,etc,eth',Pe0,et_c,et_h
                  
                  if(c_target_eloss_on .eq. 1) then
                     Ek = hscorre(ih) 
                     Pk = hscorrp(ih)
                  Else
                     Ek = hsenergy(ih)
                     Pk = hsp(ih)
                  EndIf

                  if(c_target_eloss_on .eq. 1) then
                     Eep = escorre(ie) 
                     Pep = escorrp(ie)
                  Else
                     Eep = esenergy(ie)
                     Pep = esp(ie)
                  EndIf

c     Write(*,*) '(cphys) ek,pk,eep,pep=',Ek,Pk,Eep,Pep

                  coseep = cos(estheta(ie))
                  cosek = cos(hstheta(ih))
                  
                  a1 = 0
                  a2 = 0
                  det = hsxp_tar(ih)*(-esxp_tar(ie)) 
     &                 + hsyp_tar(ih)*(-esyp_tar(ie)) + 1.0
                  a1 = hsxp_tar(ih)*hsxp_tar(ih) 
     &                 + hsyp_tar(ih)*hsyp_tar(ih) + 1.0
                  a1 = sqrt(a1)
                  a2 = esxp_tar(ie)*esxp_tar(ie) 
     &                 + esyp_tar(ie)*esyp_tar(ie) + 1.0
                  a2 = sqrt(a2)
                  cosepk = det/a1/a2
                  
                  Plamsq = Pe0*Pe0 + Pep*Pep + Pk*Pk
     &                 -2.0 * Pe0 * Pep * coseep
     &                 -2.0 * Pe0 * Pk * cosek 
     &                 +2.0 * Pep * Pk * cosepk
                  
                  Elam_h = Et_h - Ek - Eep
                  Elam_c = Et_c - Ek - Eep
                  Elam_t = Et_t - Ek - Eep
                  Mlamsq_h = Elam_h*Elam_h - Plamsq
                  Mlamsq_c = Elam_c*Elam_c - Plamsq
                  Mlamsq_t = Elam_t*Elam_t - Plamsq
                  
                  if(Mlamsq_h .lt. 0) then
                     be_h = -4000.
                  Else
                     be_h = (sqrt(Mlamsq_h) - mass_lambda)*1000
                  EndIf
                  if(Mlamsq_c .lt. 0) then
                     be_c = -4000.
                  Else
                     be_c = (sqrt(Mlamsq_c) - mass_lambda
     &                    - local_mb)*1000
                  EndIf
                  if(Mlamsq_t .lt. 0) then
                     be_t = -4000.
                  Else
                     be_t = (sqrt(Mlamsq_t) - mass_lambda
     &                    - gtarg_core_mass(gtarg_num))*1000
                  EndIf
                  
                  cmissmass_tar(cn) = Real(be_t)
                  cmissmass_h(cn) = Real(be_h)
                  cmissmass_c(cn) = Real(be_c)

                  CTIME_COIN_COR(cn) = delta_ctime
                  
               EndIf            ! < cnphysics_max
            ENDIF               ! good events
         EndDo                  ! enphysics loop
      EndDo                     ! hnphysics loop
      
c     --- add filter to reduce multiplicity.
c      Do i=1,cnphysics
c         filter(i) = 1
c      EndDo
c      temp_phys = 0
c     ---!! make sure all common block except multiplitity are filled here. 
c      Do i=1,cnphysics
c         if(filter(i) .eq. 1) then
c            temp_phys = temp_phys + 1
c            cmissmass_h(temp_phys) = cmissmass_h(i)
c            cmissmass_tar(temp_phys) = cmissmass_tar(i)
c            CTIME_COIN_COR(temp_phys) = ctime_coin_cor(i)
c            cnhks(temp_phys) = cnhks(i)
c            cnhes(temp_phys) = cnhes(i)
c         EndIf
c      EndDo

c      cnphysics = temp_phys
     
c     --- check multiplicity
      
      if(cnphysics .gt. 0) then
         c_hks_multi = 1
         c_hes_multi = 1
      Else
         c_hks_multi = 0
         c_hes_multi = 0
      EndIf
      
      Do i=1,cnphysics-1
         Do j=i,cnphysics
            num1=cnhks(i)
            num2=cnhks(j)
            if(num1 .ne. num2) c_hks_multi = c_hks_multi + 1
            num1=cnhes(i)
            num2=cnhes(j)
            if(num1 .ne. num2) c_hes_multi = c_hes_multi + 1
         EndDo
      EndDo

c     --- fill coin histogram
      
      Call HF1(cidnphys,float(cnphysics),1.)
      Call HF1(cidhksmulti,float(c_hks_multi),1.)
      Call HF1(cidhesmulti,float(c_hes_multi),1.)

c      Do i=1,cnphysics
c         tempp1 = esp(cnhes(i))
c         tempp2 = hsp(cnhks(i))
c         Call HF1(cidmissmassh,cmissmass_h(i),1.)
c         Call HF1(cidmissmassc,cmissmass_tar(i),1.)
c         Call HF1(cidmissmassczoom,cmissmass_tar(i),1.)
cc     Write(*,*) '(cphys)pep,pk=',cidpeppk,tempp1,tempp2
c         Call HF2(cidpeppk,tempp1,tempp2,1.)
cc     --- zoom only
c         Call HF1(cidcointimezoom,ctime_coin_cor(i),1.)
c      EndDo

      return
      end
