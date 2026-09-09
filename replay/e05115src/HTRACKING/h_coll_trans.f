      subroutine h_coll_trans(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods :  Transforms tracks from hks target to 
*     -                          collimator plane.
*     -
*     -      Required Input BANKS     HKS_TARGET
*     -
*     -      Output BANKS             HKS_SIEVE
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any

*     Coordinates on collimator plane:
*     X=left, Y=up, Z=downstream
*     L.Yuan  04/19/2005  First version     
*--------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*12 here
      parameter (here= 'h_coll_trans')
*     
      logical ABORT
      character*(*) err
*
      include 'gen_data_structures.cmn'
      INCLUDE 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      include 'hks_recon_elements.cmn'
      include 'hks_physics_sing.cmn'
*     local variables
      integer*4 istat,itrk, i,j
      real*8 sum(2), hut(3), term
*
*=============================Executable Code ================================
      ABORT= .FALSE.
      err= ' '
      
*     Check for correct initialization.
      
      if (h_t2s_initted.ne.1) then
         istat = 2
         return
      endif
      istat = 1
      
*     Loop over tracks.
      
      do itrk = 1,hntracks_tar
*     
*     Reset COSY sums.
         do i = 1,2
            sum(i) = 0.
         enddo
c     --- xp,yp at target, point target x=y=0 (cm)
         hut(1) = (hxp_tar(itrk)-Center_of_htar(1))/Nfactor_of_htar(1)  
         ! rad
         hut(2) = (hyp_tar(itrk)-Center_of_htar(2))/Nfactor_of_htar(2)  
         ! rad
         hut(3) = (hp_tar(itrk)-Center_of_htar(3))/Nfactor_of_htar(3) 
         ! GeV/c
c         hut(3) = hdelta_tar(itrk)
*     Compute COSY sums.
         do i = 1,h_num_t2s_terms
            term = 1.
            do j = 1,3
               if (h_t2s_expon(j,i).ne.0.)
     $              term = term*hut(j)**h_t2s_expon(j,i)
            enddo
*
            sum(1) = sum(1) + term * h_t2s_coeff(1,i)
            sum(2) = sum(2) + term * h_t2s_coeff(2,i)
         enddo
*     fill Hks Collimator variables
         hx_sv(itrk) = sum(1)
         hy_sv(itrk) = sum(2)
*     Angles are not calculated on Collimator plane
         hxp_sv(itrk) = 0.0
         hyp_sv(itrk) = 0.0
*     
      enddo                     ! end loop on track
**
      return
      end

         
