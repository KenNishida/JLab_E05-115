      subroutine e_coll_trans(ABORT,err)
*--------------------------------------------------------
*     -
*     -   Purpose and Methods :  Transforms tracks from hes target to 
*     -                          collimator plane.
*     -
*     -      Required Input BANKS     Enge_TARGET
*     -
*     -      Output BANKS             Enge_SIEVE
*     -
*     -   Output: ABORT           - success or failure
*     -         : err             - reason for failure, if any

*     Coordinates on collimator plane:
*     X=right, Y=down, Z=downstream
*     L.Yuan  04/19/2005  First version     
*--------------------------------------------------------------
      IMPLICIT NONE
      SAVE
*     
      character*12 here
      parameter (here= 'e_coll_trans')
*     
      logical ABORT
      character*(*) err
*
      include 'gen_data_structures.cmn'
      INCLUDE 'hes_data_structures.cmn'
      include 'hes_tracking.cmn'
      include 'hes_recon_elements.cmn'
      include 'hes_physics_sing.cmn'
*     local variables
      integer*4 istat,itrk, i,j
      real*8 sum(2), hut(3), term
*
*=============================Executable Code ================================
      ABORT= .FALSE.
      err= ' '
      
*     Check for correct initialization.
      
      if (e_t2s_initted.ne.1) then
         istat = 2
         return
      endif
      istat = 1
      
*     Loop over tracks.
      
      do itrk = 1,entracks_tar
*     
*     Reset COSY sums.
         do i = 1,2
            sum(i) = 0.
         enddo
c     --- xp,yp at target, point target x=y=0 (cm)
         hut(1) = (exp_tar(itrk)-Center_of_etar(1))/Nfactor_of_etar(1) 
         ! rad
         hut(2) = (eyp_tar(itrk)-Center_of_etar(2))/Nfactor_of_etar(2) 
         ! rad
         hut(3) = (ep_tar(itrk)-Center_of_etar(3))/Nfactor_of_etar(3)
c         hut(3) = edelta_tar(itrk)
*     Compute COSY sums.
         do i = 1,e_num_t2s_terms
            term = 1.
            do j = 1,3
               if (e_t2s_expon(j,i).ne.0.)
     $              term = term*hut(j)**e_t2s_expon(j,i)
            enddo
*
            sum(1) = sum(1) + term * e_t2s_coeff(1,i)
            sum(2) = sum(2) + term * e_t2s_coeff(2,i)
         enddo
*     fill Enge Collimator variables
         ex_sv(itrk) = sum(1)
         ey_sv(itrk) = sum(2)
*     Angles are not calculated on Collimator plane
         exp_sv(itrk) = 0.0
         eyp_sv(itrk) = 0.0
*     
         enddo         ! end loop on track
**
         return
         end

         
