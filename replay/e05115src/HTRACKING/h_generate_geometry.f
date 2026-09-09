      subroutine h_generate_geometry
*
*     This subroutine reads in the wire layer parameters and fills all the
*     geometrical constants used in Track Fitting for the HKS spectrometer
*     The constants are stored in sos_geometry.cmn
*
*     d.f. geesaman           2 Sept 1993
*     modified                14 feb 1994 for CTP input.
*                             Change SLAYER_PARAM to individual arrays
* $Log: h_generate_geometry.f,v $
* Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
* e05115 src repository for software development
*
* Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
* Revision 1.6  2005/04/21 23:25:29  miyoshi
* add target variables readout for debuging only
*
* Revision 1.5  2005/04/08 20:43:49  miyoshi
* add comments
*
* Revision 1.4  2005/03/07 22:07:06  yuan
* Modify hdc_readout_corr calculation for HDC
*
* Revision 1.3  2004/12/24 21:33:07  miyoshi
* change name plane to layer
*
* Revision 1.2  2004/12/24 19:47:07  miyoshi
* change minor
*
* Revision 1.1.1.1  2004/08/30 21:21:39  miyoshi
* new dir
*
* Revision 1.7  1996/09/05 13:30:18  saw
* (JRA) Format statement changes
*
* Revision 1.6  1996/04/30 17:13:09  saw
* (JRA) Set up card drift time delay structures
*
* Revision 1.5  1995/10/10 14:21:21  cdaq
* (JRA) Calculate wire velocity correction parameters.  Cosmetics and comments
*
* Revision 1.4  1995/05/22 19:45:40  cdaq
* (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*
* Revision 1.3  1995/04/01  20:42:06  cdaq
* (SAW) Use sdc_layers_per_chamber instead of (sdc_num_layers/sdc_num_chambers)
*
* Revision 1.2  1994/11/22  20:19:22  cdaq
* (SPB) Recopied from hms file and modified names for SOS
* (SAW) Remove hardwired layer and chamber counts.
*
* Revision 1.1  1994/02/21  16:14:17  cdaq
* Initial revision
*
*
      implicit none
      include "hks_data_structures.cmn"
      include "hks_tracking.cmn"
      include "hks_geometry.cmn"
      include "gen_run_info.cmn"
*
*     local variables
      logical missing_card_no
      integer*4 pln,i,j,k,pindex,ich
      real*4 cosalpha,sinalpha,cosbeta,sinbeta,cosgamma,singamma,z0
      real*4 stubxchi,stubxpsi,stubychi,stubypsi
      real*4 sumsqupsi,sumsquchi,sumcross,denom
      Character*35 filename
      Integer*4 temp_lun
      Parameter(temp_lun=88)
      Integer*4 temp_lun2
      Parameter(temp_lun2=84)
*     
*     open track output file
*     
      if(h_track_output_on .eq. 1) then
         filename='output/hks_track_output_%d.dat'
         call g_sub_run_number(filename,gen_run_number)
         open(temp_lun,STATUS='OLD',FILE=filename,ERR=87)
         Write(*,*) filename,' is opened successfully on lun=',temp_lun
         go to 88
 87      Write(*,*) 'hks track output file is not opened :',filename
         Write(*,*) 'exexuting HKS tracking code'
         close(temp_lun)
 88      continue
      EndIF

      if(h_tar_output_on .eq. 1) then
         filename='output/htar_%d.dat'
         call g_sub_run_number(filename,gen_run_number)
         open(temp_lun2,STATUS='OLD',FILE=filename,ERR=83)
         Write(*,*) filename,' is opened successfully on lun=',temp_lun
         go to 82
 83      Write(*,*) 'hks target outoput file is not opened :',filename
         Write(*,*) 'exexuting HKS targ trans'
         close(temp_lun2)
 82      continue
      EndIF

*     read basic parameters from CTP input file
*     hdc_zpos(pln) = Z0
*     hdc_alpha_angle(pln) = ALPHA
*     hdc_beta_angle(pln)  = BETA
*     hdc_gamma_angle(pln) = GAMMA
*     hdc_pitch(pln)       = Wire spacing
*     hdc_nrwire(pln)      = Number of wires
*     hdc_central_wire(pln) = Location of center of wire 1
*     hdc_sigma(pln)       = sigma
*     
      hdc_layers_per_chamber = hdc_num_layers / hdc_num_chambers
      
      missing_card_no = .false.
      do j=1,hmax_num_dc_layers
         do i=1,hdc_max_wires_per_layer
            if (hdc_card_no(i,j).eq.0) then
               write(6,*) 'card number = 0 for wire,layer=',i,j
               missing_card_no = .true.
               hdc_card_no(i,j)=1 !avoid 0 in array index
               hdc_card_delay(1)=0 !no delay for wires
            endif
         enddo
      enddo
      if (missing_card_no) write(6,*) 
     &     'missing hdc_card_no(IGNORE THIS-JRA)'
*
*     loop over all layers
*     
      do pln=1,hdc_num_layers
         hdc_layer_num(pln)=pln
         z0=hdc_zpos(pln)
         cosalpha = cos(hdc_alpha_angle(pln))
         sinalpha = sin(hdc_alpha_angle(pln))
         cosbeta  = cos(hdc_beta_angle(pln))
         sinbeta  = sin(hdc_beta_angle(pln))
         cosgamma = cos(hdc_gamma_angle(pln))
         singamma = sin(hdc_gamma_angle(pln))
*     
         hsinbeta(pln) = sinbeta
         hcosbeta(pln) = cosbeta
*     make sure cosbeta is not zero
         if(abs(cosbeta).lt.1e-10) then
            write(hluno,'(38Hunphysical beta rotation in hks layer ,
     &           i4,10H     beta=,f10.5)') pln,hdc_beta_angle(pln)
         endif
         htanbeta(pln) = sinbeta / cosbeta
*     
*     compute chi,psi to x,y,z transformation coefficient(comments are beta=gamma=0)
         hzchi(pln) = -cosalpha*sinbeta + sinalpha*cosbeta*singamma !  =0.
         hzpsi(pln) =  sinalpha*sinbeta + cosalpha*cosbeta*singamma !  =0.
         hxchi(pln) = -cosalpha*cosbeta - sinalpha*sinbeta*singamma !-cos(a)
         hxpsi(pln) =  sinalpha*cosbeta - cosalpha*sinbeta*singamma ! sin(a)
         hychi(pln) =  sinalpha*cosgamma ! sin(a)
         hypsi(pln) =  cosalpha*cosgamma ! cos(a)
*     
*     stub transformations are done in beta=gamma=0 system
         stubxchi = -cosalpha   !-cos(a)
         stubxpsi =  sinalpha   ! sin(a)
         stubychi =  sinalpha   ! sin(a)
         stubypsi =  cosalpha   ! cos(a)
         
*     parameters for wire propogation correction. dt=distance from centerline of
*     chamber = ( xcoeff*x + ycoeff*y )*corr / veloc.
* For HKS DC, all cared are at +Y or -Y sides 
*
         hdc_readout_corr(pln) = 1./sinalpha
*     
*     fill spsi0,schi0,sz0  used in stub fit
*     
         sumsqupsi = hzpsi(pln)**2 + hxpsi(pln)**2 + hypsi(pln)**2 ! =1.
         sumsquchi = hzchi(pln)**2 + hxchi(pln)**2 + hychi(pln)**2 ! =1.
         sumcross =   hzpsi(pln)*hzchi(pln) + hxpsi(pln)*hxchi(pln)
     &        + hypsi(pln)*hychi(pln) ! =0.
         denom = sumsqupsi*sumsquchi-sumcross**2 ! =1.
         hpsi0(pln) = (-z0*hzpsi(pln)*sumsquchi ! =0.
     &        +z0*hzchi(pln)*sumcross) / denom
         hchi0(pln) = (-z0*hzchi(pln)*sumsqupsi ! =0.
     &        +z0*hzpsi(pln)*sumcross) / denom
*     calculate magnitude of sphi0                                   ! =z0
         hphi0(pln) = sqrt(
     &        (z0+hzpsi(pln)*hpsi0(pln)+hzchi(pln)*hchi0(pln))**2
     &        + (hxpsi(pln)*hpsi0(pln)+hxchi(pln)*hchi0(pln))**2
     &        + (hypsi(pln)*hpsi0(pln)+hychi(pln)*hchi0(pln))**2 )
         if(z0.lt.0) hphi0(pln)=-hphi0(pln)        
*     
*     sstubcoef used in stub fits. check these.  I don't think they are correct
         denom = stubxpsi*stubychi - stubxchi*stubypsi !  =1.
         hstubcoef(pln,1)= stubychi/(hdc_sigma(pln)*denom) !sin(a)/sigma
         hstubcoef(pln,2)= -stubxchi/(hdc_sigma(pln)*denom) !cos(a)/sigma
         hstubcoef(pln,3)= hphi0(pln)*hstubcoef(pln,1) !z0*sin(a)/sig
         hstubcoef(pln,4)= hphi0(pln)*hstubcoef(pln,2) !z0*cos(a)/sig
*     
*     xsp and ysp used in space point pattern recognition
*     
         hxsp(pln) = hychi(pln) / denom !sin(a)
         hysp(pln) = -hxchi(pln) / denom !cos(a)
*     
*     compute track fitting coefficients
*     
         hlayer_coeff(1,pln)= hzchi(pln) !  =0.
         hlayer_coeff(2,pln)=-hzchi(pln) !  =0.
         hlayer_coeff(3,pln)= hychi(pln)*(hdc_zpos(pln)-hlocrayzt) !sin(a)*(z-slocrayzt)
         hlayer_coeff(4,pln)= hxchi(pln)*(hlocrayzt-hdc_zpos(pln)) !cos(a)*(z-slocrayzt)
         hlayer_coeff(5,pln)= hychi(pln) !sin(a)
         hlayer_coeff(6,pln)=-hxchi(pln) !cos(a)
         hlayer_coeff(7,pln)= hzchi(pln)*hypsi(pln) - hychi(pln)*hzpsi(pln) !0.
         hlayer_coeff(8,pln)=-hzchi(pln)*hxpsi(pln) + hxchi(pln)*hzpsi(pln) !0.
         hlayer_coeff(9,pln)= hychi(pln)*hxpsi(pln) - hxchi(pln)*hypsi(pln) !1.
*
      enddo                  !  end loop over all layers
      
*     djm 10/2/94 generate/store the inverse matrices HAAINV3(i,j,pindex) used in solve_3by3_hdc
*     pindex = 1    layer 1 missing from hdc1
*     pindex = 2    layer 2 missing from hdc1
*     etc.
*     pindex = 7    layer 1 missing from hdc2
*     pindex = 8    layer 2 missing from hdc2
*     etc.
*     pindex = 13   hdc1 no missing layers
*     pindex = 14   hdc2 no missing layers
      
*     
*     The following is pretty gross, but might actually work for an
*     arbitrary number of chambers if each chamber has the same number of
*     layers and hdc_num_layers is HDC_NUM_CHAMBERS * # of layers/chamber
*     
      do pindex=1,hdc_num_layers+HDC_NUM_CHAMBERS
         
*     generate the matrix HAA3 for an hdc missing a particular layer
         do i=1,3
            do j=1,3
               HAA3(i,j)=0.
               if(j.lt.i)then   ! HAA3 is symmetric so only calculate 6 terms
                  HAA3(i,j)=HAA3(j,i)
               else
                  if(pindex.le.hdc_num_layers) then
                     ich = (pindex-1)/(hdc_layers_per_chamber)+1
                     do k=(ich-1)*(hdc_layers_per_chamber)+1
     $                    ,ich*(hdc_layers_per_chamber)
                        if(pindex.ne.k) then
                           HAA3(i,j)=HAA3(i,j) 
     &                          + hstubcoef(k,i)*hstubcoef(k,j)
                        endif
                     enddo
                  else
                     ich = pindex - hdc_num_layers
                     do k=(ich-1)*(hdc_layers_per_chamber)+1
     $                    ,ich*(hdc_layers_per_chamber)
                        HAA3(i,j)=HAA3(i,j) 
     &                       + hstubcoef(k,i)*hstubcoef(k,j)
                     enddo
                  endif
               endif            !end test j lt i
            enddo               !end j loop
         enddo                  !end i loop
         
*     form the inverse matrix HAAINV3 for each configuration
         HAAINV3(1,1,pindex)=(HAA3(2,2)*HAA3(3,3)-HAA3(2,3)**2)
         HAAINV3(1,2,pindex)=-(HAA3(1,2)*HAA3(3,3)-HAA3(1,3)*HAA3(2,3))
         HAAINV3(1,3,pindex)=(HAA3(1,2)*HAA3(2,3)-HAA3(1,3)*HAA3(2,2))
         HDET3(pindex)=HAA3(1,1)*HAAINV3(1,1,pindex)
     $        +HAA3(1,2)*HAAINV3(1,2,pindex)
     $        +HAA3(1,3)*HAAINV3(1,3,pindex)
         if(abs(hdet3(pindex)).le.1e-20)then
            write(6,*) 
     &           '*************************************************'
            write(6,*)
     &           'Warning!!'
            write(6,*) 
     &           ' Determinate of matrix HAA3(i,j) is nearly zero.'
            write(6,*) 
     &           'All tracks using pindex=',pindex,' will be zerfucked.'
            write(6,*)'Fix problem in h_generate_geometry.f or else!'
            write(6,*) 
     &           '***************************************************'
            hdet3(pindex)=1.
         endif
         HAAINV3(1,1,pindex)=HAAINV3(1,1,pindex)/HDET3(pindex)
         HAAINV3(1,2,pindex)=HAAINV3(1,2,pindex)/HDET3(pindex)
         HAAINV3(1,3,pindex)=HAAINV3(1,3,pindex)/HDET3(pindex)
         HAAINV3(2,2,pindex)=
     &        (HAA3(1,1)*HAA3(3,3)-HAA3(1,3)**2)/HDET3(pindex)
         HAAINV3(2,3,pindex)= -(HAA3(1,1)*HAA3(2,3) 
     &        - HAA3(1,2)*HAA3(3,1))
     $        /HDET3(pindex)
         HAAINV3(3,3,pindex)= (HAA3(1,1)*HAA3(2,2)-HAA3(1,2)**2)
     &        /HDET3(pindex)
         
      enddo                     !end pindex loop
      
*     for debug write out all parameters
      if(hdebugflaggeometry.ne.0) then
         write(hluno,*) '   HKS LAYER PARAMETERS:  '
         write(hluno,*) ' layer   z0      alpha      beta     gamma   ',
     &        ' wire number  center  resolution'
         write(hluno,*) '  number                                 ',
     &        'spaciing      wires  position '
         write(hluno,1000) (hdc_layer_num(j),
     &        hdc_zpos(j),
     &        hdc_alpha_angle(j),
     &        hdc_beta_angle(j),
     &        hdc_gamma_angle(j),
     &        hdc_pitch(j),
     &        hdc_nrwire(j),
     &        hdc_central_wire(j),
     &        hdc_sigma(j),j=1,hdc_num_layers)
 1000    format(1x,i4,f9.4,3f10.6,f8.4,i6,f10.4,f10.6)
         write(hluno,*) '  layer  hzchi',
     &        '     hzpsi     hxchi     hxpsi     hychi     hypsi '
         write(hluno,1001) 
     &        (i, hzchi(i),hzpsi(i),hxchi(i),hxpsi(i),hychi(i),
     &        hypsi(i),i=1,hdc_num_layers )
 1001    format(i5,6f10.6)
         write(hluno,*) 'layer',
     &        '    hpsi0       hchi0       hphi0'
         write(hluno,1002) 
     &        (i, hpsi0(i),hchi0(i),hphi0(i),i=1,hdc_num_layers)
 1002    format(i5,3f12.6)
         write(hluno,*) '  layer',
     &        '     sstubcoef 1        2              3             4'
         write(hluno,1003) (i, hstubcoef(i,1),hstubcoef(i,2),
     &        hstubcoef(i,3),
     &        hstubcoef(i,4),i=1,hdc_num_layers)
 1003    format(i5,4f15.6)
         write(hluno,*) '                 hlayer_coeff '
         write(hluno,*) ' layer     1        2       3        4     ',
     &        '5      6       7       8        9'
         do j=1,hdc_num_layers
            write(hluno,1004) j,(hlayer_coeff(i,j),i=1,9) 
         enddo                  ! end of print over layers loop
 1004    format(1x,i3,f10.5,2f8.3,f9.3,4f8.3,f9.3)
*     
      endif                     !   end if on debug print out
      return
      end
