      subroutine e_dc1_generate_geometry
*     
*     This subroutine reads in the wire plane parameters and fills all the
*     geometrical constants used in Track Fitting for the Enge spectrometer
*     The constants are stored in hnss_geometry.cmn
*
*     copy from s_generate_geometry.f
*     T. Miyoshi May 11 2003
*
*
      implicit none
      include "hes_data_structures.cmn"
      include "hes_tracking.cmn"
      include "hes_geometry.cmn"
      include "gen_run_info.cmn"
*     
*     local variables
      logical missing_card_no
      integer*4 pln,i,j,k,pindex,ich
      real*4 cosalpha,sinalpha,cosbeta
      real*4 sinbeta,cosgamma,singamma,z0
      real*4 stubxchi,stubxpsi,stubychi,stubypsi
      real*4 sumsqupsi,sumsquchi,sumcross,denom

      Character*35 filename
      Integer*4 temp_lun
      Parameter(temp_lun=86)
      Integer*4 temp_lun2
      Parameter(temp_lun2=82)
      real*4 raddeg
      parameter (raddeg = 3.14159265/180.)

      if(e_track_output_on .eq. 1) then
         filename='output/hes_track_output_%d.dat'
         call g_sub_run_number(filename,gen_run_number)
         open(temp_lun,STATUS='OLD',FILE=filename,ERR=87)
         Write(*,*) filename,' is opened successfully on lun=',temp_lun
         go to 88
 87      Write(*,*) 'hes track output file is not opened :',filename
         Write(*,*) 'exexuting ENGE tracking code'
         close(temp_lun)
 88      continue
      EndIF

      if(e_tar_output_on .eq. 1) then
         filename='output/etar_%d.dat'
         call g_sub_run_number(filename,gen_run_number)
         open(temp_lun2,STATUS='OLD',FILE=filename,ERR=83)
         Write(*,*) filename,' is opened successfully on lun=',temp_lun
         go to 81
 83      Write(*,*) 'hes target outoput file is not opened :',filename
         Write(*,*) 'exexuting ENGE targ trans'
         close(temp_lun2)
 81      continue
      EndIF

*     
*     read basic parameters from CTP input file
c     edc1_zpos(pln) = Z0
c     edc1_alpha_angle(pln) = ALPHA
c     edc1_beta_angle(pln)  = BETA
c     edc1_gamma_angle(pln) = GAMMA
c     edc1_pitch(pln)       = Wire spacing
c     edc1_nrwire(pln)      = Number of wires
c     edc1_central_wire(pln) = Location of center of wire 1
c     edc1_sigma(pln)       = sigma
*     
      
      missing_card_no = .false.
      do j=1,emax_num_dc1_layers
         do i=1,edc1_max_wires_per_layer
            if (edc1_card_no(i,j).eq.0) then
c     write(6,*) 'card number = 0 for wire,layer=',i,j
               missing_card_no = .true.
               edc1_card_no(i,j)=1 !avoid 0 in array index
               edc1_card_delay(1)=0 !no delay for wires
            endif
         enddo
      enddo
      if (missing_card_no) write(6,*) 
     &     'missing edc1_card_no(IGNORE THIS-JRA)'
      
*     
*     loop over all layers
c     open(UNIT=92,FILE='PARAM/edriftcoeff.param',STATUS='OLD')
c     Do pln=1,edc1_num_layers
c     Read(92,*) (edrift_coeff(i,pln),i=1,4) 
c     EndDo
c     close(92)
      
      do pln=1,edc1_num_layers
c         edc1_layer_num(pln)=pln
         z0=edc1_zpos(pln)
         cosalpha = cos(edc1_alpha_angle(pln)-90*raddeg)
         sinalpha = sin(edc1_alpha_angle(pln)-90*raddeg)
         cosbeta  = cos(edc1_beta_angle(pln))
         sinbeta  = sin(edc1_beta_angle(pln))
         cosgamma = cos(edc1_gamma_angle(pln))
         singamma = sin(edc1_gamma_angle(pln))
*     
         edc1sinbeta(pln) = sinbeta
         edc1cosbeta(pln) = cosbeta
*     make sure cosbeta is not zero
         if(abs(cosbeta).lt.1e-10) then
            write(eluno,'(39H unphysical beta rotation in Enge layer,i4,
     &           10H    beta=,f10.5)') pln,edc1_beta_angle(pln)
         endif
         edc1tanbeta(pln) = sinbeta / cosbeta
*              
*     compute chi,psi to x,y,z transformation coefficient(comments are beta=gamma=0)

         edc1zchi(pln) = -cosalpha*sinbeta + sinalpha*cosbeta*singamma !  =0.
         edc1zpsi(pln) =  sinalpha*sinbeta + cosalpha*cosbeta*singamma !  =0.
         edc1xchi(pln) =  cosalpha*cosbeta - sinalpha*sinbeta*singamma !-cos(a)
         edc1xpsi(pln) =  sinalpha*cosbeta - cosalpha*sinbeta*singamma ! sin(a)
         edc1ychi(pln) =  sinalpha*cosgamma ! sin(a)
         edc1ypsi(pln) =  cosalpha*cosgamma ! cos(a)
         
*     stub transformations are done in beta=gamma=0 system
         stubxchi = -cosalpha   !-cos(a)
         stubxpsi =  sinalpha   ! sin(a)
         stubychi =  sinalpha   ! sin(a)
         stubypsi =  cosalpha   ! cos(a)
         
*     parameters for wire propogation correction. 
*     dt=distance from centerline of
*     chamber = ( xcoeff*x + ycoeff*y )*corr / veloc.
         
         if (cosalpha .le. 0.707) then  
c     x-like wire, need dist. from x=0 line
c     edc_readout_x(pln) = .true.
            edc1_readout_corr(pln) = 1./sinalpha
         else                           
c     y-like wire, need dist. from y=0 line
c     edc_readout_x(pln) = .false.
            edc1_readout_corr(pln) = 1./cosalpha
         endif
         
*     
*     fill edc1psi0,edc1chi0,ez0  used in stub fit
*     
         sumsqupsi = edc1zpsi(pln)**2 + edc1xpsi(pln)**2 + edc1ypsi(pln)**2 ! =1.
         sumsquchi = edc1zchi(pln)**2 + edc1xchi(pln)**2 + edc1ychi(pln)**2 ! =1.
         sumcross = edc1zpsi(pln)*edc1zchi(pln) + edc1xpsi(pln)*edc1xchi(pln)
     &        + edc1ypsi(pln)*edc1ychi(pln) ! =0.
         denom = sumsqupsi*sumsquchi-sumcross**2 ! =1.
         edc1psi0(pln) = (-z0*edc1zpsi(pln)*sumsquchi ! =0.
     &        +z0*edc1zchi(pln)*sumcross) / denom
         edc1chi0(pln) = (-z0*edc1zchi(pln)*sumsqupsi ! =0.
     &        +z0*edc1zpsi(pln)*sumcross) / denom
*     calculate magnitude of sphi0                   ! =z0
         edc1phi0(pln) = sqrt(
     &        (z0+edc1zpsi(pln)*edc1psi0(pln)+edc1zchi(pln)*edc1chi0(pln))**2
     &        + (edc1xpsi(pln)*edc1psi0(pln)+edc1xchi(pln)*edc1chi0(pln))**2
     &        + (edc1ypsi(pln)*edc1psi0(pln)+edc1ychi(pln)*edc1chi0(pln))**2 )
         if(z0.lt.0) edc1phi0(pln)=-edc1phi0(pln)        
*     
*     sstubcoef used in stub fits. check these.  I don't think they are correct
         denom = stubxpsi*stubychi - stubxchi*stubypsi !  =1.
         edc1stubcoef(pln,1)= stubychi/(edc1_sigma(pln)*denom) !sin(a)/sigma
         edc1stubcoef(pln,2)= -stubxchi/(edc1_sigma(pln)*denom) !cos(a)/sigma
         edc1stubcoef(pln,3)= edc1phi0(pln)*edc1stubcoef(pln,1) !z0*sin(a)/sig
         edc1stubcoef(pln,4)= edc1phi0(pln)*edc1stubcoef(pln,2) !z0*cos(a)/sig
*     
*     xsp and ysp used in space point pattern recognition
*     
         edc1xsp(pln) = edc1ychi(pln) / denom !sin(a)
         edc1ysp(pln) = -edc1xchi(pln) / denom !cos(a)
*     
*     compute track fitting coefficients
*     
         edc1layer_coeff(1,pln)= edc1zchi(pln) !  =0.
         edc1layer_coeff(2,pln)=-edc1zchi(pln) !  =0.
         edc1layer_coeff(3,pln)= edc1xchi(pln)*(edc1_zpos(pln)-elocrayzt) !cos(a)*(z-slocrayzt)
         edc1layer_coeff(4,pln)= edc1ychi(pln)*(edc1_zpos(pln)-elocrayzt) !sin(a)*(z-slocrayzt)
c         edc1layer_coeff(3,pln)= edc1ychi(pln)*(edc1_zpos(pln)-elocrayzt) !sin(a)*(z-slocrayzt)
c         edc1layer_coeff(4,pln)= edc1xchi(pln)*(elocrayzt-edc1_zpos(pln)) !cos(a)*(z-slocrayzt)
         edc1layer_coeff(5,pln)= edc1xchi(pln) !cos(a)
         edc1layer_coeff(6,pln)= edc1ychi(pln) !sin(a)
c         edc1layer_coeff(5,pln)= edc1ychi(pln) !sin(a)
c         edc1layer_coeff(6,pln)=-edc1xchi(pln) !cos(a)
         edc1layer_coeff(7,pln)= edc1zchi(pln)*edc1ypsi(pln) - edc1ychi(pln)*edc1zpsi(pln) !0.
         edc1layer_coeff(8,pln)=-edc1zchi(pln)*edc1xpsi(pln) + edc1xchi(pln)*edc1zpsi(pln) !0.
         edc1layer_coeff(9,pln)= edc1ychi(pln)*edc1xpsi(pln) - edc1xchi(pln)*edc1ypsi(pln) !1.
*     
      enddo                     !  end loop over all layers
      
*     djm 10/2/94 generate/store the inverse matrices 
*     SAAINV3(i,j,pindex) used in solve_3by3_hdc
*     pindex = 1    layer 1 missing from edc
*     pindex = 2    layer 2 missing from edc
*     etc.
*     pindex = 7    layer 1 missing from edc2
*     pindex = 8    layer 2 missing from edc2
*     etc.
*     pindex = 13   edc no missing layers
*     pindex = 14   edc2 no missing layers      
*     
*     The following is pretty gross, but might actually work for an
*     arbitrary number of chambers if each chamber has the same number of
*     layers and edc_num_layers is EDC_NUM_CHAMBERS * # of layers/chamber
*     
      
      do pindex=1,emax_num_dc1_layers+1
         
*     generate the matrix EAA3 for an edc1 missing a particular layer
         do i=1,3
            do j=1,3
               EAA3(i,j)=0.
               if(j.lt.i)then   ! EAA3 is symmetric so only calculate 6 terms
                  EAA3(i,j)=EAA3(j,i)
               else
                  if(pindex.le.emax_num_dc1_layers) then
                     ich = 1
                     do k=1,emax_num_dc1_layers
                        if(pindex.ne.k) then
                           EAA3(i,j)=EAA3(i,j) 
     &                          + edc1stubcoef(k,i)*edc1stubcoef(k,j)
                        endif
                     enddo
                  else
                     ich = pindex - emax_num_dc1_layers
                     do k=(ich-1)*(emax_num_dc1_layers)+1
     $                    ,ich*(emax_num_dc1_layers)
                        EAA3(i,j)=EAA3(i,j) 
     &                       + edc1stubcoef(k,i)*edc1stubcoef(k,j)
                     enddo
                  endif
               endif            !end test j lt i
            enddo               !end j loop
c     Write(*,*) pindex,eaa3(i,1),eaa3(i,2),eaa3(i,3)
         enddo                  !end i loop
         
*     form the inverse matrix EAAINV3 for each configuration
         EAAINV3(1,1,pindex)=(EAA3(2,2)*EAA3(3,3)-EAA3(2,3)**2)
         EAAINV3(1,2,pindex)=-(EAA3(1,2)*EAA3(3,3)-EAA3(1,3)*EAA3(2,3))
         EAAINV3(1,3,pindex)=(EAA3(1,2)*EAA3(2,3)-EAA3(1,3)*EAA3(2,2))
         EDET3(pindex)=EAA3(1,1)*EAAINV3(1,1,pindex)+EAA3(1,2)*EAAINV3(1,2
     $        ,pindex)+EAA3(1,3)*EAAINV3(1,3,pindex)
c     Write(*,*) 'edet3=',pindex,EDET3(pindex)
         if(abs(edet3(pindex)).le.1e-20)then
            write(6,*)
     &           '****************************************************'
            write(6,*) 
     &           'Warning! Deter. of matrix EAA3(i,j) is nearly zero.'
            write(6,*) 'All tracks using pindex=',pindex,
     $           ' will be zerfucked.'
            write(6,*) 'Fix problem in h_generate_geometry.f or else!'
            write(6,*) 
     &           '****************************************************'
            edet3(pindex)=1.
         endif
         EAAINV3(1,2,pindex)=EAAINV3(1,2,pindex)/EDET3(pindex)
         EAAINV3(1,3,pindex)=EAAINV3(1,3,pindex)/EDET3(pindex)
         EAAINV3(2,2,pindex)=(EAA3(1,1)*EAA3(3,3)-EAA3(1,3)**2)
     &        /EDET3(pindex)
         EAAINV3(2,3,pindex)= -(EAA3(1,1)*EAA3(2,3)-EAA3(1,2)*EAA3(3,1))
     $        /EDET3(pindex)
         EAAINV3(3,3,pindex)=(EAA3(1,1)*EAA3(2,2)-EAA3(1,2)**2)
     &        /EDET3(pindex)
         
      enddo                     !end pindex loop
      
*     for debug write out all parameters
      
      
      if(edebugflaggeometry.ne.0) then
         write(eluno,'(''    Enge LAYER PARAMETERS: '')')
         write(eluno,'('' layer   z0      alpha      beta     gamma    wire  ''
     &        '' number  center  resolution'')')
         write(eluno,'('' number                                      spacing ''
     &        '' wires  position'')')
         write(eluno,1000) (edc1_layer_num(j),
     &        edc1_zpos(j),
     &        edc1_alpha_angle(j),
     &        edc1_beta_angle(j),
     &        edc1_gamma_angle(j),
     &        edc1_pitch(j),
     &        edc1_nrwire(j),
     &        edc1_central_wire(j),
     &        edc1_sigma(j),j=1,emax_num_dc1_layers) 
 1000    format(1x,i4,f9.4,3f10.6,f8.4,i6,f10.4,f10.6)
         write(eluno,*) 'layer  edc1zchi',
     &        '     edc1zpsi     edc1xchi     edc1xpsi     
     &   edc1ychi     edc1ypsi'
         write(eluno,1001) (i, edc1zchi(i),edc1zpsi(i),edc1xchi(i),edc1xpsi(i),
     &        edc1ychi(i),edc1ypsi(i),i=1,edc1_num_layers )
 1001    format(i5,6f10.6)
         write(eluno,*) 'layer    edc1psi0',
     &        '    edc1psi0       edc1chi0       edc1phi0'
         write(eluno,1002) 
     &        (i, edc1psi0(i),edc1chi0(i),edc1phi0(i),i=1,edc1_num_layers)
 1002    format(i5,3f12.6)
         write(eluno,*), 'layer         edc1stubcoef 1        2',
     &        '                3             4'
         write(eluno,1003) (i, edc1stubcoef(i,1),edc1stubcoef(i,2),
     &        edc1stubcoef(i,3),edc1stubcoef(i,4),i=1,edc1_num_layers)
 1003    format(i5,4f15.6)
         write(eluno,*) '                 klayer_coeff'
         write(eluno,*) ' layer     1        2       3        4',
     &        '        5       6       7       8        9'
         do j=1,edc1_num_layers
            write(eluno,1004) j,(edc1layer_coeff(i,j),i=1,9) 
         enddo                  ! end of print over layers loop
 1004    format(1x,i3,f10.5,2f8.3,f9.3,4f8.3,f9.3)
*     
      endif                     !   end if on debug print out
      
      return
      end
