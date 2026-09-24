      subroutine e_dc2_generate_geometry
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

c--   Local variable to transfer to e_dc2_best_stub
c--   L.Yuan   09/24/2009
*----------------------------------------------------------------------
* MATRICES FOR 3 PARAMETER FITS.
*
*     CTPTYPE=parm
*
* matrix AA and its inverse AAINV 
* array of determinants of AA

      real*8 edc2aa3(3,3),edc2aainv3(3,3,emax_num_dc2_layers+1)      
      real*8 edc2det3(emax_num_dc2_layers+1)             

      common/EDC2_TFIT_MATRIX/
     &     edc2aa3,
     &     edc2aainv3,
     &     edc2det3

*----------------------------------------------------------------------

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
c     edc2_zpos(pln) = Z0
c     edc2_alpha_angle(pln) = ALPHA
c     edc2_beta_angle(pln)  = BETA
c     edc2_gamma_angle(pln) = GAMMA
c     edc2_pitch(pln)       = Wire spacing
c     edc2_nrwire(pln)      = Number of wires
c     edc2_central_wire(pln) = Location of center of wire 1
c     edc2_sigma(pln)       = sigma
*     
      edc2_layers_per_chamber = edc2_num_layers 
      missing_card_no = .false.
      do j=1,emax_num_dc2_layers
         do i=1,edc2_max_wires_per_layer
            if (edc2_card_no(i,j).eq.0) then
c     write(6,*) 'card number = 0 for wire,layer=',i,j
               missing_card_no = .true.
               edc2_card_no(i,j)=1 !avoid 0 in array index
               edc2_card_delay(1)=0 !no delay for wires
            endif
         enddo
      enddo
      if (missing_card_no) write(6,*) 
     &     'missing edc2_card_no(IGNORE THIS-JRA)'
      
*     
*     loop over all layers
c     open(UNIT=92,FILE='PARAM/edriftcoeff.param',STATUS='OLD')
c     Do pln=1,edc2_num_layers
c     Read(92,*) (edrift_coeff(i,pln),i=1,4) 
c     EndDo
c     close(92)
      
      do pln=1,edc2_num_layers
         edc2_layer_num(pln)=pln
         z0=edc2_zpos(pln)
         cosalpha = cos(edc2_alpha_angle(pln))
         sinalpha = sin(edc2_alpha_angle(pln))
cDK         cosalpha = cos(edc2_alpha_angle(pln)-90*raddeg)
cDK         sinalpha = sin(edc2_alpha_angle(pln)-90*raddeg)
         cosbeta  = cos(edc2_beta_angle(pln))
         sinbeta  = sin(edc2_beta_angle(pln))
         cosgamma = cos(edc2_gamma_angle(pln))
         singamma = sin(edc2_gamma_angle(pln))
*     
         edc2sinbeta(pln) = sinbeta
         edc2cosbeta(pln) = cosbeta
*     make sure cosbeta is not zero
         if(abs(cosbeta).lt.1e-10) then
            write(eluno,'(39H unphysical beta rotation in Enge layer,i4,
     &           10H    beta=,f10.5)') pln,edc2_beta_angle(pln)
         endif
         edc2tanbeta(pln) = sinbeta / cosbeta
*              
*     compute chi,psi to x,y,z transformation coefficient(comments are beta=gamma=0)

         edc2zchi(pln) = -cosalpha*sinbeta + sinalpha*cosbeta*singamma !  =0.
         edc2zpsi(pln) =  sinalpha*sinbeta + cosalpha*cosbeta*singamma !  =0.
         edc2xchi(pln) = -cosalpha*cosbeta - sinalpha*sinbeta*singamma !-cos(a)
         edc2xpsi(pln) =  sinalpha*cosbeta - cosalpha*sinbeta*singamma ! sin(a)
         edc2ychi(pln) =  sinalpha*cosgamma ! sin(a)
         edc2ypsi(pln) =  cosalpha*cosgamma ! cos(a)
         
*     stub transformations are done in beta=gamma=0 system
         stubxchi = -cosalpha   !-cos(a)
         stubxpsi =  sinalpha   ! sin(a)
         stubychi =  sinalpha   ! sin(a)
         stubypsi =  cosalpha   ! cos(a)
         
*     parameters for wire propogation correction. 
*     dt=distance from centerline of
*     chamber = ( xcoeff*x + ycoeff*y )*corr / veloc.
         
         edc2_readout_corr(pln) = 1./sinalpha
c         if (cosalpha .le. 0.707) then  
c     x-like wire, need dist. from x=0 line
c     edc_readout_x(pln) = .true.
c            edc2_readout_corr(pln) = 1./sinalpha
c         else                           
c     y-like wire, need dist. from y=0 line
c     edc_readout_x(pln) = .false.
c            edc2_readout_corr(pln) = 1./cosalpha
c         endif
         
*     
*     fill edc2psi0,edc2chi0,ez0  used in stub fit
*     
         sumsqupsi = edc2zpsi(pln)**2 + edc2xpsi(pln)**2 + edc2ypsi(pln)**2 ! =1.
         sumsquchi = edc2zchi(pln)**2 + edc2xchi(pln)**2 + edc2ychi(pln)**2 ! =1.
         sumcross = edc2zpsi(pln)*edc2zchi(pln) + edc2xpsi(pln)*edc2xchi(pln)
     &        + edc2ypsi(pln)*edc2ychi(pln) ! =0.
         denom = sumsqupsi*sumsquchi-sumcross**2 ! =1.
         edc2psi0(pln) = (-z0*edc2zpsi(pln)*sumsquchi ! =0.
     &        +z0*edc2zchi(pln)*sumcross) / denom
         edc2chi0(pln) = (-z0*edc2zchi(pln)*sumsqupsi ! =0.
     &        +z0*edc2zpsi(pln)*sumcross) / denom
*     calculate magnitude of sphi0                   ! =z0
         edc2phi0(pln) = sqrt(
     &        (z0+edc2zpsi(pln)*edc2psi0(pln)+edc2zchi(pln)*edc2chi0(pln))**2
     &        + (edc2xpsi(pln)*edc2psi0(pln)+edc2xchi(pln)*edc2chi0(pln))**2
     &        + (edc2ypsi(pln)*edc2psi0(pln)+edc2ychi(pln)*edc2chi0(pln))**2 )
         if(z0.lt.0) edc2phi0(pln)=-edc2phi0(pln)        
*     
*     sstubcoef used in stub fits. check these.  I don't think they are correct
         denom = stubxpsi*stubychi - stubxchi*stubypsi !  =1.
         edc2stubcoef(pln,1)= stubychi/(edc2_sigma(pln)*denom) !sin(a)/sigma
         edc2stubcoef(pln,2)= -stubxchi/(edc2_sigma(pln)*denom) !cos(a)/sigma
         edc2stubcoef(pln,3)= edc2phi0(pln)*edc2stubcoef(pln,1) !z0*sin(a)/sig
         edc2stubcoef(pln,4)= edc2phi0(pln)*edc2stubcoef(pln,2) !z0*cos(a)/sig
*     
*     xsp and ysp used in space point pattern recognition
*     
         edc2xsp(pln) = edc2ychi(pln) / denom !sin(a)
         edc2ysp(pln) = -edc2xchi(pln) / denom !cos(a)
*     
*     compute track fitting coefficients
*     
         edc2layer_coeff(1,pln)= edc2zchi(pln) !  =0.
         edc2layer_coeff(2,pln)=-edc2zchi(pln) !  =0.
cDK         edc2layer_coeff(3,pln)= edc2xchi(pln)*(edc2_zpos(pln)-elocrayzt) !cos(a)*(z-slocrayzt)
cDK         edc2layer_coeff(4,pln)= edc2ychi(pln)*(edc2_zpos(pln)-elocrayzt) !sin(a)*(z-slocrayzt)
         edc2layer_coeff(3,pln)= edc2ychi(pln)*(edc2_zpos(pln)-elocrayzt) !sin(a)*(z-slocrayzt)
c         edc2layer_coeff(4,pln)= edc2xchi(pln)*(elocrayzt-edc2_zpos(pln)) !cos(a)*(z-slocrayzt)
         edc2layer_coeff(4,pln)= edc2xchi(pln)*(edc2_zpos(pln)-elocrayzt) !cos(a)*(z-slocrayzt)
cDK         edc2layer_coeff(5,pln)= edc2xchi(pln) !cos(a)
cDK         edc2layer_coeff(6,pln)= edc2ychi(pln) !sin(a)
         edc2layer_coeff(5,pln)= edc2ychi(pln) !sin(a)
c         edc2layer_coeff(6,pln)=-edc2xchi(pln) !cos(a)
         edc2layer_coeff(6,pln)=edc2xchi(pln) !cos(a)
         edc2layer_coeff(7,pln)= edc2zchi(pln)*edc2ypsi(pln) - edc2ychi(pln)*edc2zpsi(pln) !0.
         edc2layer_coeff(8,pln)=-edc2zchi(pln)*edc2xpsi(pln) + edc2xchi(pln)*edc2zpsi(pln) !0.
         edc2layer_coeff(9,pln)= edc2ychi(pln)*edc2xpsi(pln) - edc2xchi(pln)*edc2ypsi(pln) !1.
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
      
      do pindex=1,edc2_num_layers+1
         
*     generate the matrix EDC2AA3 for an edc2 missing a particular layer
         do i=1,3
            do j=1,3
               EDC2AA3(i,j)=0.
               if(j.lt.i)then   ! EDC2AA3 is symmetric so only calculate 6 terms
                  EDC2AA3(i,j)=EDC2AA3(j,i)
               else
                  if(pindex.le.edc2_num_layers) then
                     ich = 1
                     do k=1,edc2_num_layers
                        if(pindex.ne.k) then
                           EDC2AA3(i,j)=EDC2AA3(i,j) 
     &                          + edc2stubcoef(k,i)*edc2stubcoef(k,j)
                        endif
                     enddo
                  else
                     ich = pindex - edc2_num_layers
                     do k=(ich-1)*(edc2_num_layers)+1
     $                    ,ich*(edc2_num_layers)
                        EDC2AA3(i,j)=EDC2AA3(i,j) 
     &                       + edc2stubcoef(k,i)*edc2stubcoef(k,j)
                     enddo
                  endif
               endif            !end test j lt i
            enddo               !end j loop
cc      Write(*,*) 'edc2aa', pindex,edc2aa3(i,1),edc2aa3(i,2),edc2aa3(i,3)
         enddo                  !end i loop
         
*     form the inverse matrix EDC2AAINV3 for each configuration
         EDC2AAINV3(1,1,pindex)=(EDC2AA3(2,2)*EDC2AA3(3,3)-EDC2AA3(2,3)**2)
         EDC2AAINV3(1,2,pindex)=-(EDC2AA3(1,2)*EDC2AA3(3,3)-EDC2AA3(1,3)*EDC2AA3(2,3))
         EDC2AAINV3(1,3,pindex)=(EDC2AA3(1,2)*EDC2AA3(2,3)-EDC2AA3(1,3)*EDC2AA3(2,2))
         EDC2DET3(pindex)=EDC2AA3(1,1)*EDC2AAINV3(1,1,pindex)+EDC2AA3(1,2)*EDC2AAINV3(1,2
     $        ,pindex)+EDC2AA3(1,3)*EDC2AAINV3(1,3,pindex)
cc      Write(*,*) 'edc2det3=',pindex,EDC2DET3(pindex)
         if(abs(edc2det3(pindex)).le.1e-20)then
            write(6,*)
     &           '****************************************************'
            write(6,*) 
     &           'Warning! Deter. of matrix EDC2AA3(i,j) is nearly zero.'
            write(6,*) 'All tracks using pindex=',pindex,
     $           ' will be zerfucked.'
            write(6,*) 'Fix problem in h_generate_geometry.f or else!'
            write(6,*) 
     &           '****************************************************'
            edc2det3(pindex)=1.
         endif
         EDC2AAINV3(1,1,pindex)=EDC2AAINV3(1,1,pindex)/EDC2DET3(pindex)
         EDC2AAINV3(1,2,pindex)=EDC2AAINV3(1,2,pindex)/EDC2DET3(pindex)
         EDC2AAINV3(1,3,pindex)=EDC2AAINV3(1,3,pindex)/EDC2DET3(pindex)
         EDC2AAINV3(2,2,pindex)=(EDC2AA3(1,1)*EDC2AA3(3,3)-EDC2AA3(1,3)**2)
     &        /EDC2DET3(pindex)
         EDC2AAINV3(2,3,pindex)= -(EDC2AA3(1,1)*EDC2AA3(2,3)-EDC2AA3(1,2)*EDC2AA3(3,1))
     $        /EDC2DET3(pindex)
         EDC2AAINV3(3,3,pindex)=(EDC2AA3(1,1)*EDC2AA3(2,2)-EDC2AA3(1,2)**2)
     &        /EDC2DET3(pindex)
         

cc      print *, 'E geo', EDC2AAINV3(1,1,pindex), EDC2AAINV3(1,3,pindex),EDC2AAINV3(2,3,pindex) 
      enddo                     !end pindex loop
      


*     for debug write out all parameters
      
      
cc      eluno = 6
      if(edebugflaggeometry.ne.0) then
         write(eluno,'(''    Enge LAYER PARAMETERS: '')')
         write(eluno,'('' layer   z0      alpha      beta     gamma    wire  ''
     &        '' number  center  resolution'')')
         write(eluno,'('' number                                      spacing ''
     &        '' wires  position'')')
         write(eluno,1000) (edc2_layer_num(j),
     &        edc2_zpos(j),
     &        edc2_alpha_angle(j),
     &        edc2_beta_angle(j),
     &        edc2_gamma_angle(j),
     &        edc2_pitch(j),
     &        edc2_nrwire(j),
     &        edc2_central_wire(j),
     &        edc2_sigma(j),j=1,edc2_num_layers) 
 1000    format(1x,i4,f9.4,3f10.6,f8.4,i6,f10.4,f10.6)
         write(eluno,*) 'layer  edc2zchi',
     &        '     edc2zpsi     edc2xchi     edc2xpsi     
     &   edc2ychi     edc2ypsi'
         write(eluno,1001) (i, edc2zchi(i),edc2zpsi(i),edc2xchi(i),edc2xpsi(i),
     &        edc2ychi(i),edc2ypsi(i),i=1,edc2_num_layers )
 1001    format(i5,6f10.6)
         write(eluno,*) 'layer    edc2psi0',
     &        '    edc2psi0       edc2chi0       edc2phi0'
         write(eluno,1002) 
     &        (i, edc2psi0(i),edc2chi0(i),edc2phi0(i),i=1,edc2_num_layers)
 1002    format(i5,3f12.6)
         write(eluno,*), 'layer         edc2stubcoef 1        2',
     &        '                3             4'
         write(eluno,1003) (i, edc2stubcoef(i,1),edc2stubcoef(i,2),
     &        edc2stubcoef(i,3),edc2stubcoef(i,4),i=1,edc2_num_layers)
 1003    format(i5,4f15.6)
         write(eluno,*) '                 klayer_coeff'
         write(eluno,*) ' layer     1        2       3        4',
     &        '        5       6       7       8        9'
         do j=1,edc2_num_layers
            write(eluno,1004) j,(edc2layer_coeff(i,j),i=1,9) 
         enddo                  ! end of print over layers loop
 1004    format(1x,i3,f10.5,2f8.3,f9.3,4f8.3,f9.3)
*     
      endif                     !   end if on debug print out
      
      return
      end
