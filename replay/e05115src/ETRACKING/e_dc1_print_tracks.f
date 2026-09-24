      subroutine e_dc1_print_tracks
*     prints the output of track matching
*     d.f. geesaman           7 Sept 1993
*     $Log: e_dc1_print_tracks.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:45  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.2  2005/03/10 16:48:24  miyoshi
*     change tracking variable name
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     
*     Revision 1.4 2004/03/22 Miyoshi
*     For E01-011
*     
*     Revision 1.3  1995/05/22 19:45:47  cdaq
*     (SAW) Split gen_data_data_structures 
*     into gen, hms, sos, and coin parts"
*     
*     Revision 1.2  1994/06/07  04:43:40  cdaq
*     (DFG) print warning if ssingle_stub is set
*     
*     Revision 1.1  1994/02/21  16:40:41  cdaq
*     Initial revision
*     
*     
      implicit none
      include "hes_data_structures.cmn"
      include "hes_tracking.cmn"
      include "gen_event_info.cmn"
*     
      external E_DC1_DPSIFUN
      real*8 E_DC1_DPSIFUN
*     local variables
      integer*4 itrack,ihit,itrk,i
      integer*4 hitnum,layernum
      real*8 ray(enum_fpray_param),calculated_position,residual
      
      if(ENTRACKS_FP .le. 0) return

*     --- dump residuals
      itrk = edc1bestchi2_pre_index(enum_fitting)
c     --- use dumping residual for the best chi square.
      if(e_res_dump_no .ne. 0) then
         Write(e_res_dump_no,'(I8,11f12.4)') gen_event_id_number,
     &        (edc1_residual_pre(itrk,i,enum_fitting),i=1,10)
      EndIf

c     ---

      if(esingle_stub .ne. 0) then
         write(eluno,*) ' Warning - hsingle_stub is set'
      endif
      write(eluno,*) ' point     x_t             y_t     ',
     &     '        xp_t        yp_t   chi**2 degrees of'
      write(eluno,*) '           [cm]            [cm]    ',
     &     '        [rad]       [rad]          freedom'
      do itrack=1,ENTRACKS_PRE
 1001    format(1x,i3,2x,4e14.6,e10.3,1x,i3)
         write(eluno,1001) itrack,
     &        EX_FP_pre1(itrack,enum_fitting),
     &        EY_FP_pre1(itrack,enum_fitting),
     &        EXP_FP_pre1(itrack,enum_fitting),
     &        EYP_FP_pre1(itrack,enum_fitting),
     &        EDC1CHI2_pre(itrack,enum_fitting),
     &        ENFREE_pre(itrack)
      enddo
      do itrack=1,ENTRACKS_pre
         edc1track_fit_num=itrack
         ray(1)=dble(EX_FP_pre1(itrack,enum_fitting))
         ray(2)=dble(EY_FP_pre1(itrack,enum_fitting))
         ray(3)=dble(EXP_FP_pre1(itrack,enum_fitting))
         ray(4)=dble(EYP_FP_pre1(itrack,enum_fitting))
         write(eluno,'(a,i3)') ' Hits in ENGE track number',itrack
         write(eluno,'(a)') 
     &        '   hit  layer  EDC1_WIRE_COORD   FIT POSITION    ',
     &        ' RESIDUAL'
*     
         do ihit=1,ENTRACK_HITS_pre(itrack,1)
            hitnum=ENTRACK_HITS_pre(itrack,ihit+1)
            layernum=EDC1_LAYER_NUM(hitnum)
            calculated_position=E_DC1_DPSIFUN(ray,layernum)
            residual=dble(EDC1_WIRE_COORD(hitnum))-calculated_position
            write(eluno,1011) hitnum,layernum,EDC1_WIRE_COORD(hitnum),
     &           calculated_position,residual
 1011       format(3x,i3,3x,i3,3x,e15.7,2d15.7)
         enddo
      enddo
      
      return
      end

