      subroutine h_print_tar_tracks
*______________________________________________________________________________
*     
*     Facility: CEBAF Hall-C software.
*     
*     Module:   h_print_tar_tracks
*     
*     Version:  0.1 (In development)  18-Nov-1993 (DHP)
*     
*     Abstract: Dump selected track data in SOS_TARGET common block.
*     
*     Author:   David H. Potterveld, Argonne National Lab, Nov. 1993
*     modified  D. F. Geesaman     21 Jan 94
*     changed name for s_target_dump to s_print_tar_tracks
*     made output lun sluno
*     $Log: h_print_tar_tracks.f,v $
*     Revision 1.1.1.1  2009/06/23 13:55:44  kawama
*
*     e05115 src repository for software development
*
*     Revision 1.1.1.1  2005/05/14 21:44:37  miyoshi
*
*
*     Revision 1.1.1.1  2004/08/30 21:21:40  miyoshi
*     new dir
*
*     Revision 1.3  1995/05/22 19:45:46  cdaq
*     (SAW) Split gen_data_data_structures into gen, hms, sos, and coin parts"
*     
*     Revision 1.2  1994/05/13  03:20:37  cdaq
*     (DFG) Check for more than zero tracks
*     
*     Revision 1.1  1994/02/21  16:38:38  cdaq
*     Initial revision
*     
*______________________________________________________________________________
      
      implicit none
      
!     Include files.
      
      include 'hks_data_structures.cmn'
      include 'hks_tracking.cmn'
      
!     Misc. variables.
      
      integer*4 itrk
      
*     ============================= Executable Code ===============================
      if(hntracks_tar .gt. 0 ) then
!     Write out header.
         write (hluno,1001) 'HKS TARGET TRACKS'
         write (hluno,1002)
         
!     Loop over tracks.
         
         do itrk = 1,hntracks_tar
            
!     Write out data lines.
            
            write (hluno,1003) itrk,
     >           hx_tar(itrk),hxp_tar(itrk),
     >           hy_tar(itrk),hyp_tar(itrk),
     >           hz_tar(itrk),
     >           hdelta_tar(itrk),
     >           hp_tar(itrk)
            
         enddo
      endif
      return
      
*     ============================ Format Statements ==============================
      
 1001 format(a)
 1002 format(/,1x,'TRK',t10,'HX_TAR',t20,'HXP_TAR',t30,'HY_TAR',t40
     $     ,'HYP_TAR',t50,'HZ_TAR',t60,'HDELTA_TAR',t72,'HP_TAR')
 1003 format(1x,i2,t8,3(f10.6,f10.5),f10.5)
      
      end


