      subroutine g_examine_epics_event
*
* Revision 2.0  2000/02/28 10:33:04  jinghua
* (JLiu) Added g_extract_epics_info to convert epics information into
* common block and fill the beam info ntuple if required.
* Removed dumpimg the first EPICS events. dumping only as required.
*
* Revision 1.4  1999/06/10 14:41:03  csa
* (JRA) Added dump for numevent up to 10
*
* Revision 1.3  1998/12/01 15:55:40  saw
* (SAW) Print out error when event has no data
*
* Revision 1.2  1996/11/05 21:40:32  saw
* (JRA) Print out just first epics event
*
* Revision 1.1  1996/08/12 18:30:13  saw
* Initial revision
*
*--------------------------------------------------------
      implicit none
      save

      character buffer*12000
      equivalence (craw(5), buffer)
      integer i,j,evlen
      integer g_important_length,find_char
      integer numevent
      logical ABORT
      character*300 err

      include 'gen_craw.cmn'
      include 'gen_run_info.cmn'
      include 'gen_filenames.cmn'

*--------------------------------------------------------
      numevent = numevent + 1

* Is the event OK?
      if (craw(3)-1.le.0) then
        write (6,*)
     1  '**g_examine_epics_event: bad record length; numevent=',
     1  numevent,', craw3=',craw(3)
        return
      endif

* First dump the event if required

      if (g_epics_output_filename.ne.' ') then  !write out event

       write (G_LUN_EPICS_OUTPUT,*) 'epics event #',numevent

      evlen=g_important_length(buffer(1:4*(craw(3)-1)))
      i = 1
      do while (i.le.evlen)
        j = find_char (buffer, i, 10)   ! 10 = NewLine character
        if (i.eq.j) goto 20
        if(i.lt.j-1) write(G_LUN_EPICS_OUTPUT,'(4x,a)') buffer(i:j-1)
 20     i = j + 1
      enddo

      endif

* Then let's extract the useful information from the EPICS event
      call g_extract_epics_info

* Finally try to fill out the beam info Ntuple
      ABORT=.false.
      call g_bm_nt_keep(ABORT,err)

      return
      end
