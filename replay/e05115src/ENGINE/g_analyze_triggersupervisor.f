      subroutine g_analyze_triggersupervisor(abort,errmsg)
*-------------------------------------------------------------------
* author: Joerg Reinhold, FIU, reinhold@fiu.edu
* created: 10/05/2009
*
* Analyze the Trigger Supervisor latched trigger pattern
*
*--------------------------------------------------------

      implicit none

      include 'gen_event_info.cmn'

      logical abort
      character*(*) errmsg
      character*20 here
      parameter (here = 'g_analyze_triggersupervisor')

      integer i

      save
      
      abort = .false.
      errmsg = ' '

c---------------------------------------------------------------

      do i=1,12
         gen_event_ts_flag(i)=.false.
c     if(iand(gen_event_ts_trigger_data,'01'X).eq.X'01')then
         if(iand(gen_event_ts_trigger_data,2**(i-1)).eq.2**(i-1))then
            gen_event_ts_flag(i)=.true.
            gen_event_ts_counter(i)=gen_event_ts_counter(i)+1
         endif
      enddo


c     The first 8 bit are latched trigger inputs
c     TS-Input   Trigger
c        1        HKSPRE
c        2        HES
c        3        COIN
c        4        LED/Selftrigger
c        5        CP0 (added to TS ~ run # 75838)
c        6        empty
c        7        empty
c        8        Pedestal
c
c     Bits 9 - 12 are latched levels
c     TS-Input   Signal
c        11       FFB
c        12       Hall A Energy Dithering
      return
      end
