      SUBROUTINE L2FDPULS(channel_id, depth, data, maxhit,
     &  npulse, L2time, L2area, L2peak, L2status)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find hits on one CDC channel, or
C-                         use in level-2.
C-
C-   Inputs  : integer channel_id : logical channel number
C-             integer depth : depth of FADC data
C-             integer data(depth) : One channel's FADC data
C-   Outputs : integer maxhit : maximum number of hits allowed
C-             integer npulse : number of hits found
C-             integer time(maxhit) : drift times of hits, in
C-                                    units 1/128 ns
C-             integer area(maxhit) : pulse area of hits
C-             integer peak(maxhit) : peak height of hits
C-             integer status(maxhit) : status byte for each hit,
C-                     bit 0 - saturation flag
C-                     bit 1 - overlap flag
C-                     bits 2/3 - unused as yet
C-                     bits 4-7 - pulse width in units of 4 FADC bins
C-                                (range 0 - 59 + overflow)
C-   Controls: none.
C-
C              **** DUMMY VERSION ****
C----------------------------------------------------------------------
      implicit none
C
      include 'd0$inc:ZebStp.inc'
      include 'd0$params:fdpara.params'
C
      integer channel_id, depth, maxhit
      integer data(depth)
      integer npulse
      integer L2time(maxhit),L2area(maxhit),
     &  L2peak(maxhit),L2status(maxhit)
C----------------------------------------------------------------------
  999 RETURN
      END
