      SUBROUTINE muplot_track(l2quad,ihist)

c       This subroutine is a companion to PLOT_LATCHES. IF the histogram
c       definitions are changed in one they must be changed in both.
c
c       This subroutine takes a (L2) track quadrant and fills a histogram
c       in the equivalent bins for the L1 latches. This is not exact since
c       the trigger and tracking quadrants do not match up exactly.
c
c       The histogram is ordered so that NORTH and SOUTH are on opposite
c       sides of CENTRAL. Each region is ordered so that the TOP is towards
c       the middle and the BOTTOM is towards the outside.
c
c       0-3     4-7     8-11    12-15   16-19   20-23   24-27   28-31
c       SN      ON      WN      CF-E    CF-W    WS      OS      SS
c
c       Created:  31-Aug-94     Paul Quintas
c       Modified: 26-Oct-94     Paul Quintas    Add entry mubook_track

      INTEGER l2quad, ihist, jhist
      INTEGER ibit, idum
      LOGICAL ok, btest

      IF (l2quad.LE.0) RETURN
c
c in the CF tracking 1 -> trigger 0 and 7 -> hist bins 13 and 14
c in the CF tracking 2 -> trigger 1 and 2 -> hist bins 15 and 16
c in the CF tracking 3 -> trigger 3 and 4 -> hist bins 17 and 18
c in the CF tracking 4 -> trigger 5 and 6 -> hist bins 12 and 19
c
      IF (l2quad.LE.3) THEN           ! WAMUS CENTRAL
        CALL hfill(ihist,2*l2quad+11.,0.,0.5)
        CALL hfill(ihist,2*l2quad+12.,0.,0.5)
      ELSEIF (l2quad.EQ.4) THEN
        CALL hfill(ihist,12.,0.,0.5)
        CALL hfill(ihist,19.,0.,0.5)
c
c in the EFN tracking 5 -> trigger 0 and 3 -> hist bins 11 and 8
c in the EFN tracking 6 -> trigger 0 and 1 -> hist bins 11 and 10
c in the EFN tracking 7 -> trigger 1 and 2 -> hist bins 10 and 9
c in the EFN tracking 8 -> trigger 2 and 3 -> hist bins 9 and 8
c
      ELSEIF (l2quad.LE.5) THEN       ! WAMUS NORTH
        CALL hfill(ihist,11.,0.,0.5)
        CALL hfill(ihist,8.,0.,0.5)
      ELSEIF (l2quad.LE.8) THEN       ! WAMUS NORTH
        CALL hfill(ihist,17.-l2quad,0.,0.5)
        CALL hfill(ihist,16.-l2quad,0.,0.5)
c
c in the EFS tracking 9  -> trigger 0 and 3 -> hist bins 20 and 23
c in the EFS tracking 10 -> trigger 0 and 1 -> hist bins 20 and 21
c in the EFS tracking 11 -> trigger 1 and 2 -> hist bins 21 and 22
c in the EFS tracking 12 -> trigger 2 and 3 -> hist bins 22 and 23
c
      ELSEIF (l2quad.LE.9) THEN       ! WAMUS SOUTH
        CALL hfill(ihist,20.,0.,0.5)
        CALL hfill(ihist,23.,0.,0.5)
      ELSEIF (l2quad.LE.12) THEN      ! WAMUS SOUTH
        CALL hfill(ihist,10.+l2quad,0.,0.5)
        CALL hfill(ihist,11.+l2quad,0.,0.5)
c
c in SAMUS tracking 13 -> trigger 0 to 4 -> hist bins 28 to 31
c in SAMUS tracking 14 -> trigger 0 to 4 -> hist bins 28 to 31
c
      ELSEIF (l2quad.LE.13) THEN      ! SAMUS NORTH
        CALL hfill(ihist,0.,0.,0.25)
        CALL hfill(ihist,1.,0.,0.25)
        CALL hfill(ihist,2.,0.,0.25)
        CALL hfill(ihist,3.,0.,0.25)
      ELSEIF (l2quad.LE.14) THEN      ! SAMUS SOUTH
        CALL hfill(ihist,28.,0.,0.25)
        CALL hfill(ihist,29.,0.,0.25)
        CALL hfill(ihist,30.,0.,0.25)
        CALL hfill(ihist,31.,0.,0.25)
c
c in the OLN tracking 15 -> trigger 0 and 3 -> hist bins 7 and 4
c in the OLN tracking 16 -> trigger 0 and 1 -> hist bins 7 and 6
c in the OLN tracking 17 -> trigger 1 and 2 -> hist bins 6 and 5
c in the OLN tracking 18 -> trigger 2 and 3 -> hist bins 5 and 4
c
      ELSEIF (l2quad.LE.15) THEN      ! OVERLAP NORTH
        CALL hfill(ihist,7.,0.,0.5)
        CALL hfill(ihist,4.,0.,0.5)
      ELSEIF (l2quad.LE.18) THEN      ! OVERLAP NORTH
        CALL hfill(ihist,23.-l2quad,0.,0.5)
        CALL hfill(ihist,22.-l2quad,0.,0.5)
c
c in the OLS tracking 19 -> trigger 0 and 3 -> hist bins 24 and 27
c in the OLS tracking 20 -> trigger 0 and 1 -> hist bins 24 and 25
c in the OLS tracking 21 -> trigger 1 and 2 -> hist bins 25 and 26
c in the OLS tracking 22 -> trigger 2 and 3 -> hist bins 26 and 27
c
      ELSEIF (l2quad.LE.19) THEN      ! OVERLAP SOUTH
        CALL hfill(ihist,24.,0.,0.5)
        CALL hfill(ihist,27.,0.,0.5)
      ELSEIF (l2quad.LE.22) THEN      ! OVERLAP SOUTH
        CALL hfill(ihist,4.+l2quad,0.,0.5)
        CALL hfill(ihist,5.+l2quad,0.,0.5)
      ENDIF

      RETURN
      ENTRY mubook_track(jhist)

      CALL hbook1(jhist,'MUON L2 TRACKS BY REGION',32,-0.5,31.5,0.)

      RETURN
      END
