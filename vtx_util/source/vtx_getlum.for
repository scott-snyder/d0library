      SUBROUTINE VTX_GETLUM(VAXTIME,TIME,LUM,XHOUR,TIME_B,ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read special VTX Luminosity file for first part of
C-               RUN Ia:  This file was made from the D0LUM database.  Return
C-               the most recent luminosity for an input time VAXTIME 
C-
C-   Inputs  : VAXTIME -- internal 64-bit vax time
C-   Outputs : LUM     -- Luminosity, corrected for multiple interactions, in
C-                        units of 10E30
C-             TIME    -- DBL3 PACKED TIME
C-             TIME_B  -- DBL3 PACKED TIME OF START OF VALIDITY RANGE
C-             XHOUR   -- How deep into validity range we are, in hours
C-             ERR     -- return code:
C-                        0 : All is well
C-                       -1 : Vax time is illegal
C-                       -2 : Requested time is too early
C-                       -3 : Requested time is too late
C-   Controls: 
C-
C-   Created   9-DEC-1992   Ed Oltman
C-   Updated  12-JAN-1993   Ed Oltman  Fix bug
C-   Updated  19-OCT-1993   Liang-Ping Chen Use EZLOC before EZPICK  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C I/O:
      INTEGER VAXTIME(2),ERR,TIME,TIME_B
      REAL    LUM,XHOUR
C locals:
      LOGICAL FIRST,OK
      INTEGER LUN,ERROR,LEN
      INTEGER TIME_0,TIME_L,TIME_1,TIME_2,T
      CHARACTER*60 LUM_FILE
      REAL LUML,LUM1,LUM2
      INTEGER LRCP
c External:
      INTEGER TRULEN
      REAL D3UXH
      LOGICAL D3UPT
c Data:
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZLOC('VTRAKS_RCP',LRCP)
        IF (LRCP .EQ. 0) THEN
          CALL INRCP('VTRAKS_RCP',ERROR)
          IF (ERROR .NE. 0) CALL ERRMSG('VTRAKS_RCP not found',
     &      'VTX_GETLUM','Luminosity file name in VTRAKS_RCP','F')
        ENDIF
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGETS('LUM_FILE',1,LUM_FILE,LEN,ERROR)
        CALL EZRSET
        CALL GTUNIT(701,LUN,ERROR)
        CALL D0OPEN(LUN,LUM_FILE(1:LEN),'IF',OK)
        IF (.NOT. OK) CALL ERRMSG('LUMINOSITY FILE OPEN ERROR',
     &    'VTX_GETLUM',
     &    'Error opening Luminosity file','F')
        READ(LUN,*) TIME_L,LUML
        READ(LUN,*) TIME_1,LUM1
        READ(LUN,*) TIME_2,LUM2
        TIME_0 = TIME_L
      ENDIF
      ERR = 0
      IF (.NOT. D3UPT(VAXTIME,TIME)) THEN
C..illegal time
        ERR = -1
        CALL ERRMSG('VTX: no LUM value','VTX_GETLUM','Illegal time','W')
        GO TO 999
      ENDIF
      IF (TIME .LT. TIME_0) THEN
c..Time way too early
        ERR = -2
        CALL ERRMSG('VTX: no LUM value','VTX_GETLUM','Time too early',
     &    'W')
        GO TO 999
      ELSEIF (TIME .LT. TIME_L) THEN
c..Time earlier then last validity range..
        REWIND LUN
        READ(LUN,*) TIME_L,LUML
        READ(LUN,*) TIME_1,LUM1
        READ(LUN,*) TIME_2,LUM2
      ELSEIF (TIME .LT. TIME_1) THEN
c..Time in last validity range, no need to rewind
        TIME_B = TIME_L
        LUM    = LUML
        XHOUR = D3UXH(TIME,TIME_B)
        GO TO 999
      ELSEIF ( TIME .LT. TIME_2) THEN
c..event still within validity range
        LUM = LUM1
        TIME_B = TIME_1
        XHOUR = D3UXH(TIME,TIME_B)
        GO TO 999
      ENDIF
c..Event beyond validity range
    1 TIME_L = TIME_1
      TIME_1 = TIME_2
      LUML   = LUM1
      LUM1   = LUM2
      READ(LUN,*,END=2) TIME_2,LUM2
      IF ( TIME .LE. TIME_2)THEN
        LUM = LUM1
        TIME_B = TIME_1
        XHOUR = D3UXH(TIME,TIME_1)
        GO TO 999
      ENDIF
c..Must have been a long pause..skip a validity range
      GO TO 1
    2 CALL ERRMSG('VTX: no lum value','VTX_GETLUM','Time too late','W')
      ERR = -3
  999 RETURN
      END
