      SUBROUTINE UDST_GET_SAHH(STATION_HITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return number of hits in SAMUS N and S
C-                         A,B,C stations
C-
C-   Inputs  : NONE
C-   Outputs : INTEGER STATION_HITS(1:6)
C-    STATION_HITS(1)= # hits in A_NORTH
C-    STATION_HITS(2)= # hits in B_NORTH
C-    STATION_HITS(3)= # hits in C_NORTH
C-    4-6 = same for SOUTH
C-   Controls:
C-
C-   Created  17-DEC-1993   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LSAHH,GZSAHH,I,STATION_HITS(1:6)
C----------------------------------------------------------------------
      CALL VZERO(STATION_HITS,6)

      LSAHH=GZSAHH()
      IF (LSAHH.LE.0) THEN
        CALL ERRMSG(' ','UDST_GET_SAHH','NO SAHH BANK','I')
        GOTO 999
      ENDIF

      STATION_HITS(1) = IQ(LSAHH+1)  + IQ(LSAHH+2)  + IQ(LSAHH+3)
      STATION_HITS(2) = IQ(LSAHH+4)  + IQ(LSAHH+5)  + IQ(LSAHH+6)
      STATION_HITS(3) = IQ(LSAHH+7)  + IQ(LSAHH+8)  + IQ(LSAHH+9)
      STATION_HITS(4) = IQ(LSAHH+10) + IQ(LSAHH+11) + IQ(LSAHH+12)
      STATION_HITS(5) = IQ(LSAHH+13) + IQ(LSAHH+14) + IQ(LSAHH+15)
      STATION_HITS(6) = IQ(LSAHH+16) + IQ(LSAHH+17) + IQ(LSAHH+18)

  999 RETURN
      END
