      SUBROUTINE VTRKFT(NLADD,LADDRS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fit tracks to all ladders:  For each category of
C-   ladder (four categories, using layers 2-1-0, 2-1, 2-0 and 1-0), clean up
C-   the resulting tracks with VCLNLAD.
C-   
C-   If a 3-layer ladder produces no track, then split it into the three 
C-   2-layer ladders.
C-
C-   Inputs  : NLADD - (initial) number of ladders
C-             LADDRS(0:2,1:NLADD) = the ladders (list of segments)
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-SEP-1993   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
c I/O:
      INTEGER NLADD,LADDRS(0:2,*)
c Locals:
      INTEGER LVTRH,NVTXT_OLD,NVTXT_NEW,LAD,START,LAST
c Externals:
      INTEGER GZVTRH, MXTRAK 
      PARAMETER (MXTRAK = 1000)
C----------------------------------------------------------------------
      LVTRH = GZVTRH()
      NVTXT_OLD = 0
      IF (LVTRH .GT. 0) NVTXT_OLD = IQ(LVTRH+2)
C
C ****  FIRST LOOP OVER ALL LAYER 2-1-0 LADDERS
C
      DO LAD = 1,NLADD
        IF (LADDRS(0,LAD) .NE. 0 .AND.
     &      LADDRS(1,LAD) .NE. 0 .AND.
     &      LADDRS(2,LAD) .NE. 0 ) CALL FTVTXT( LADDRS(0,LAD) )
      ENDDO
      LVTRH = GZVTRH()
      NVTXT_NEW = 0
      IF (LVTRH .GT. 0) NVTXT_NEW = IQ(LVTRH+2)
      START = NVTXT_OLD + 1
      LAST  = NVTXT_NEW
      CALL VCLNLAD(START,LAST,1)
      NVTXT_OLD = LAST
C
C ****  NOW LOOP OVER ALL 2-1 LADDERS
C
      DO LAD = 1,NLADD
        IF (LADDRS(0,LAD) .EQ. 0 .AND.
     &      LADDRS(1,LAD) .NE. 0 .AND.
     &      LADDRS(2,LAD) .NE. 0 ) CALL FTVTXT( LADDRS(0,LAD) )
      ENDDO
      LVTRH = GZVTRH()
      NVTXT_NEW = 0
      IF (LVTRH .GT. 0) NVTXT_NEW = IQ(LVTRH+2)
      START = NVTXT_OLD + 1
      LAST  = NVTXT_NEW
      CALL VCLNLAD(START,LAST,2)
      NVTXT_OLD = LAST  
C
C ****  NOW  LOOP OVER ALL 1-0 LADDERS
C
      DO LAD = 1,NLADD
        IF (LADDRS(0,LAD) .NE. 0 .AND.
     &      LADDRS(1,LAD) .NE. 0 .AND.
     &      LADDRS(2,LAD) .EQ. 0 ) CALL FTVTXT( LADDRS(0,LAD) )
      ENDDO
      LVTRH = GZVTRH()
      NVTXT_NEW = 0
      IF (LVTRH .GT. 0) NVTXT_NEW = IQ(LVTRH+2)
      START = NVTXT_OLD + 1
      LAST  = NVTXT_NEW
      CALL VCLNLAD(START,LAST,4)
      NVTXT_OLD = LAST
C
C ****  FINALLY, LOOP OVER ALL 2-0 LADDERS
C
      DO LAD = 1,NLADD
        IF (LADDRS(0,LAD) .NE. 0 .AND.
     &      LADDRS(1,LAD) .EQ. 0 .AND.
     &      LADDRS(2,LAD) .NE. 0 ) CALL FTVTXT( LADDRS(0,LAD) )
      ENDDO
      LVTRH = GZVTRH()
      NVTXT_NEW = 0
      IF (LVTRH .GT. 0) NVTXT_NEW = IQ(LVTRH+2)
      START = NVTXT_OLD + 1
      LAST  = NVTXT_NEW
      CALL VCLNLAD(START,LAST,3)
  999 RETURN
      END
