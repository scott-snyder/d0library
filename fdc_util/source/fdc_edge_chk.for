      FUNCTION FDC_EDGE_CHK(HALF,LADDER,QTRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if a one layer track goes through a
C-   second theta sector.
C-
C-   Returned value  : .true. if track doesn't go through
C-                     .false. if it does
C-   Inputs  : HALF,LADDER,QTRAK
C-
C-   Created  27-JUL-1993   Susan K. Blessing  Based on FDC_TRACK_CHK.
C-   Updated  24-AUG-1994   Susan K. Blessing  Put GTFALH calls in FIRST
C-    loop.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FDC_EDGE_CHK
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C   Inputs:
      INTEGER HALF,LADDER(0:2)
      REAL QTRAK(26)
C
C   Local:
      INTEGER H
      INTEGER LFTRH
      INTEGER LAYER
      INTEGER IER
      INTEGER UNIT,QUAD,SECTOR,WIRE,UBIT
C
      REAL Z0(0:1)
      REAL XC,YC,ZC(0:1,0:1)   ! ZC is over half and inner/outer theta
      REAL ZWIRE
      REAL XPOS,YPOS
C
C   Functions:
      INTEGER GZFTRH
C
      LOGICAL LAYER_OK
      LOGICAL FIRST
C
      SAVE FIRST,Z0
      DATA FIRST /.TRUE./
C
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        LFTRH = GZFTRH()
        Z0(0)= Q(LFTRH+3)
        Z0(1)= Q(LFTRH+4)
C
        DO H = 0, 1
          CALL GTFALH(H,0,0,0,0,XC,YC,ZC(H,0))
          CALL GTFALH(H,0,4,0,0,XC,YC,ZC(H,1))
        END DO
C
        FIRST = .FALSE.
      ENDIF
C
      FDC_EDGE_CHK= .TRUE.
C
C If track goes through a sector at the z position of wire 0, reject.
C
C Get z position of a wire 0 in missing layer of HALF.
      IF (LADDER(0).EQ.0) THEN
        ZWIRE = ZC(HALF,0)
      ELSE
        ZWIRE = ZC(HALF,1)
      END IF
      XPOS = QTRAK(4) + QTRAK(7)* (ZWIRE - Z0(HALF) )
      YPOS = QTRAK(5) + QTRAK(8)* (ZWIRE - Z0(HALF) )
C
      CALL FGET_SECTOR(XPOS,YPOS,HALF,LAYER,QUAD,SECTOR)
      IF (SECTOR.GT.-1) FDC_EDGE_CHK = .FALSE.

  999 RETURN
      END
