      SUBROUTINE ZTRKVT(ZVTX,VERXYZ,ERRXYZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : from user's ZVTX in the road definition 
C-               figure out which primary vertex is and fill VERXYZ
C-               array with this vertex's X,Y,Z information
C-
C-   Inputs  : ZVTX: vertex position in Z specified in user's road
C-   Outputs : VERXYZ: array containing primary vertex's X,Y,Z coordinates
C-             ERRXYZ: array containing errors for the vertex's X,Y,Z
C-
C-   Created   4-SEP-1992   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LVERH, GZVERH, LVERT
      REAL    ZVTX, VERXYZ(3), ERRXYZ(3), DIF, DIFF
C
C----------------------------------------------------------------------
C
      CALL VZERO(VERXYZ,3)
      CALL VFILL(ERRXYZ,3,9999.0)
      DIF = 999.9
C
      LVERH = GZVERH()
      IF (LVERH .LE. 0) RETURN
      LVERT = LQ(LVERH - 1)
  100 IF (LVERT .LE. 0) RETURN
      DIFF = ABS(ZVTX - Q(LVERT+5))
      IF (DIFF .LT. DIF) THEN
        DIF = DIFF
        CALL UCOPY(Q(LVERT+3),VERXYZ,3)
        CALL UCOPY(Q(LVERT+6),ERRXYZ,3)
      ENDIF
      LVERT = LQ(LVERT)
      GOTO 100
C
  999 RETURN
      END
