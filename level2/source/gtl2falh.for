      SUBROUTINE GTL2FALH(HALF,UNIT,QUAD,SECT,WIRE,XC,YC,ZC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the location of the center of the
C-                         specified wire.
C-
C-   Inputs  : HALF,UNIT,QUAD,SECT,WIRE - identifies specific request
C-   Outputs : XC,YC,ZC - location of center of requested wire
C-
C-   Created  28-FEB-1989   Jeffrey Bantly 
C-   Updated  19-MAR-1990   Jeffrey Bantly  cleanup,use GZFALH 
C-   Updated  15-JUN-1992   Yi-Cheng Liu ( for Level2 stuff )
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER HALF,UNIT,QUAD,SECT,WIRE
      INTEGER LKFASE,LKWIRE
      INTEGER GZL2FASE
C
      REAL    XC,YC,ZC
C----------------------------------------------------------------------
      XC = 0.0
      YC = 0.0
      ZC = 0.0
      LKFASE=GZL2FASE(HALF,UNIT,QUAD,SECT)
      IF(LKFASE.LE.0) THEN
        CALL ERRMSG('FTRAKS-Bank not booked','GTL2FALH',
     &        'FALH not booked, wire position not available','W')
        GOTO 999
      ENDIF
      LKWIRE=LKFASE+6+IC(LKFASE+4)*WIRE
      IF (LKWIRE.GT.4000000) GOTO 999
      XC=C(LKWIRE+1)
      YC=C(LKWIRE+2)
      ZC=C(LKWIRE+3)
C
C----------------------------------------------------------------------
  999 RETURN
      END
