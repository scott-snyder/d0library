      SUBROUTINE GTFASE(HALF,UNIT,QUAD,SECT,WIRE,XC,YC,ZC,SDRIFT,CDRIFT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the location of the center of the
C-                         specified wire and drift field directions.
C-
C-   Inputs  : HALF,UNIT,QUAD,SECT,WIRE = logical channel address
C-   Outputs : XC,YC,ZC      = location of center of requested wire
C-             SDRIFT,CDRIFT = drift directions of wire
C-
C-   Created  27-MAR-1991   Jeffrey Bantly   modified GTFALH
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER HALF,UNIT,QUAD,SECT,WIRE
      INTEGER LKFASE,LKWIRE
      INTEGER GZFASE
C
      REAL    XC,YC,ZC
      REAL    SDRIFT,CDRIFT
C----------------------------------------------------------------------
      XC = 0.0
      YC = 0.0
      ZC = 0.0
      SDRIFT = 0.0
      CDRIFT = 0.0
      LKFASE=GZFASE(HALF,UNIT,QUAD,SECT)
      IF(LKFASE.LE.0) THEN
        CALL ERRMSG('FTRAKS-Bank not booked','GTFASE',
     &        'FALH not booked, wire position not available','W')
        GOTO 999
      ENDIF
      LKWIRE=LKFASE+6+IC(LKFASE+4)*WIRE
      XC=C(LKWIRE+1)
      YC=C(LKWIRE+2)
      ZC=C(LKWIRE+3)
      SDRIFT=SIN( C(LKWIRE+5) )
      CDRIFT=COS( C(LKWIRE+5) )
C
C----------------------------------------------------------------------
  999 RETURN
      END
