      SUBROUTINE GTFALH(HALF,UNIT,QUAD,SECT,WIRE,XC,YC,ZC)
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
C-   Updated  26-AUG-1994   Susan K. Blessing  Store information locally. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER H,U,QU,S,W
      INTEGER MQUAD,MSECT,MWIRE
      INTEGER HALF,UNIT,QUAD,SECT,WIRE
      INTEGER LKFASE,LKWIRE
      INTEGER GZFASE
C
      REAL XC,YC,ZC
      REAL XC_THETA(0:MXHALF,0:MXQUAD,0:MXSECT,0:MXWIRT)
      REAL YC_THETA(0:MXHALF,0:MXQUAD,0:MXSECT,0:MXWIRT)
      REAL ZC_THETA(0:MXHALF,0:MXQUAD,0:MXSECT,0:MXWIRT)
      REAL XC_PHI(0:MXHALF,0:MXSECP,0:MXWIRP)
      REAL YC_PHI(0:MXHALF,0:MXSECP,0:MXWIRP)
      REAL ZC_PHI(0:MXHALF,0:MXSECP,0:MXWIRP)
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
C
        DO H = 0, MXHALF
          DO U = 0, MXUNIT
            IF (U.EQ.0) THEN
              MQUAD = MXQUAD
              MSECT = MXSECT
              MWIRE = MXWIRT
            ELSE
              MQUAD = 0
              MSECT = MXSECP
              MWIRE = MXWIRP
            END IF
C
            DO QU = 0, MQUAD
              DO S = 0, MSECT
C
                LKFASE = GZFASE(H,U,QU,S)
                IF(LKFASE.LE.0) THEN
                  CALL ERRMSG('FTRAKS-Bank not booked','GTFALH',
     &              'FALH not booked, wire position not available',
     &              'W')
                  GOTO 999
                ENDIF
C
                DO W = 0, MWIRE
C
                  LKWIRE = LKFASE+6+IC(LKFASE+4)*W
                  IF (U.EQ.0) THEN
                    XC_THETA(H,QU,S,W) = C(LKWIRE+1)
                    YC_THETA(H,QU,S,W) = C(LKWIRE+2)
                    ZC_THETA(H,QU,S,W) = C(LKWIRE+3)
                  ELSE
                    XC_PHI(H,S,W) = C(LKWIRE+1)
                    YC_PHI(H,S,W) = C(LKWIRE+2)
                    ZC_PHI(H,S,W) = C(LKWIRE+3)
                  END IF
C
                END DO
              END DO
            END DO
          END DO
        END DO
C
      END IF
C
      XC = 0.0
      YC = 0.0
      ZC = 0.0
C
      IF (UNIT.EQ.0) THEN
        XC = XC_THETA(HALF,QUAD,SECT,WIRE)
        YC = YC_THETA(HALF,QUAD,SECT,WIRE)
        ZC = ZC_THETA(HALF,QUAD,SECT,WIRE)
      ELSE
        XC = XC_PHI(HALF,SECT,WIRE)
        YC = YC_PHI(HALF,SECT,WIRE)
        ZC = ZC_PHI(HALF,SECT,WIRE)
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
