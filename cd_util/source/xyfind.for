      SUBROUTINE XYFIND (IFLAG, CONT, XDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculate X and Y of vertex 
C-
C-   Inputs  : CONT - array of segment or track parameters
C-   Outputs : XDATA(1) - X of vertex
C-             XDATA(2) - Y of vertex
C-             XDATA(3) - CHI2 of segment
C-             XDATA(4) - PHI of segment
C-   Controls: IFLAG = 1 - segments
C-                   = 2 - tracks
C-
C-   Created   5-OCT-1992   Alexandre Zinchenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST
      INTEGER IFLAG, IER
      REAL CONT(*), XDATA(*), XYBO(2), PI, PHI, XG, YG, CHI2
      REAL XFIND, YFIND
      LOGICAL EZERROR
C      DATA xb,yb/-0.3,0.1/
      DATA FIRST /.TRUE./
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        PI = ACOS(-1.)
        CALL EZPICK('XYVERT_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('XYVERT','XYFIND',
     &    'Unable to find bank XYVERT_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('XYPLANEO',XYBO,IER)
        CALL EZRSET
      ENDIF
      PHI = CONT(1)
      XG = CONT(2)
      YG = CONT(3)
      IF (IFLAG.EQ.0) THEN
        CHI2 = CONT(5)
      ELSE
        CHI2 = CONT(7)
      ENDIF
      XFIND = 0.
      YFIND = 0.
      IF ((PHI.GE.PI/4.AND.PHI.LT.3*PI/4.).OR.
     &    (PHI.GE.5*PI/4.AND.PHI.LT.7*PI/4.)) THEN
        XFIND = (XYBO(2)-YG)/TAN(PHI) + XG
        CHI2 = ABS(CHI2)
      ELSE
        YFIND = (XYBO(1)-XG)*TAN(PHI) + YG
        CHI2 = -ABS(CHI2)
      ENDIF
      XDATA(1) = XFIND
      XDATA(2) = YFIND
      XDATA(3) = CHI2
      XDATA(4) = PHI
C----------------------------------------------------------------------
  999 RETURN
      END
