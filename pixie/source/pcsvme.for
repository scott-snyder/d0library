      SUBROUTINE PCSVME(MNPHI,MXPHI,PCR1,PCR2,PCZ1,PCZ2,SCL,MET)
C--------------------------------------------------------------------
C-
C-   Purpose and Methods : DRAWS THE SIDE VIEW MISSING ENERGY IF
C-                         IT IS IN THE CURRENT PHI RANGE.
C-
C-   Inputs  : MNPHI,MXPHI   RANGE OF CURRENT PHI
C-             PCR1          INNER RADIUS OF CC
C-             PCR2          OUTER RADIUW OF CC
C-             PCZ1          Z-VALUE OF INNER SURFACE OF EC
C-             PCZ2          Z-VALUE OF OUTER SURFACE OF EC
C-             SCL           SCALING OF ENERGY FOR PLOT
C-
C-   Outputs : MET - Valuse of missing Et drawn
C-   Controls:
C-
C-   Modified  30-SEP-1991   Nobu Oshima ( Add 'IER' for the new GTPNUT)
C-   Created   29-SEP-1989   CARY Y. YOSHIKAWA
C-
C--------------------------------------------------------------------
      IMPLICIT NONE
C--------------------------------------------------------------------
      REAL ENUT(4),ET,THETA,ETA,PHI,SIG(3),PI,X1,Y1,X2,Y2,
     &     ETACC, PCR1,PCR2,PCZ1,PCZ2,SCL,MET
      INTEGER NUM,PHIBIN,MNPHI,MXPHI,MMNPHI,MMXPHI
      INTEGER GZPNUT,LPNUT, IER
C-
      MET=0.
      PI=3.1415927
      DO 100 NUM=5,1,-1
        LPNUT=GZPNUT(NUM)
        IF(LPNUT.NE.0) GO TO 200
  100 CONTINUE
      GO TO 999
  200 CONTINUE
      CALL GTPNUT(NUM,ENUT,ET,THETA,ETA,PHI,SIG,IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG('PIXIE','PCSVME','GTPNUT FAILURE!','W')
        GO TO 999
      ENDIF
      MET=ET
      IF (PHI.LT.0) PHI=PHI+2*PI
      PHIBIN=INT(64.0/(2*PI)*PHI+1)
      IF (MXPHI.LT.32) THEN
        MMXPHI=MXPHI+32
        MMNPHI=MNPHI+32
      ELSEIF (MXPHI.GE.32) THEN
        MMXPHI=MXPHI-32
        MMNPHI=MNPHI-32
        IF (PHIBIN.GT.MXPHI .AND. PHIBIN.LE.64) PHIBIN=PHIBIN-64
      ENDIF
      IF (PHIBIN.GE.MNPHI .AND. PHIBIN.LE.MXPHI) THEN
        X1=0.
        Y1=PCR1
        X2=0.
        Y2=PCR1+SCL*ET
C Do not let arrow exceed cal dim
        IF(Y2.GT.PCR2)Y2=PCR2
        CALL PCAROW(X1,Y1,X2,Y2)
      ELSEIF(PHIBIN.GE.MMNPHI .AND. PHIBIN.LE.MMXPHI) THEN
        X1=0.
        Y1=-PCR1
        Y2=-(PCR1+SCL*ET)
        IF(Y2.LT.-PCR2)Y2=-PCR2
        CALL PCAROW(X1,Y1,X2,Y2)
      ENDIF
  999 RETURN
      END
