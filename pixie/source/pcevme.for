      SUBROUTINE PCEVME (R1,R2,SCL,MET,ILVL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO DRAW THE MISSING TRANSVERSE ENERGY
C-
C-   Inputs  : R1   INNER RADIUS OF CALORIMETER
C-             R2   OUTER RADIUS OF CALORIMETER
C-             SCL  SCALING FACTOR TO FIT TRANS ENERGY IN CALORIMETER
C-   Outputs : MET  Value of Missing Et used in the plot
C-   Controls:
C-
C-   Modified  10-AUG-1992   Nobu Oshima ( Return PNUT version level )
C-   Modified  30-SEP-1991   Nobu Oshima ( Add 'IER' for the new GTPNUT)
C-   Created    8-SEP-1989   CARY Y. YOSHIKAWA
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PI.DEF'
      REAL R1,R2,SCL,ENUT(4),ET,TH,ETA,PHI,SIG(3),X1,X2,Y1,Y2
      REAL MET
      INTEGER NUM,ILVL, IER
C-
      MET=0.
      CALL GTPNUT_TOTAL(NUM,IER)
      IF (NUM .LE. 0) GO TO 999
C-
      IF (ILVL.GT.0 .AND. ILVL.LT.NUM) THEN
        NUM = ILVL
      ELSE
        ILVL = NUM
      ENDIF
C-
      CALL GTPNUT(NUM,ENUT,ET,TH,ETA,PHI,SIG,IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG('PIXIE','PCEVME','GTPNUT FAILURE!','W')
        GO TO 999
      ENDIF
      IF (ET .GT. 99999.) ET = 99999.
      MET=ET
      X1=R1*COS(PHI)
      Y1=R1*SIN(PHI)
      X2=X1+SCL*ET*COS(PHI)
      Y2=Y1+SCL*ET*SIN(PHI)
C-
C--- Do not let misseing Et exceeding cal. bound
      IF(SCL*ABS(ET).GT.(R2-R1))THEN
         X2=R2*COS(PHI)
         Y2=R2*SIN(PHI)
      ENDIF
      CALL PCAROW(X1,Y1,X2,Y2)
C-
  999 RETURN
      END
