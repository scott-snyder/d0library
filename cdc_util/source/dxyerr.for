      SUBROUTINE DXYERR (WIRE,X,ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns drift distance dependent error on
C-                         hit coordinate. The error function is:
C-
C-                         ERR(X) = A*EXP(-B*X)+SQRT(C1+C2*X) (cm)
C-
C-   For the region X <= XCUT the error function is evaluated exactly
C-   For the region X >  XCUT the error function is evaluated by 
C-                       lookup table
C- 
C-   Inputs  : WIRE - Wire number (0 -> 6)
C-           : X    - Drift coordinate (cm) 
C-   Outputs : ERR  - Error in drift coordinate (cm)
C-   Controls: 
C-
C-   Created  24-APR-1992   Domenico Pizzuto
C-   Updated  31-DEC-1992   Qizhong Li-Demarteau  added EZERROR, EZRSET 
C-                            and default values for ERRSC to avoid crash
C-
C----------------------------------------------------------------------

      IMPLICIT NONE

C- Drift distance seperating the two regions
      REAL XCUT
      PARAMETER (XCUT = 1.0)
C- Number of slices in region X > XCUT
      INTEGER NUMSLH
      PARAMETER (NUMSLH = 50)

      INTEGER WIRE,I,ERRR,J, IER
      REAL EYXHI (NUMSLH,2),XX,X,ERRSC (2),ERRX1I,ERRX1O
      REAL XST,ERR,AI,BI,C1I,C2I,AO,BO,C1O,C2O,XSTEPH
      LOGICAL FIRST
      LOGICAL EZERROR
      DATA AI,BI,C1I,C2I /.097,6.160,5.812E-5,7.480E-5/
      DATA AO,BO,C1O,C2O /.063,5.139,2.053E-4,8.550E-5/

      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
       FIRST = .FALSE.
       CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DXYERR',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
       CALL EZGET('ERRISW',ERRSC(1),ERRR)
       IF (ERRR .NE. 0) ERRSC(1) = .013
       CALL EZGET('ERROSW',ERRSC(2),ERRR)
       IF (ERRR .NE. 0) ERRSC(2) = .018
       CALL EZRSET
C- Normalization factor at X=1 cm
       ERRX1I = AI*EXP (-BI)+SQRT (C1I+C2I)
       ERRX1O = AO*EXP (-BO)+SQRT (C1O+C2O)

C- Setup table for region X > XCUT
       XSTEPH  = (7.0-XCUT)/FLOAT (NUMSLH)
       XST     = XCUT+XSTEPH/2.
        DO 15 I = 1, NUMSLH
         EYXHI (I,1) = (AI*EXP (-BI*XST)+SQRT (C1I+C2I*XST))/ERRX1I
         EYXHI (I,2) = (AO*EXP (-BO*XST)+SQRT (C1O+C2O*XST))/ERRX1O
         XST = XST+XSTEPH
   15  CONTINUE
      END IF

      IF (WIRE.GT.0.AND.WIRE.LT.6) THEN
       J = 1
      ELSE
       J = 2
      END IF

      XX = ABS (X)

      IF (XX.LE.XCUT) THEN
        IF (J.EQ.1) THEN
         ERR = (AI*EXP (-BI*XX)+SQRT (C1I+C2I*XX))/ERRX1I
        ELSE
         ERR = (AO*EXP (-BO*XX)+SQRT (C1O+C2O*XX))/ERRX1O
        END IF
       ERR = ERR*ERRSC (J)
      ELSE
       I = INT ((XX-XCUT)/XSTEPH)+1
       IF (I.GT.NUMSLH) I = NUMSLH
       ERR = EYXHI (I,J)*ERRSC (J)
      END IF
        
  999 RETURN
      END
