      FUNCTION CJET_CONE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Shell for cone algorithm jet finder.
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: CAJETS_RCP BANK
C-
C-   Created   2-OCT-1989   Gerald C. Blazey, Harrison B. Prosper
C-   Updated  13-SEP-1990   Harrison B. Prosper
C-      Added code for CAPH
C-   Updated   2-OCT-1990   Chip Stewart   - NEW CAJETS RCP - NO FIRST ONLY INI
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZHSTR.LINK'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
      INCLUDE 'D0$INC:CJET_ALGORITHM.INC'
C
      INTEGER IER,I,J
      REAL    R
      EQUIVALENCE (J,R)
      LOGICAL FIRST,OK,CLUFND,CJET_CONE
C
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
      CALL CONCLI                     ! PARAMETERS FOR CONCLU
      CALL CLUPRI                     ! PARAMETERS FOR CLUPRE
      CALL SPLINI                     ! PARAMETERS FOR SPLJET
      IF(FIRST) THEN
        FIRST=.FALSE.
      END IF
C
C ****  Book/Fill CAPH bank for cone algorithm
C
      CALL BKCAPH(LCAPH)
      IF ( LCAPH .LE. 0 ) THEN
        CALL ERRMSG
     &    ('CALORIMETER','CJET_CONE','Unable to book CAPH','W')
        OK = .FALSE.
        GOTO 999
      ENDIF
      CALL CAPHFL_INT(K_ALGORITHM,A_CONE_JET)
C
C ****  FILL remainder of CAPH bank with ALGORITHM parameters
C
      IF ( NPARAMS.NE.NPARAMS_CONE) THEN
        CALL ERRMSG
     &    ('CJET_CONE PARAMETERS WRONG','CJET_CONE',
     &    'Unable to FILL CAPH','W')
        OK = .FALSE.
        GOTO 999
      ENDIF
C
      DO I = 2, NPARAMS
        J = ALG_PARAMS(I)
        CALL CAPHFL_REAL (K_BASE + I - 1 , R )
      END DO
C
C ****  Find jets (book/fill JETS bank)
C
      OK = CLUFND()
C
  999 CONTINUE
      CJET_CONE = OK
      RETURN
      END
