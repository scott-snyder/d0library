      SUBROUTINE CJET_PARAM_GET(NALG,CHAR,ALG,TYP,NPAR,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GETS CJET PARAMETERS - SETS PARAMETERS TO DEFAULTS
C-   IF CJET_PARAM_SET IS NOT CALLED PRIOR TO CJET_PARAM_GET
C-
C-   Inputs  : NALG   = ALGORITHM NUMBER - UP TO MAX_ALG
C-                     IF 0 THEN USE ALGORITHM NUMBER LEFT OVER FROM LAST CALL 
C-                     TO CJET_PARAM_SET
C-                     IF STILL 0 THEN RETURN DEFAULT CJET ALGORITHM PARAMETERS
C-                              'CONE' R=0.7 Et=8gev ...
C- 
C-   Outputs : ALG   = PACKED RCP ALGORITHM  PARAMETERS 
C-                     ALG(1) HOLLERITH FOR 'CONE' 'NEIGHBOR' 'USER'
C-             TYP   = PACKED RCP ALGORITHM  TYPES (FROM EZ_GET_VALUE_TYPE)
C-             NPAR  = NUMBER OF packed WORDS  PARAMETERS RETURNED
C-             IER   = ERROR CODE -  0 = OK
C-                   = -1  NALG not in valid range - over MAX_ALG or not set in
C-                     CJET_PARAM_SET
C-                   = -2  NPAR GREATER THAN MAX_PAR
C-   Controls: NONE
C-
C-   Created  2-OCT-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,J,K,MAX_ALG,MAX_PAR
      PARAMETER ( MAX_PAR = 20 )
      PARAMETER ( MAX_ALG = 30 )
      INTEGER NALG,ALG(*),TYP(*),NPAR,IER,MAX_ALG_TMP
      INTEGER NALG1,ALG1(MAX_PAR,MAX_ALG),TYP1(MAX_PAR,MAX_ALG)
      INTEGER NPAR1(MAX_ALG)
      CHARACTER CHAR*(*),CHAR1(MAX_ALG)*20
C----------------------------------------------------------------------
      SAVE ALG1,TYP1,NPAR1,NALG1
      DATA NALG1 / 1 /
      DATA NPAR1(1) / 8 /
      DATA (ALG1(I,1),I=1,8) / 0,0.7,8.0,0.5,0.01,1.0,1.0,1.0 /
      DATA (TYP1(I,1),I=1,8) / 11,7*2/
      DATA CHAR1(1) /'CONE'/
C----------------------------------------------------------------------
      IER = 0
      IF ( NALG .EQ. 0 ) THEN
        NALG = NALG1
        IF (NALG.EQ.0) THEN
          IER = -1
          GOTO 999
        END IF
      ELSE IF (NALG.GT.MAX_ALG_TMP) THEN
        IER = -1
        GOTO 999
      ELSE 
        NALG1 = NALG                      ! set ALG number for next call
      END IF
      NPAR = NPAR1(NALG)
      CHAR = CHAR1(NALG)
      IF( NPAR .GT. MAX_PAR .OR. NPAR.LT.1) THEN
        IER = -2
        GOTO 999
      END IF
      CALL UCOPY( ALG1(1,NALG), ALG(1), NPAR )
      CALL UCOPY( TYP1(1,NALG), TYP(1), NPAR )
  999 RETURN
C
C ****  ENTRY CJET_PARAM_SET SET PJET PARAMETERS TO VALUES INPUT.
C
      ENTRY CJET_PARAM_SET(NALG,CHAR,ALG,TYP,NPAR,IER)
      IER = 0
      NALG1 = NALG
      IF( NALG1 .GT. MAX_ALG) THEN
        IER = -1
        GOTO 1999
      END IF
      IF( NALG1 .LT. 1) THEN
        IER = -1
        GOTO 1999
      END IF
      MAX_ALG_TMP = MAX (MAX_ALG_TMP,NALG1)
      NPAR1(NALG1) = NPAR
      IF( NPAR1(NALG1) .GT. MAX_PAR) THEN
        IER = -2
        GOTO 1999
      END IF
      CHAR1(NALG1) = CHAR
      CALL UCOPY( ALG(1), ALG1(1,NALG1), NPAR ) 
      CALL UCOPY( TYP(1), TYP1(1,NALG1), NPAR )
C----------------------------------------------------------------------
 1999 RETURN
      END
