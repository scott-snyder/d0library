      SUBROUTINE PUSETA(PARNAM,CVALUE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the value of specified character-value
C-   parameters in the current PXPARAMS array.  Use PUGETA to get the
C-   value of specified character-value parameters.  Check for errors
C-   with the routine PUGETA_ERROR(ERROR).
C-
C-   Inputs  : PARAM [C*]: Parameter name
C-             CVALUE[C*]: Parameter value to be set (PUSETA)
C-
C-   Outputs : CVALUE[C*]: Parameter value requested (PUGETA)
C-
C-   Controls: None
C-
C-   Created   4-APR-1991   Lupe Howell
C-   Updated  24-JAN-1992   Lupe Howell  Update for SGI
C-   Updated   6-MAY-1993   Harrison B. Prosper
C-    Fix access violation problem in call to EZ_GET_ARRAY - NPARAM
C-    is hard coded to 1
C-   Updated  25-MAY-1993   Marc Paterno  Corrected FLINT complaints
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PARNAM
      CHARACTER*(*) CVALUE
C
      CHARACTER*4  CTEMP
      CHARACTER*80 CVAL,MESS
      CHARACTER*32  REM,BANK_NAME,LAST_BANK_NAME
      DATA LAST_BANK_NAME / ' ' /
      LOGICAL GET
      INTEGER IER,LEN,CLEN,J,K,LLL,LENGTH,LEN1,LEN2,IVAL,ITYPE,ERROR
      INTEGER NPARAM
C----------------------------------------------------------------------
      GET   = .FALSE.
      GOTO 1

C----------------------------------------------------------------------
C
C ****  ENTRY PUGETA
C
C----------------------------------------------------------------------
      ENTRY PUGETA ( PARNAM, CVALUE )
      GET   = .TRUE.
      GOTO 1
    1 CONTINUE
      IER = 0
C
C ****  Get name of current PARAMS array
C
      CALL EZTELL(BANK_NAME,LENGTH)
      IF ( BANK_NAME(1:LENGTH) .NE. LAST_BANK_NAME(1:LENGTH) ) THEN
        LEN1 = INDEX(BANK_NAME,'PX_')
        IF ( LEN1 .GT. 0 ) THEN
          LEN1 = LEN1 + 3
        ELSE
          IER = -1
          CALL ERRMSG('PIXIE','PUSETV/PUGETV',
     &      'You must select the correct RCP bank','W')
          GOTO 999
        ENDIF
        LEN2 = INDEX(BANK_NAME,'_RCP')
        IF ( LEN2 .GT. 0 ) THEN
          LEN2 = LEN2 - 1
        ELSE
          IER = -1
          CALL ERRMSG('PIXIE','PUSETV/PUGETV',
     &      'You must select the correct RCP bank','W')
          GOTO 999
        ENDIF
        LAST_BANK_NAME = BANK_NAME
      ENDIF
C
C ****  Get/Set value
C
      IF ( GET ) THEN
        NPARAM = 1
        CALL EZ_GET_ARRAY
     &    ('PXPARAMS',PARNAM,NPARAM,IVAL,CVAL,ITYPE,REM,IER)
        CALL SWORDS(CVAL,K,J,CLEN)
        CVALUE = CVAL(1:J)
      ELSE
        CALL SWORDS(CVALUE,K,J,CLEN)
        READ( CVALUE(1:J), 2120 ) CTEMP
 2120   FORMAT(A4)
        CALL DCTOH (4,CTEMP,IVAL,LLL)    ! Convert to hollarit
        CALL EZ_SET_ARRAY('PXPARAMS',PARNAM,IVAL,IER)
      ENDIF
C
C ****  Check error code
C
      IF ( IER .NE. 0 ) THEN
        IF ( GET ) THEN
          MESS = 'Error getting parameter '//PARNAM(1:LEN(PARNAM))
          CALL ERRMSG('PIXIE','PUGETA',MESS,'W')
        ELSE
          MESS = 'Error setting parameter '//PARNAM(1:LEN(PARNAM))
          CALL ERRMSG('PIXIE','PUSETA',MESS,'W')
        ENDIF
      ENDIF
  999 RETURN
C
C **** ENTRY PUGETA_ERROR
C
      ENTRY PUGETA_ERROR(ERROR)
      ERROR = IER
      END
