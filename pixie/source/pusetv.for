      SUBROUTINE PUSETV( PARNAM, IVAL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the value of specified parameter in the
C-   current PXPARAMS array. Use PUGETV to get the value of the parameter.
C-   Use PUSETA, PUGETA for character-valued parameters. Use PUGETYPE
C-   to return the parameter type. Make sure the correct RCP bank has
C-   been selected with EZPICK before calling PUSETV. Check for errors
C-   with the routine PUGET_ERROR(ERROR).
C-
C-   Returned value  : TRUE     if NO error, false otherwise
C-   Inputs  : PARNAM   [C*]    Parameter name
C-             IVAL     [I,L,R] Value
C-   Outputs : None
C-   Controls: none
C-
C-   Created   1-JUL-1988   Olivier Callot
C-   Updated  27-JUL-1988   Olivier Callot Add Hash method ( Michael Peters )
C-   Updated  15-FEB-1989   Lupe Rosas     Add character parameter entries
C-   Updated  28-SEP-1990   Lupe Howell    Increase MAXPAR to 150
C-   Updated  19-NOV-1990   Harrison B. Prosper
C-      Complete re-write
C-   Updated  21-FEB-1991   Lupe Howell  Modify the call to EZ_SET_ARRAY
C-   parameters were wrong.
C-   Updated  19-MAR-1991   Harrison B. Prosper
C-      Add entry point PUGET_ERROR
C-   Updated   4-APR-1991   Lupe Howell  PUGETA and PUSETA where taken out
C-   Updated  24-FEB-1992   Lupe Howell  Update for SGI.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PARNAM
      INTEGER       IVAL
      INTEGER ITYPE
      INTEGER ERROR
C
      INTEGER LEN1,LEN2,IDX,LENGTH,TEMP_TYPE,IER
      CHARACTER*1   POST
      CHARACTER*32  REM,BANK_NAME,LAST_BANK_NAME
      CHARACTER*80  CVAL,MESS
      LOGICAL GET, GET_TYPE, EZERROR
      SAVE LAST_BANK_NAME
C----------------------------------------------------------------------
      GET   = .FALSE.
      GET_TYPE = .FALSE.
      GOTO 1
C
C ****  ENTRY point to GET parameter
C
      ENTRY PUGETV ( PARNAM, IVAL )
      GET   = .TRUE.
      GET_TYPE = .FALSE.
      GOTO 1
C
      ENTRY PUGETYPE ( PARNAM, ITYPE )
      GET   = .TRUE.
      GET_TYPE =.TRUE.
C
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
        CALL EZ_GET_ARRAY
     &    ('PXPARAMS',PARNAM,1,IVAL,CVAL,TEMP_TYPE,REM,IER)
        IF ( GET_TYPE )
     &    ITYPE = TEMP_TYPE
      ELSE
        CALL EZ_SET_ARRAY('PXPARAMS',PARNAM,IVAL,IER)
      ENDIF
C
C ****  Check error code
C
      IF ( IER .NE. 0 ) THEN
        IF ( GET ) THEN
          MESS = 'Error getting parameter '//PARNAM(1:LEN(PARNAM))
          CALL ERRMSG('PIXIE','PUGETV',MESS,'W')
        ELSE
          MESS = 'Error setting parameter '//PARNAM(1:LEN(PARNAM))
          CALL ERRMSG('PIXIE','PUSETV',MESS,'W')
        ENDIF
      ENDIF
  999 RETURN
C
      ENTRY PUGET_ERROR(ERROR)
      ERROR = IER
      END
