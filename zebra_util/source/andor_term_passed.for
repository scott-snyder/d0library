      FUNCTION ANDOR_TERM_PASSED(ANDOR_TERM_NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retrieves l1 AND/OR terms from GLOB bank
C-
C-   Inputs  : The name of the term to be retrieved
C-   Outputs :
C-   Controls:
C-
C-   Created  27-MAY-1993   Amber S. Boehnlein
C-   Modified 6-OCT-1995    ASB, set offset to 21
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LGLOB,GZGLOB
      LOGICAL ANDOR_TERM_PASSED
      CHARACTER*(*) ANDOR_TERM_NAME
      INTEGER       ANDOR_TERM_INDEX
      INTEGER IER
      INTEGER OFFSET
      PARAMETER (OFFSET = 21)
      INTEGER LONGWORD
      PARAMETER (LONGWORD=32)
      INTEGER IWORD,IBIT,TEST_WORD
      LOGICAL FIRST,OK,EZERROR
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
      CALL INRCP('TRIGGER_RESOURCES_RCP',IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG ( 'RCP error',
     &      'GET_ANDOR_TERMS', ' TRIGGER_RESOURCES_RCP not found', 'F')
        ENDIF                           ! if ier .eq. 0
      FIRST = .FALSE.
      ENDIF
      ANDOR_TERM_PASSED = .FALSE.
C
C ****  Find out what number corresponds to the desired term
C
      CALL EZPICK('TRIGGER_RESOURCES_RCP')
        OK = .NOT.EZERROR(IER)
        IF(OK) THEN
        CALL EZGET(ANDOR_TERM_NAME, ANDOR_TERM_INDEX, IER)
        ELSEIF (.NOT.OK) THEN
          CALL ERRMSG ( 'RCP error',
     &      'GET_ANDOR_TERMS', ' TRIGGER_RESOURCES_RCP not found', 'F')
        ENDIF
      CALL EZRSET
C
C ****  Decode the number
C
       IWORD = INT(ANDOR_TERM_INDEX/LONGWORD)
       IBIT  = MOD(ANDOR_TERM_INDEX,LONGWORD)
C<<
C
C ****  Unpack that term from the glob bank
C
      LGLOB = GZGLOB()    ! GET LINK.
        IF ( LGLOB .LE. 0 ) THEN
          CALL ERRMSG('No GLOB bank','GET_ANDOR_TERMS',
     &      'Unable to unpack terms','W')
          GOTO 999
        ENDIF
      TEST_WORD = IQ(LGLOB+OFFSET+IWORD)
      ANDOR_TERM_PASSED = BTEST(TEST_WORD,IBIT)
  999 RETURN
      END
