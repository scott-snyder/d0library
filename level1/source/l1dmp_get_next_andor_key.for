      SUBROUTINE L1DMP_GET_NEXT_ANDOR_KEY(NAME, TERM_NUM, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the next Andor Term from the Resource RCP bank.
C-      NOTE:   The routine L1DMP_GET_NEXT_ANDOR_KEY_INIT must be called prior
C-              to calling this routine.
C-
C-   Inputs  : none
C-   Outputs : NAME     The name given for the Andor Term
C-             TERM_NUM The hardware index of the Andor Term
C-             OK       Indicates whether an Andor Term was found
C-   Controls: none
C-
C-   Created   8-NOV-1991   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L1UTIL_PICK_RESOURCE_RCP
C
      CHARACTER*(*) NAME
      INTEGER TERM_NUM
      LOGICAL OK
C
      INTEGER RCPOK
      PARAMETER (RCPOK = 0)
C
      INTEGER KEY_LEN
C
      CHARACTER*79 BUF
      INTEGER IER
C
      INTEGER CURID, ID
      SAVE CURID, ID
C
      CALL L1UTIL_PICK_RESOURCE_RCP
      ID = 0
      OK = .FALSE.
      CALL EZGNXT(' ', CURID, ID)
      IF (ID .EQ. 0) GOTO 999
C
      CALL EZGET1(ID, 1, 1, 1, TERM_NUM, IER)
      IF (IER .NE. RCPOK) THEN
        CALL EZGET_ERROR_TEXT(IER,BUF)
        CALL ERRMSG (' EZGET1',
     &    'L1DMP_GET_FIRST_ANDOR_KEY',BUF,'F')
        GOTO 999
      ENDIF
C
      CALL EZGETN(ID, NAME, KEY_LEN)
      CALL EZERR(IER)
      IF (IER .NE. RCPOK) THEN
        CALL EZGET_ERROR_TEXT(IER,BUF)
        CALL ERRMSG( ' EZGETN','L1DMP_GET_FIRST_ANDOR_KEY',BUF,'F')
        GOTO 999
      ENDIF
      IF (KEY_LEN .LT. LEN(NAME)) THEN
        NAME(KEY_LEN+1:LEN(NAME)) = ' '
      ENDIF
C
      CALL EZRSET()
      OK = .TRUE.
C
  999 RETURN
C----------------------------------------------------------------------
      ENTRY L1DMP_GET_NEXT_ANDOR_KEY_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize to get the first Andor Key from the
C-      Resource RCP bank.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   8-NOV-1991   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      CURID = 1
      RETURN
      END
