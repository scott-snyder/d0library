      FUNCTION EZHDRI (BANK1,OPTION1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return numerical SRCP header information.
C-
C-   Inputs  : BANK1       Name of SRCP bank.
C-             OPTION1     Name of datum required
C-
C-                         'IDENTIFIERS'      Number of Identifiers
C-                         'VALUES'           Number of values
C-                         'MAX_IDENTIFIERS'  Maximum number of Ids
C-                         'MAX_VALUES'       Maximum number of values
C-                         'WORDS/RECORD'     Number of words/record
C-                         'WORDS/BANK'       Total size of SRCP bank
C-
C-   Outputs : None
C-
C-                         Error code:
C-                         0 --- OK
C-                         See EZERR and EZGET_ERROR_TEXT
C-   Controls: None
C-
C-   Created   3-OCT-1988   Harrison B. Prosper
C-   Updated  14-NOV-1988   Harrison B. Prosper
C-                          Now uses new SRCP format
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BANK1,OPTION1
      CHARACTER*32  OPTION
      REAL          RDAT
      INTEGER EZHDRI
      INTEGER       IDAT,ID,L
      EQUIVALENCE ( RDAT, IDAT  )
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
      CALL EZZLOC (BANK1,LSRCP,ID)
      IF ( LSRCP .GT. 0 ) THEN
        ERRSRC = EZS_SUCCESS
C
        L = LEN(OPTION1)
        CALL UPCASE (OPTION1(1:L),OPTION(1:L))
C
        IF     ( OPTION(1:2) .EQ. 'ID' ) THEN
          IDAT = IC(LSRCP+JJIDS)
        ELSEIF ( OPTION(1:2) .EQ. 'VA' ) THEN
          IDAT = IC(LSRCP+JJVAL)
        ELSEIF ( OPTION(1:6) .EQ. 'MAX_ID' ) THEN
          IDAT = IC(LSRCP+JJNIDS)
        ELSEIF ( OPTION(1:6) .EQ. 'MAX_VA' ) THEN
          IDAT = IC(LSRCP+JJNVAL)
        ELSEIF ( OPTION(1:7) .EQ. 'WORDS/R' ) THEN
          IDAT = IC(LSRCP+JJNWRD)
        ELSEIF ( OPTION(1:2) .EQ. 'VE' ) THEN
          IDAT = IC(LSRCP+JJVERS)
        ELSEIF ( OPTION(1:7) .EQ. 'WORDS/B' ) THEN
          IDAT = IC(LSRCP-1)
        ELSE
          ERRSRC = EZS_BAD_ARGUMENT
        ENDIF
      ELSE
        ERRSRC = EZS_BANK_NOTFOUND
      ENDIF
      EZHDRI = IDAT
  999 RETURN
      END
