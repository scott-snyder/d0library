      SUBROUTINE PU_GOTO_ACTION(ACTIONUM,ACTION_NAME,IDX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the index of a requested action.
C-   The search for the action can be done by action number or by action name
C-   If ACTIONUM is 0 the the search will be done using ACTION_NAME.
C-
C-   Inputs  : ACTIONUM   [C*]: Number of the action desired
C-                             If this number is set to 0 it will search
C-                             the action by name.
C-             ACTION_NAME[C*]: name of the action desired
C-
C-   Outputs : IDX        [I ]: Array indexfor the desired action.
C-
C-   Created  18-DEC-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ACTIONUM,IDX
      CHARACTER*(*) ACTION_NAME
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER IER
C
      INTEGER NUMBER_ELEMENTS
      PARAMETER( NUMBER_ELEMENTS = 21 )
C
      LOGICAL ACTIVE,NUMBER,FOUND
C
      CHARACTER*(*) ARRAY_NAME
      PARAMETER( ARRAY_NAME = 'PXSCREEN' )
      CHARACTER*32 PARNAME
      CHARACTER*80 REM,AVAL
      CHARACTER*2 CNUM
C
      INTEGER EZZSHFT,EZZAND,ID,IPVAL,IPTV,IPTT,IPTO,IPTI,ALEN
      INTEGER TOTAL,LAST_IPOINT,IPOINT,VALPOINT,I,J
      INTEGER JVAL,LVAL,ITYP,NPARAM,TEMP_POINT
C----------------------------------------------------------------------
C
C ****  Check the action number to determine
C ****  how the search is going to be
C
      IF ( ACTIONUM .GT. 0 ) THEN
        NUMBER = .TRUE.
      ELSE
        NUMBER = .FALSE.
      ENDIF
      CALL WORD(ACTION_NAME,I,J,ALEN)
      FOUND = .FALSE.
      IDX = 0
      WRITE(CNUM,FMT='(I2)')ACTIONUM
C
C ****  Check if an SRCP bank has been selected
C
      IF ( ISRCP .LE. 0 ) THEN
        IER = EZS_BANK_NOTSELECTED
        ERRSRC = IER
        GOTO 999
      ENDIF
C
C ****  Get the bank address
C
      LSRCP = KSRCP(ISRCP)
      IF ( LSRCP .LE. 0 ) THEN
        IER = EZS_BANK_NOTFOUND
        ERRSRC = IER
        GOTO 999
      ENDIF
C
C ****  Get pointers to data within SRCP bank
C
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
C
C ****  Get index into RCP bank for the given array-name
C
      CALL EZGETI (ARRAY_NAME,ID,IER)
      IF ( IER .NE. 0 ) THEN
        GOTO 999
      ENDIF
C
C ****  Get number of values/identifier from type list
C
      IPVAL = EZZSHFT(IC(IPTO+ID),-NBITS)       ! Pointer to values-list
      TOTAL = EZZAND(IC(IPTT+IPVAL),MASK)       ! Zero upper word
C
C ****  Make pointers point to absolute location
C
      IPTV = IPTV + IPVAL
      IPTT = IPTT + IPVAL
C
      ACTIVE = .TRUE.
      NPARAM = 0
      IPOINT = 1                        ! Start at first word in PXSCREEN
      LAST_IPOINT = 1
C
C ****  Skip NSCREEN element 
C
      CALL EZ_GET_NEXT_ELEMENT(IPOINT,IC(IPTV),IC(IPTT),
     &    PARNAME,JVAL,AVAL,LVAL,ITYP,REM,VALPOINT)
C
C
      DO WHILE ( ACTIVE )
C
C ****  Exit if we've gone beyond the given array
C
        IF ( IPOINT .GT. TOTAL ) GOTO 999
C
C ****  Get element of array (param, value, remark) and update pointer
C
        LAST_IPOINT = IPOINT            ! Remember previous pointer value
        CALL EZ_GET_NEXT_ELEMENT(IPOINT,IC(IPTV),IC(IPTT),
     &    PARNAME,JVAL,AVAL,LVAL,ITYP,REM,VALPOINT)
C
        IF ( NUMBER ) THEN
          IF ( PARNAME(1:4) .EQ. 'NAME' ) THEN
            NPARAM = NPARAM + 1
            IF ( NPARAM .EQ. ACTIONUM ) THEN
              ACTIVE = .FALSE.            ! Found screen so EXIT
              FOUND = .TRUE.
            ENDIF
          ENDIF
        ELSE
          TEMP_POINT = IPOINT   ! remenber ointer to NAME
          CALL EZ_GET_NEXT_ELEMENT(IPOINT,IC(IPTV),IC(IPTT), ! Getting action
     &      PARNAME,JVAL,AVAL,LVAL,ITYP,REM,VALPOINT)
          IPOINT = TEMP_POINT 
          IF ( PARNAME(1:6) .EQ. 'ACTION' ) THEN
            IF( AVAL(1:ALEN) .EQ. ACTION_NAME(1:ALEN) ) THEN
              FOUND = .TRUE.
              ACTIVE = .FALSE.
            ENDIF
          ENDIF
        ENDIF
        IF ( .NOT. FOUND ) THEN
C
C ****  Skip remainder of screen block
C
          DO I =  1,NUMBER_ELEMENTS
            CALL EZ_GET_NEXT_ELEMENT(IPOINT,IC(IPTV),IC(IPTT),
     &           PARNAME,JVAL,AVAL,LVAL,ITYP,REM,VALPOINT)
          ENDDO
        ENDIF
      ENDDO
C
C ****  Check to see if the SCRENUM requested was found
C
      IF ( ACTIVE ) THEN
        CALL ERRMSG(' NO SCREEN FOUND','PU_GOTO_ACTION',
     &    'No action found under number '//CNUM,'W')
      ELSE
C
C ****  At the right screen
C
        IDX = LAST_IPOINT
      ENDIF

  999 RETURN
      END
