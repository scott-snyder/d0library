      SUBROUTINE PU_GOTO_SCREEN(SCRENUM,IDX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the pointer IDX for the screen with
C-   screen number SCRENUM. The pointer IDX provides direct access to
C-   the screen block in PXSCREEN.
C-   
C-   Inputs  : SCRENUM   [I ] - Number of the screen desire
C-
C-   Outputs : IDX       [I ] - Array Index for the desired screen
C-
C-   Created   4-MAY-1990   Lupe Howell
C-   Updated  15-JAN-1991   Harrison B. Prosper  
C-      Remove middle argument 
C-   Updated  15-MAY-1991   Harrison B. Prosper  
C-      Change NVIEWPORT to PICKABLE, remove middle argument
C-   Updated  13-JUN-1991   Lupe Howell  Error messages posted  
C-   Updated  30-OCT-1992   Lupe Howell  NUMBER_ELEMENTS decreased 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER SCRENUM, IDX
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER IER
C
      INTEGER NUMBER_ELEMENTS
      PARAMETER( NUMBER_ELEMENTS = 20 ) 
C
      LOGICAL ACTIVE
C
      CHARACTER*(*) ARRAY_NAME
      PARAMETER( ARRAY_NAME = 'PXSCREEN' )
      CHARACTER*32 PARNAME
      CHARACTER*80 REM,AVAL
      CHARACTER*2 CNUM
C
      INTEGER EZZSHFT,EZZAND,ID,IPVAL,IPTV,IPTT,IPTO,IPTI
      INTEGER TOTAL,LAST_IPOINT,IPOINT,VALPOINT,I,J
      INTEGER JVAL,LVAL,ITYP,NPARAM
C----------------------------------------------------------------------
      IDX = 0
      WRITE(CNUM,FMT='(I2)')SCRENUM
C
C ****  Check if the screen number SCRENUM is valid 
C
      IF ( SCRENUM .LE. 0 ) THEN
        CALL ERRMSG(' BAD SCREEN NUMBER','PU_GOTO_SCREEN',
     &    'Illegal screen number '//CNUM,'W')
        GOTO 999
      ENDIF
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
        IF ( PARNAME(1:4) .EQ. 'NAME' ) THEN
          NPARAM = NPARAM + 1
          IF ( NPARAM .EQ. SCRENUM ) THEN
            ACTIVE = .FALSE.            ! Found screen so EXIT
          ELSE
C
C ****  Skip remainder of screen block
C
            DO I =  1,NUMBER_ELEMENTS
              CALL EZ_GET_NEXT_ELEMENT(IPOINT,IC(IPTV),IC(IPTT),
     &           PARNAME,JVAL,AVAL,LVAL,ITYP,REM,VALPOINT)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C
C ****  Check to see if the SCRENUM requested was found
C
      IF ( ACTIVE ) THEN
        CALL ERRMSG(' NO SCREEN FOUND','PU_GOTO_SCREEN',
     &    'No screen found under number '//CNUM,'W')
      ELSE
C
C ****  At the right screen
C
        IDX = LAST_IPOINT
      ENDIF

  999 RETURN
      END
