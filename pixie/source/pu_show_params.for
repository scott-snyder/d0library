      SUBROUTINE PU_SHOW_PARAMS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given one or more parameters show the
C-   name and value for each parameter.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-OCT-1990   LUPE HOWELL, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PFNUM,KEY
      INTEGER LENGTH,I,J,L,IDX,IER,CONTROL
      CHARACTER*10 CNUMBER
      CHARACTER*32 BANK_NAME, NAME,ARRAY_NAME
      CHARACTER*80 STRING,RECORD,LINE,MESS
      LOGICAL LAST
      REAL    RRVAL
      INTEGER IIVAL,ITYPE,LEN1,LEN2
      LOGICAL LLVAL
      CHARACTER*4 CCVAL
      EQUIVALENCE(RRVAL,IIVAL,LLVAL,CCVAL)
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
C
C ****  Clear screen
C
      CALL OUTMSG('1')
C
C ****  Get name of currently selected RCP bank
C
      CALL EZTELL(BANK_NAME,LENGTH)
      LEN1 = INDEX(BANK_NAME,'PX_')  + 3
      LEN2 = INDEX(BANK_NAME,'_RCP') - 1 
      MESS = ' Current Package : '//BANK_NAME(LEN1:LEN2)
      CALL OUTMSG(MESS)
C
C ****  Prompt user for array name (SCREEN or PARAM)
C
      CALL GETPAR(1,' Enter SCREEN or PARAM > ','U',STRING)
      IF ( PFNUM() .EQ. 4 ) THEN
        GOTO 999
      ENDIF
C
      IF ( STRING(1:1) .EQ. 'S' ) THEN
C
C ****  Prompt user for array element name 
C
        CALL GETPAR(1,' Enter SCREEN Name > ','U',STRING)
        IF ( PFNUM() .EQ. 4 ) THEN
          GOTO 999
        ENDIF
C
        ARRAY_NAME = 'PXSCREEN'
C
        CALL SWORDS(STRING,I,J,L)
        CALL PU_GET_SCREEN_INDEX(STRING(I:J),IDX,IER)
        IF ( IER .EQ. 2 ) THEN
          CALL INTMSG
     &    (' PU_SHOW_PARAMS: Screen requested was not found ')
          GOTO 999
        ENDIF
C
      ELSEIF ( STRING(1:1) .EQ. 'P' ) THEN
        IDX = 1
        ARRAY_NAME = 'PXPARAMS'
      ELSE
        GOTO 999
      ENDIF
C
C ****  Prompt user for NAMES of parameters
C
      CALL GETPAR(1,' Enter name(s) > ','U',RECORD)
      IF ( PFNUM() .EQ. 4 ) THEN
        GOTO 999
      ENDIF

      CONTROL = 0
      LAST    = .FALSE.
      CALL INTMSG(' ')
      DO WHILE ( .NOT. LAST )
        CALL GET_NEXT_TOKEN(RECORD,',',I,J,L,LAST,CONTROL)
        NAME = RECORD(I:J)
        CALL EZ_GET_ELEMENT(ARRAY_NAME,NAME,IDX,1,RRVAL,ITYPE,IER)
        IF ( IER .NE. 0 ) THEN
          MESS = ' PU_SHOW_PARAMS: Parameter requested was not found '
     &           //NAME 
          CALL INTMSG(MESS)
          CALL WAITIT(3.0)
          GOTO 999
        ENDIF
C
C ****  Convert value to a character string
C
        IF     ( ITYPE .EQ. VTINT ) THEN
          WRITE(UNIT=CNUMBER,FMT='(I10)')  IIVAL
        ELSEIF ( ITYPE .EQ. VTLOG ) THEN
          IF ( LLVAL ) THEN
            WRITE(UNIT=CNUMBER,FMT='(5X,A5)') ' TRUE'
          ELSE
            WRITE(UNIT=CNUMBER,FMT='(5X,A5)') 'FALSE'
          ENDIF
        ELSEIF ( ITYPE .EQ. VTREAL ) THEN
          WRITE(UNIT=CNUMBER,FMT='(F10.4)') RRVAL
        ELSEIF ( ITYPE .GT. VTCHR ) THEN
          CNUMBER = '      '//CCVAL
        ELSE
          CNUMBER = ' '
        ENDIF
C
C ****  Write the parameter
C
        LINE = ' '
        WRITE(LINE,300) NAME, CNUMBER
        CALL INTMSG(LINE)
      ENDDO
C
C **** Waiting for a key to be struck
C
      CALL PFWAIT(KEY)
C
  999 RETURN
C
C ****  FORMATS
C
  100 FORMAT(A)
  300 FORMAT(1X,1X,A32,1X,A10)

      END
