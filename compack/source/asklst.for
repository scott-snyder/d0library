      SUBROUTINE ASKLST (PRT,NLIST,BOOK,STR,CMD,BACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prompt user for list of up 16 items (integers).
C-
C-   Inputs:   PRT         Prompt
C-             NLIST       Number of items
C-
C-   Outputs : BOOK        Item requests (boolean flags)
C-             STR         String containing list of item numbers
C-             CMD         String containing command line
C-             BACK        If true go back to upper menu level
C-
C-   Note:     The following commands are recognized:
C-             ALL         ALL BOOK flags SET
C-             NONE,CLEAR  ALL BOOK flags CLEARED
C-
C-   Created   4-MAR-1988   Harrison B. Prosper
C-   Modified 17-MAY-1988
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL       OK,BACK,ACTIVE,ALL
      INTEGER       NLIST
      LOGICAL       BOOK(*)
      CHARACTER*(*) STR
      INTEGER       ERR,N,NN,I,J,K,L,NP,LEN,NMAX
      PARAMETER(    NMAX = 16 )
      INTEGER       NUMBER(NMAX)
      REAL          VALUE
      CHARACTER*16  RANGE
      CHARACTER*64  STRING
      CHARACTER*(*) CMD,PRT,ERRMSG
      PARAMETER( ERRMSG = ' %ASKLST-ERROR-')
C----------------------------------------------------------------------
C
      NUMBER(1) = 1
      NUMBER(2) = NLIST
      CALL VNUMI (2,NUMBER,'[','...',']',RANGE,L)
C
C ****  Prompt for item requests
C
      NP = LEN (PRT)
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        STRING=PRT
        CALL GETSTR (STRING(1:NP)//' '//RANGE(1:L)//' > ',STRING,OK,
     &    BACK) 
        IF ( BACK ) GOTO 999
        IF ( OK  ) THEN
        CMD = STRING ! Return command string
        NN = 0  ! Item request counter
C
C ****  Check for CLEAR or RESET
C
          IF ( (INDEX(CMD,'C').GT.0) .OR. (INDEX(CMD,'R').GT.0 ) )THEN
            DO 30 I = 1,NLIST
              BOOK(I) = .FALSE.
   30       CONTINUE
            STR = ' '
            GOTO 999
          ENDIF
          DO WHILE ( active )
            N = VALUE (STRING,I,J,ERR)
C
C ****  NOTE: If ERR equals zero then string contains non-numeric characters
C
            active = ERR. GT. 0
            IF ( active ) THEN
              IF ( (N .GE. 1) .AND. (N .LE. NLIST)  ) THEN
                BOOK(N) = .TRUE.
                NN = NN + 1
                NUMBER(NN) = N
              ELSE
                CALL INTMSG (ERRMSG//'IVALID Item number '''//
     &          string(I:J)//'''; Out of range')
              ENDIF
              STRING  = STRING(J+1:)
            ELSE
              IF ( CMD(1:3) .EQ. 'ALL' ) THEN
                NN = NLIST
              ENDIF
            ENDIF
          ENDDO
        ELSE
          NN = NLIST
          CMD = 'ALL'
          ACTIVE = .FALSE.
        ENDIF
      ENDDO
C
C ****  Construct string of items requested 
C
      NP = LEN(STR)
      IF ( NN .GE. NLIST ) THEN
C
C ****  Set ALL item request flags
C
        DO 20 I = 1,NLIST
          BOOK(I) = .TRUE.
   20   CONTINUE
        NUMBER(1) = 1
        NUMBER(2) = NLIST
        CALL VNUMI (2,NUMBER,'(','...',')',STR(1:NP),N)
      ELSE
        IF ( NN. LE. 0 ) THEN
          CMD = 'NONE'
        ELSE
          CMD = 'SOME'
          CALL VNUMI (NN,NUMBER,'(',',',')',STR(1:NP),N)
        ENDIF
      ENDIF
C
  999 RETURN
      END
