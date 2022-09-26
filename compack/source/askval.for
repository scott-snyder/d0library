C DEC/CMS REPLACEMENT HISTORY, Element ASKVAL.FOR
C *2    11-MAY-1988 10:43:49 HARRY "COMPACK routine to prompt for ONE integer from a SET of integers"
C *1    22-APR-1988 09:55:08 HARRY "COMPACK routine to prompt for and check a range of INTEGERS or REALS"
C DEC/CMS REPLACEMENT HISTORY, Element ASKVAL.FOR
      SUBROUTINE ASKVAL (PRT,NV,VALS,DEFAUL,STR,NUMBER,BACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prompt user for ONE of several INTEGER values 
C-                         in the set (VALS(1),...,VAS(NV)) and set default
C-                         value if RETURN entered.
C-
C-   Inputs:   PRT         Prompt
C-             NV          Number of values
C-             VALS(*)     Set of acceptable integer values
C-             DEFAUL      Default value
C-
C-   Outputs:  STR         String indicating value selected
C-             NUMBER      Value selected
C-             BACK        If true go back to upper menu level
C-
C-   Created  14-APR-1988   Harrison B. Prosper
C-   Modified  9-MAY-1988
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL       OK,BACK,ACTIVE
      INTEGER       VALS(*),DEFAUL,NUMBER
      INTEGER       I,J,K,L,NV,M,N,NP,CODE
      CHARACTER*(*) PRT,STR
      REAL          VALUE
      CHARACTER*16  DEFSTR
      CHARACTER*64  STRING
      CHARACTER*80  VALSTR
      CHARACTER*(*) ERRMSG
      PARAMETER(    ERRMSG = ' %ASKVAL-ERROR-')
C&IF LINUX
C&      character*1024 tmp
C&ENDIF
C----------------------------------------------------------------------
C
      NP = LEN (PRT)
      CALL VNUMI (NV,VALS(1),'(',',',')',VALSTR,M)
      CALL VNUMI (1,DEFAUL,'[',',',']',DEFSTR,N)
C
C ****  Ask for value
C
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
C&IF LINUX
C&        tmp = prt
C&        CALL GETSTR (tmp(1:NP)//' '//DEFSTR(1:N)//'> ',STRING,OK,BACK)
C&ELSE
        CALL GETSTR (PRT(1:NP)//' '//DEFSTR(1:N)//'> ',STRING,OK,BACK)
C&ENDIF
        IF ( BACK ) GOTO 999
        IF ( OK  ) THEN
C
C ****  Extract number
          NUMBER = VALUE (STRING,I,J,CODE)
C
C ****  If CODE .EQ. 0 then string contains non-numeric characters
          IF ( CODE .GT. 0 ) THEN
C
C ****  Check values
            DO 10 K = 1, NV
              OK = NUMBER .EQ. VALS(K)
              IF ( OK ) GOTO 20
   10       CONTINUE
   20       CONTINUE
            IF ( OK ) THEN
              ACTIVE = .FALSE.
            ELSE
              CALL INTMSG (ERRMSG//'BADVALUE: '//STRING(I:J))
              CALL INTMSG (' Valid values are: '//VALSTR(1:M))
            ENDIF
          ELSE
            NUMBER = DEFAUL
            ACTIVE = .FALSE.
          ENDIF
        ELSE
          NUMBER = DEFAUL
          ACTIVE = .FALSE.
        ENDIF
      ENDDO
C
C ****  Create string containing value selected
C
      NP = LEN(STR)
      CALL VNUMI (1,NUMBER,'(',',',')',STR(1:NP),N)
  999 RETURN
      END
