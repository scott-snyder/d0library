      SUBROUTINE EZMERGE_BANKS(BANK1,BANK2,BANKOUT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Merges two given RCP banks into another RCP bank.
C-
C-   Inputs  : BANK1   [C*]: First RCP bank to merge
C-             BANK2   [C*]: Second RCP bank to concatinate
C-
C-   Outputs : BANKOUT [C*]:
C-
C-   Controls: IER      [I]: If 0 Okay
C-
C-   Created  30-OCT-1991   Lupe Howell
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*)BANK1
      CHARACTER*(*)BANK2
      CHARACTER*(*)BANKOUT
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      INTEGER MAXREC
      PARAMETER( MAXREC = 1500 )
C
      CHARACTER*40 CURRBANK
      CHARACTER*132 PARAMS2(MAXREC),BUFFER(MAXREC)
      CHARACTER*132 NOT_FOUND_LIST(MAXREC),FOUND_LIST(MAXREC)
      CHARACTER*132 CTEMP
      INTEGER I,II,J,CLEN,NEXT,TOTAL2,NREC
      INTEGER IERR,FL,TRULEN
      INTEGER TOT_F,TOT_NOTF
      LOGICAL FOUND,EZERROR,EZCHEK
C----------------------------------------------------------------------
      CALL EZTELL(CURRBANK,I)
      CALL WORD(CURRBANK,I,J,CLEN)
C
C ****  Copy the first bank into the output bank
C
      I = 0
      J = 0
      CALL EZCOPY(BANK1,BANKOUT,I,J,IER)
      IF ( IER .NE. 0 ) THEN
        CTEMP = ' Problem copying bank '//BANK1
        CALL ERRMSG(' SRCP','EZMERGE_BANKS', CTEMP(1:TRULEN(CTEMP)) 
     &    ,'W')
        GOTO 999
      ENDIF
C
C ****  Get the parameter names of the second file
C
      NEXT = 1
      I = 0
      CALL EZPICK(BANK2)
      IF ( EZERROR(IER) ) THEN
        CTEMP = ' Problem accessing '//BANK2
        CALL ERRMSG(' SRCP','EZMERGE_BANKS', CTEMP(1:TRULEN(CTEMP))
     &      ,'W')
        GOTO 999
      ENDIF
      DO WHILE ( NEXT .NE. -1 )
        I = I + 1
        CALL EZGET_NEXT_NAME(PARAMS2(I),NEXT)
      ENDDO
      CALL EZRSET
      TOTAL2 = I
C
C ****  Make 2 lists of parameters.  Ones with the elements
C ****  not found in BANKOUT another with the ones found
C
      TOT_F    = 0
      TOT_NOTF = 0
      CALL EZPICK(BANKOUT)
      DO I = 1, TOTAL2
        FOUND = EZCHEK(PARAMS2(I))
        IF ( FOUND ) THEN
          TOT_F = TOT_F + 1
          FOUND_LIST(TOT_F) = PARAMS2(I)
        ELSE
          TOT_NOTF = TOT_NOTF + 1
          NOT_FOUND_LIST(TOT_NOTF) = PARAMS2(I)
        ENDIF
      ENDDO
C
C ****  Merge the elements found in both files
C
      CALL EZPICK(BANK2)
      DO I = 1, TOT_F
        CALL EZFETCH(FOUND_LIST(I),MAXREC,NREC,BUFFER,IER)
        CALL EZMERGE_PARAMS(BANKOUT,FOUND_LIST(I),NREC,BUFFER,IER)
        IF( IER .NE. 0 ) THEN
          CTEMP = ' Problem merging bank '//BANKOUT
          CALL ERRMSG(' SRCP','EZMERGE_BANKS', CTEMP(1:TRULEN(CTEMP))
     &      ,'W')
        GOTO 800
        ENDIF
      ENDDO
C
C ****  Add the elements NOT found in both files
C
      DO I = 1, TOT_NOTF
        CALL EZFETCH(NOT_FOUND_LIST(I),MAXREC,NREC,BUFFER,IER)
        CALL EZPICK(BANKOUT)
        DO II = 1 , NREC
          FL = TRULEN(BUFFER(II))
          FL = MAX(1,FL)
          CALL EZADD(BUFFER(II)(1:FL),1,IER)
        ENDDO
        CALL EZRSET  ! Reseting Bankout
      ENDDO
C
C ****  Ending the merged bank
C
      CALL EZPICK(BANKOUT)
      CALL EZEND
      CALL EZRSET  ! Reseting Bankout
C
  800 CALL EZRSET
  999 RETURN
      END
