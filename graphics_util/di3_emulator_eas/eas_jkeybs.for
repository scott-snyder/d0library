       SUBROUTINE JKEYBS(DSPDEV, PHYDEV, ECHOLV, MAXCHR, STRING, ACTUAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  To invoke the virtual KEYBOARD input function.
C-
C-   Inputs  : DSPDEV,   A currently selected display device
C-             PHYDEV,   Physical input device
C-             ECHOLV,   Echo level (0=no echo, 1=display on device,...)
C-             MAXCHR    Maximum number of characters
C-
C-   Outputs : STRING,   Character string returned
C-             ACTUAL    Actual number of characters
C-   Controls:
C-
C-   Created   4-SEP-1990   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
       IMPLICIT NONE
       INTEGER  DSPDEV, PHYDEV, ECHOLV, MAXCHR, ACTUAL
       CHARACTER*80  STRING
C
       CHARACTER*10  STR
       CHARACTER*1  ICH
       INTEGER  STRLEN, BUT, I
       EXTERNAL  ERRHND
C
       IF(MAXCHR .GT. 80 .OR. MAXCHR .LT. 1) THEN
         CALL ERROR('MAXIMUM NUMBER OF CHARACTER
     & NOT IN THE RANGE OF 1 & 80')
       ENDIF
C
       IF(ECHOLV .GT. 32767 .OR. ECHOLV .LT. 0) THEN
         CALL ERROR('ECHOLV NOT IN THE RANGE OF 0 & 32767')
       ENDIF
C
       ACTUAL = 0
C
       CALL PCONN('KBHANDLER', 4, 1, 'HOST_MESSAGE', ERRHND)
C
       CALL PSNST('C-INPUT> ',1, 'MESSAGE_DISPLAY', ERRHND)
C
    1  CONTINUE
       CALL PGETW(STR, STRLEN, ERRHND)
       IF(STRLEN .LE. 0) GOTO 9999
       READ (STR(1:STRLEN), '(A1)', ERR=9999) ICH
       BUT = ICHAR(ICH)
       IF(BUT .GT. 47 .AND. BUT .LT. 58) BUT = BUT - 48
C
       IF(BUT .NE. 127) THEN
         ACTUAL = ACTUAL + 1
         STRING(ACTUAL:ACTUAL) = STR(1:1)
       ELSE
         ACTUAL = ACTUAL - 1
         IF(ACTUAL .LT. 0) ACTUAL = 0
       ENDIF
C
       IF(BUT .NE. 13) THEN
         IF(ECHOLV .GT. 0)
     &    CALL PSNST(' '//STRING(1:ACTUAL),1, 'MESSAGE_DISPLAY', ERRHND)
         IF(ACTUAL .LT. MAXCHR) GOTO 1
       ENDIF
C
       IF(BUT .EQ. 13) ACTUAL = ACTUAL - 1
       DO I=1,80-ACTUAL
         STRING(ACTUAL+I:ACTUAL+I) = ' '
       ENDDO
C
C-- Done
C
       CALL PSNST(' ', 1, 'MESSAGE_DISPLAY', ERRHND)
       CALL PDI('KBHANDLER', 4, 1, 'HOST_MESSAGE', ERRHND)
       CALL PPURGE(ERRHND)
       GOTO 999
 9999  CONTINUE
       CALL ERROR('ERROR DECODING INPUT: '//STR(:STRLEN))
C
  999  RETURN
       END
