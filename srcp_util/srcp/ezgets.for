      SUBROUTINE EZGETS (PARAM,IDX,STRING,LENGTH,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the IDX'th character string in the
C-                         array PARAM from the currently selected 
C-                         RCP bank. NOTE: EZGETS works on both mixed
C-                         and string arrays.
C-                         
C-
C-   Inputs  : PARAM       Name of array parameter containing string.
C-             IDX         Index of string. 1--first string, 2--second etc.
C-   Outputs : STRING      character string.
C-             LENGTH      String length.
C-             IER         Error code. 0--> OK.
C-   Controls: None
C-
C-   Created  12-APR-1989   Harrison B. Prosper
C-   Updated  10-NOV-1989   Harrison B. Prosper
C-   Works off variable length strings
C-   Updated  28-MAR-1990   Harrison B. Prosper  
C-      Corrected II .LT. TOTAL to II .LE. TOTAL 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM
      INTEGER       IDX
      CHARACTER*(*) STRING
      INTEGER       LENGTH
      INTEGER       IER
C
      INTEGER L,II,JJ,KK,IVAL,TYPE,NN,TOTAL
      INTEGER JDX,KDX,ID,LPAR,LSTR,LLL
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
C
      LPAR   = LEN(PARAM)
      LSTR   = LEN(STRING)
      STRING = ' '
      LENGTH = 0
      IER    = 0
      IF ( IDX .GT. 0 ) THEN
        JDX = IDX
      ELSE
        JDX = 1                         ! If IDX=0 take 1st string
      ENDIF
C
C ****  Get Parameter ID
C
      CALL EZGETI (PARAM(1:LPAR),ID,IER)
      IF ( IER .NE. 0 ) GOTO 999
C
C ****  Loop over array and find next string
C
      KDX = 0
      II  = 1
  100 CONTINUE
      CALL EZGET2 (ID,II,II,1,IVAL,TYPE,NN,TOTAL,IER)
C
      IF ( TYPE .GT. VTCHR ) THEN       ! Check for character type
        KDX = KDX + 1
        LLL = TYPE-VTCHR                ! Get string length
        IF ( KDX .EQ. JDX ) THEN        ! Is this the right string
          LENGTH = LLL
          CALL EZGETC (PARAM(1:LPAR),II,LLL,STRING(1:LSTR),IER)
          GOTO 999
        ELSE
          NN = 1 + (LLL-1)/4            ! Get number of 32-bit words
          II = II + NN                  ! Skip NN words
        ENDIF
      ELSE
        II = II + 1                     ! Go to next word in array
      ENDIF
      IF ( II .LE. TOTAL ) GOTO 100
C
  999 RETURN
      END
