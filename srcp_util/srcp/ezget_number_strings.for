      SUBROUTINE EZGET_NUMBER_STRINGS (PARAM,NUMBER,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the number of strings in specified 
C-                         array.
C-
C-   Inputs  : PARAM  [C*]  Name of array parameter containing strings.
C-   Outputs : NUMBER [I]   Number of strings
C-             IER         Error code. 0--> OK.
C-   Controls: None
C-
C-   Created   4-JUN-1992   Harrison B. Prosper   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM
      INTEGER NUMBER
      INTEGER       IER
C
      INTEGER L,II,JJ,KK,IVAL,ITYPE,NN,TOTAL
      INTEGER JDX,KDX,ID,LPAR,LSTR,LLL
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
C
      LPAR   = LEN(PARAM)
      NUMBER = 0
      IER    = 0
C
C ****  Get Parameter ID
C
      CALL EZGETI (PARAM(1:LPAR),ID,IER)
      IF ( IER .NE. 0 ) GOTO 999
C
C ****  Loop over array and count number strings
C
      KDX = 0
      II  = 1
  100 CONTINUE
      CALL EZGET2 (ID,II,II,1,IVAL,ITYPE,NN,TOTAL,IER)
C
      IF ( ITYPE .GT. VTCHR ) THEN    ! Check for character type
        KDX = KDX + 1
        LLL = ITYPE-VTCHR             ! Get string length
        NN = 1 + (LLL-1)/4            ! Get number of 32-bit words
        II = II + NN                  ! Skip NN words
      ELSE
        II = II + 1                   ! Go to next word in array
      ENDIF
      IF ( II .LE. TOTAL ) GOTO 100
C
      NUMBER = KDX
C
  999 RETURN
      END
