      SUBROUTINE BKTND2(LTND2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank TND2.
C-
C-   Inputs  : LTPHY  [I] Address of the parent bank.
C-                          = 0, will find it for you.
C-   Outputs : LTND2  [I] Address of booked TND2 bank.
C-   Controls: None
C-
C-   Created  16-APR-1996 22:49:45.89  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTPHY
      INTEGER LTND2
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,IXIO
      INTEGER GZTPHY
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTND2.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LTND2 = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('TND2','1I -F',IXIO)        ! Describe Bank format
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
        LTPHY = GZTPHY()
      IF ( LTPHY .LE. 0 ) THEN
        GOTO 999
      ENDIF
C
      NL = 0
      NS = 0
      ND = 83
      CALL MZBOOK(Idvstp,LTND2,LTPHY,-IZTND2,'TND2',NL,NS,ND,IXIO,0)
C
C ****  Book a stand-alone bank
C
C      CALL MZBOOK(IXMAIN,LTND2,0,2,'TND2',NL,NS,ND,IXIO,0)
  999 RETURN
      END
