      SUBROUTINE BKTND1(LTND1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank TND1.
C-
C-   Inputs  : LTPHY  [I] Address of the parent bank.
C-                          = 0, will find it for you.
C-   Outputs : LTND1  [I] Address of booked TND1 bank.
C-   Controls: None
C-
C-   Created  16-APR-1996 22:18:24.71  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTPHY
      INTEGER LTND1
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,IXIO
      INTEGER GZTPHY
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEbstp.INC'
      INCLUDE 'D0$LINKS:IZTND1.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LTND1 = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('TND1','1I -F',IXIO)        ! Describe Bank format
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
      CALL MZBOOK(IDVSTP,LTND1,LTPHY,-IZTND1,'TND1',NL,NS,ND,IXIO,0)
C
C ****  Book a stand-alone bank
C
C      CALL MZBOOK(IXMAIN,LTND1,0,2,'TND1',NL,NS,ND,IXIO,0)
  999 RETURN
      END
