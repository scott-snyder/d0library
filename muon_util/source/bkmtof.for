      SUBROUTINE BKMTOF(LMUON,ITRACK,NMSCT,LMTOF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank MTOF.
C-
C-   Inputs  : LMUON  [I] Address of the parent bank.
C-                          = 0, will find it for you.
C-             ITRACK [I] Track number {0= first track}
C-             NMSCT  [I] No of Associated scint hits
C-   Outputs : LMTOF  [I] Address of booked MTOF bank.
C-   Controls: None
C-
C-   Created  25-FEB-1994 10:44:19.81  acharya
C-   Revised  09-aug-1994 D. Wood: moved ND calculation out of IF(FIRST)
C-                                 clause
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LMUON
      INTEGER LMTOF
      INTEGER ITRACK
      INTEGER NMSCT
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,NR,IXIO
      INTEGER GZMUON
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMTOF.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LMTOF = 0
C
      IF(FIRST) THEN
         NL = 0               ! NL (total number of links)
         NS = 0               ! NS (number of struct. links)
         NR = 16              ! No of repitition words
         CALL MZFORM('MTOF','3I/1I 1B 2I 12F',IXIO)  ! Describe Bank format 
         FIRST=.FALSE.
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LMUON .LE. 0 ) THEN
        LMUON = GZMUON(ITRACK)
      ENDIF
      IF ( LMUON .LE. 0 ) THEN
        GOTO 999
      ENDIF
C
      ND = NMSCT*NR+3      ! ND (number of data words)
      CALL MZBOOK(IXMAIN,LMTOF,LMUON,-IZMTOF,'MTOF',NL,NS,ND,IXIO,0)
      IQ(LMTOF+1)=1                 ! Version nUmber
      IQ(LMTOF+2)=NR                ! Number of repitition words
      IQ(LMTOF+3)=NMSCT             ! associated scintillator hits
  999 RETURN
      END
