      SUBROUTINE BKL2FD(LL2FD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book L2FD, the CD_MATCH intermediate tracking
C-                         results bank.  Individual tracking candidates
C-                         are assigned separate linearly chained banks.
C-
C-   Inputs  : NCHA = # of FDC cells within road we need to store hit results
C-   Outputs : LL2FD = pointer to current L2FD bank
C-   Controls:
C-
C-   Created  30-MAR-1992   D. Claes
C-   Modified 30-JUN-1992   YI-CHENG LIU   ( for FDC, and disable NCHA )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZL2FD.LINK'
C     INCLUDE 'D0$PARAMS:L2FD.PARAMS'
      INCLUDE 'D0$PARAMS:L2CD.PARAMS'
      INTEGER NCHA,LL2FD,LSUP1,LSUP2
      INTEGER IOH, GZFRES
      SAVE IOH
      INTEGER NSIZ                      ! Total number of data words
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      NSIZ = NCHA*NREP_L2CD + NTOP_L2CD
C
      IF ( FIRST ) THEN
        CALL MZFORM('L2FD','5I 4F/9I',IOH)
        FIRST = .FALSE.
      END IF
C
C--   Find link to supporting parent bank
C
      LSUP1 = GZFRES()
      IF (LSUP1 .LE. 0) RETURN
C
C--   Book it
C
      CALL MZBOOK(IXMAIN,LL2FD,LSUP1,-IZL2FD,'L2FD',1,1,NSIZ,IOH,0)
C
   20 CONTINUE
      IQ(LL2FD+1) = 1         ! version number
      IQ(LL2FD+2) = NTOP_L2CD ! size of fixed portion of bank
      IQ(LL2FD+3) = NREP_L2CD ! size of repeating hit list
      IQ(LL2FD+4) = NCHA      ! Number of cells found within road
C
  999 RETURN
      END
