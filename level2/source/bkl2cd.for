      SUBROUTINE BKL2CD(NCHA,LL2CD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book L2CD, the CD_MATCH intermediate tracking
C-                         results bank.  Individual tracking candidates
C-                         are assigned separate linearly chained banks.
C-
C-   Inputs  : NCHA = # of CDC cells within road we need to store hit results
C-   Outputs : LL2CD = pointer to current L2CD bank 
C-   Controls:
C-
C-   Created  30-MAR-1992   D. Claes
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZL2CD.LINK'
      INCLUDE 'D0$PARAMS:L2CD.PARAMS'
      INTEGER NCHA,LL2CD,LSUP1,LSUP2
      INTEGER IOH, GZFRES
      SAVE IOH
      INTEGER NSIZ                      ! Total number of data words
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      NSIZ = NCHA*NREP_L2CD + NTOP_L2CD
C
      IF ( FIRST ) THEN
        CALL MZFORM('L2CD','5I 4F/9I',IOH)
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
      CALL MZBOOK(IXMAIN,LL2CD,LSUP1,-IZL2CD,'L2CD',1,1,NSIZ,IOH,0)
C
   20 CONTINUE
      IQ(LL2CD+1) = 1         ! version number
      IQ(LL2CD+2) = NTOP_L2CD ! size of fixed portion of bank
      IQ(LL2CD+3) = NREP_L2CD ! size of repeating hit list
      IQ(LL2CD+4) = NCHA      ! Number of cells found within road
C
  999 RETURN
      END
