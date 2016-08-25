      SUBROUTINE TB90_CALOR_CHANNEL_BOOK(ETA,PHI,LYR,HBIN,HLO,HHI
     &  ,WLO,WBIN,IOFF1,IOFF2,NBOOK,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK HISTOGRAMS TO WATCH CHANNEL ETA,PHI,LYR
C-
C-   Inputs  : ETA,PHI,LYR
C-             HBINS
C-             HLO,HHI
C-             WLO,WBIN
C-   Outputs : NBOOK
C-             IER = 0 OK
C-                 = -1 NO CHANNEL THERE
C-                 = -2 ALREADY BOOKED
C-   Controls: NONE
C-
C-   Created  26-JUN-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST/.TRUE./
      INTEGER ETA,PHI,LYR,HBIN,WBIN,IOFF1,IOFF2,NBOOK,IER
      REAL    HLO,HHI,WLO
      INTEGER*2 DONE(5700)
      INTEGER ADD,SEQ,BOOK_COUNT
      INTEGER IADC,IBLS,ITOW,IDEP
      CHARACTER*80 TITLE
      
C----------------------------------------------------------------------
      IF (FIRST) THEN
         CALL VZERO(DONE(1),2850)
         BOOK_COUNT = 0
         FIRST = .FALSE.
C
C ****  READ IN TB90 CHANNEL TABLES
C
         CALL  TB90L1_INTAB(IER)      
         IF (IER.NE.0) THEN
           CALL STAMSG('NO TB90L1_SORT_ORD TABLE',.TRUE.)
           GOTO 999
         END IF
      END IF
C
C ****  PHYSICS ADDRESS CHANNEL WATCH
C
      IER = 0
      CALL TB90_PHYADC(ETA,PHI,LYR,ADD,SEQ)
      IF (SEQ.LE.0) GOTO 60
      IF (DONE(SEQ).NE.0) GOTO 70
      DONE(SEQ) = 1
      BOOK_COUNT = BOOK_COUNT + 1
      NBOOK = BOOK_COUNT
      IDEP =  iand( ISHFT(ADD,-2), 15)
      ITOW =  iand( ISHFT(ADD,-6), 3)
      IBLS   =  iand( ISHFT(ADD,-8), 7)
      IADC   =  iand( ISHFT(ADD,-11), 31)
      WRITE(TITLE,1001)ETA,PHI,LYR,IADC,IBLS,ITOW,IDEP
      CALL HBOOK1(IOFF1+NBOOK,TITLE,HBIN,HLO,HHI,0.0)
      TITLE(1:5) = 'WATCH'
      CALL HBOOK1(IOFF2+NBOOK,TITLE,WBIN,WLO,WLO+WBIN,0.0)
  999 RETURN
   60 CONTINUE
      IER = -1
      RETURN
   70 CONTINUE
      IER = -2
      RETURN
 1001 FORMAT(' HIST ET ',I2,' PH ',I2,' LY ',I2,
     & ' ADC ',I2,' BLS ',I2,' TOW ',I2,' DEP ',I2,'$')
      END
