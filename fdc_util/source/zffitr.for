      SUBROUTINE ZFFITR ( NISA , VECT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill FITR, bank containing the ISAJET tracks
C-                         found in the FDC
C-
C-   Inputs  : NISA         :   Track #
C-             VECT         :   Data to put in the bank
C-   Outputs :    Fill FITR for this track
C-
C-   Created  10-JUN-1988   Ghita Rahal-Callot
C-   Updated  30-JAN-1989   Jeffrey Bantly  for use with FDC tracks 
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up 
C-   Updated   2-DEC-1991   Robert E. Avery  Increase number of tracks 
C-                                      added per push.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFITR.LINK'
C
      INTEGER LKFITR, NISA, IOMAST, NPOINT
      INTEGER GZFITR,NEXCESS,NTRKS
C
      REAL VECT(9)
      SAVE NTRKS
      DATA NTRKS /500/
C----------------------------------------------------------------------
C
C  Create the bank if no FITR bank present.
C
      LKFITR=GZFITR()
      IF( LKFITR .LE. 0 ) CALL BKFITR(LKFITR)
C
C  If needed, push the bank.
C
      LKFITR=GZFITR()
      NEXCESS = IQ(LKFITR-1) - IQ(LKFITR+1)*IQ(LKFITR+2) - 2
      IF ( NEXCESS .LT. IQ(LKFITR+2) ) THEN
        CALL MZPUSH ( IXCOM, LKFITR, 0, NTRKS*IQ(LKFITR+2),' ')
      ENDIF
      NPOINT = LKFITR + 2 + IQ(LKFITR+1)*IQ(LKFITR+2)
      IQ ( LKFITR + 1 ) = IQ ( LKFITR + 1 ) + 1
      CALL UCOPY ( VECT, Q(NPOINT+1), IQ( LKFITR+2) )
C-----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
