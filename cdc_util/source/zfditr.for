      SUBROUTINE ZFDITR ( NISA , VECT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill DITR, bank containing the ISAJET tracks
C-   found in the CDC
C-
C-   Inputs  : NISA         :   Track #
C-             VECT         :   Data to put in the bank
C-   Outputs :    Fill DITR for this track
C-
C-   Created  10-JUN-1988   Ghita Rahal-Callot
C-   Updated  12-JUL-1989   Qizhong Li-Demarteau  remove CDCTRL.INC 
C-                                   and add version # for bank DITR
C-   Updated  12-AUG-1991   Tom Trippe  use GZDRTH to avoid DTRH rebook
C-                                   and impose D0 standards
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INCLUDE 'D0$LINKS:IZDITR.LINK'
C
      INTEGER NISA, IOMAST, NDAMAX, NPOINT, ISETVN, GZDTRH
      REAL VECT(*)
      LOGICAL IPASS
      DATA NDAMAX /9/
      DATA IPASS / .TRUE. /
C----------------------------------------------------------------------
      IF ( NISA .EQ. 1 ) THEN
C
C ****  Create the bank if it is the first track stored; but first add one
C ****  link to LDTRH to hang DITR
C
        IF ( IPASS ) THEN
          IPASS = .FALSE.
          CALL MZFORM ( 'DITR', '2I -F', IOMAST)
        ENDIF
        LDTRH = GZDTRH()
        IF (LDTRH .LE. 0) CALL BKDTRH
        CALL MZBOOK( IXMAIN, LDITR, LDTRH, -IZDITR,
     &               'DITR', 0, 0, 20*NDAMAX + 2, IOMAST, 0)
        IQ(LDITR) = ISETVN(IQ(LDITR),0)
        IQ ( LDITR + 2 ) = NDAMAX
      ENDIF
C
C ****  If needed, push the bank
C
      IF ( IQ(LDITR-1) - IQ(LDITR+1)*IQ(LDITR+2) -2 .LT.
     &         IQ(LDITR+2) ) THEN
        CALL MZPUSH ( IXCOM,  LDITR, 0, 10*IQ(LDITR+2),' ')
      ENDIF
      NPOINT = LDITR + 2 + IQ(LDITR+1)*IQ(LDITR+2)
      IQ ( LDITR + 1 ) = IQ ( LDITR + 1 ) + 1
      CALL UCOPY ( VECT, Q(NPOINT+1), IQ( LDITR+2) )
  999 RETURN
      END
