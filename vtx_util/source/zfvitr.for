      SUBROUTINE ZFVITR ( NISA , VECT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill VITR, bank containing the ISAJET tracks
C-   found in the VTX
C-
C-   Inputs  : NISA         :   Track #
C-             VECT         :   Data to put in the bank
C-   Outputs :    Fill VITR for this track
C-
C-   Created  10-JUN-1988   Ghita Rahal-Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVITR.LINK'
C
      INTEGER GZVITR, GZVTRH
      INTEGER LVITR, LVTRH
      INTEGER NISA, IOMAST, NDAMAX, NPOINT
      REAL VECT(*)
      DATA NDAMAX /9/
      LOGICAL IPASS
      DATA IPASS / .TRUE. /
C----------------------------------------------------------------------
      IF ( NISA .EQ. 1 ) THEN
C
C ****  Create the bank if it is the first track stored;
C
        IF ( IPASS ) THEN
          IPASS = .FALSE.
          CALL MZFORM ( 'VITR', '2I -F', IOMAST)
        ENDIF
        LVTRH = GZVTRH()
        CALL MZBOOK(IXMAIN, LVITR, LVTRH, -IZVITR,
     &               'VITR', 0, 0, 20*NDAMAX + 2, IOMAST, 0)
        IQ ( LVITR + 2 ) = NDAMAX
      ENDIF
C
C ****  If needed, push the bank
C
      LVITR = GZVITR()
      IF ( IQ(LVITR-1) - IQ(LVITR+1)*IQ(LVITR+2) -2 .LT.
     &         IQ(LVITR+2) ) THEN
        CALL MZPUSH ( IXCOM,  LVITR, 0, 10*IQ(LVITR+2),' ')
      ENDIF
      LVITR = GZVITR()
      NPOINT = LVITR + 2 + IQ(LVITR+1)*IQ(LVITR+2)
      IQ ( LVITR + 1 ) = IQ ( LVITR + 1 ) + 1
      CALL UCOPY ( VECT, Q(NPOINT+1), IQ( LVITR+2) )
  999 RETURN
      END
