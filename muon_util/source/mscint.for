      SUBROUTINE MSCINT(ITRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Muon scintillator track matching code
C-
C-   Inputs  : MSCT, MUOT, MUOH, MHTT bank
C-             ITRAK  Track number in MUOT
C-   Outputs : MSCT bank
C-   Controls: none
C-
C-   Created   8-FEB-1994   Atsushi Taketani
C-   Modified  6/94   MF    Match to a single track
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
      INTEGER  NTRAKS, ITRAK, IPTR(4), NPTR, ACTIVE, I
      REAL     XYZ(3,4)
      INTEGER  DUM1,DUM2,DUM3,DUM4,DUM5,DUM6
      INTEGER  LMUOT, GZMUOT, IFW2, IAL, K, LMSHT
C
C----------------------------------------------------------------------
C
        LMUOT = GZMUOT(ITRAK)
        IF (LMUOT.EQ.0 ) GOTO 100
        IFW2 = IQ(LMUOT+5)
        CALL MNCHIT( ITRAK, IPTR, XYZ, NPTR, ACTIVE )
          IFW2 = IBCLR( IFW2, 18)
        IF ( NPTR.EQ.0 ) THEN
          CALL MNMFUL( ITRAK, IPTR, XYZ, NPTR, ACTIVE )
          IFW2 = IBSET( IFW2, 18)
        END IF
        IF ( NPTR.NE.0 ) THEN
          LMUOT = GZMUOT(ITRAK)
          CALL BKMSHT( LMUOT, DUM1, LMSHT )
          CALL MSHTFL( ITRAK, NPTR, IPTR ) 
          DO I=1,NPTR
            CALL MSCTFL(3,DUM1,IPTR(I),DUM3,ITRAK,                      
     &                  DUM4,DUM5,XYZ(1,I),DUM6)
          END DO
          IF ( NPTR.EQ.1 ) THEN 
            IFW2 = IBCLR( IFW2, 19)
          ELSE
            IFW2 = IBSET( IFW2, 19)
          END IF
        END IF
        IF ( ACTIVE.EQ.0 ) THEN
          IFW2 = IBCLR( IFW2, 16)
        ELSE
          IFW2 = IBSET( IFW2, 16)
        END IF
        IF ( NPTR.EQ.0 ) THEN
          IFW2 = IBCLR( IFW2, 17)
        ELSE
          IFW2 = IBSET( IFW2, 17)
        END IF
        LMUOT = GZMUOT(ITRAK)
        IQ(LMUOT+5) = IFW2
  100 CONTINUE
C
  999 RETURN
      END
