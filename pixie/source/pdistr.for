      SUBROUTINE PDISTR 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the ISAJET tracks contained in the bank
C-                         DITR.
C-
C-
C-   Inputs  :  RAD : Radius at which the track must be drawn, beginning
C-                    at its vertex 
C-   Outputs : 
C-
C-   Created:  14-JUN-1988  Ghita Rahal-Callot
C-   Updated:  17-AUG-1991  Tom Trippe   Get LDITR using GZDITR, not link area  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
C
      INTEGER LSDITR, NTRAC, I, IREJ, GZDITR
      REAL RAD, VIN(6), VOUT(6), PHI, TET
      LOGICAL IPASS
      DATA IPASS /.TRUE. /
C----------------------------------------------------------------------
      IF ( IPASS ) THEN
        IPASS = .FALSE.
        RAD = C ( LDGEH + 9 )
      ENDIF
      LDITR = GZDITR()
      IF ( LDITR .LE. 0 ) GO TO 999
      NTRAC = IQ ( LDITR + 1 )
      CALL PUOPEN
      CALL PXCOLR('RED')
      DO 100 I = 1, NTRAC 
        LSDITR = LDITR + (I-1) * IQ ( LDITR+2 ) + 2
        VIN(1) = Q ( LSDITR+1)
        VIN(2) = Q ( LSDITR+2)
        VIN(3) = Q ( LSDITR+3)
        PHI = Q ( LSDITR + 4 )
        TET = Q ( LSDITR + 5 )
        VIN(4) = COS ( PHI ) * SIN ( TET )
        VIN(5) = SIN ( PHI ) * SIN ( TET )
        VIN(6) = COS ( TET )
        CALL EXTCYL ( VIN, VOUT, RAD, IREJ )
        IF ( IREJ .NE. 0 ) GO TO 100
        CALL J3MOVE ( VIN(1), VIN(2), VIN(3) )
        CALL J3DRAW ( VOUT(1), VOUT(2), VOUT(3) )
  100 CONTINUE
      CALL JRCLOS
  999 RETURN
      END
