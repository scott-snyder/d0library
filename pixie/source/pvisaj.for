      SUBROUTINE PVISAJ 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the ISAJET tracks contained in the bank
C-                         VITR.
C-
C-
C-   Inputs  :  RAD : Radius at which the track must be drawn, beginning
C-                    at its vertex 
C-   Outputs : 
C-
C-   Created  14-JUN-1988   Ghita Rahal-Callot
C-   Updated  24-OCT-1988   Ghita Rahal-Callot  : adapted to VTX 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER GZVITR
      INTEGER LSVITR, NTRAC, I, IREJ, LVITR
      REAL RAD, VIN(6), VOUT(6), PHI, TET
      LOGICAL IPASS
      DATA IPASS /.TRUE. /
C----------------------------------------------------------------------
      IF ( IPASS ) THEN
        IPASS = .FALSE.
        RAD = C ( LVGEH + 9 )
      ENDIF
      LVITR = GZVITR()
      IF ( LVITR .LE. 0 ) GO TO 999
      NTRAC = IQ ( LVITR + 1 )
      CALL PUOPEN
      DO 100 I = 1, NTRAC 
        LSVITR = LVITR + (I-1) * IQ ( LVITR+2 ) + 2
        VIN(1) = Q ( LSVITR+1)
        VIN(2) = Q ( LSVITR+2)
        VIN(3) = Q ( LSVITR+3)
        PHI = Q ( LSVITR + 4 )
        TET = Q ( LSVITR + 5 )
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
