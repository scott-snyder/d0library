      SUBROUTINE CDHITS ( VIN, VOUT, IVOUT, IADR )
C======================================================================
C
C   Purpose and Methods : Build the array VOUT for a given wire
C                         IHIT. This array will be loaded in the
C                         ZEBRA bank DSEC.
C
C   Inputs  :
C             IADR(1..3)   =  Layer, Sector, Cell
C-            VIN(1)   X-GLOBAL incoming track
C-            VIN(2)   Y-GLOBAL
C-            VIN(3)   Z-GLOBAL
C-            VIN(4)   X-GLOBAL outgoing track
C-            VIN(5)   Y-GLOBAL
C-            VIN(6)   Z-GLOBAL
C-            VIN(7)   PULSE HEIGHT ( Integrated charge)
C-            VIN(8)   Track length in the cell ( dx**2 + dy**2 + dz**2)
C-            VIN(9)   Track id.=2**11*Secondary track #+Primary track#
C   Outputs :
C             VOUT(1)   B   WIRE number
C             VOUT(2)   F   Position of the +phi solution in the cell frame (cm)
C             VOUT(3)   F   Position of the -phi solution in the cell frame (cm)
C             VOUT(4)   F   Z position ( cm )
C             VOUT(5)   F   Error on the position ( cm )
C             VOUT(6)   F   Error on Z position
C             VOUT(7)   F   Ionisation of the hit in M.I.P. units
C             VOUT(8)   F   Error on the previous value
C             VOUT(9)   B   STATUS word  ( not filled )
C             VOUT(10)  I   Track number
C
C-   Created  02-JAN-1986   T. Trippe
C             15-APR-1986   G. Rahal   adapted to CDC
C-   Updated  11-FEB-1988   Ghita Rahal-Callot : Modified the data to store
C-                          in DSEC and take geometry from Geometry banks.
C-   Updated  13-JUL-1988   Ghita Rahal-Callot : mod Z error = 9999. for the
C-                          intermediate SW
C-   Updated  17-SEP-1993   Qizhong Li-Demarteau changed errors:
C-                                 SW error from 0.007 to 0.016 
C-                                  Z error from 2.0 to 0.25
C
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$LINKS:IZDALH.LINK'
      INCLUDE 'D0$LINKS:IZDRFT.LINK'
C
      REAL    VIN (*), VOUT (*), XCOR, X, Y, STAGG
      INTEGER IADR (*), IVOUT (*), I, LDALL, LDALS, LDRFT, IPT
C======================================================================
C
      LDALH = LC ( LSCDC - IZDALH )
      IF ( LDALH .LE. 0 ) THEN
        WRITE ( LOUT, * ) '****** CDHITS : bank LDALH not defined'
        CALL EXIT(1)
      ENDIF
      IF ( LDGEH .LE. 0 ) THEN
        WRITE ( LOUT, * ) '****** CDHITS : bank LDGEH not defined'
        CALL EXIT(1)
      ENDIF
      LDALL = LC ( LDALH - IADR(1) - 1 )
      IF ( LDALL .LE. 0 ) THEN
        WRITE ( LOUT, * ) '****** CDHITS : bank LDALL not defined',
     &    ' for the layer', IADR(1)
        CALL EXIT(1)
      ENDIF
      LDALS = LC ( LDALL - IADR(2) - 1 )
      IF ( LDALS .LE. 0 ) THEN
        WRITE ( LOUT, * ) '****** CDHITS : bank LDALS not defined',
     &    ' for the sector ', IADR(2)
        CALL EXIT(1)
      ENDIF
      IVOUT (1)= IADR(3)
      X = (VIN(1) + VIN(4)) / 2.
      Y = (VIN(2) + VIN(5)) / 2.
      IPT = LDALS + 6 + IC ( LDALS + 6 ) * IADR(3)
      LDRFT = LC ( LDGEH - IZDRFT )
      IF ( LDRFT .LE. 0 ) THEN
        WRITE ( LOUT, * ) '**** CDHITS : Bank LDRFT not defined'
        CALL EXIT(1)
      ENDIF
      STAGG = C ( LDRFT + 26 + IADR(3) )
      XCOR     =  (X-C(IPT+1)) * C(LDALS+3)  + (Y-C(IPT+2)) * C(LDALS+4)
      VOUT (2) =   STAGG + ABS(XCOR)
      VOUT (3) =   STAGG - ABS(XCOR)
      VOUT (4) = (VIN(3) + VIN(6) ) / 2. - C(IPT+3)
      VOUT (5) = 0.016
C
C ****  Wires 1 to 5 should not have Z information
C
      IF (IADR(3) .EQ. 0 .OR. IADR(3) .EQ. 6  ) THEN
        VOUT (6) = 0.25
      ELSE
        VOUT (6) = 9999.
      ENDIF
      VOUT (7) = VIN (7)
      VOUT (8) = SQRT ( VOUT(7) )
      VOUT (9) = 0.
      IVOUT (10) = IFIX ( VIN(9))
      RETURN
      END

