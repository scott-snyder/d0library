      SUBROUTINE PRDITR ( PRUNIT, KDITR, NDITR, CARFL, IPRFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the bank DITR
C-
C-   Inputs  : PRUNIT : unit number
C-              KDITR : bank address (not used) 
C-              NDITR : bank number  (not used)
C-              CARFL : Character flag (not used)
C-              IPRFL : Level of print ( not used )
C-   Outputs :
C-
C-   Created  30-JUN-1988   Ghita Rahal-Callot
C-   Updated  12-JUL-1989   Qizhong Li-Demarteau  change to D0 standard
C-   Updated   6-SEP-1991   Tom Trippe  added GZDITR call, D0 std IBITS
C-   Updated   4-MAR-1992   Qizhong Li-Demarteau  removed IBITS machine block 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
      INTEGER LSDITR, NTRAC, I, J, IPT, NVERS, GZDITR
      INTEGER PRUNIT, KDITR, NDITR, IPRFL
      CHARACTER*(*) CARFL
C----------------------------------------------------------------------
C
      IF(LDITR .LE. 0) LDITR = GZDITR()
      IF(LDITR .LE. 0) THEN
        WRITE(PRUNIT,1001) LDITR        
 1001   FORMAT(/' Wrong Address, LDITR =',I10)
        GO TO 999
      ENDIF
C
      IPT = LDITR
      NTRAC = IQ ( IPT + 1 )
      NVERS = IBITS(IQ(LDITR),13,5) 
      WRITE ( PRUNIT, 12) NVERS, NTRAC
   12 FORMAT('  Bank DITR: ISAJET track bank for CDC   (version',I2,')'
     &  /, 10X,'# of ISAJET tracks inside the CDC:',I4)
      IF ( NTRAC .LE. 0 ) GO TO 999
      WRITE ( PRUNIT,13)
   13 FORMAT(/,5X,'X ver',5X,'Y ver',5X,'Z ver',5X,' Phi ',
     &         5X,' Teta',2X,'Momentum',5X,'Masse',5X,'trk #',
     &         1X,'asso track')
      LSDITR = IPT + 2
      DO 100 I = 1, NTRAC
        WRITE(PRUNIT,14) (Q(LSDITR+J),J=1,IQ(IPT+2))
   14   FORMAT(7F10.5,2F10.0)
        LSDITR = LSDITR + IQ (IPT+2)
  100 CONTINUE
C
  999 RETURN
      END
