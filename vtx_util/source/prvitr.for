      SUBROUTINE PRVITR( LUNPR, LVITR, NVITR, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the bank VITR
C-
C-   Inputs  : LUNPR  [I] : Unit for output
C-             LVITR  [I] : Pointer on the VITR bank. If 0, uses GZVITR to get.
C-             NVITR  [I] : Number of the VITR bank ( dummy )
C-             CFL   [C*] : Character flag.   ( dummy )
C-             IFL    [I] : Level of printing ( dummy )
C-   Outputs :
C-
C-   Created  30-JUN-1988   Ghita Rahal-Callot
C-   Updated  22-AUG-1991   Tom Trippe  add GZVITR call
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LSVITR, NTRAC, I, J, IPT, GZVITR
      INTEGER LUNPR, LVITR, NVITR, IFL
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      IPT = LVITR
      IF ( IPT .LE. 0 ) IPT = GZVITR()
      IF ( IPT .LE. 0 ) GO TO 999
      NTRAC = IQ ( IPT + 1 )
      WRITE ( LUNPR, 12) NTRAC
   12 FORMAT(10X,'**** # of ISAJET tracks inside the VTX:',I4)
      IF ( NTRAC .LE. 0 ) GO TO 999
      WRITE ( LUNPR,13)
   13 FORMAT(/,5X,'X ver',5X,'Y ver',5X,'Z ver',5X,' Phi ',
     &         5X,' Teta',2X,'Momentum',5X,'Masse',5X,'trk #',
     &         1X,'asso track')
      LSVITR = IPT + 2
      DO 100 I = 1, NTRAC
        WRITE(LUNPR,14) (Q(LSVITR+J),J=1,IQ(IPT+2))
   14   FORMAT(7F10.5,2F10.0)
        LSVITR = LSVITR + IQ (IPT+2)
  100 CONTINUE
  999 RETURN
      END
