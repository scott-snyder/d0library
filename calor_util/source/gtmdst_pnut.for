      SUBROUTINE GTMDST_PNUT(NUM,ENUT,ET,TH,ETA,PHI,SIG,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : gets the NUM-th neutrino information from the
C-                         MDST bank.
C-
C-   Inputs  : NUM = # of the PNUT bank which is desired
C-   Outputs :
C-             ENUT(I) = Ith 4vector component of the missing Pt
C-             ET = Et of the "missing neutrino"
C-             TH = theta of the "missing neutrino"
C-             ETA = eta  of the "missing neutrino"
C-             PHI = phi  of the "missing neutrino"
C-             SIG(3) = pxsig,pysig,pzsig
C-   Controls:
C-
C-   Created  25-APR-1991   Geoff Forden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'

      REAL    ENUT(4),ET,TH,ETA,PHI,SIG(3)
      INTEGER NUM,LZMDST,GZMDST,IOFF,NREP,NUM_NUT,IER,NUM_PNUT
C----------------------------------------------------------------------
      IER = 0
      ENUT(1)=0.
      ENUT(2)=0.
      ENUT(3)=0.
      ENUT(4)=0.
      TH=0.
      ETA=0.
      PHI=0.
      SIG(1)=0.
      SIG(2)=0.
      SIG(3)=0.

      LZMDST=GZMDST()
      IF( LZMDST.LE.0 ) THEN
        IER = -4
        GOTO 999
      ENDIF

      NUM_PNUT=IQ(LZMDST+9)
      IF( NUM.GT.NUM_PNUT ) THEN
        IER = -5
        GOTO 999
      ENDIF

      NREP=IQ(LZMDST+8)
      IOFF=IQ(LZMDST+10)+(NUM-1)*NREP-1

      ENUT(1)=Q(LZMDST+IOFF+1)
      ENUT(2)=Q(LZMDST+IOFF+2)
      ENUT(3)=Q(LZMDST+IOFF+3)
      ENUT(4)=Q(LZMDST+IOFF+4)

      SIG(1)=Q(LZMDST+IOFF+5)
      SIG(2)=Q(LZMDST+IOFF+6)
      SIG(3)=Q(LZMDST+IOFF+7)

      ET=Q(LZMDST+IOFF+8)
      TH=Q(LZMDST+IOFF+9)
      ETA=Q(LZMDST+IOFF+10)
      PHI=Q(LZMDST+IOFF+11)

  999 RETURN

      ENTRY GTMDST_PNUT_TOTAL(NUM_NUT,IER)
      LZMDST=GZMDST()
      IF( LZMDST.LE.0 ) THEN
        IER = -4
        NUM_NUT = 0
      ELSE
        NUM_NUT=IQ(LZMDST+9)
        IER=0
      ENDIF
      RETURN

      END
