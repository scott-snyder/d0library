      SUBROUTINE GTMDST_PTAU(ITAU,E,ET,THETA,ETA,PHI,RMS_WID,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Get the information on the ITAUth tau particle
C-                          found from the MDST bank.
C-
C-   Inputs  : ITAU = number of tau selected
C-   Outputs :
C-             E(I) = Ith component of the Tau's 4-vector ( E(1)=Px,...,E(4)
C-                      =energy )
C-             ET
C-             THETA
C-             ETA =
C-             PHI =
C-             RMS_WIDTH = width of the tau jets calorimeter hits
C-   Controls:
C-
C-   Created  25-APR-1991   Geoff Forden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'

      REAL    E(4),THETA,ETA,PHI,RMS_WID,ET
      INTEGER LZMDST,GZMDST,NUM_TAU,NREP,IOFF,ITAU,IER,NUM_PTAU
C----------------------------------------------------------------------
      IER = 0
      E(1)=0.
      E(2)=0.
      E(3)=0.
      E(4)=0.
      ET = 0.
      THETA=0.
      ETA=0.
      PHI=0.
      RMS_WID = 0.

      LZMDST=GZMDST()
      IF( LZMDST.LE.0 ) THEN
        IER = -4
        GOTO 999
      ENDIF

      NUM_TAU=IQ(LZMDST+15)
      IF( ITAU.GT.NUM_TAU) THEN
        IER = -5
        GOTO 999
      ENDIF

      NREP=IQ(LZMDST+14)
      IOFF=IQ(LZMDST+16)+(ITAU-1)*NREP-1

      E(1)=Q(LZMDST+IOFF+1)
      E(2)=Q(LZMDST+IOFF+2)
      E(3)=Q(LZMDST+IOFF+3)
      E(4)=Q(LZMDST+IOFF+4)
      ET = Q(LZMDST+IOFF+5)

      THETA=Q(LZMDST+IOFF+6)
      ETA=Q(LZMDST+IOFF+7)
      PHI=Q(LZMDST+IOFF+8)
      RMS_WID=Q(LZMDST+IOFF+9)
  999 RETURN

      ENTRY GTMDST_PTAU_TOTAL(NUM_PTAU,IER)
      LZMDST=GZMDST()
      IF( LZMDST.LE.0 ) THEN
        NUM_PTAU=0
        IER=-4
      ELSE
        NUM_PTAU=IQ(LZMDST+15)
        IER=0
      ENDIF
      RETURN

      END
