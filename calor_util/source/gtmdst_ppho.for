      SUBROUTINE GTMDST_PPHO(IPHO,E,ET,SIG,THETA,ETA,PHI,CONE_NRG,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   Returns the PPHO information from the MDST
C-                           bank.
C-
C-   Inputs  :
C-              IPHO = which photon candidate you want
C-   Outputs :
C-              E(4) = Px, Py, Pz, Etot
C-              ET
C-              SIG(3) = PXSIG**2, PYSIG**2,PTSIG
C-              THETA = theta of photon
C-              ETA = eta of photon
C-              PHI = phi of photon
C-              CONE_NRG(5) = 1 -- EM energy outside central tower
C-                            2 -- total energy in core cone
C-                            3 -- total energy in isolation cone
C-                            4 -- EM energy in core cone
C-                            5 -- EM energy in isolation cone
C-   Controls:
C-
C-   Created  25-APR-1991   Geoff Forden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'

      REAL    E(4),SIG(4),THETA,ETA,PHI,CONE_NRG(5),DIST,ET
      INTEGER NUM_TRCKS,GZMDST,LZMDST,NREP,NUM_PHO,IOFF
      INTEGER NUM_PPHO,IER,IPHO,IERR,LPARH,GZPARH,LPPHO
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
      SIG(1)=0.
      SIG(2)=0.
      SIG(3)=0.
      SIG(4)=0.

      LZMDST=GZMDST()
      IF( LZMDST.LE.0 ) THEN
        IER = -4
        GOTO 999
      ENDIF

      NUM_PHO=IQ(LZMDST+18)
      IF( IPHO.GT.NUM_PHO) THEN
        IER = -5
        GOTO 999
      ENDIF

      NREP=IQ(LZMDST+17)
      IOFF=IQ(LZMDST+19)+(IPHO-1)*NREP-1

      E(1)=Q(LZMDST+IOFF+1)
      E(2)=Q(LZMDST+IOFF+2)
      E(3)=Q(LZMDST+IOFF+3)
      E(4)=Q(LZMDST+IOFF+4)
      ET  =Q(LZMDST+IOFF+5)

      SIG(1)=Q(LZMDST+IOFF+6)
      SIG(2)=Q(LZMDST+IOFF+7)
      SIG(3)=Q(LZMDST+IOFF+8)
      SIG(4)=Q(LZMDST+IOFF+9)

      THETA=Q(LZMDST+IOFF+10)
      ETA=Q(LZMDST+IOFF+11)
      PHI=Q(LZMDST+IOFF+12)

      CONE_NRG(1)=Q(LZMDST+IOFF+13)
      CONE_NRG(2)=Q(LZMDST+IOFF+14)
      CONE_NRG(3)=Q(LZMDST+IOFF+15)
      CONE_NRG(4)=Q(LZMDST+IOFF+16)
      CONE_NRG(5)=Q(LZMDST+IOFF+17)

  999 RETURN

      ENTRY GTMDST_PPHO_TOTAL(NUM_PPHO,IER)
      LZMDST=GZMDST()
      IF( LZMDST.LE.0 ) THEN
        NUM_PPHO=0
        IER=-4
      ELSE
        NUM_PPHO=IQ(LZMDST+18)
        IER=0
      ENDIF
      RETURN

      END
