      SUBROUTINE GTMDST_PELC(IELC,E,ET,SIG,THETA,ETA,PHI,CONE_NRG,DIST,
     &  NUM_TRCKS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   Returns the PELC information from the MDST
C-                           bank.
C-
C-   Inputs  :
C-              IELC = which electron candidate you want
C-   Outputs :
C-              E(4) = Px, Py, Pz, Etot
C-              ET
C-              SIG(3) = PXSIG**2, PYSIG**2,PTSIG
C-              THETA = theta of electron
C-              ETA = eta of electron
C-              PHI = phi of electron
C-              CONE_NRG(5) = 1 -- EM energy outside central tower
C-                            2 -- total energy in core cone
C-                            3 -- total energy in isolation cone
C-                            4 -- EM energy in core cone
C-                            5 -- EM energy in isolation cone
C-              DIST = distance of closest approach of central track
C-              NUM_TRCKS = number of central tracks in cluster road
C-   Controls:
C-
C-   Created  25-APR-1991   Geoff Forden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    E(4),SIG(4),THETA,ETA,PHI,CONE_NRG(5),DIST,ET
      INTEGER NUM_TRCKS,GZMDST,LZMDST,NREP,NUM_ELC,IOFF
      INTEGER NUM_PELC,IER,IELC,IERR
C----------------------------------------------------------------------
      IER = 0
      E(1)=0.
      E(2)=0.
      E(3)=0.
      E(4)=0.
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

      NUM_ELC=IQ(LZMDST+12)
      IF( IELC.GT.NUM_ELC) THEN
        IER = -5
        GOTO 999
      ENDIF

      NREP=IQ(LZMDST+11)
      IOFF=IQ(LZMDST+13)+(IELC-1)*NREP-1

      E(1)=Q(LZMDST+IOFF+1)
      E(2)=Q(LZMDST+IOFF+2)
      E(3)=Q(LZMDST+IOFF+3)
      E(4)=Q(LZMDST+IOFF+4)
      ET  =Q(LZMDST+IOFF+5)
      SIG(1)=Q(LZMDST+IOFF+6)
      SIG(2)=Q(LZMDST+IOFF+7)
      SIG(3)=Q(LZMDST+IOFF+8)

      THETA=Q(LZMDST+IOFF+9)
      ETA=Q(LZMDST+IOFF+10)
      PHI=Q(LZMDST+IOFF+11)

      CONE_NRG(1)=Q(LZMDST+IOFF+12)
      CONE_NRG(2)=Q(LZMDST+IOFF+13)
      CONE_NRG(3)=Q(LZMDST+IOFF+14)
      CONE_NRG(4)=Q(LZMDST+IOFF+15)
      CONE_NRG(5)=Q(LZMDST+IOFF+16)

      NUM_TRCKS=IFIX(Q(LZMDST+IOFF+17))

      DIST=Q(LZMDST+IOFF+18)

  999 RETURN

      ENTRY GTMDST_PELC_TOTAL(NUM_PELC,IER)
      LZMDST=GZMDST()
      IF( LZMDST.LE.0 ) THEN
        NUM_PELC = 0
        IER = -4
      ELSE
        NUM_PELC=IQ(LZMDST+12)
        IER=0
      ENDIF
      RETURN

      END
