      SUBROUTINE GTMDST_JETS(IJETS,IVERS,E,THT,PHI,ETA,PHI_WID,
     &  ETA_WID,EMFRAC,ISHARE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get jet data from MDST bank
C-
C-   Inputs  :
C-            IJETS = which jet to get
C-   Outputs :
C-            IVERS = Bank version number
C-            E(7) = EX, EY, EZ, ET, sig**2(Ex), sig**2(Ey)
C-            THT = theta of jet
C-            PHI = phi of jet
C-            ETA = eta of jet
C-            PHI_WID  [R]     Phi width
C-            ETA_WID  [R]     Eta width
C-            EMFRAC   [R]     Fraction of total Et in EM
C-            ISHARE   [I]     Energy shared? ( 0 = No )
C-            IER = error code, 0 = ok, -4 = no JETS info
C-  Entry Point
C-      GTMDST_JETS_TOTAL (NUM_JETS,IER)
C-
C-   to get the total number of JETS banks.
C-
C-   Created  25-APR-1991   Geoff Forden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LKCAPH.INC'

      REAL    E(7),THT,PHI,ETA

      INTEGER IJETS,IVERS,GZMDST,LZMDST,NUM_JETS,NREP,IOFF
      INTEGER NUM_PJET,IER,LCAPH

      REAL    ALGORITHM,ETA_WID,PHI_WID,EMFRAC
      INTEGER ISHARE
C----------------------------------------------------------------------
      IER = 0
      E(1)=0.
      E(2)=0.
      E(3)=0.
      E(4)=0.
      E(5)=0.
      E(6)=0.
      E(7)=0.
      THT=0.
      PHI=0.
      ETA=0.
C
      LZMDST = GZMDST()
      IF (LZMDST.LE.0) THEN
        IER = -4
        GOTO 999
      ENDIF
C
      LCAPH = JCAPH
      IF( LCAPH.LE.0 ) THEN
        IER = -4
        GOTO 999
      ENDIF
      NUM_JETS = IQ(LCAPH+3)
      NREP = IQ(LCAPH+1)
      IF( IJETS.GT.NUM_JETS ) THEN
        IER = -5
        GOTO 999
      ENDIF

      IOFF=IQ(LCAPH+2)+(IJETS-1)*NREP-1
      E(1)=Q(LZMDST+IOFF+1)
      E(2)=Q(LZMDST+IOFF+2)
      E(3)=Q(LZMDST+IOFF+3)
      E(4)=Q(LZMDST+IOFF+4)
      E(5)=Q(LZMDST+IOFF+5)
      E(6)=Q(LZMDST+IOFF+6)
      E(7)=Q(LZMDST+IOFF+7)
      THT=Q(LZMDST+IOFF+10)
      ETA=Q(LZMDST+IOFF+11)
      PHI=Q(LZMDST+IOFF+12)

      ALGORITHM=Q(LZMDST+IOFF+13)

      ETA_WID=Q(LZMDST+IOFF+14)
      PHI_WID=Q(LZMDST+IOFF+15)
      EMFRAC=Q(LZMDST+IOFF+16)
      ISHARE=IQ(LZMDST+IOFF+17)
      IVERS = IQ(LZMDST+IOFF+18)

  999 RETURN

      ENTRY GTMDST_JETS_TOTAL(NUM_PJET,IER)

      LCAPH = JCAPH
      IF( LCAPH.LE.0 ) THEN
        NUM_PJET=0
        IER=-4
      ELSE
        NUM_PJET=IQ(LCAPH+3)
        IER=0
      ENDIF
      RETURN

      END
