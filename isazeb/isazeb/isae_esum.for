      SUBROUTINE ISAE_ESUM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       fill ESUM for ISAJET
C-
C-   Created  24-JAN-1992   Serban D. Protopopescu
C-   Modified 22-FEB-1992   A. Boehnlein, M.V.S. Rao, fixed intialization
C-                          for etmiss.
C-   Updated  27-APR-1992   Serban Protopopescu  check ESUM bank existance 
C-   Updated  24-AUG-1992   Serban Protopopescu  list only primary vertex 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      REAL    ET(40),ETA(40),PHI(40),ETA_DET(40),NUS_SUM(4),ZVTX,THETA
      INTEGER IFLAG(40)
      INTEGER LPJET,LISAL,GZPJET,GZISAL,LISV1,LISP1,GZISV1,GZISAE
      INTEGER NOBJ(0:LAST_TYPE),ID_OBJ(40),NTOT,IER,NFIX,NR
      INTEGER LESUM,LDSUM,I,K,ID,GZESUM
      LOGICAL OK
C----------------------------------------------------------------------
C
      IF(GZISAE().LE.0) GOTO 999        ! no ISAJET data
      IF(GZESUM('ISAE').GT.0) GOTO 999  ! ESUM bank for ISAE exists
      NTOT=0
C
C     find all vertices
C
      NOBJ(ID_VERTEX)=0
      LISV1=GZISV1()
C
      IF(LISV1.NE.0) THEN
C
C         use only first vertex banks 
        NTOT=NTOT+1
        NOBJ(ID_VERTEX)=NOBJ(ID_VERTEX)+1
        ID_OBJ(NTOT)=ID_VERTEX
        IFLAG(NTOT)=IQ(LISV1+1)
        ET(NTOT)=SQRT(Q(LISV1+2)**2+Q(LISV1+3)**2)
        ETA_DET(NTOT)=Q(LISV1+7)
        ETA(NTOT)=Q(LISV1+8)
        PHI(NTOT)=Q(LISV1+9)
        ZVTX=PHI(NTOT)
        ID=IABS(IQ(LISV1+1))
C               add tau's
        DO WHILE (LISV1.GT.0)  
          IF(ID.EQ.16) THEN
            NTOT=NTOT+1
            NOBJ(ID_TAU)=NOBJ(ID_TAU)+1
            ID_OBJ(NTOT)=ID_TAU
            IFLAG(NTOT)=IQ(LISV1+1)
            ET(NTOT)=SQRT(Q(LISV1+2)**2+Q(LISV1+3)**2)
            CALL ISPETA(Q(LISV1+2),THETA,PHI(NTOT),ETA(NTOT))
            CALL DET_ETA(ZVTX,THETA,ETA_DET(NTOT))
          ENDIF
          LISV1=LQ(LISV1)
        ENDDO
C
      ENDIF
C
C       jets
C
      NOBJ(ID_JET)=0
      LPJET=GZPJET()
C
      IF(LPJET.NE.0) THEN
C          sort banks so they are in increasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LPJET must be refetched
        CALL ZSORT(IXCOM,LPJET,6)
        LPJET=GZPJET()
        CALL ZTOPSY(IXCOM,LPJET)
        LPJET=GZPJET()
C
C         loop through PJET banks
        DO WHILE (LPJET.GT.0)
          NTOT=NTOT+1
          NOBJ(ID_JET)=NOBJ(ID_JET)+1
          ID_OBJ(NTOT)=ID_JET
          IFLAG(NTOT)=0
          ET(NTOT)=Q(LPJET+2)
          THETA=Q(LPJET+9)
          CALL DET_ETA(ZVTX,THETA,ETA_DET(NTOT))
          ETA(NTOT)=Q(LPJET+10)
          PHI(NTOT)=Q(LPJET+8)
          LPJET=LQ(LPJET)          ! pointer to next jet
        ENDDO
C
      ENDIF
C
C       electrons
C
      NOBJ(ID_ELECTRON)=0
      LISAL=GZISAL()
C
      IF(LISAL.NE.0) THEN
C          sort banks so they are in increasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LISAL must be refetched
        CALL ZSORT(IXCOM,LISAL,6)
        LISAL=GZISAL()
        CALL ZTOPSY(IXCOM,LISAL)
        LISAL=GZISAL()
C
C         loop through ISAL banks
        DO WHILE (LISAL.GT.0)
          ID=IABS(IQ(LISAL+1))
          IF(ID.EQ.12) THEN
            NTOT=NTOT+1
            NOBJ(ID_ELECTRON)=NOBJ(ID_ELECTRON)+1
            ID_OBJ(NTOT)=ID_ELECTRON
            IFLAG(NTOT)=IQ(LISAL+1)
            ET(NTOT)=SQRT(Q(LISAL+2)**2+Q(LISAL+3)**2)
            THETA=Q(LISAL+8)
            CALL DET_ETA(ZVTX,THETA,ETA_DET(NTOT))
            ETA(NTOT)=Q(LISAL+9)
            PHI(NTOT)=Q(LISAL+7)
          ENDIF
          LISAL=LQ(LISAL)          ! pointer to next lepton
        ENDDO
C
      ENDIF
C
C       muons
C
      NOBJ(ID_MUON)=0
      LISAL=GZISAL()
C
      IF(LISAL.NE.0) THEN
C
C         loop through ISAL banks
        DO WHILE (LISAL.GT.0)
          ID=IABS(IQ(LISAL+1))
          IF(ID.EQ.14) THEN
            NTOT=NTOT+1
            NOBJ(ID_MUON)=NOBJ(ID_MUON)+1
            ID_OBJ(NTOT)=ID_MUON
            IFLAG(NTOT)=IQ(LISAL+1)
            ET(NTOT)=SQRT(Q(LISAL+2)**2+Q(LISAL+3)**2)
            THETA=Q(LISAL+8)
            CALL DET_ETA(ZVTX,THETA,ETA_DET(NTOT))
            ETA(NTOT)=Q(LISAL+9)
            PHI(NTOT)=Q(LISAL+7)
          ENDIF
          LISAL=LQ(LISAL)          ! pointer to next lepton
        ENDDO
C
      ENDIF
      NOBJ(ID_ETMISS) = 0    
      CALL ISA_NUS_SUM(NUS_SUM,OK)
      IF(OK) THEN
        NTOT=NTOT+1
        NOBJ(ID_ETMISS)=NOBJ(ID_ETMISS)+1
        ID_OBJ(NTOT)=ID_ETMISS
        IFLAG(NTOT)=0
        ET(NTOT)=SQRT(NUS_SUM(1)**2+NUS_SUM(2)**2)
        CALL ISPETA(NUS_SUM,THETA,PHI(NTOT),ETA(NTOT))
        CALL DET_ETA(ZVTX,THETA,ETA_DET(NTOT))
      ENDIF
C
C       book and fill ESUM
C
      CALL BKESUM('ISAE',NTOT,LESUM)
      IQ(LESUM+4)=NTOT
      NFIX=IQ(LESUM+2)
      NR=IQ(LESUM+3)
      LDSUM=LESUM+NFIX
      DO I=0,LAST_TYPE
        IQ(LESUM+5+I)=NOBJ(I)
      ENDDO
      DO K=1,NTOT
        IQ(LDSUM+JESUM_ID)=ID_OBJ(K)
        IQ(LDSUM+JESUM_FLAG)=IFLAG(K)
        Q(LDSUM+JESUM_PT)=ET(K)
        Q(LDSUM+JESUM_ETA_DET)=ETA_DET(K)
        Q(LDSUM+JESUM_ETA)=ETA(K)
        Q(LDSUM+JESUM_PHI)=PHI(K)
        LDSUM=LDSUM+NR
      ENDDO
  999 RETURN
      END
