      SUBROUTINE TOP_DILEP_L2INFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   4-JAN-1995   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TOP_DILEP_ANALYSIS.INC'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      REAL     NL1MU,NL2MU,NL1JT,NL2JT
      INTEGER  NMAX
      PARAMETER (NMAX = 20)
      INTEGER  IORDER(NMAX), WORK(NMAX)
      REAL     L1MUON(nvar_l2muon,nmax),L1JET(nvar_l2jet,nmax)
      REAL     L2MUON(nvar_l2muon,nmax),L2JET(nvar_l2jet,nmax)
      REAL     L1MU(nvar_l2muon,nwant_l1muon)
      REAL     L1JT(nvar_l2jet,nwant_l1jet)
      REAL     L2MU(nvar_l2muon,nwant_l2muon)
      REAL     L2JT(nvar_l2jet,nwant_l2jet)
      REAL     L1MISSET(nvar_l1met),L2MISSET(nvar_l2met)
      REAL     L1MET(nvar_l1met),L2MET(nvar_l2met)
      INTEGER  minpmuo, minpjet
      REAL     mindr,dr
      CHARACTER*4  BTYPE
      INTEGER  J, IFLAG, IER, K, IWANT
      REAL     ET, ETA, DETETA, PHI
      REAL     ET1,PHI1,SCET1,ET2,PHI2,SCET2
C----------------------------------------------------------------------
C
C  GET L1 MUON INFO
C----------------------------------------
C
      BTYPE='TRGR'
      NL1MU=0
      CALL VZERO (IORDER, NMAX)
      CALL GTESUM_SORT( BTYPE, ID_MUON, NMAX, IORDER, WORK, IER)
      IF ( IER .EQ. 0 ) THEN
        J=1
        DO WHILE (ier.EQ.0)
          CALL GTESUM(BTYPE, ID_MUON, IORDER(J), ET, ETA, DETETA,
     &          PHI, IFLAG, IER)
          IF ( IER .EQ. 0 ) THEN
            IF (J.EQ.NMAX) IER=1
            L1MUON(1,J) = ET    ! et of muon number j
            L1MUON(2,J) = ETA   ! eta of muon number j
            L1MUON(3,J) = PHI   ! phi of muon number j
            NL1MU = NL1MU+1
            j=j+1
          ENDIF
        ENDDO
      ENDIF
C
C  GET L2 MUON INFO
C----------------------------------------
C
      BTYPE='FILT'
      NL2MU=0
      CALL VZERO (IORDER, NMAX)
      CALL GTESUM_SORT( BTYPE, ID_MUON, NMAX, IORDER, WORK, IER)
      IF ( IER .EQ. 0 ) THEN
        J=1
        DO WHILE (ier.EQ.0)
          CALL GTESUM(BTYPE, ID_MUON, IORDER(J), ET, ETA, DETETA,
     &          PHI, IFLAG, IER)
          IF ( IER .EQ. 0 ) THEN
            IF (J.EQ.NMAX) IER=1
            L2MUON(1,J) = ET    ! et of muon number j
            L2MUON(2,J) = ETA   ! eta of muon number j
            L2MUON(3,J) = PHI   ! phi of muon number j
            NL2MU = NL2MU+1
            j=j+1
          ENDIF
        ENDDO
      ENDIF
C
C ****  Get PMUO information
C
      CALL gtslink('ISOLMUON', nwant_muon, ntot_muon, muon_link)
      IF(ntot_muon.GT.nwant_muon) ntot_muon=nwant_muon
      DO k=1,nl1mu
        minpmuo=999.
        mindr=999.
        DO j=1,ntot_muon
          dr=sqrt((l1muon(2,k)-iq(muon_link(j)+16))**2.+
     &          (l1muon(3,k)-iq(muon_link(j)+17))**2.)
          IF(dr.LT.mindr) THEN
            minpmuo=j
            mindr=dr
          ENDIF
        ENDDO
        l1muon(4,k)=float(minpmuo)
        l1muon(5,k)=mindr
      ENDDO
      DO k=1,nl2mu
        minpmuo=999.
        mindr=999.
        DO j=1,ntot_muon
          dr=sqrt((l2muon(2,k)-iq(muon_link(j)+16))**2.+
     &          (l2muon(3,k)-iq(muon_link(j)+17))**2.)
          IF(dr.LT.mindr) THEN
            minpmuo=j
            mindr=dr
          ENDIF
        ENDDO
        l2muon(4,k)=float(minpmuo)
        l2muon(5,k)=mindr
      ENDDO
C
C ****  Get L1 jet info
C
      BTYPE='TRGR'
      NL1JT=0
      CALL VZERO (IORDER, NMAX)
      CALL GTESUM_SORT( BTYPE, ID_JET, NMAX, IORDER, WORK, IER)
      IF ( IER .EQ. 0 ) THEN
        J=1
        DO WHILE ( IER .EQ. 0 )
          CALL GTESUM(BTYPE, ID_JET, IORDER(J), ET, ETA, DETETA,
     &          PHI, IFLAG, IER)
          IF ( IER .EQ. 0 ) THEN
            IF(J.EQ.NMAX) IER=1
            L1JET(1,J) = ET    ! et of muon number j
            L1JET(2,J) = ETA   ! eta of muon number j
            L1JET(3,J) = PHI   ! phi of muon number j
            NL1JT = NL1JT+1
            J=J+1
          ENDIF
        ENDDO
      ENDIF
C
C ****  Get L2 jet info
C
      BTYPE='FILT'
      NL2JT=0
      CALL VZERO (IORDER, NMAX)
      CALL GTESUM_SORT( BTYPE, ID_JET, NMAX, IORDER, WORK, IER)
      IF ( IER .EQ. 0 ) THEN
        J=1
        DO WHILE ( IER .EQ. 0 )
          CALL GTESUM(BTYPE, ID_JET, IORDER(J), ET, ETA, DETETA,
     &          PHI, IFLAG, IER)
          IF ( IER .EQ. 0 ) THEN
            IF(J.EQ.NMAX) IER=1
            L2JET(1,J) = ET    ! et of muon number j
            L2JET(2,J) = ETA   ! eta of muon number j
            L2JET(3,J) = PHI   ! phi of muon number j
            NL2JT = NL2JT+1
            J=J+1
          ENDIF
        ENDDO
      ENDIF
C
C ****  Get PJET information
C
      CALL gtslink('TOP_JETS', nwant_jet, ntot_jet, jets_link)
      IF(ntot_jet.GT.nwant_jet) ntot_jet=nwant_jet
      DO k=1,nl1jt
        minpjet=999.
        mindr=999.
        DO j=1,ntot_jet
          dr=sqrt((l1jet(2,k)-iq(jets_link(j)+16))**2.+
     &          (l1jet(3,k)-iq(jets_link(j)+17))**2.)
          IF(dr.LT.mindr) THEN
            minpjet=j
            mindr=dr
          ENDIF
        ENDDO
        l1jet(4,k)=float(minpjet)
        l1jet(5,k)=mindr
      ENDDO
      DO k=1,nl2jt
        minpjet=999.
        mindr=999.
        DO j=1,ntot_jet
          dr=sqrt((l2jet(2,k)-iq(jets_link(j)+16))**2.+
     &          (l2jet(3,k)-iq(jets_link(j)+17))**2.)
          IF(dr.LT.mindr) THEN
            minpjet=j
            mindr=dr
          ENDIF
        ENDDO
        l2jet(4,k)=float(minpjet)
        l2jet(5,k)=mindr
      ENDDO
C
C ****  Get L1 and L2 mEt info
C
      BTYPE='TRGR'
      IWANT=1
      CALL GTESUM(BTYPE, ID_ETMISS, IWANT, ET, ETA, DETETA,
     &          PHI, IFLAG, IER)
      IF ( IER .EQ. 0 ) THEN
        L1MET(1) = ET    ! et of L1 mEt
        L1MET(2) = PHI   ! phi of L1 mEt
      ELSE
        CALL UDST_GET_L12_MET(ET1,PHI1,SCET1,ET2,PHI2,SCET2)
        L1MET(1) = ET1    ! et of L1 mEt
        L1MET(2) = PHI1   ! phi of L1 mEt
      ENDIF
C
      BTYPE='FILT'
      IWANT=1
      CALL GTESUM(BTYPE, ID_ETMISS, IWANT, ET, ETA, DETETA,
     &          PHI, IFLAG, IER)
      IF ( IER .EQ. 0 ) THEN
        L2MET(1) = ET    ! et of L1 mEt
        L2MET(2) = PHI   ! phi of L1 mEt
      ELSE
        CALL UDST_GET_L12_MET(ET1,PHI1,SCET1,ET2,PHI2,SCET2)
        L2MET(1) = ET2    ! et of L1 mEt
        L2MET(2) = PHI2   ! phi of L1 mEt
      ENDIF
C
      GOTO 999
C
      ENTRY TOP_DILEP_GET_L1MUON(L1MU)
C
      DO K=1,3
        DO J=1,5
          L1MU(J,K)=L1MUON(J,K)
        ENDDO
      ENDDO
      GOTO 999
C
      ENTRY TOP_DILEP_GET_L2MUON(L2MU)
C
      DO K=1,3
        DO J=1,5
          L2MU(J,K)=L2MUON(J,K)
        ENDDO
      ENDDO
      GOTO 999
C
      ENTRY TOP_DILEP_GET_L1JET(L1JT)
C
      DO K=1,3
        DO J=1,5
          L1JT(J,K)=L1JET(J,K)
        ENDDO
      ENDDO
      GOTO 999
C
      ENTRY TOP_DILEP_GET_L2JET(L2JT)
C
      DO K=1,3
        DO J=1,5
          L2JT(J,K)=L1JET(J,K)
        ENDDO
      ENDDO
      GOTO 999
C
      ENTRY TOP_DILEP_GET_L1MET(L1MISSET)
C
      DO J=1,2
        L1MISSET(J)=L1MET(J)
      ENDDO
      GOTO 999
C
      ENTRY TOP_DILEP_GET_L2MET(L2MISSET)
C
      DO J=1,2
        L2MISSET(J)=L2MET(J)
      ENDDO
      GOTO 999
C
C----------------------------------------------------------------------
  999 RETURN
      END
