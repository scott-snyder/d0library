      LOGICAL FUNCTION CMATCH_TRIGELEC(LPELC,L1THR,L2THR,MASK,CONE,FRCT,
     &  ET_L1,ET_L2,L2EM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : check whether the event was triggered on a
C-                          given PELC or PPHO bank.
C-
C-   Inputs  : lpelc - address of PELC or PPHO bank
C-             l1thr - level 1 Et threshold
C-             l2thr - level 2 Et threshold
C-             rcone - radius of isolation cone
C-             rfract- fraction of energy in cone
C-             mask  - shape cut mask (for bit mask describing cuts in L2EM)
C-                     Bit #         If TRUE
C-                     -----         -------
C-                       0            Cuts are set for electron
C-                       1            Cuts are set for photon
C-                       2            Track match required
C-                       3            Veto on track
C-                       4            Longitudinal shape cuts used
C-                       5            Transverse shape cuts used
C-                       6            Tight shape cuts used
C-                       7            Isolation cut used
C-
C-   Outputs : l2em  - pointer to matched entry in L2EM bank with highest
C-                      level 1 Et
C-
C-   Controls:
C-
C-   Created   5-DEC-1992   Ulrich Heintz
C-   Updated  19-DEC-1993   Ulrich Heintz  check for invalid eta/phi values
C-                                         limit J to =< JMAX
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK/LIST'
      INCLUDE 'D0$LINKS:IZL2EM.LINK/LIST'
      INTEGER JMAX,I,J,K,IER
      PARAMETER( JMAX = 200 )
      INTEGER LPELC,MASK,L2EM,LL2EM,CUTS(JMAX),TETA,TPHI,VERSION,LVERH
      INTEGER LVERT,IPTR(JMAX),NREP,NL2CAND,IOFF,FLAG,JBIT,GZVERH
      INTEGER LFRES,GZFRES
      REAL    L1THR,L2THR,CONE,FRCT,PHI_PELC,ETA_PELC,Z,DCOS,COSMAX
      REAL    CONE_L2(JMAX),FRACT_L2(JMAX),FET(JMAX),TET(JMAX)
      REAL    AETA(JMAX),APHI(JMAX),ET,ETA,ETAD,PHI,ET_L2,ET_L1
      REAL    DCOS_FROM_ETA_PHI,ETA_L1,PHI_L1
      REAL    DCOS_MAX       ! matching parameter (cos>dcos_max)
      DATA    DCOS_MAX/0.99/
      INTEGER NFOUND(ID_ALL:LAST_TYPE),ITAG1,ITAG2
      LOGICAL FIRST
      DATA    FIRST/.TRUE./
C
      L2EM = 0
      IF(ITAG1.NE.IQ(LHEAD+7).OR.ITAG2.NE.IQ(LHEAD+8))THEN ! same event
        ITAG1=IQ(LHEAD+7) ! save L1 crossing number
        ITAG2=IQ(LHEAD+8)
C... get L2EM version number
        LL2EM=0
C
C--   GET LINK TO SUPPORTING FRES BANK
        LFRES = GZFRES()
C
        IF(LFRES.NE.0) LL2EM = LQ(LFRES - IZL2EM)
C
        IF(LL2EM.GT.0)THEN
          VERSION=IQ(LL2EM+1)
        ELSE
          VERSION=0
        ENDIF
C
C... get event vertex
        Z=0.
        LVERH=GZVERH()
        IF(LVERH.GT.0)THEN
          LVERT=LQ(LVERH-IZVERT)
          IF(LVERT.GT.0)Z=Q(LVERT+5)   ! z-position of vertex
        ENDIF
C
C...  loop over all objects in L2EM bank and make a list
        J=0
        DO WHILE (LL2EM.GT.0)
          NREP = IQ(LL2EM+2)
          NL2CAND = IQ(LL2EM+3)
          IOFF=0
          DO I = 1, NL2CAND
            IF(IQ(LL2EM+IOFF+28).EQ.0)THEN  ! candidate passed
              J=J+1
              IF(J.GT.JMAX)THEN
                CALL ERRMSG('L2EM','CMATCH_TRIGELEC',
     &            'too many candidates','W')
                J=JMAX
                GOTO 200
              ENDIF
              IPTR(J)=LL2EM+IOFF
              TETA=IQ(LL2EM+IOFF+4)
              TPHI=IQ(LL2EM+IOFF+5)
              CALL CONV_TCOOR(TPHI,TETA,PHI_L1,ETA_L1)
              CALL GTESUM_COUNTS ('TRGR',NFOUND,IER)
              IF(IER.NE.0)GOTO 903
              DO K=1,NFOUND(ID_ELECTRON)
                CALL GTESUM('TRGR',ID_ELECTRON,K,ET,ETA,ETAD,PHI,FLAG,
     &            IER)
                IF(abs(ETA_L1-ETAD).lt.0.01.AND.abs(PHI_L1-PHI).lt.0.01)
     &            THEN
                  TET(J)=ET
                  GOTO 100
                ENDIF
              ENDDO
  100         CONTINUE
              AETA(J) = Q(LL2EM+IOFF+30)
              APHI(J) = Q(LL2EM+IOFF+31)
              IF(VERSION.LT.3)THEN
                CUTS(J)=0
              ELSE
                CUTS(J) = IQ(LL2EM+IOFF+36)
              ENDIF
              FET(J)  = Q(LL2EM+IOFF+9)
              CONE_L2(J)= Q(LL2EM+IOFF+23)
              FRACT_L2(J)= Q(LL2EM+IOFF+24)
            ENDIF
            IOFF=IOFF+NREP
          ENDDO
          LL2EM=LQ(LL2EM)
        ENDDO
      ENDIF
C
  200 CONTINUE
      ET_L2=0.
      ET_L1=0.
      CMATCH_TRIGELEC=.FALSE.
C
C... determine detector eta, phi for PELC bank
      IF(LPELC.LE.0)GOTO 901  ! error
      PHI_PELC=Q(LPELC+10)
      CALL DET_ETA(Z,Q(LPELC+8),ETA_PELC)
C
C... check whether there is at least one matching candidate
      COSMAX=DCOS_MAX
      DO I=1,J
        IF(FET(I).GE.L2THR.AND.TET(I).GE.L1THR)THEN           ! check thresholds
          IF(VERSION.LT.3.OR.IAND(MASK,CUTS(I)).EQ.MASK)THEN  ! check mask
            IF(JBIT(MASK,8).EQ.0.OR.                ! check isolation parameters
     &        (CONE_L2(I).EQ.CONE.AND.FRACT_L2(I).LE.FRCT))THEN
              IF(AETA(I).GT.-999.AND.APHI(I).GT.-999)THEN
                DCOS=DCOS_FROM_ETA_PHI(AETA(I),APHI(I),ETA_PELC,
     &            PHI_PELC)
                IF(DCOS.GT.DCOS_MAX.AND.TET(I).GT.ET_L1)THEN
                  CMATCH_TRIGELEC=.TRUE.
                  COSMAX=DCOS
                  ET_L2=FET(I)
                  ET_L1=TET(I)
                  L2EM=IPTR(I)
                ENDIF
              ELSE  ! if L2EM bank doesn't contain eta/phi values
                CALL ERRMSG('no eta/phi','CMATCH_TRIGELEC',
     &            'no match possible','W')
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
  999 RETURN
  901 CALL ERRMSG('LPELC=0','CMATCH_TRIGELEC',' ','W')
      RETURN
  903 CALL ERRMSG('GTESUM_COUNTS','CMATCH_TRIGELEC','error returned',
     &  'W')
      RETURN
  904 CALL ERRMSG('L2EM','CMATCH_TRIGELEC','old version','W')
      RETURN
      END
