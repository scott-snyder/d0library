      LOGICAL FUNCTION MU3_B
C----------------------------------------------------------------------
C-   Purpose:  Flags trimuon events for B physics.
C-
C-   Controls: MU3_B.RCP
C-
C-   Created:  21 Dec 1992  Andrzej Zieminski, Daria Zieminska
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPMUO.LINK'
      INCLUDE 'D0$INC:PI.DEF/LIST'
C
      INTEGER GZPARH,LPARH,NZBANK,GZPMUO,LPMUO
      INTEGER MUON1,MUON2,MUON3,LMUOT1,LMUOT2,LMUOT3
      INTEGER QL1,QL2,QL3,CDMATCH1,CDMATCH2,CDMATCH3,IERR
      INTEGER NMUONS,NTOT,N3MU,NACC,NBDL,NCDM,NNOCOS,NS1,NS2,NS3,I,J,K
      INTEGER NOBJ(10),ID_OBJ(50)
      LOGICAL CUTRHD,CUTMRC,CUTIFW,CUTBDL,CUTCDM,CUTCAL,CUTCOS
      LOGICAL FIRST
C
      REAL    BDLCUT,DPHCUT,DTHCUT,ISOCUT,CALCUT
      REAL    BDL1,BDL2,BDL3,PHI1,PHI2,THE1,THE2,PHI3,THE3
      REAL    DPHI,ISO1,ISO2,ISO3,CAL1,CAL2,CAL3
      REAL    DTHE,ET(50)
      LOGICAL BITON(50)
C
      DATA    FIRST/.TRUE./
C-----------------------------------------------------------------------
C
      MU3_B = .FALSE.
C
      IF (FIRST) THEN
        FIRST = .FALSE. 
C   Get parameters from MU3_B.RCP
       CALL INRCP('MU3_B_RCP',IERR)
       IF (IERR.NE.0) THEN
            CALL ERRMSG('MU3_B_RCP not found','MU3_B',' ','W')
            stop  'MU3_B_RCP not found'
       ELSE
            CALL EZPICK('MU3_B_RCP')
       ENDIF
        CALL EZPICK('MU3_B_RCP')
        CALL EZGET('CUTRHD',CUTRHD,IERR)
        CALL EZGET('CUTMRC',CUTMRC,IERR)
        CALL EZGET('CUTIFW',CUTIFW,IERR)
        CALL EZGET('CUTBDL',CUTBDL,IERR)
        CALL EZGET('CUTCDM',CUTCDM,IERR)
        CALL EZGET('CUTCAL',CUTCAL,IERR)
        CALL EZGET('CUTCOS',CUTCOS,IERR)
        CALL EZGET('ISOCUT',ISOCUT,IERR)
        CALL EZGET('CALCUT',CALCUT,IERR)
        CALL EZGET('BDLCUT',BDLCUT,IERR)
        CALL EZGET('DPHCUT',DPHCUT,IERR)
        CALL EZGET('DTHCUT',DTHCUT,IERR)
        CALL EZRSET
        NTOT = 0
        N3MU = 0
        NACC = 0
      ENDIF
C
      NTOT = NTOT + 1
C
C-- decode reco bits
      IF (CUTRHD) THEN
        CALL UNPACK_RECO_BITS(NOBJ,ID_OBJ,ET,BITON)
        IF (NOBJ(3) .LT. 3) GO TO 999            ! MU3_B = false
        N3MU = N3MU + 1
      ENDIF
C
C-- skip if do not want to test muon parameters
      IF (.NOT.CUTMRC) THEN
        MU3_B = .TRUE.
        NACC=NACC + 1
        GO TO 999
      ENDIF
      LPARH  = GZPARH()
      LPMUO  = LQ(LPARH - IZPMUO)
      NMUONS = NZBANK(IXCOM,LPMUO)
      IF(NMUONS.LE.2) GO TO 999
C
C-- loop over PMUO banks
      DO 10 I = 1, NMUONS - 1
        DO 20 J = I + 1, NMUONS
          MUON1 = GZPMUO(I)
          MUON2 = GZPMUO(J)
C
C-- Quality 
          IF(.NOT.CUTIFW) GOTO 22
          QL1 = IQ(MUON1 + 9)
          QL2 = IQ(MUON2 + 9) 
          IF(QL1.GE.2 .AND. QL2.GE.2) GOTO 20
C
C-- CD match
   22     IF(.NOT.CUTCDM) GOTO 23
          CDMATCH1 = IQ(MUON1 + 6)
          CDMATCH2 = IQ(MUON2 + 6)
          IF(CDMATCH1.EQ.0 .AND. CDMATCH2.EQ.0) GOTO 20
C
C-- Cosmic rays cut
   23     IF(.NOT.CUTCOS) GOTO 24
          THE1 = Q(MUON1 + 15)
          PHI1 = Q(MUON1 + 17)
          THE2 = Q(MUON2 + 15)
          PHI2 = Q(MUON2 + 17)
          DPHI = ABS(PHI1 - PHI2)*180./PI
          IF (DPHI .GT. 180.) DPHI = 360. - DPHI
          DTHE = ABS(THE1 + THE2)*180./PI
          DTHE = ABS(DTHE - 180.)
          IF(DPHI.GT.DPHCUT .AND. DTHE.LT.DTHCUT) GOTO 20
C
C--  Bdl cut
   24     IF(.NOT.CUTBDL) GOTO 25
          NS1    = IQ(MUON1 - 2)
          NS2    = IQ(MUON2 - 2)
          LMUOT1 = LQ(MUON1 - NS1 - 1)
          LMUOT2 = LQ(MUON2 - NS2 - 1)
          BDL1   = Q(LMUOT1 + 22)
          BDL2   = Q(LMUOT2 + 22)
          IF(BDL1.LE.BDLCUT .AND. BDL2.LE.BDLCUT) GOTO 20
C
C--  Calorimetr energy
   25     IF(.NOT.CUTCAL) GOTO 26
          ISO1 = Q(MUON1 + 30)
          ISO2 = Q(MUON2 + 30)
          CAL1 = Q(MUON1 + 34)
          CAL2 = Q(MUON2 + 34)
          IF((ISO1.LT.ISOCUT.AND.CAL1.LT.CALCUT).AND.
     &       (ISO2.LT.ISOCUT.AND.CAL2.LT.CALCUT)) GOTO 20
   26     CONTINUE
C
C--  Good muon pair, look for 3rd good muon          
          DO 30 K=1,NMUONS
            IF (K.EQ.I.OR.K.EQ.J) GO TO 30
            MUON3 = GZPMUO(K)
            IF(CUTCOS) THEN  ! check if 1 and 3 or 2 and 3  back to back
              THE3 = Q(MUON3 + 15)
              PHI3 = Q(MUON3 + 17)
              DPHI = ABS(PHI1 - PHI3)*180./PI
              IF (DPHI .GT. 180.) DPHI = 360. - DPHI
              DTHE = ABS(THE1 + THE3)*180./PI
              DTHE = ABS(DTHE - 180.)
              IF(DPHI.GT.DPHCUT .AND. DTHE.LT.DTHCUT) GOTO 30
              DPHI = ABS(PHI2 - PHI3)*180./PI
              IF (DPHI .GT. 180.) DPHI = 360. - DPHI
              DTHE = ABS(THE2 + THE3)*180./PI
              DTHE = ABS(DTHE - 180.)
              IF(DPHI.GT.DPHCUT .AND. DTHE.LT.DTHCUT) GOTO 30
            END IF
            MUON3 = GZPMUO(K)
            IF(CUTCDM) THEN
              CDMATCH3 = IQ(MUON3 + 6)
C              IF(CDMATCH3.EQ.0) GOTO 30
            END IF
            IF(CUTIFW) THEN
              QL3 = IQ(MUON3 + 9) 
              IF( QL3.GE.2) GOTO 30
              IF(QL1+QL2+QL3.GE.4) GOTO 30
              IF(QL1*QL2*QL3.GT.0) GOTO 30
            END IF
            IF(CUTBDL) THEN 
              NS3    = IQ(MUON3 - 2)
              LMUOT3 = LQ(MUON3 - NS3 - 1)
              BDL3   = Q(LMUOT3 + 22)
              IF( BDL3.LE.BDLCUT) GOTO 30
            END IF
            IF(CUTCAL) THEN 
              ISO3 = Q(MUON3 + 30)
              CAL3 = Q(MUON3 + 34)
              IF (ISO3.LT.ISOCUT.AND.CAL3.LT.CALCUT) THEN
                IF(CDMATCH3.EQ.0) GOTO 30
              END IF
            END IF
            MU3_B = .TRUE.
            NACC = NACC + 1
            GO TO 999 
   30     CONTINUE 
C          
   20   CONTINUE
   10 CONTINUE
  999 RETURN 
      END
