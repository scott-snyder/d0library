      LOGICAL FUNCTION EL2_B
C----------------------------------------------------------------------
C-   Purpose:  To select dielectron events.
C-
C-   Controls: EL2_B.RCP
C-
C-   Created:   5 Dec 1992  Andrzej Zieminski
C    Last modification: 11-Jan-1993
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
      INCLUDE 'D0$LINKS:IZPPHO.LINK'
      INCLUDE 'D0$LINKS:IZPMUO.LINK'
      INCLUDE 'D0$INC:PI.DEF/LIST'

      INTEGER IERR,I,J,NOBJS
      INTEGER NTOT,N2EL,NACC
      INTEGER NOBJ(10),ID_OBJ(50)

      INTEGER GZPARH,LPARH,NZBANK
      INTEGER LPELC,GZPELC,LPPHO,GZPPHO,LPMUO,GZPMUO
      INTEGER IDL1,IDL2,ID1,ID2,L1,L2,I1,I2,NELS,NPELC,NPPHO,N1
      INTEGER ISTATUS

      LOGICAL FIRST
      LOGICAL CUTRHD,CUTOEE,CUTOEG,CUTPTM,CUTBNK,CUTPTE,CUTMSS
      LOGICAL TRACK_MATCH
      INTEGER IFWCUT,EMMASK
      REAL    PTCUT, PTLOSE, PTTIGT, PTMCUT ,MSSCUT,PTHICT

      REAL    ET1,ET2,ETM,PTMMAX,P4PSI(4), INVMSS
      REAL    ET(50)
      LOGICAL BITON(50)

      INTEGER NL1L2_TRGR,L1L2_TRGR(40),PTR,IER,NTRIG,NFILT,SAL/32/
      LOGICAL L1NAME_PASSED,L2NAME_PASSED,PASSED1,PASSED2
      CHARACTER*32 TRIG_BIT_NAMES(32),FILT_BIT_NAMES(128)
      LOGICAL BACK

      DATA    FIRST/.TRUE./
C-----------------------------------------------------------------------

      EL2_B = .FALSE.

      IF(FIRST) THEN
        FIRST = .FALSE. 
C   Get parameters from the RCP file
        CALL INRCP('EL2_B_RCP',IERR)
        IF (IERR.NE.0) THEN
             CALL ERRMSG('EL2_B_RCP not found','EL2_B',' ','W')
             stop  'EL2_B_RCP not found'
        ELSE
             CALL EZPICK('EL2_B_RCP')
        ENDIF
        CALL EZGET_l('CUTRHD',CUTRHD,IERR)
        CALL EZGET_l('CUTOEE',CUTOEE,IERR)  ! At least two electrons
        CALL EZGET_l('CUTOEG',CUTOEG,IERR)  ! At least one electron
        CALL EZGET_l('CUTPTE',CUTPTE,IERR)  ! cut on PTmin of the pair
        CALL EZGET_l('CUTPTM',CUTPTM,IERR)  ! REQUIRE A MUON 
        CALL EZGET_l('CUTMSS',CUTMSS,IERR)  ! accept pairs w mass < MSSCUT
        CALL EZGET('PTLOSE',PTLOSE,IERR)  ! loose pt cut (with mu)
        CALL EZGET('PTTIGT',PTTIGT,IERR)  ! tight pt cut (no mu)
        CALL EZGET('PTMCUT',PTMCUT,IERR)  ! muon pt(min)
        CALL EZGET_i('IFWCUT',IFWCUT,IERR)  ! muon IFW4 
        CALL EZGET('MSSCUT',MSSCUT,IERR)  ! mazimum invariant mass 
        CALL EZGET('PTHICT',PTHICT,IERR)  ! maximum Pt for both e's
        CALL EZGET_i('EMMASK',EMMASK,IERR)  ! bit mask for CLEANEM cuts
c+
c trigger and filter bits
c-
        CALL EZGET_i('NTRIG',NTRIG,IERR)
        CALL EZGET_i('NFILT',NFILT,IERR)
        CALL EZ_GET_CHARS('TRIG_BIT_NAMES',SAL,TRIG_BIT_NAMES(1),IERR)
        CALL EZ_GET_CHARS('FILT_BIT_NAMES',SAL,FILT_BIT_NAMES(1),IERR)
c+-
        NTOT = 0
        N2EL = 0
        NACC = 0
        CALL EZRSET
      ENDIF
C
      NTOT = NTOT + 1
      CALL HCDIR('//PAWC',' ')  ! go to PAW directory
C
C-- decode reco bits
C-- cut on ETmin
      IF (CUTRHD) THEN
        CALL UNPACK_RECO_BITS(NOBJ,ID_OBJ,ET,BITON)
        IF (CUTOEE. AND. NOBJ(2) .LT. 2) GOTO 999
        IF (CUTOEG. AND. NOBJ(2) .LT. 1) GOTO 999 
        IF ((NOBJ(1)+ NOBJ(2)) .LT. 2) GOTO 999 
        N2EL = N2EL + 1
      ENDIF
C
C--  check muons, no cut on electron/photon Et
      PTCUT = PTTIGT
      LPMUO = GZPMUO(1)
      DO WHILE(LPMUO.NE.0)
        BACK = BTEST(IQ(LPMUO+44),6).OR.BTEST(IQ(LPMUO+44),7) ! COSMIC REJECT.
        IF((Q(LPMUO+14).GE.PTMCUT).AND.(IQ(LPMUO+9).LE.IFWCUT).AND.
     1     (.NOT.BACK)) THEN
C+
C SELECTION EVENT IF THERE EXIST AN ELECTRON TOGETHER WITH MUON.
C-
          LPELC = GZPELC()
          DO WHILE(LPELC.NE.0)
            IF(Q(LPELC+7).GE.1.5) THEN
C+
C APPLY TRACK MATCHING TO PELC.
C-
              TRACK_MATCH = .TRUE.
              CALL CHECK_EM_QUALITY(LPELC,EMMASK,TRACK_MATCH)
              IF(.NOT.TRACK_MATCH) GOTO 101
              GOTO 777
            ENDIF
101         LPELC = LQ(LPELC)
          ENDDO
C+-
          PTCUT = PTLOSE ! Loose cuts on Et
          GOTO 47 
        ENDIF
        LPMUO = LQ(LPMUO)
      ENDDO
      IF(CUTPTM) PTCUT = PTTIGT  ! No good muon was found - tight cuts on Et.
C
C-- skip if do not want to test electron/gamma parameters
C
C-- here only to extract information from electron/gamma banks
47    LPARH  = GZPARH()
      LPELC=LQ(LPARH-IZPELC)
      NPELC=NZBANK(IXCOM,LPELC)
      LPPHO=LQ(LPARH-IZPPHO)
      NPPHO=NZBANK(IXCOM,LPPHO)
      NELS =NPELC
      IF(.NOT.CUTOEE) NELS=NPELC+NPPHO
      N1 = NPELC
      IF(CUTOEE.OR.NPPHO.EQ.0) N1=NPELC-1
c
c-- START LOOP OVER 1ST ELECTRON CANDIDATE
c-- DOUBLE LOOP OVER PELC AND PPHO BANKS ! NO PPHO BANKS
      DO 100 I1=1,N1
        IDL1=0
        LPELC=GZPELC()
        DO I=1,I1
              IF (I.EQ.1) THEN
                L1=LPELC
              ELSE
                L1=LQ(L1)
              ENDIF
        ENDDO
C
        ID1=IQ(L1+2)
        ET1=Q(L1+7)
        IF(CUTPTE.AND.ET1.LE.PTCUT) GOTO 100
C+
C APPLY TRACK MATCHING TO PELC AND UPPER LIMIT CUT ON PT.
C-
        TRACK_MATCH = .TRUE.

        CALL CHECK_EM_QUALITY(L1,EMMASK,TRACK_MATCH)
        IF(.NOT.TRACK_MATCH) GOTO 100
        IF(ET1.GT.PTHICT) GOTO 100

c-- START LOOP OVER 2ND ELECTRON CANDIDATE

        DO 200 I2=I1+1,NELS
          IDL2=0
          IF(I2.GT.NPELC)THEN
            IF(CUTOEE) GOTO 200
            IDL2=1
          ENDIF
C
          J=I2-I1
          DO I=1,J
            IF (I.EQ.1) THEN
              IF(I1+1.EQ.(NPELC+1)) THEN
                L2=GZPPHO()
              ELSE
                L2=LQ(L1)
              ENDIF
            ELSE
              IF(I1+I.EQ.(NPELC+1)) THEN
                L2=GZPPHO()
              ELSE
                L2=LQ(L2)
              ENDIF
            ENDIF
          ENDDO
C
          ID2=IQ(L2+2)
          ET2=Q(L2+7)
          IF(CUTPTE.AND.ET2.LE.PTCUT) GOTO 200
C+
C APPLY TRACK MATCHING TO PELC.
C-
          TRACK_MATCH = .TRUE.
          CALL CHECK_EM_QUALITY(L2,EMMASK,TRACK_MATCH)
          IF(.NOT.TRACK_MATCH) GOTO 200
          IF(ET2.GT.PTHICT) GOTO 200

C
          IF(CUTMSS)THEN
            P4PSI(1) = Q(L1+3)+Q(L2+3)
            P4PSI(2) = Q(L1+4)+Q(L2+4)
            P4PSI(3) = Q(L1+5)+Q(L2+5)
            P4PSI(4) = Q(L1+6)+Q(L2+6)
            INVMSS = SQRT(ABS(P4PSI(4)**2-P4PSI(1)**2-P4PSI(2)**2-
     1                        P4PSI(3)**2))
            IF(INVMSS.LT.MSSCUT) GOTO 777
          ELSE
            GO TO 777
          ENDIF
  200   CONTINUE
  100 CONTINUE
      GOTO 999
C+
C FILTER BIT SELECTION
C-
777   CONTINUE
      PASSED1 = .FALSE.
      PASSED2 = .FALSE.
C
      DO I = 1,NTRIG
        IF(L1NAME_PASSED(TRIG_BIT_NAMES(I))) PASSED1 = .TRUE.
      ENDDO
C
      DO I = 1,NFILT
        IF(L2NAME_PASSED(FILT_BIT_NAMES(I))) PASSED2 = .TRUE.
      ENDDO
C
      IF(PASSED1.AND.PASSED2) THEN
        EL2_B = .TRUE.
        NACC = NACC + 1
        GOTO 999
      ENDIF

  999 CONTINUE
      RETURN
      END
