      SUBROUTINE VEEHIS
C----------------------------------------------------------------------
C
C  Booking and filling histogram for VEES package
C
C  9-JUL-1990 Daria Zieminska
C  12-AUG-1991 Tom Trippe  add EZRSET call
C  Updated   1-SEP-1993   Oscar de J. Ramírez G. Extend booking for lambdas
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER IER,ERR,NHIST,LPVES,GZPVES,NDIL
      INTEGER L1,L2,H1,H2
      PARAMETER (NHIST=22)               ! 14 NEW HIST FOR LAMBDAS
      INTEGER NVMAX,NV,NM,NE,NP,NN,NPV,NTAU,IVEE,NID(NHIST),IHIST,IVER
      INTEGER STAT,IBITS
      INTEGER RUN,ID
      PARAMETER (NVMAX=5)
      LOGICAL FIRST,MCDATA,OK
      CHARACTER*45 NAME(NHIST)
      REAL VEEHIST(4,NHIST)
      REAL VEE(61),MOM,LENGTH,PHI,THE,DELMOM,DELPHI,DELTHE,PROB,RATIO
      REAL PROBMIN,PT,PTMIN,L1PT,L2PT
      DATA FIRST/.TRUE./
      REAL L1MOM,L1LENGTH,L1PHI,L1THE,L1DELMOM,L1DELPHI,L1DELTHE,L1PROB
      REAL L2MOM,L2LENGTH,L2PHI,L2THE,L2DELMOM,L2DELPHI,L2DELTHE,L2PROB
      REAL L1RATIO,L2RATIO
      DATA NAME/' Number Of Secondary Vertices',
     &  ' Ks Ratio Of Decay Momenta',
     &  ' Ks Pt Momentum  Gev',
     &  ' Ks Fit Probability ',
     &  ' Ks Proper Length  cm',
     &  ' Ks Momentum - Ks Momentum(Isa)',
     &  ' Ks Phi - Ks Phi(Isa) ',
     &  ' Ks theta - Ks theta(Isa) ',
     &  ' Lambda1 Ratio Of Decay Momenta',
     &  ' Lambda1 Pt Momentum  Gev',
     &  ' Lambda1 Fit Probability ',
     &  ' Lambda1 Proper Length  cm',
     &  ' Lambda1 Momentum - Lambda1 Momentum(Isa)',
     &  ' Lambda1 Phi - Lambda1 Phi(Isa) ',
     &  ' Lambda1 theta - Lambda1  theta(Isa) ',
     &  ' Lambda2 Ratio Of Decay Momenta',
     &  ' Lambda2 Pt Momentum  Gev',
     &  ' Lambda2 Fit Probability ',
     &  ' Lambda2 Proper Length  cm',
     &  ' Lambda2 Momentum - Lambda2 Momentum(Isa)',
     &  ' Lambda2 Phi - Lamda2 Phi(Isa) ',
     &  ' Lambda2 theta - Lambda2  theta(Isa) '/
C----------------------------------------------------------------------
C
C Create/set HBOOK directory VEES
C
      CALL DHDIR('VEES_RCP','HBOOK_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('VEES','VEEHIS',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      IF (FIRST) THEN
        PROBMIN=0.05
        PTMIN=1.0
        CALL EZPICK('VEES_RCP')
        CALL EZGET('VEEHIST(1)',VEEHIST(1,1),ERR)
        CALL EZGET('MCDATA',MCDATA,ERR)
        CALL EZRSET
        FIRST = .FALSE.
        DO 100 IHIST=1,NHIST
          IF (VEEHIST(1,IHIST).EQ.1.) THEN
            IF (.NOT.MCDATA.AND.IHIST.EQ.6) GO TO 100
            IF (.NOT.MCDATA.AND.IHIST.EQ.7) GO TO 100
            IF (.NOT.MCDATA.AND.IHIST.EQ.8) GO TO 100
            IF (.NOT.MCDATA.AND.IHIST.EQ.13) GO TO 100
            IF (.NOT.MCDATA.AND.IHIST.EQ.14) GO TO 100
            IF (.NOT.MCDATA.AND.IHIST.EQ.15) GO TO 100
            IF (.NOT.MCDATA.AND.IHIST.EQ.20) GO TO 100
            IF (.NOT.MCDATA.AND.IHIST.EQ.21) GO TO 100
            IF (.NOT.MCDATA.AND.IHIST.EQ.22) GO TO 100
            CALL HBOOK1(IHIST,NAME(IHIST),NINT(VEEHIST(2,IHIST)),
     &        VEEHIST(3,IHIST),VEEHIST(4,IHIST),0.)
          END IF
  100   CONTINUE
      END IF
      CALL GTPARH(NV,NM,NE,NP,NN,NPV,NTAU,NDIL)
      IF (VEEHIST(1,1).EQ.1.) CALL HFF1(1,NID(1),FLOAT(NPV),1.)
      IF (NPV.LE.0) GO TO 1000
      CALL EVNTID(RUN,ID)
      IF (NPV.GT.0) THEN
        DO 202 IVEE=1,NPV
          CALL GTPVES(IVEE,VEE)
          CALL UCOPY(VEE(2),STAT,1)
          STAT=IBITS(STAT,1,1)
C          IF (STAT.EQ.0) GO TO 200
          IF (VEE(13).GT.0..AND.VEE(16).GT.0.) THEN
            RATIO=VEE(13)/VEE(16)
            IF (RATIO.GT.1.) RATIO=1./RATIO
          ELSE
            RATIO=0.
          END IF
          MOM=VEE(19)
          LENGTH=VEE(25)
          PHI=VEE(23)
          THE=VEE(21)
          LPVES=GZPVES(IVEE)
          PROB=Q(LPVES+27)
          PT=MOM*SIN(THE)
          IF (VEEHIST(1,2).EQ.1.) THEN
            IF (PROB.GT.PROBMIN.AND.LENGTH.LT.20.AND.PT.GT.PTMIN) THEN
              CALL HFF1(2,NID(2),RATIO,1.)
            END IF
          END IF
          IF (VEEHIST(1,3).EQ.1.) THEN
            IF (PROB.GT.PROBMIN.AND.LENGTH.LT.20.) THEN
              CALL HFF1(3,NID(3),PT,1.)
            END IF
          END IF
          IF (VEEHIST(1,4).EQ.1.) THEN
            IF (LENGTH.LT.20.) THEN
              CALL HFF1(4,NID(4),PROB,1.)
            END IF
          END IF
          IF (VEEHIST(1,5).EQ.1.) THEN
            IF (PROB.GT.PROBMIN.AND.PT.GT.PTMIN) THEN
              CALL HFF1(5,NID(5),LENGTH,1.)
            END IF
          END IF
          IF (MCDATA.AND.PROB.GT.PROBMIN) THEN
C          IF (MCDATA) THEN
            CALL VEE_MC(MOM,PHI,THE,DELMOM,DELPHI,DELTHE,OK)
            IF (.NOT.OK) GO TO 200
            IF (VEEHIST(1,6).EQ.1.) THEN
              CALL HFF1(6,NID(6),DELMOM,1.)
            END IF
            IF (VEEHIST(1,7).EQ.1.) THEN
              CALL HFF1(7,NID(7),DELPHI,1.)
            END IF
            IF (VEEHIST(1,8).EQ.1.) THEN
              CALL HFF1(8,NID(8),DELTHE,1.)
            END IF
          END IF
  200     CONTINUE
          L1 = 17
          IF (VEE(L1+13).GT.0..AND.VEE(L1+16).GT.0.) THEN
            L1RATIO=VEE(L1+13)/VEE(L1+16)
            IF (L1RATIO.GT.1.) L1RATIO=1./L1RATIO
          ELSE
            L1RATIO=0.
          END IF
          L1MOM=VEE(L1+19)
          L1LENGTH=VEE(L1+25)
          L1PHI=VEE(L1+23)
          L1THE=VEE(L1+21)
          LPVES=GZPVES(IVEE)
          L1PROB=Q(LPVES+L1+27)
          L1PT=L1MOM*SIN(L1THE)
          H1=8
          IF (VEEHIST(1,H1+1).EQ.1.) THEN
            IF (L1PROB.GT.PROBMIN.AND.L1LENGTH.LT.75.AND.PT.GT.PTMIN)
     &        THEN
              CALL HFF1(H1+1,NID(H1+1),L1RATIO,1.)
            END IF
          END IF
          IF (VEEHIST(1,H1+2).EQ.1.) THEN
            IF (L1PROB.GT.PROBMIN.AND.L1LENGTH.LT.75.) THEN
              CALL HFF1(H1+2,NID(H1+2),L1PT,1.)
            END IF
          END IF
          IF (VEEHIST(1,H1+3).EQ.1.) THEN
            IF (L1LENGTH.LT.60.) THEN
              CALL HFF1(H1+3,NID(H1+3),L1PROB,1.)
            END IF
          END IF
          IF (VEEHIST(1,H1+4).EQ.1.) THEN
            IF (L1PROB.GT.PROBMIN.AND.PT.GT.PTMIN) THEN
              CALL HFF1(H1+4,NID(H1+4),L1LENGTH,1.)
            END IF
          END IF
          IF (MCDATA.AND.PROB.GT.PROBMIN) THEN
            CALL VEE_MC(L1MOM,L1PHI,L1THE,L1DELMOM,L1DELPHI,L1DELTHE,OK)
            IF (.NOT.OK) GO TO 201
            IF (VEEHIST(1,H1+5).EQ.1.) THEN
              CALL HFF1(H1+5,NID(H1+5),L1DELMOM,1.)
            END IF
            IF (VEEHIST(1,H1+6).EQ.1.) THEN
              CALL HFF1(H1+6,NID(H1+6),L1DELPHI,1.)
            END IF
            IF (VEEHIST(1,H1+7).EQ.1.) THEN
              CALL HFF1(H1+7,NID(H1+7),L1DELTHE,1.)
            END IF
          END IF
  201     CONTINUE
          L2 = 34
          IF (VEE(L2+13).GT.0..AND.VEE(L2+16).GT.0.) THEN
            L2RATIO=VEE(L2+13)/VEE(L2+16)
            IF (L2RATIO.GT.1.) L2RATIO=1./L2RATIO
          ELSE
            L2RATIO=0.
          END IF
          L2MOM=VEE(L2+19)
          L2LENGTH=VEE(L2+25)
          L2PHI=VEE(L2+23)
          L2THE=VEE(L2+21)
          LPVES=GZPVES(IVEE)
          L2PROB=Q(LPVES+L2+27)
          L2PT=L2MOM*SIN(L2THE)
          H2=15
          IF (VEEHIST(1,H2+1).EQ.1.) THEN
            IF (L2PROB.GT.PROBMIN.AND.L2LENGTH.LT.75.AND.PT.GT.PTMIN)
     &        THEN
              CALL HFF1(H2+1,NID(H2+1),L2RATIO,1.)
            END IF
          END IF
          IF (VEEHIST(1,H2+2).EQ.1.) THEN
            IF (L2PROB.GT.PROBMIN.AND.L2LENGTH.LT.75.) THEN
              CALL HFF1(H2+2,NID(H2+2),L2PT,1.)
            END IF
          END IF
          IF (VEEHIST(1,H2+3).EQ.1.) THEN
            IF (L2LENGTH.LT.60.) THEN
              CALL HFF1(H2+3,NID(H2+3),L2PROB,1.)
            END IF
          END IF
          IF (VEEHIST(1,H2+4).EQ.1.) THEN
            IF (L2PROB.GT.PROBMIN.AND.PT.GT.PTMIN) THEN
              CALL HFF1(H2+4,NID(H2+4),L2LENGTH,1.)
            END IF
          END IF
        IF (MCDATA.AND.PROB.GT.PROBMIN) THEN
          CALL VEE_MC(L2MOM,L2PHI,L2THE,L2DELMOM,L2DELPHI,L2DELTHE,OK)
          IF (.NOT.OK) GO TO 202
          IF (VEEHIST(1,H2+5).EQ.1.) THEN
            CALL HFF1(H2+5,NID(H2+5),L2DELMOM,1.)
          END IF
          IF (VEEHIST(1,H2+6).EQ.1.) THEN
            CALL HFF1(H2+6,NID(H2+6),L2DELPHI,1.)
          END IF
          IF (VEEHIST(1,H2+7).EQ.1.) THEN
            CALL HFF1(H2+7,NID(H2+7),L2DELTHE,1.)
          END IF
        END IF
C
  202 CONTINUE
      END IF
 1000 RETURN
      END
