      SUBROUTINE TOP_LEPTONS_EVENT_SHAPE(I_JT,LJETS_VEC,SPHER,PLAN,
     &  GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFour_Shape,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  :  I_JT        Number of jets in event
C-              LJETS_VEC   Array of jet addresses
C-   Outputs :  SPHER       Sphericity (def 1)
C-              PLAN        Planarity  (def 1)
C-              GSPHER      Sphericity (def 2)
C-              GAPLAN      Aplanarity (def 2)
C-              EMAXSH      EMAX shape variable
C-              ETMAXSH     ETMAX shape variable
C-              IER         0/2 if okay/bad
C-   Controls: 
C-
C-   Created  11-JUL-1993   joey thompson
C-   Modified 4-Dec-1993    Uses D0 Library Definition of Sphericity
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
C
      LOGICAL  CORR_JETS,FIRST
C
      INTEGER  NOB_MAX
      PARAMETER( NOB_MAX = 10)
      INTEGER  I_JT,J_JT,LJETS_VEC(NOB_MAX)
      INTEGER  IER,JER
      INTEGER  N_JT,Four_Count
      INTEGER  MODE,NWAY,NSORT,NDIM
      INTEGER  JET_PT_INDEX(NOB_MAX)
      INTEGER  CMFRAME
C
      REAL     SPHER,PLAN,GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFour_Shape
      REAL     E,ET,PX,PY,PZ,PHI,ETA
      REAL     JET_4VEC(4,NOB_MAX),JET_ET(NOB_MAX)

      REAL     POUT(10)
      REAL     P3_EIGVAL(3),P3_EIGVEC(3,3),PTOT(4),MOBJ

      REAL     ESUM, ETSUM
      REAL     ESum4,ETSum4
C
      DATA FIRST/.TRUE./
C     Don't boost calculation to cm frame
      DATA CMFRAME/0/        
C----------------------------------------------------------------------
C
C *** Place default values in parameters
C
      SPHER   = -.1
      PLAN    = -.1
      GSPHER  = -.1
      GAPLAN  = -.1
      EMAXSH  = -.1
      ETMAXSH = -.1
      EFour_Shape = -.1
      IF (I_JT.LT.3) THEN
        IER = 2
        GO TO 999
      ENDIF
C
      JER=0
      IF (FIRST)THEN
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('JETS_CORR',CORR_JETS,JER)
        CALL EZRSET
        IF (JER.NE.0) CALL ERRMSG('Err in TOP_LEPTONS_RCP params'
     &    ,'TOP_LEPTONS_EVENT_SHAPE',' ','F')
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
C *** Assemble jet 4-vectors (PX,PY,PZ,E) and ET array
C
      J_JT=I_JT
      IF(J_JT.GT.10) J_JT=10
      IF(CORR_JETS) THEN
        DO N_JT = 1,J_JT
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(N_JT),E,ET,PX,PY,PZ,
     1                                  PHI,ETA,IER)
          IF (IER.GT.0) THEN
            JET_4VEC(1,N_JT) = PX
            JET_4VEC(2,N_JT) = PY
            JET_4VEC(3,N_JT) = PZ
            JET_4VEC(4,N_JT) = E
            JET_ET(N_JT)     = ET
          ELSE
            CALL UCOPY(Q(LJETS_VEC(N_JT)+2),JET_4VEC(1,N_JT),4)
            JET_ET(N_JT)     = Q(LJETS_VEC(N_JT)+6)
          ENDIF
        ENDDO
      ELSE
        DO N_JT=1,J_JT
          CALL UCOPY(Q(LJETS_VEC(N_JT)+2),JET_4VEC(1,N_JT),4)
          JET_ET(N_JT)     = Q(LJETS_VEC(N_JT)+6)
        ENDDO
      ENDIF
C
C *** Order jets in ET
C
      MODE = 1   !Real sort
      NWAY = 1   !Descending sort
      NSORT = 0  !Sort first I_JT elements of Jet_PT
      CALL SORTZV(JET_ET,Jet_PT_INDEX,I_JT,MODE,NWAY,NSORT)
C
C *** Get sphericity and planarity
C
      NDIM = 2
      CALL Top_Leptons_Util_Sphericity(NDIM,I_JT,JET_4VEC,POUT)
      Plan = POUT(10)
      NDIM = 3
      CALL Top_Leptons_Util_Sphericity(NDIM,I_JT,JET_4VEC,POUT)
      Spher = POUT(10)
C
C *** Get Sphericity and Aplanarity as defined in Barger and Phillips
C
      CALL Sphericity0(I_JT,JET_4VEC,CMFRAME,GSPHER,GY,GAPLAN,
     &  P3_EigVal,P3_EigVec,PTOT,MOBJ)
C
C *** Calculate E and ET Max shape parameters
C
      ESum  = 0.
      ETSum = 0.
      ESum4 = 0.
      ETSum4= 0.
      EFour_Shape = 0.
C
      DO N_JT = 1,I_JT
        ESum  = ESum + Jet_4VEC(4,Jet_PT_INDEX(N_JT))
        ETSum = ETSum + Jet_ET(Jet_PT_INDEX(N_JT))
      ENDDO
C
      IF (I_JT.GE.4) THEN 
        Four_Count = 4
      ELSE
        Four_Count = I_JT
      ENDIF
C
      DO N_JT = 1,Four_Count
        ESum4  = ESum4 + Jet_4VEC(4,Jet_PT_INDEX(N_JT))
        ETSum4 = ETSum4 + Jet_ET(Jet_PT_INDEX(N_JT))
      ENDDO
C
      IF (ESum.GT.0 .AND. ETSum.GT.0) THEN
        EMaxSh  = Jet_4VEC(4,Jet_PT_INDEX(1))/ESum
        ETMaxSh = Jet_ET(Jet_PT_INDEX(1))/ETSum
      ENDIF
C
      IF (ESum4.GT.0) THEN
        EFour_Shape = ETSum4/ESum4
      ENDIF
C
      IER=0
C
  999 RETURN
      END
