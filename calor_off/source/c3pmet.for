      FUNCTION C3PMET()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correct missing Et for lost muon momentum
C-
C-   Created  10-OCT-1989   Michael W. Peters
C-   Updated  12-SEP-1990   Serban D. Protopopescu  (to use RCP file) 
C-   Updated  22-MAR-1993   Mark Sosebee (Check for "gold" muons; fill
C-                          PNUT(3) with PNUT(2) if no corrections made)
C-   Updated   3-JUN-1993   Harrison B. Prosper  
C-      Disable CLEANMU for now 
C-   Updated  23-JUN-1993   Darien Wood
C-     Replace CLEANMU test with simple check on IFW4 (expedient solution).
C-   Modified 14-JAN-1994   Stan Krzywdzinski - fill remaining 4 elements
C-                          of error matrix.
C-                          Change sigEt=Q(LPNUT3+13) calculation when
C-                          error matrix is available.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL C3PMET
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C
C protecting the links LMUON and LMUOT within this routine.
C
      INCLUDE 'D0$INC:ZLINKC.INC'
      EQUIVALENCE ( LPMUO,CSTLNK(LNKMX-1))
      EQUIVALENCE ( LMUON,CSTLNK(LNKMX-2))
C
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER GZPNUT,GZPMUO,GZMUON
      INTEGER LPMUO,LMUON
      INTEGER N_PMUOS, N_PMUOS_PASS
      INTEGER IER
      INTEGER TEST_BIT,TEST_WORD
      REAL SCL,EX,EY,EZ,ETOT,ETSQ,PHI,EZOE,ETA,THETA,ELCAL
      REAL S2PMU(3),PMU(3)
      LOGICAL FIRST,EZERR,NO_GOLD_MUONS,GOLD_MUON,BTEST
      LOGICAL USE_GOLD_ONLY,USE_SILVER,DO_ANALYSIS
      LOGICAL GETMAT
      REAL SIGET
      COMMON N_PMUOS, N_PMUOS_PASS
      DATA FIRST/.TRUE./
    
C----------------------------------------------------------------------
C
C          Create/Set HBOOK directory for C3PMET
C
      CALL DHDIR('C3PMET_RCP','HBOOK_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('C3PMET','C3PMET_ANL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
C
      IF ( FIRST )THEN                  ! LOCAL INIT
        FIRST = .FALSE.
C
        N_PMUOS = 0
        N_PMUOS_PASS = 0
C
        CALL EZPICK('C3PMET_RCP')
        IF (EZERR(IER)) THEN
          CALL ERRMSG('C3PMET','C3PMET',
     &      'C3PMET RCP bank not found ','W')
          USE_GOLD_ONLY = .TRUE.
          USE_SILVER = .FALSE.
          DO_ANALYSIS = .FALSE.
        ELSE
C
C ****  read parameters ****
C
          CALL EZGET('USE_GOLD_ONLY',USE_GOLD_ONLY,IER)
          CALL EZGET('USE_SILVER',USE_SILVER,IER)
          CALL EZGET('DO_ANALYSIS',DO_ANALYSIS,IER)
          CALL EZRSET
        ENDIF
      CALL HBOOK1(8,'Scale factor',50,0.,1.,0.)
      ENDIF
C
      C3PMET=.TRUE.
C
C ****  DISABLE CLEANMU
C ***** but reenable gold testing
C
      NO_GOLD_MUONS = .TRUE.
      GOLD_MUON     = .FALSE.
C
      TEST_BIT = 0
      PMU(1)=0.
      PMU(2)=0.
      PMU(3)=0.
      S2PMU(1)=0.
      S2PMU(2)=0.
      S2PMU(3)=0.

C     Get the pointer to the first PMUO bank:

      LPMUO=GZPMUO(0)
100   CONTINUE
      IF(LPMUO.LE.0) GO TO 500
      N_PMUOS = N_PMUOS + 1
C
C ****  DISABLE CLEANMU...
C
C      CALL CLEANMU(LPMUO)
C      TEST_WORD = IQ(LPMUO + 45)
C      GOLD_MUON = BTEST(TEST_WORD,TEST_BIT)
C 
C ***** ...but replace it with simple IFW4 check
      IF(IQ(LPMUO+9).EQ.0) THEN
        GOLD_MUON = .TRUE.
      ELSE
        GOLD_MUON = .FALSE.
      ENDIF  
C
       IF(.NOT.GOLD_MUON) THEN
         LPMUO=LQ(LPMUO)  ! Pointer to the next PMUO bank
         GO TO 100
       ENDIF
       N_PMUOS_PASS = N_PMUOS_PASS + 1
        
        LMUON=LQ(LPMUO-IQ(LPMUO-2)-2)
        IF(LMUON.GT.0) THEN
          SCL=1.-Q(LMUON+24)/Q(LPMUO+13)
        ELSE
          CALL ERRMSG('C3PMET','C3PMET',
     &      'MUON bank not found ','W')
          SCL=1.
        ENDIF
        CALL HFILL(8,SCL,0,1.)
C       Sum the corrections:

        PMU(1)=PMU(1)+SCL*Q(LPMUO+10)
        PMU(2)=PMU(2)+SCL*Q(LPMUO+11)
        PMU(3)=PMU(3)+SCL*Q(LPMUO+12)
        S2PMU(1)=S2PMU(1)+SCL**2*Q(LPMUO+18)
        S2PMU(2)=S2PMU(2)+SCL**2*Q(LPMUO+19)
        S2PMU(3)=S2PMU(3)+SCL**2*Q(LPMUO+20)

        NO_GOLD_MUONS=.FALSE.
       LPMUO=LQ(LPMUO)  ! Pointer to the next PMUO bank
       GO TO 100
500   CONTINUE

C     Check that PNUT(2) bank exists.  If not, build it.

      LPNUT2=GZPNUT(2)
      IF(LPNUT2.EQ.0 ) THEN
        CALL ERRMSG('C3PMET','C3PMET',
     &        'PNUT2 not found - will build PNUT2','W')
        CALL C2PMET
        LPNUT2=GZPNUT(2)
      ENDIF
      IF(LPNUT2.LE.0) GO TO 999
      GETMAT=IQ(LPNUT2+1).GE.3                  ! check PNUT #2 version

C     If any muons passing cuts found, fill PNUT(3) with the corrections.
C     Otherwise put the contents of PNUT(2) into PNUT(3).

      CALL BKPNUT(3)
      LPNUT3=GZPNUT(3)
      IF(LPNUT3.LE.0) GO TO 999
      GETMAT=GETMAT.AND.(IQ(LPNUT3+1).GE.3)     ! check PNUT #3 version
      LPNUT2=GZPNUT(2)
      IF(NO_GOLD_MUONS) THEN

        Q(LPNUT3+3) = Q(LPNUT2+3)
        Q(LPNUT3+4) = Q(LPNUT2+4)
        Q(LPNUT3+5) = Q(LPNUT2+5)
        Q(LPNUT3+6) = Q(LPNUT2+6)
        Q(LPNUT3+7) = Q(LPNUT2+7)
        Q(LPNUT3+8) = Q(LPNUT2+8)
        Q(LPNUT3+9) = Q(LPNUT2+9)
        Q(LPNUT3+10) = Q(LPNUT2+10)
        Q(LPNUT3+11) = Q(LPNUT2+11)
        Q(LPNUT3+12) = Q(LPNUT2+12)
        Q(LPNUT3+13) = Q(LPNUT2+13)
        Q(LPNUT3+14) = Q(LPNUT2+14)
        IF(GETMAT) THEN
          Q(LPNUT3+15) = Q(LPNUT2+15)
          Q(LPNUT3+16) = Q(LPNUT2+16)
          Q(LPNUT3+17) = Q(LPNUT2+17)
          Q(LPNUT3+18) = Q(LPNUT2+18)
        ENDIF

      ELSE

        EX=Q(LPNUT2+3)-PMU(1)
        EY=Q(LPNUT2+4)-PMU(2)
        EZ=Q(LPNUT2+5)-PMU(3)
        ETSQ=EX**2+EY**2+0.0001
        ETOT=SQRT(ETSQ+EZ**2)
        Q(LPNUT3+3)=EX
        Q(LPNUT3+4)=EY
        Q(LPNUT3+5)=EZ
        Q(LPNUT3+6)=ETOT
        Q(LPNUT3+7)=SQRT(ETSQ)
C
C   calculate PHI, THETA and ETA:
C
        PHI=ATAN2(EY,EX+.001)
        IF(PHI.LT.0) PHI=PHI+TWOPI
        EZOE=EZ/ETOT
        THETA=ACOS(EZOE)
        ETA=10.*SIGN(1.,EZOE)
        IF(ABS(EZOE).LT.0.999) ETA=-ALOG(TAN(THETA/2.))
        Q(LPNUT3+8)=THETA
        Q(LPNUT3+9)=ETA
        Q(LPNUT3+10)=PHI
        Q(LPNUT3+11)=Q(LPNUT2+11)+S2PMU(1)
        Q(LPNUT3+12)=Q(LPNUT2+12)+S2PMU(2)
        Q(LPNUT3+13)=SQRT( (EX**2*Q(LPNUT3+11)+
     &                         EY**2*Q(LPNUT3+12)+.0001)/ETSQ )
        Q(LPNUT3+14)=Q(LPNUT2+14)
        IF(GETMAT) THEN
          Q(LPNUT3+15)=Q(LPNUT2+15)+S2PMU(3)
C   Copy over calorimeter correlations since muon correlations
C   are not available
          Q(LPNUT3+16)=Q(LPNUT2+16)
          Q(LPNUT3+17)=Q(LPNUT2+17)
          Q(LPNUT3+18)=Q(LPNUT2+18)
C   Account for dExdEy correlation in calculating SigEt
          SIGET=Q(LPNUT3+13)**2+2.*EX*EY*Q(LPNUT3+16)/ETSQ
          IF(SIGET.GE.0.) Q(LPNUT3+13)=SQRT(SIGET)
        ENDIF

      ENDIF
      IF(DO_ANALYSIS) CALL C3PMET_ANL  ! C3PMET histograms if flag set true 
999   RETURN
      END
