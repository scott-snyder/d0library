      SUBROUTINE CHTANL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOKS AND FILLS HISTOGRAMS FOR CAHITS PACKAGE
C-
C-   ENTRY CHTANL_SUM
C-         Print summaries
C-   AVRGS(1) = scalar ET average (no CC/EC corrections)
C-   AVRGS(2) = scalar ET average (after CC/EC corrections)
C-   AVRGS(3) = missing ET average (no CC/EC corrections)
C-   AVRGS(4) = missing ET average (after CC/EC corrections)
C-
C-   Created  27-APR-1989   Rajendran Raja
C-   Updated  26-FEB-1990   Serban D. Protopopescu  (defined standard hists)
C-   Updated  30-NOV-1991   Serban D. Protopopescu  (added summaries)
C-   Updated  16-JAN-1992   Chip Stewart  (added check D0_ANALYSIS in SUM entry)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER,GZCATE,GZPNUT,LCATE,LPNUT,I,IETA,IPHI
      INTEGER K,NCHT,NEMT,LDCATE,NR,GZISAE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      REAL   E(4),ET,WT,ISA_WEIGHT,X,SCALAR_ET
      REAL    EX,EY,NUS_SUM(4),ISA_MET,DIFF
      REAL    AVRGS(9),HSTATI
      INTEGER LUN,SSUNIT
      LOGICAL FIRST,DO_ANALYSIS,MONTE,VERIFY,FLGVAL,OK,HEXIST,LHIST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      MONTE=GZISAE().GT.0
      VERIFY=FLGVAL('VERIFY')
C         Create/Set HBOOK directory for CAHits
      CALL DHDIR('CAHITS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','CHTANL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET('DO_ANALYSIS',DO_ANALYSIS,IER)
        CALL EZRSET
        CALL HBOOK1(1,'Missing ET (first pass)$',50,0.,100.,0.)
        CALL HBOOK1(2,'Scalar ET (first pass)$',50,0.,1000.,0.)
        CALL HBOOK1(3,'X=ET2/(0.5*SCALAR_ET)$',50,0.,25.0,0.)
        CALL HBOOK1(4,'Total energy in EC (+z)$',50,0.,500.,0.)
        CALL HBOOK1(5,'Total energy in CC upper half$',50,0.,200.,0.)
        CALL HBOOK1(6,'Total energy in CC lower half$',50,0.,200.,0.)
        CALL HBOOK1(7,'Total energy in EC (-z)$',50,0.,500.,0.)
        LHIST = DO_ANALYSIS.OR.VERIFY
        IF(MONTE.and.LHIST) THEN
          CALL HBOOK1(8,' calculated MET - ISAJET MET (1st pass)',
     &        50,-50.,50.,0.)
          CALL HBOOK1(9,' sqrt(diff(EX)**2+diff(EY)**2) (1st pass)',
     &        50,0.,100.,0.)
          CALL HBOOK1(28,' calculated MET - ISAJET MET (2nd pass)',
     &        50,-50.,50.,0.)
          CALL HBOOK1(29,' sqrt(diff(EX)**2+diff(EY)**2 (2nd pass)',
     &        50,0.,100.,0.)
        ENDIF
      ENDIF
C
      IF (LHIST) CALL C2PANL
C
C FILL HISTOGRAMS HERE
      WT=1.0
C
C        if montecarlo use weights
      IF(MONTE) THEN
        WT=ISA_WEIGHT()
        IF(WT.EQ.0) WT=1.0
      ENDIF
      LCATE=GZCATE()
      IF(LCATE.GT.0) THEN
        E(1)=0
        E(2)=0
        E(3)=0
        E(4)=0
        NR=IQ(LCATE+2)
        NCHT=MOD(IQ(LCATE+3),CATENM)
        NEMT=IQ(LCATE+3)/CATENM
C
C          sum over towers
        DO K=NEMT+1,NCHT
          LDCATE=LCATE+(K-1)*NR
          I=1
          IF(IQ(LDCATE+14).EQ.2) THEN
            IETA=IQ(LDCATE+12)
            IPHI=IQ(LDCATE+13)
            IF(IETA.GT.-13.AND.IPHI.LT.33) I=2
            IF(IETA.GT.-13.AND.IPHI.GT.32) I=3
            IF(IETA.GT.12) I=4
            E(I)=E(I)+Q(LDCATE+7)
          ENDIF
        ENDDO
        DO I=1,4
          CALL HFILL(I+3,E(I),0.,WT)
        ENDDO
      ENDIF
C
C       missing ET quantities (1st pass)
      LPNUT=GZPNUT(1)
      ET=Q(LPNUT+7)
      EX=Q(LPNUT+3)
      EY=Q(LPNUT+4)
      CALL HFILL(1,ET,0.,WT)
      IF(Q(LPNUT+14).NE.0.)THEN
        SCALAR_ET = Q(LPNUT+14)
      ELSE
        SCALAR_ET = 1.E-09
      ENDIF
      X = ET**2/(0.5*SCALAR_ET)
      CALL HFILL(2,SCALAR_ET,0.,WT)
      CALL HFILL(3,X,0.,WT)
C
C         compare to ISAJET
      IF ( MONTE .AND. LHIST) THEN
        CALL ISA_NUS_SUM(NUS_SUM,OK)
        ISA_MET=SQRT(NUS_SUM(1)**2+NUS_SUM(2)**2)
        DIFF=SQRT((EX-NUS_SUM(1))**2+(EY-NUS_SUM(2))**2)
        CALL HFILL(8,ET-ISA_MET,0,WT)
        CALL HFILL(9,DIFF,0,WT)
        LPNUT=GZPNUT(2)
        ET=Q(LPNUT+7)
        EX=Q(LPNUT+3)
        EY=Q(LPNUT+4)
        DIFF=SQRT((EX-NUS_SUM(1))**2+(EY-NUS_SUM(2))**2)
        CALL HFILL(28,ET-ISA_MET,0,WT)
        CALL HFILL(29,DIFF,0,WT)
      ENDIF
C
C ****  ANALYZE AND WRITE OUT EVENTS THAT HAVE SIGNIFICANT
C ****  CC OR EC EM/FH ENERGY
C
C      CALL CAL_ANALYZE_FIRST_DATA

      GOTO 999
C
      ENTRY CHTANL_SUM
C
      CALL DHDIR('CAHITS_RCP','HBOOK_DIRECTORY',IER,' ')
      CALL VZERO(AVRGS(1),4)
      AVRGS(1)=HSTATI(1,1,' ',0)
      IF(HEXIST(21)) AVRGS(2)=HSTATI(21,1,' ',0)
      AVRGS(3)=HSTATI(2,1,' ',0)
      IF(HEXIST(21)) AVRGS(4)=HSTATI(22,1,' ',0)
      AVRGS(5)=HSTATI(3,1,' ',0)
      AVRGS(6)=HSTATI(4,1,' ',0)
      AVRGS(7)=HSTATI(5,1,' ',0)
      AVRGS(8)=HSTATI(7,1,' ',0)
      AVRGS(9)=HSTATI(6,1,' ',0)
      LUN=SSUNIT()
      WRITE(LUN,FMT=100) AVRGS
      IF(.NOT.LHIST) CALL HDELET(0)
C
  999 RETURN
 100  FORMAT(//' CAHITS summary:'/,
     &'Avrg miss.  ET pass 1=',E10.4,', Avrg miss.  ET pass 2=',E10.4,/,
     &'Avrg scalar ET pass 1=',E10.4,', Avrg scalar ET pass 2=',E10.4,/,
     &'Avrg X=ET2/(0.5*SCALAR_ET)=',E10.4,/,
     &'Avrg energy in EC (+z)=',E10.4,', Avrg energy in CC(+y)=',E9.4,/,
     &'Avrg energy in EC (-z)=',E10.4,', Avrg energy in CC(-y)=',E9.4)
      END
