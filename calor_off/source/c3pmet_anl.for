      FUNCTION C3PMET_ANL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-        histograms for C3PMET package (MET 3rd pass)
C-
C-   ENTRY C3PMET_FIN
C-          add summaries to standard output
C-
C-   Created  12-SEP-1990   Serban D. Protopopescu
C-   Updated   9-NOV-1990   Rajendran Raja  protected links
C-   Updated  20-NOV-1990   Rajendran Raja  Plots missing ET even if
C-                          no  MEt3 
C-   Updated  30-NOV-1991   Serban D. Protopopescu  add summaries 
C-   Updated  05-APR-1993   Mark Sosebee (modifications for revised C3PMET
C-                                        package)
C-
C----------------------------------------------------------------------

      IMPLICIT NONE
      LOGICAL C3PMET_ANL,C3PMET_FIN
      LOGICAL NOT_MC,VERIFY,FLGVAL,OK
      LOGICAL DO_ANALYSIS,FIRST    
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER IER,GZPNUT,GZISAE
      INTEGER LUN,SSUNIT
      INTEGER N_PMUOS, N_PMUOS_PASS
      REAL    MET,MET2,MET3,MET2_3,EX,EY,NUS_SUM(4),ISA_MET,DIFF
      REAL    WT,ISA_WEIGHT,SCALAR_ET,X
      REAL    AVRGS(2),HSTATI
      COMMON N_PMUOS, N_PMUOS_PASS
      DATA FIRST/.TRUE./

C----------------------------------------------------------------------
C
C          Create/Set HBOOK directory for C3PMET
C
      C3PMET_ANL=.TRUE.
C
       IF(FIRST)THEN
        FIRST = .FALSE.
        CALL EZPICK('C3PMET_RCP')
        CALL EZGET('DO_ANALYSIS',DO_ANALYSIS,IER)
        CALL EZRSET
        CALL EZPICK('CALEVT_RCP')
        CALL EZGET('SURVEY_CONSTANTS',NOT_MC,IER)
        CALL EZRSET
        VERIFY=FLGVAL('VERIFY')
C
C      Book the histograms; histogram #5 is always filled, so 
C      book it first:

        CALL HBOOK1(5,'ME?t! PNUT2-PNUT3 ',50,-50.,50.,0.)
        IF(DO_ANALYSIS.OR.VERIFY) THEN
          CALL HBOOK1(1,'Missing E?t! from PNUT(3)$',50,0.,100.,0.)
          CALL HBOOK1(9,'Missing E?t! from PNUT(2)$',50,0.,100.,0.)
          CALL HBOOK1(2,'Scalar E?t! from PNUT(3)$',50,0.,1000.,0.)
          CALL HBOOK1(3,'X = ME?t!**2/(0.5*SCALAR_E?t!) from PNUT3',
     &       50,0.,25.,0.)
          CALL HBOOK2(4,'PNUT(2) vs. PNUT(3)',50,0.,100.,50,0.,100.,0.)
C       Check for MC data, fill these hiostograms if it is:
           IF(.NOT.NOT_MC) THEN
            CALL HBOOK1(6,' calculated ME?t! - ISAJET ME?t!',
     &        50,-50.,50.,0.)
            CALL HBOOK1(7,' sqrt(diff(E?x!)**2+diff(E?y!)**2)',
     &        50,0.,100.,0.)
          ENDIF
        ENDIF
      ENDIF
C
C
      WT=1.0
      IF(.NOT.NOT_MC) THEN
C       if montecarlo use weights
        WT=ISA_WEIGHT()
        IF(WT.EQ.0) WT=1.0
      ENDIF
C
      LPNUT3 = GZPNUT(3)  ! pointer to PNUT(3) bank
      LPNUT2 = GZPNUT(2)  ! pointer to PNUT(2) bank
      MET2=Q(LPNUT2+7)
      MET3=Q(LPNUT3+7)
      SCALAR_ET = Q(LPNUT3+14)
      MET2_3=MET2-MET3
       CALL HFILL(5,MET2_3,0,1.)

      IF(.NOT.DO_ANALYSIS.AND..NOT.VERIFY) GOTO 999
C
      IF(Q(LPNUT3+14).NE.0.)THEN
        SCALAR_ET = Q(LPNUT3+14)
      ELSE
        SCALAR_ET = 1.E-09
      ENDIF
      EX=Q(LPNUT3+3)
      EY=Q(LPNUT3+4)
      X = MET3**2/(0.5*SCALAR_ET)
       CALL HFILL(1,MET3,0,1.)
       CALL HFILL(9,MET2,0,1.)
       CALL HFILL(2,SCALAR_ET,0,1.)
       CALL HFILL(3,X,0.,WT)
       CALL HFILL(4,MET2,MET3,1.)
C
C     compare to ISAJET

      IF (.NOT.NOT_MC) THEN
        CALL ISA_NUS_SUM(NUS_SUM,OK)
        IF(OK) THEN
        ISA_MET=SQRT(NUS_SUM(1)**2+NUS_SUM(2)**2)
        CALL HFILL(6,MET-ISA_MET,0,WT)
        DIFF=SQRT((EX-NUS_SUM(1))**2+(EY-NUS_SUM(2))**2)
        CALL HFILL(7,DIFF,0,WT)
       ENDIF
      ENDIF
      GOTO 999
C
      ENTRY C3PMET_FIN
      C3PMET_FIN=.TRUE.
      CALL DHDIR('C3PMET_RCP','HBOOK_DIRECTORY',IER,' ')
      AVRGS(1)=HSTATI(1,1,' ',0)
      AVRGS(2)=HSTATI(2,1,' ',0)
      LUN=SSUNIT()
      WRITE(LUN,FMT=100) AVRGS
      WRITE(LUN,FMT=101) N_PMUOS, N_PMUOS_PASS
C
  999 RETURN
 100  FORMAT(//' C3PMET summary:'/,
     &  ' Avg miss. Et, PNUT(3) =',E10.4,', Avg scalar Et, pass 3
     & = ',E10.4)
 101  FORMAT(' Number of PMUO banks =',I6,' , Number pasing cuts =',I6)
      END
