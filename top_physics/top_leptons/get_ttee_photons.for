      SUBROUTINE GET_TTEE_PHOTONS(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-NOV-1993   Balamurali V
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:vert.INC'
      INCLUDE 'D0$INC:PHOT.INC'
      INCLUDE 'D0$LINKS:IZPPHO.LINK'
C
      REAL    E,ET,PHI,ETA,THE,DET_ETA,CHISQ,TCSQ,FISO,SIGET,CORR
      REAL    FEM,ET_ISOL1,ET_ISOL2
      REAL    MT,DPHIMET
      INTEGER NCELL
      INTEGER*2 CLEANEM1,CLEANEM2
      REAL    HV_SCALE,EM_SCALE(3),CQUAN(50)
      REAL    EX,EY,SIGXSQ,SIGYSQ,ETOT_CONE,EEM_CORE
      INTEGER I,J,IER,N,STATUS,TRK,NCVAR,VERSION
      INTEGER PASS,LEVEL,TSTCSQ
      INTEGER LPPHO,GZPPHO,NZBANK,LHMTP,LCACL
      INTEGER*2 ISTATUS(2)
      EQUIVALENCE(STATUS,ISTATUS)
      LOGICAL OK,FIRST,MC
      DATA    FIRST,MC/.TRUE.,.FALSE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL EZPICK('TTEE_RCP')
        CALL EZGET('LEVEL',LEVEL,IER)
        CALL EZGET('HV_SCALE_FACTOR',HV_SCALE,IER)
        CALL EZGETA('EM_SCALE_FACTORS',0,0,0,N,IER)
        CALL EZGETA('EM_SCALE_FACTORS',1,N,1,EM_SCALE,IER)
        CALL EZRSET
        CALL RECO_VERSION(VERSION,PASS)
        IF(VERSION.GT.10)HV_SCALE=1.
        IF(LQ(LHEAD-17).GT.0)MC=.TRUE.
        FIRST = .FALSE.
      ENDIF
C
C ** Initialize
C
      NPHOT = 0
      DO I = 1,MAX_PHO
        DO J = 1,4
          IPHOT(J,I) = 0
        ENDDO
        DO J = 1,15
          RPHOT(J,I) = 0.0
        ENDDO
      ENDDO
C
C ** Get photon variables
C
      LPPHO = GZPPHO()
      IF (LPPHO .GT. 0)THEN
        NPHOT = NZBANK(IXCOM,LPPHO)
        CALL ZSORT(IXCOM,LPPHO,7)
        LPPHO=GZPPHO()
        CALL ZTOPSY(IXCOM,LPPHO)
        LPPHO=GZPPHO()
      ENDIF
      I = 0
      DO WHILE (LPPHO .GT. 0)
        I = I+1
        IF(I .GT. MAX_PHO)GOTO 999
C
        EX  = Q(LPPHO+3)
        EY  = Q(LPPHO+4)
        E   = Q(LPPHO+6)
        ET  = Q(LPPHO+7)
        THE = Q(LPPHO+8)
        ETA = Q(LPPHO+9)
        PHI = Q(LPPHO+10)
        SIGXSQ  = Q(LPPHO+11)
        SIGYSQ  = Q(LPPHO+12)
        IF(SIGXSQ.GE.0..AND.SIGYSQ.GE.0)THEN
          SIGET   = SQRT(SIGXSQ+SIGYSQ)
        ELSE
          CALL ERRMSG('SIGXSQ or SIGYSQ is negative','GET_TTEE_PHOTONS',
     &      ' ','W')
        ENDIF
        DET_ETA = Q(LPPHO+19)
C
        ETOT_CONE = Q(LPPHO+16)
        EEM_CORE  = Q(LPPHO+17)
        IF(EEM_CORE .GT. 0)THEN
          FISO = (ETOT_CONE-EEM_CORE)/EEM_CORE
        ENDIF
C
        CORR = HV_SCALE*EM_SCALE(2)
        IF(DET_ETA .LT. -13.)CORR=HV_SCALE*EM_SCALE(1)
        IF(DET_ETA .GT. 13.)CORR=HV_SCALE*EM_SCALE(3)
        IF(MC)CORR=1.0
C
C        LHMTP = LQ(LPPHO-1)
C        IF(LHMTE.GT.0)THEN
C          CHISQ =Q(LHMTP+7)
C        ENDIF
C
C        LCACL = LQ(LPPHO-2)
C        IF(LCACL.GT.0)THEN
C          FH1 = Q(LCACL+19)
C        ENDIF
C
        TRK = 1
        CALL CLEANEM(LPPHO,TRK,OK,STATUS)
        CLEANEM1 = ISTATUS(1)
        CLEANEM2 = ISTATUS(2)
        CALL CLEANEM_CQUANS(NCVAR,CQUAN)
        CHISQ = CQUAN(4)
        FEM   = CQUAN(9)
        NCELL = CQUAN(21)
        ET_ISOL1 = CQUAN(25)
        ET_ISOL2 = CQUAN(26)
        TCSQ     = FLOAT(TSTCSQ(DET_ETA,CHISQ,LEVEL))
        MT      = 0
        DPHIMET = 0
C
        IPHOT(1,I)=NPHOT
        IPHOT(2,I)=NCELL
        IPHOT(3,I)=CLEANEM1
        IPHOT(4,I)=CLEANEM2
C
        RPHOT(1,I)=ET
        RPHOT(2,I)=PHI
        RPHOT(3,I)=ETA
        RPHOT(4,I)=THE
        RPHOT(5,I)=DET_ETA
        RPHOT(6,I)=CHISQ
        RPHOT(7,I)=TCSQ
        RPHOT(8,I)=FISO
        RPHOT(9,I)=SIGET
        RPHOT(10,I)=CORR
        RPHOT(11,I)=FEM
        RPHOT(12,I)=ET_ISOL1
        RPHOT(13,I)=ET_ISOL2
        RPHOT(14,I)=MT
        RPHOT(15,I)=DPHIMET
        RPHOT(16,I)=E
C
        LPPHO = LQ(LPPHO)
      ENDDO
C
  999 RETURN
      END
