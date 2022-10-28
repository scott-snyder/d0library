      SUBROUTINE GET_ELECTRONS(IER)
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
      INCLUDE 'D0$INC:VERT.INC'
      INCLUDE 'D0$INC:ELEC.INC'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
C
      REAL    E,ET,PHI,ETA,THE,DET_ETA,CHISQ,TCSQ,FISO,SIGET,CORR
      REAL    FEM,ET_ISOL1,ET_ISOL2,DEDX,RDPHI,DZ_DR,MATCH_SIG
      REAL    TRD_INFO,TRD_MEAN,MT,DPHIMET,CDCMIP,FDCMIP
      INTEGER NCELL,NTRK,DTRK,NTRKCON
      INTEGER*2 CLEANEM1,CLEANEM2
      REAL    HV_SCALE,EM_SCALE(3),CQUAN(50),TQUAN(50)
      REAL    EX,EY,SIGXSQ,SIGYSQ,ETOT_CONE,EEM_CORE,DIST
      INTEGER I,J,IER,N,STATUS,TRK,NCVAR,NTVAR
      INTEGER VERSION,PASS,LEVEL,TSTCSQ
      INTEGER LPELC,GZPELC,NZBANK,LHMTE,LCACL
      INTEGER LZTRK,LDTRK,LFDCT
      INTEGER*2 ISTATUS(2)
      EQUIVALENCE(STATUS,ISTATUS)
      LOGICAL OK,FIRST,MC
      DATA    FIRST,MC/.TRUE.,.FALSE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL EZPICK('TTEE_RCP')
        CALL EZGET_i('LEVEL',LEVEL,IER)
        CALL EZGET('HV_SCALE_FACTOR',HV_SCALE,IER)
        CALL EZGETA_i('EM_SCALE_FACTORS',0,0,0,N,IER)
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
      DO I = 1,MAX_ELE
        DO J = 1,6
          IELEC(J,I) = 0
        ENDDO
        DO J = 1,22
          RELEC(J,I) = 0.0
        ENDDO
      ENDDO
      NELEC = 0
C
C ** Get electron variables
C
      LPELC = GZPELC()
      IF (LPELC .GT. 0)THEN
        NELEC = NZBANK(IXCOM,LPELC)
        CALL ZSORT(IXCOM,LPELC,7)
        LPELC=GZPELC()
        CALL ZTOPSY(IXCOM,LPELC)
        LPELC=GZPELC()
      ENDIF
      I = 0
      DO WHILE (LPELC .GT. 0)
        I = I+1
        IF(I .GT. MAX_ELE)GOTO 999
C
        EX  = Q(LPELC+3)
        EY  = Q(LPELC+4)
        E   = Q(LPELC+6)
        ET  = Q(LPELC+7)
        THE = Q(LPELC+8)
        ETA = Q(LPELC+9)
        PHI = Q(LPELC+10)
        SIGXSQ  = Q(LPELC+11)
        SIGYSQ  = Q(LPELC+12)
        IF(SIGXSQ.GE.0..AND.SIGYSQ.GE.0)THEN
          SIGET   = SQRT(SIGXSQ+SIGYSQ)
        ELSE
          CALL ERRMSG('SIGXSQ or SIGYSQ is negative','GET_ELECTRONS',
     &      ' ','W')
        ENDIF
        DET_ETA = Q(LPELC+19)
        NTRK    = INT(Q(LPELC+21))
        DIST    = Q(LPELC+22)
C
        ETOT_CONE = Q(LPELC+16)
        EEM_CORE  = Q(LPELC+17)
        IF(EEM_CORE .GT. 0)THEN
          FISO = (ETOT_CONE-EEM_CORE)/EEM_CORE
        ENDIF
C
        CORR = HV_SCALE*EM_SCALE(2)
        IF(DET_ETA .LT. -13.)CORR=HV_SCALE*EM_SCALE(1)
        IF(DET_ETA .GT. 13.)CORR=HV_SCALE*EM_SCALE(3)
        IF(MC)CORR=1.0
C
C        LHMTE = LQ(LPELC-1)
C        IF(LHMTE.GT.0)THEN
C          CHISQ =Q(LHMTE+7)
C        ENDIF
C
C        LZTRK = LQ(LPELC-3)
C        IF(LZTRK.GT.0)THEN
C          LZFIT = LQ(LZTRK-1)
C          IF(LZFIT.GT.0)THEN
C            DEDX = Q(LZFIT+30)
C            PHI_ZFIT = Q(LZFIT+10)
C            THE_ZFIT = Q(LZFIT+13)
C          ENDIF
C        ENDIF
C
C        LCACL = LQ(LPELC-2)
C        IF(LCACL.GT.0)THEN
C          FH1 = Q(LCACL+19)
C        ENDIF
C
        TRK = 1
        CALL CLEANEM(LPELC,TRK,OK,STATUS)
        CLEANEM1 = ISTATUS(1)
        CLEANEM2 = ISTATUS(2)
        CALL CLEANEM_CQUANS(NCVAR,CQUAN)
        CHISQ = CQUAN(4)
        FEM   = CQUAN(9)
        NCELL = CQUAN(21)
        ET_ISOL1 = CQUAN(25)
        ET_ISOL2 = CQUAN(26)
        CALL CLEANEM_TQUANS(NTVAR,TQUAN)
        NTRKCON   = TQUAN(2)
        RDPHI     = TQUAN(3)
        MATCH_SIG = TQUAN(12)
C **Write fdc or cdc dedx
        LZTRK = LQ(LPELC-3)
        LDTRK = LQ(LZTRK-7)
        LFDCT = LQ(LZTRK-8)
        IF (LDTRK.NE.0)THEN
          CDCMIP = Q(LDTRK+20)
          DEDX = CDCMIP
        ENDIF
        IF (LFDCT.NE.0)THEN
          FDCMIP = Q(LFDCT+20)
          DEDX = FDCMIP
          IF(LDTRK.NE.0.AND.LFDCT.NE.0)THEN
            CALL ERRMSG('Both CDC and FDC MIP present','GET_ELECTRONS',
     &        'The larger value written to ntuple','W')
            IF (CDCMIP.GT.FDCMIP)DEDX=CDCMIP
          ENDIF
        ENDIF
C
        TRD_INFO  = TQUAN(18)
        TRD_MEAN  = TQUAN(19)
        TCSQ      = FLOAT(TSTCSQ(DET_ETA,CHISQ,LEVEL))
        DZ_DR   = 0
        MT      = 0
        DPHIMET = 0
C
        IELEC(1,I)=NCELL
        IELEC(2,I)=CLEANEM1
        IELEC(3,I)=CLEANEM2
        IELEC(4,I)=NTRK
        IELEC(5,I)=DTRK
        IELEC(6,I)=NTRKCON
C
        RELEC(1,I)=ET
        RELEC(2,I)=PHI
        RELEC(3,I)=ETA
        RELEC(4,I)=THE
        RELEC(5,I)=DET_ETA
        RELEC(6,I)=CHISQ
        RELEC(7,I)=TCSQ
        RELEC(8,I)=FISO
        RELEC(9,I)=SIGET
        RELEC(10,I)=CORR
        RELEC(11,I)=FEM
        RELEC(12,I)=ET_ISOL1
        RELEC(13,I)=ET_ISOL2
        RELEC(14,I)=DEDX
        RELEC(15,I)=RDPHI
        RELEC(16,I)=DZ_DR
        RELEC(17,I)=MATCH_SIG
        RELEC(18,I)=TRD_INFO
        RELEC(19,I)=TRD_MEAN
        RELEC(20,I)=MT
        RELEC(21,I)=DPHIMET
        RELEC(22,I)=E
C
        LPELC = LQ(LPELC)
      ENDDO
C
  999 RETURN
      END
