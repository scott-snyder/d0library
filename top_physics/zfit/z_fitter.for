      SUBROUTINE Z_FITTER(RUN,EVENT,LEP,LEP_ERR,MET,MET_ERR,FIT2PAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform kinematic fit for two-body decays
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-FEB-1993   Pushpa C. Bhat
C-   Updated  27-FEB-1993   Pushpa C. Bhat
C-   Updated  21-JUN-1993   Pushpa C. Bhat  Make the package use
C-                                          DSTs via GET_DILEPTONS
C-   Updated   4-JUL-1993   Pushpa C. Bhat
C-   Updated   4-SEP-1993   Pushpa C. Bhat  Calculate default errors in
C-                                              FIT2_ERROR_MATRIX
C-   Updated  26-SEP-1993   Pushpa C. Bhat  Add FIT2 bank to keep FIT
C-                                          parameters
C-   Updated  11-OCT-1993   Pushpa C. Bhat  Add error matrix to FIT2 bank,
C-                                          Take out ntuple-writing as default
C-   Updated   9-APR-1994   Pushpa C. Bhat  Make Z_fitter utilities from 
C-                                          FIT_TWO package
C-   Updated  20-APR-1994   Pushpa Bhat  Return FIT2 parametetrs and do not 
C-                          John Hobbs   try to write FIT2 bank if not
C-                                       requested 
C-   Updated   5-JUL-1994   Pushpa Bhat  Include calculation of SIG_MASS
C-                                       to incorporate measurement resolution
C-                                       into the BW constraint
C-                                       + some other cosmetic changes
C-
C----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'D0$INC:FIT_TWO.INC'
      PARAMETER (NPARM = 6)
C
      DIMENSION    STEP(NPARM),PARMIN(NPARM),PARMAX(NPARM),XP(NPARM)
      DIMENSION ARGLIS(NPARM)
      DIMENSION ELEC_MAT(3,3),EIG(3),AMAT(3,3),BMAT(3,3),EIGEN(8)
      DIMENSION BLEP(2,4),BLEP_ERR(2,3,3),BMET(2),BMET_ERR(2,2)
      DIMENSION EMAT(NPARM,NPARM),H_MAT(NOBS,NOBS)
C
      INTEGER RUNNO,EVONUM
      INTEGER ICLEAN,ISTAT
      INTEGER LFILE,LHFILE,PRINT_LEV,ISKIP,LFIT2,LPROC
      INTEGER SKIP_EVTS(100),NSKIP_EVT
      INTEGER RUN,EVENT
C
      REAL IXM(NOBS) , IXME1(9),IXME2(9),IXME(4), MASS,
     &ZMASS(2) !ZMASS RANGE
      REAL    SIGTHSQ,SIGPHISQ
      REAL OBS_ZPEAK,OBS_ZWIDTH,CHISQ_CUT1,CHISQ_CUT2,METERR_SCALE
      REAL LEP(2,4),LEP_ERR(2,3,3),MET(2),MET_ERR(2,2)
      REAL    FIT2PAR(50),P1,P2,FITMASS
      REAL    PXL1,PXL2,PYL1,PYL2,PZL1,PZL2
      REAL    XVAL1,XVAL2,DENR,STRETCH_FUNC
C
      LOGICAL FIRST,OK,CALCULATE_ERRORS
      LOGICAL WRITE_FIT2_FILE,OPENED_FIT2_FILE
      LOGICAL FIT_TWO_EVENT,EZERROR,OFF_DIAG
      LOGICAL WRITE_NTUPLE,WRITE_FIT2_BANK
C
      CHARACTER*10 PARNAM(NPARM),CHNAM(NPARM)
      CHARACTER*80 FILE
C----------------------------------------------------------------------
      EXTERNAL FCN
C----------------------------------------------------------------------
      DATA PARNAM / 'ZPX','ZPY','ZPZ','TH STAR','PHI STAR','Z_MASS'/
C      DATA STEP / 0.1,0.1,0.1,.02,.02,.1/
      DATA FIRST/.TRUE./
      SAVE FIRST
      DATA LUNID/6/
C----------------------------------------------------------------------
      FIT_TWO_EVENT = .TRUE.
      FIX_ZMASS=.FALSE.
      MAXINP = 2 * NOBS
      OFF_DIAG=.TRUE.
      BW_CONSTR_RES = .FALSE.
      BW_CONSTR_SPC = .FALSE.
C
C ****  Read rcp file for the first event.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
      CALL INRCP('FIT_TWO_RCP',IER)
      IF(IER .NE. 0)THEN
        CALL ERRMSG('Z_FITTER','Z_fitter',
     &    'Cannot find file','FIT_TWO_RCP','F')
      ENDIF
        CALL EZPICK('FIT_TWO_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('Z_FITTER','Z_fitter',
     &      'Cannot find bank FIT_TWO_RCP','W')
          GOTO 999
        ENDIF
C
        CALL EZGET('DO_ELECTRONS',DO_ELECTRONS,IER)
        CALL EZGET('DO_MUONS',DO_MUONS,IER)
        IF(.NOT.DO_ELECTRONS.AND..NOT.DO_MUONS)
     &    CALL ERRMSG('RCP',' FIT_TWO_EVENT',
     &' NO VALID SELECTION MADE','F')
C
        CALL EZGET('OBS_ZPEAK',OBS_ZPEAK,IER)
        ZMASS_OBS=OBS_ZPEAK
        CALL EZGET('OBS_ZWIDTH',OBS_ZWIDTH,IER)
        ZWID_OBS=OBS_ZWIDTH
        CALL EZGET('FIX_ZMASS',FIX_ZMASS,IER)
        CALL EZGET('OFF_DIAG',OFF_DIAG,IER)
        CALL EZGET('CALCULATE_ERRORS',CALCULATE_ERRORS,IER)
        CALL EZGET('SCALE_MISSET_ERRORS',METERR_SCALE,IER)
        IF(IER.NE.0)METERR_SCALE=1.
        CALL EZGET('MINUIT_PRINT_LEVEL',PRINT_LEV,IER)
        CALL EZGET('BW_CONSTRAINT',BW_CONSTR,IER)
        CALL EZGET('BW_CONSTRAINT_RES',BW_CONSTR_RES,IER)
        CALL EZGET('BW_CONSTRAINT_SPC',BW_CONSTR_SPC,IER)
        CALL EZGET('WRITE_FIT2_FILE',WRITE_FIT2_FILE,IER)
        CALL EZGET('WRITE_NTUPLE',WRITE_NTUPLE,IER)
        CALL EZGET('WRITE_FIT2_BANK',WRITE_FIT2_BANK,IER)
C
        CALL EZGETA('SKIP_EVENTS',0,100,1,NSKIP_EVTS,IER)
        IF(NSKIP_EVTS.GT.100)THEN
          CALL ERRMSG('RCP',' FIT_TWO_EVENT',
     &' CANNOT SKIP MORE THAN 100 EVENTS, ARRAY SMALL','F')
        ENDIF
        CALL EZGETA('SKIP_EVENTS',1,NSKIP_EVTS,1,SKIP_EVTS,IER)
C
        CALL EZRSET
C
C ****  Open output file if needed
C
        IF(WRITE_FIT2_FILE)THEN
          CALL GTUNIT (LUNID,LUNIT,STATUS)
          IF(STATUS.EQ.0) THEN
            CALL D0OPEN(LUNIT,'FIT2_FILE','OF',OPENED_FIT2_FILE)
          ELSE
            CALL ERRMSG('NOUNIT','FIT_TWO_EVENT',
     &        'Cannot get unit number for FIT2_FILE','W')
          ENDIF
        ENDIF
      ENDIF
   10 CONTINUE
C
C
      ISKIP=1
      DO I=1,2
        BMET(I)=MET(I)
        DO J=1,2
          BMET_ERR(I,J)=MET_ERR(I,J)*METERR_SCALE
        ENDDO
        DO J=1,4
          BLEP(I,J)=LEP(I,J)
        ENDDO
        DO J=1,3
          DO K=1,3
            BLEP_ERR(I,J,K)=LEP_ERR(I,J,K)
          ENDDO
        ENDDO
      ENDDO
C
C ****  Copy the measured variables into the array XM
C
      DO I=1,3
        XM(I)=BLEP(1,I)
        XM(I+3)=BLEP(2,I)
      ENDDO
      XM(7)=BMET(1) + BLEP(1,1) + BLEP(2,1)
      XM(8)=BMET(2) + BLEP(1,2) + BLEP(2,2)
      IF(DO_MUONS)THEN
        XM(7)=BMET(1)
        XM(8)=BMET(2)
        J=0
        DO I=1,2
          XM(I+J)=BLEP(I,4)*SIN(BLEP(I,2))*COS(BLEP(I,3))
          XM(I+J+1)=BLEP(I,4)*SIN(BLEP(I,2))*SIN(BLEP(I,3))
          XM(I+J+2)=BLEP(I,4)*COS(BLEP(I,2)) 
          J=2
        ENDDO
      ENDIF
      MASS=2.*(BLEP(1,4)*BLEP(2,4) - (XM(1)*XM(4)+
     &XM(2)*XM(5)+XM(3)*XM(6)))
      IF ( MASS.GT.0 ) THEN
        MASS=SQRT(MASS)
      ELSE
        MASS=0.
      ENDIF
C
C ****  CALCULATE INITIAL PARAMETERS FOR FIT
C
      CALL GET_INIPAR(XP)
      IF(DO_MUONS)THEN
        DO I=1,3
          XM(I)=BLEP(1,I)
          XM(I+3)=BLEP(2,I)
        ENDDO
      ENDIF
C
      WRITE(LUNIT,*)
     &   '*******************************************************'
      WRITE(LUNIT,*)
     &   '*******************************************************'
      WRITE(LUNIT,*)' RUN= ',RUN,' EVENT=',EVENT
      WRITE(LUNIT,*)
     &   '*******************************************************'
      WRITE(LUNIT,*)(XM(I),I=1,8)
      WRITE(LUNIT,*)MASS
C
C ****  BOOK the full error matrix for the fit
C
      CALL LSQ_BKMATRIX('ERR_MATRIX',NOBS,NOBS,IER)
C
      DO I=1,NOBS
        DO J=1,NOBS
          E_MAT(I,J)=0.
          G_MAT(I,J)=0.
        ENDDO
      ENDDO
C
C ****  If default errors to be used calculate error-matrix for
C ****  the two particles separately.
C
      IF(CALCULATE_ERRORS)THEN
        CALL FIT2_ERROR_MATRIX(XM,BLEP_ERR,IER)
      ENDIF
C
C ****  Copy the lepton error matrices into full error matrix
C
      DO I=1,3
        DO J=1,3
          E_MAT(I,J)     = BLEP_ERR(1,I,J)
          E_MAT(I+3,J+3) = BLEP_ERR(2,I,J)
        ENDDO
      ENDDO
C
C ****  Missing Et errors (Decaying Particle PX, PY errors)
C
      E_MAT(7,7)=BMET_ERR(1,1)
      E_MAT(7,8)=BMET_ERR(1,2)
      E_MAT(8,7)=BMET_ERR(2,1)
      E_MAT(8,8)=BMET_ERR(2,2)
C
C ****  RE-ZERO THE OFF-DIAGONAL ELEMENTS IF NOT TO BE USED
C
      IF(.NOT.OFF_DIAG)THEN
        DO I=1,NOBS
          DO J=1,NOBS
            IF(I.NE.J)E_MAT(I,J)=0.
          ENDDO
        ENDDO
      ENDIF
C
C ****  CALCULATE ERROR IN MASS**2
C
      sig_mass=(mass/sqrt(2.))*sqrt(e_mat(1,1)/(blep(1,4))**2.+
     &e_mat(4,4)/(blep(2,4))**2.)
      IF(DO_MUONS)THEN
      sig_mass=(mass/sqrt(2.))*sqrt(e_mat(1,1)*(blep(1,4))**2.+
     &e_mat(4,4)*(blep(2,4))**2.)
      ENDIF
      WRITE(LUNIT,*)'SIG_MASS= ',SIG_MASS
C
C
C ****  Fill the error matrix and invert it
C
      CALL LSQ_MATRIX_FILL('ERR_MATRIX',E_MAT,NOBS,NOBS,2,IER)
C
      WRITE(LUNIT,*)' ERR_MATRIX'
      WRITE(LUNIT,*)E_MAT(1,1),E_MAT(2,2),E_MAT(3,3)
      WRITE(LUNIT,*)E_MAT(4,4),E_MAT(5,5),E_MAT(6,6)
      WRITE(LUNIT,*)E_MAT(7,7),E_MAT(8,8)
C
      CALL LSQ_MATRIX_DIAG('ERR_MATRIX','U_MAT','EI_MAT',IER)
      IF(IER.GT.0)WRITE(LUNIT,*)' ERROR IN DIAG'
      CALL LSQ_MATRIX_GET('EI_MAT',EIGEN,8,1,2,IER)
      DO I=1,8
        IF(EIGEN(I).LT.0)
     &  WRITE(LUNIT,*)'NEGATIVE EIGEN VALUES FOR E_MAT'
        IF(EIGEN(I).EQ.0)WRITE(LUNIT,*)'ZERO EIGEN VALUES FOR E_MAT'
        IF(EIGEN(I).LE.0)
     &  WRITE(LUNIT,*)'RUN ',RUN,'EVENT  ',EVENT,EIGEN(I)
      ENDDO
      CALL LSQ_MATRIX_INVERT('ERR_MATRIX','INV_MATRIX',IER)
      IF(IER .NE. 0)WRITE(LUNIT,*)' ERROR IN INVERTING E_MAT'
      CALL LSQ_MATRIX_GET('INV_MATRIX',G_MAT,NOBS,NOBS,2,IER)
C
C ****  MINUIT INITIALIZATION
C
      CALL MNINIT(5,LUNIT,7)
C
C ****  Set up initial parameters for the fit
C
C      CALL GET_INIPAR(XP)
C
      STEP(1)=.05*E_MAT(7,7)
      STEP(2)=.05*E_MAT(8,8)
      STEP(3)=.05*.15*SQRT(ABS(BLEP(1,3)+BLEP(2,3)))
      STEP(4)=.001
      STEP(5)=.001
      STEP(6)=.05
      DO I=1,NPARM
        IF(I.LT.NPARM)THEN
          CALL MNPARM(I,PARNAM(I),XP(I),STEP(I),0.,0.,IER)
        ELSE
          XP(I)=ZMASS_OBS
          STEP(I)=0.05
          CALL MNPARM(I,PARNAM(I),XP(I),STEP(I),0.,0.,IER)
        ENDIF
        IF(FIX_ZMASS)THEN
          ARGLIS(1)=6.
          CALL MNEXCM(FCN,'FIX',ARGLIS,1,IER)
        ENDIF
        IF(IER .NE. 0)THEN
          WRITE(LUNIT,'(A,I8)') 'UNABLE TO DEFINE PARAMETER NO,', I
          STOP
        ENDIF
      ENDDO
C
      CALL MNSETI(' Fitting two prong decays')
C
      ARGLIS(1)=PRINT_LEV
      CALL MNEXCM(FCN,'SET PRINT',ARGLIS,1,IER)
C
      CALL MNEXCM(FCN,'MINI',0.,0,IER,0)
C
C        CALL MNEXCM(FCN,'MINOS',3.,1,IER)
C
C ****  PRINT OUT CURRENT PARAMETER VALUES AND ERRORS
C
        CALL MNPOUT(6,CHNAM(6),XP(6),ERR,BND1,BND2,IVAR)
        CALL MNSTAT(FMIN,FEDM,ERR,NPARI,NPARX,ISTAT)
        WRITE(6,*)' FMIN=',FMIN,' FEDM=',FEDM
      IF(WRITE_FIT2_FILE)THEN
        WRITE(LUNIT,*)' RUN= ',RUN,' EVENT=',EVENT
        WRITE(LUNIT,*)' FMIN=',FMIN,' FEDM=',FEDM
      ENDIF
      ARGLIS(1)=10
      CALL MNEXCM(FCN,'END',0,0,IER)
      CALL MNEXCM(FCN,'CALL FCN',ARGLIS,1,IER)
C
C ****  Store FIT2 bank parameters
C
      DO I=1,50
        FIT2PAR(I)=0.
      ENDDO
      FIT2PAR(1)=1
C
C ****  LEPTON ID
C
      IF(DO_ELECTRONS) FIT2PAR(2)=1
      IF(DO_MUONS)     FIT2PAR(2)=2
      FIT2PAR(3)=2    !NUMBER OF DEGREES OF FREEDOM
      IF(BW_CONSTR.OR.BW_CONSTR_RES.OR.BW_CONSTR_SPC)FIT2PAR(3)=3
      FIT2PAR(4)=FMIN
C
      DO I=1,6
        FIT2PAR(I+4)=XM(I)
      ENDDO
      P1=0.
      DO I=1,3
        FIT2PAR(I+10)=XVAL_SAVE(I)
        P1=P1+XVAL_SAVE(I)**2.
      ENDDO
      IF(P1.GT.0)THEN
        P1=SQRT(P1)
      ELSE
        P1=0
      ENDIF
      P2=0.
      DO I=1,3
        FIT2PAR(I+13)=XVAL_SAVE(I+3)
        P2=P2+XVAL_SAVE(I+3)**2.
      ENDDO
      IF(P2.GT.0)THEN
        P2=SQRT(P2)
      ELSE
        P2=0.
      ENDIF
      FIT2PAR(17)=XP(1)
      FIT2PAR(18)=XP(2)
      FIT2PAR(19)=XVAL_SAVE(7)
      FIT2PAR(20)=XVAL_SAVE(8)
      DO I=1,3
        FIT2PAR(I+20)=E_MAT(1,I)
        FIT2PAR(I+26)=E_MAT(4,I)
      ENDDO
      FIT2PAR(24)=E_MAT(2,2)
      FIT2PAR(25)=E_MAT(2,3)
      FIT2PAR(26)=E_MAT(3,3)
      FIT2PAR(30)=E_MAT(5,5)
      FIT2PAR(31)=E_MAT(5,6)
      FIT2PAR(32)=E_MAT(6,6)
      FIT2PAR(33)=E_MAT(7,7)
      FIT2PAR(34)=E_MAT(7,8)
      FIT2PAR(35)=E_MAT(8,8)
      FIT2PAR(36)=(MET(1)**2.+MET(2)**2.)
      IF(FIT2PAR(36).GT.0)THEN
        FIT2PAR(36)=SQRT(FIT2PAR(36))
      ELSE
        FIT2PAR(36)=-1.0
      ENDIF
      FIT2PAR(37)=MASS
C
      FITMASS=(P1+P2)**2.-(FIT2PAR(11)+FIT2PAR(14))**2.
     &                   -(FIT2PAR(12)+FIT2PAR(15))**2.
     &                   -(FIT2PAR(13)+FIT2PAR(16))**2.
C
C ****  For Z to mumu
C
      IF(DO_MUONS) THEN
        P1 = 1/XVAL_SAVE(1)
        P2 = 1/XVAL_SAVE(4)
        PXL1 = P1*SIN(XVAL_SAVE(2))*COS(XVAL_SAVE(3))
        PYL1 = P1*SIN(XVAL_SAVE(2))*SIN(XVAL_SAVE(3))
        PZL1 = P1*COS(XVAL_SAVE(2))
        PXL2 = P2*SIN(XVAL_SAVE(5))*COS(XVAL_SAVE(6))
        PYL2 = P2*SIN(XVAL_SAVE(5))*SIN(XVAL_SAVE(6))
        PZL2 = P2*COS(XVAL_SAVE(5))
C
        FITMASS = (P1+P2)**2. - (PXL1+PXL2)**2.
     &          -(PYL1+PYL2)**2. - (PZL1+PZL2)**2.
      ENDIF
      IF ( FITMASS.GT.0 ) THEN
        FITMASS=SQRT(FITMASS)
      ELSE
        FITMASS=0.
      ENDIF
      FIT2PAR(38)=FITMASS
      FIT2PAR(39)=ISTAT
C
C ****  Get error matrix for parameters from Minuit
C
      IF(ISTAT.NE.3)GOTO 800
      CALL MNEMAT(EMAT,NPARM)
      CALL LSQ_MATRIX_FILL('PERR_MAT',EMAT,NPARM,NPARM,2,IER)
      CALL LSQ_MATRIX_TRANSPOSE('DER_MAT','DTR_MAT',IER)
      CALL LSQ_MATRIX_MULTIPLY('PERR_MAT','DTR_MAT','INT_MAT',IER)
      CALL LSQ_MATRIX_MULTIPLY('DER_MAT','INT_MAT','RES_MAT',IER)
      CALL LSQ_MATRIX_ADD('ERR_MATRIX','RES_MAT','H_MAT',1.,-1.,IER)
      CALL LSQ_MATRIX_GET('H_MAT',H_MAT,NOBS,NOBS,2,IER)
C      CALL LSQ_MATRIX_GET('ERR_MATRIX',H_MAT,NOBS,NOBS,2,IER)
      DO I=1,NOBS
        XVAL1=XM(I)
        XVAL2=XVAL_SAVE(I)
        DENR=SQRT(ABS(H_MAT(I,I)))
        IF(DENR.NE.0)THEN
          STRETCH_FUNC=(XVAL1-XVAL2)/DENR
        ELSE
          STRETCH_FUNC=-100.
        ENDIF
        FIT2PAR(39+I)=STRETCH_FUNC
      ENDDO
      FIT2PAR(48)=PROB(FMIN,2)
      IF(BW_CONSTR.OR.BW_CONSTR_RES.OR.BW_CONSTR_SPC.OR.FIX_ZMASS)
     &  FIT2PAR(48)=PROB(FMIN,3)
C
  800 CONTINUE
      LPROC=0
      IF(WRITE_FIT2_BANK)THEN
        CALL BKFIT2(LPROC,LFIT2)
        CALL FIT2FL(LFIT2,FIT2PAR)
      ENDIF
C
      CALL LSQ_MATRIX_DELETE('ERR_MATRIX',IER)
      CALL LSQ_MATRIX_DELETE('INV_MATRIX',IER)
      CALL LSQ_MATRIX_DELETE('B_MAT1',IER)
      CALL LSQ_MATRIX_DELETE('A_MAT',IER)
C
      IF(WRITE_NTUPLE)CALL FIT2_NTUPLE(IER)
  900 CONTINUE
  999 RETURN
      END
