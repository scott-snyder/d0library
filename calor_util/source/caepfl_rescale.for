      SUBROUTINE CAEPFL_RESCALE(RCP_FILE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : OVERWRITE NEW CAEP BANK FROM EXISTING ONE
C-                         Uses current CSFW/CSFC bank structure to fix
C-                         cell energies in CAEP. The old energy scale is
C-                         divided out by using the OLD_CAEP_CSF_RCP file
C-                         to point to the CSF_RCP which was used to generate
C-                         the original CAEP bank.
C-
C-   Inputs  : RCP_FILE [C] RCP (CAHITS.RCP) command list OLD_CAEP_CSF_RCP
C-   Outputs : new CAEP
C-   Controls: RCP_FILE
C-
C-   Created   1-APR-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_FILE
      INTEGER IER
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      LOGICAL EZERR,CEXIST,FIRST,DO_CONGEV,TCOR
      INTEGER I,LCSFH,GZCSFH,LCSFW,GZCSFW,CAL_MODULE
      INTEGER LOC,NA,IA,IMOD,JMOD,LMOD,ILYR,IETA,ETALIM(2),LYRLIM(2)
      INTEGER IETA0,NETA,ILYR0,NLYR,IW0,IW,ICSFW,NCH,POINT,IPHI
      INTEGER LCSFC,GZCSFC,ICSFC,LCSFC1,LZFIND
      CHARACTER OLD_CSF_RCP*80,PARAM*32,MSG*80,MODULE*4
      REAL    A(10),W(1:NETAL),AWC
      INTEGER PAKADR,TRULEN,LENF,GZCAEP
      BYTE    BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
      SAVE FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
C
C ****  FETCH CONSTANTS FROM RCP
C
        CALL EZPICK(RCP_FILE)
        IF ( .NOT.EZERR(IER)) THEN
          CALL EZGET_l ('CSF_CORRECTIONS',   TCOR,IER)
          CALL EZGETS('OLD_CAEP_CSF_RCP',1,OLD_CSF_RCP,LENF,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG(' WRONG_CAHITS_RCP','CAEPFL_RECALE',
     &        ' NEED OLD_CAEP_CSF_RCP','W')
            IER = -1
            CALL EZRSET
            GOTO 999
          END IF
          CALL EZGET_l('DO_ADC_TO_GEV',DO_CONGEV,IER)
        END IF
        CALL EZRSET
        CALL EZLOC('CSF_RCP',LOC)
        IF (LOC.GT.0) CALL EZDROP('CSF_RCP')
        CALL INRCP(OLD_CSF_RCP,IER)
        CALL ERRMSG('OLD_CAEP_CSF_RCP','CAEPFL_RESCALE',OLD_CSF_RCP,'I')
        CALL EZPICK('CSF_RCP')
        CALL EZGET('A',A,IER)  !  CCEM ECEM CCMG ICD  ECMG
        IF(IER.NE.0) GOTO 1999
        CALL EZGET_SIZE('A',NA,IER)  !  NUMBER OF MODULES
        IF (NA.NE.10) THEN
          MSG = ' NEED 10 VALUES IN A '
          CALL ERRMSG('A_WRONG','CAEPFL_RESCALE',MSG,'W')
          IER = -6
          GOTO 1999
        END IF
        LCSFH = GZCSFH ()
        DO IA = 1, 10
          C(LCSFH+1+IA) = C(LCSFH+1+IA)/A(IA)
        END DO
C
C ****  LOOP OVER CELLS
C
        LMOD = 0
        DO ILYR = 1, NLYRL
          DO IETA = 1, NETAL
            IF(IETA.EQ.0) LMOD = 0
            IF(CEXIST(IETA,1,ILYR).OR.CEXIST(-IETA,1,ILYR)) THEN
              IMOD = CAL_MODULE(IETA,ILYR,MODULE)
              IF(IMOD.EQ.0) IMOD = CAL_MODULE(-IETA,ILYR,MODULE)
              IF(IMOD.EQ.0) THEN
                 WRITE(MSG,101)IETA,ILYR
  101            FORMAT(1X,' NO MODULE FOR ETA ',I3,' LYR ',I3)
                 CALL ERRMSG('CAL_MODULE_ERR','CAEPFL_RESCALE',MSG,'W')
                 IER = -3
                 GOTO 1999
              END IF
              JMOD = IMOD + 100*ILYR
              IF(JMOD.NE.LMOD) THEN
                LMOD = JMOD
C
C ****  FETCH CSF.RCP STUFF
C
                CALL CAL_MODULE_LIMITS(IMOD,ETALIM,LYRLIM)
                IETA0=ETALIM(1)
                NETA=ETALIM(2)-ETALIM(1)+1
                ILYR0=LYRLIM(1)
                NLYR=LYRLIM(2)-LYRLIM(1)+1
                PARAM = 'W_'//MODULE(1:TRULEN(MODULE))
C
C **** W INDEXED IN ILYR FOR CC,EC, W INDEXED IN IETA FOR MG,ICD
C
                IW0 = ILYR0  ! ILYR0 FOR  CC,EC
                IF ( (IMOD.GE.3).AND.(IMOD.LE.5) ) IW0=ETALIM(1) ! MG,ICD
                CALL EZGET(PARAM,W(IW0),IER)
                IF(IER.NE.0) GOTO 1999
              ENDIF
            END IF
C  W INDEXED IN ILYR FOR CC,EC, W INDEXED IN IETA FOR MG,ICD
            IW = ILYR
            IF ( (IMOD.GE.3).AND.(IMOD.LE.5) ) IW = ABS(IETA)
            ICSFW =ILYR+(IABS(IETA)-1)*NLYRL  ! 1 to 629
            LCSFW = GZCSFW ()
            IF((A(IMOD)*W(IW)).GT.0)
     &        C(LCSFW+1+ICSFW)
     &        = C(LCSFW+1+ICSFW)/ (A(IMOD)*W(IW))
          END DO
        END DO
        CALL EZRSET
      END IF
C
      CALL DROP_OLD_PNUTS   !drop old pnut banks
C
      IER = 0
      LCAEP = GZCAEP ()
      NCH = IQ(LCAEP+3)                    ! Number of cells with data
      IF((LCAEP.LE.0).OR.(NCH.EQ.0)) THEN
        IER = -1
        GOTO 999
      END IF
      POINT = 3
      LCSFW = GZCSFW ()
      IF(LCSFW.LE.0) THEN
        IER = -2
        GOTO 999
      END IF
      LCSFC = GZCSFC ()
      IF(LCSFC.LE.0) THEN
        IER = -2
        GOTO 999
      END IF
      DO 1001 I = 1, NCH                    ! Loop over cells
        PAKADR = IQ(LCAEP+POINT+1)
        ILYR  = BYTES(BYTE2)
        IPHI  = BYTES(BYTE3)
        IETA  = BYTES(BYTE4)
        ICSFW =ILYR+(IABS(IETA)-1)*NLYRL  ! 1 to 629
        IF((LCSFC.GT.0).AND.TCOR) LCSFC1 = LZFIND(IDVSTP,LCSFC,ILYR,2)
C
C ****  Check crate validity range here
C
        ICSFC = IPHI+(IETA+NETAL)*NPHIL
        IF(LCSFC1.GT.0) THEN
          AWC = C(LCSFW+1+ICSFW) * C(LCSFC1+2+ICSFC) !sampling weights
        ELSE
          AWC = C(LCSFW+1+ICSFW)  !sampling weights
        END IF
        Q(LCAEP+POINT+2)=Q(LCAEP+POINT+2)*AWC
        IF(.NOT.DO_CONGEV) PAKADR=IBSET(PAKADR,5)  ! not converted to GeV
        IF(DO_CONGEV)      PAKADR=IBCLR(PAKADR,5)  ! converted to GeV
        IQ(LCAEP+POINT+1) =PAKADR
        POINT = POINT + 2
 1001 CONTINUE
  999 RETURN
 1999 CALL EZRSET
      RETURN
      END
