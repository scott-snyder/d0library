      SUBROUTINE CSFBUILD(MAIN_RCP_FILE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the CSF bank structure which contains the
C-   sampling fraction weights adn corrections contained in CSF_RCP.
C-   CSF banks contain the conversion factor from calorimeter cell ADC
C-   counts to total energy for every cell in the CC, ECN, ECS, ICD
C-   and Massless Gaps. See D0$STP$CAL:CSFMAKE.DOC for more detail.
C-
C-   Inputs  : MAIN_RCP_FILE [C] RCP FILE 
C-   Outputs : IER [I] 0= OK
C-   Controls: CSF_RCP
C-
C-   Created   2-MAR-1992   Harrison B. Prosper, Chip Stewart
C-   Updated   5-FEB-1994   Chip Stewart  - EZLOC/EZDROP fix 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      LOGICAL CEXIST,RENORM,LCORRECT,KEEP_CORRECT(NLYRL),LMSG,LMSG1
      INTEGER IETA,IPHI,ILYR,IMOD,LMOD,CAL_MODULE,TRULEN,JMOD,IER
      INTEGER GZCSFH,GZCSFC,GZCSFW,LCSFH,LCSFC,LCSFW,NCSFC,NCSFW
      INTEGER IETA0,ILYR0,I,J,IW,IW0,ETALIM(2),LYRLIM(2),NETA,NLYR
      INTEGER LOC,NA,LFILE,LZLOC,NCSFH,LZFIND
      REAL    CORRECT(1600)
      REAL    W(1:NETAL),ETAPHI(2),A(10)
      CHARACTER*(*) MAIN_RCP_FILE,MAIN_RCP
      PARAMETER( MAIN_RCP = 'CSF_RCP' )
      CHARACTER MODULE*4,MODULE_RCP*32,PARAM*32,MSG*80,FILENAME*132
      CHARACTER SRC*4,MODULE_RCPFILE*132,MRCP(10)*32
      DATA KEEP_CORRECT/NLYRL*.FALSE./,LMSG/.FALSE./
C----------------------------------------------------------------------
      IER = 0
C
C ****  BOOK CSF BANK STRUCTURE
C
      LCSFH = LZLOC( IDVSTP, 'CSFH', NCSFH )
      IF (LCSFH.GT.0) THEN
        CALL ERRMSG('NO_REBUILD_CSF','CSFBUILD',
     &    ' EXISTING CSF STRUCTURE NOT OVERWRITTEN ','W')
        IER = -5
        GOTO 999
      END IF
      CALL STP_GSLINK('CSFBUILD',NCSFW )
      CALL STP_GSLINK('CSFBUILD',NCSFC )
      CALL BKCSFH(LCSFH)
      CALL BKCSFW(0,LCSFW)
      STP_LSLINK(NCSFW) = GZCSFW ()
C
C ****  GET OVERALL CONVERSION 'A' : CHARGE to TOTAL ENERGY
C
      CALL EZLOC(MAIN_RCP,LOC)
      IF (LOC.GT.0) CALL EZDROP(MAIN_RCP)
      CALL INRCP(MAIN_RCP_FILE,IER)
      CALL EZPICK(MAIN_RCP)
      CALL EZGET('A',A,IER)  ! CCEM ECEM CCMG ICD ECMG CCFH ECIH ECMH CCCH ECOH
      IF(IER.NE.0) GOTO 1999
      CALL EZGETS('DATA_SOURCE',1,SRC,LOC,IER)   ! D0,TB,MIX,PLT
      CALL EZGET_SIZE('A',NA,IER)  !  NUMBER OF MODULES
      IF (NA.NE.10) THEN  
        MSG = ' NEED 10 VALUES IN A '
        CALL ERRMSG('A_WRONG','CSFBUILD',MSG,'W')
        IER = -6
        GOTO 1999
      END IF
      CALL EZRSET
      LCSFH = GZCSFH ()
      CALL UCOPY(A,C(LCSFH+2),10)
C
C ****  LOOP OVER CELLS
C
      LMOD = 0
      DO ILYR = 1, NLYRL
        DO IETA = -NETAL, NETAL
          IF(IETA.EQ.0) LMOD = 0
          IF(CEXIST(IETA,1,ILYR)) THEN
            IMOD = CAL_MODULE(IETA,ILYR,MODULE)
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
              CALL EZPICK(MAIN_RCP)
              PARAM = 'W_'//MODULE(1:TRULEN(MODULE))
C
C **** W INDEXED IN ILYR FOR CC,EC, W INDEXED IN IETA FOR MG,ICD
C
              IW0 = ILYR0  ! ILYR0 FOR  CC,EC
              IF ( (IMOD.GE.3).AND.(IMOD.LE.5) ) IW0=ETALIM(1) ! IETA FOR MG,ICD
              CALL EZGET(PARAM,W(IW0),IER)
              IF(IER.NE.0) GOTO 1999
              PARAM = 'SPOT_'//MODULE(1:TRULEN(MODULE))
              CALL EZGET(PARAM,ETAPHI,IER)
              RENORM = IER.EQ.0
              PARAM = 'CORRECT_'//MODULE(1:TRULEN(MODULE))
              CALL EZGET_l(PARAM,LCORRECT,IER)
              LCORRECT = LCORRECT .AND. (IER.EQ.0)
              CALL EZRSET
              IF(LMSG) THEN
                IF(IETA.LT.0) THEN
                  WRITE(MSG,101)MODULE(1:4),'NORTH',ILYR
                ELSE
                  WRITE(MSG,101)MODULE(1:4),'SOUTH',ILYR
                END IF
  101           FORMAT(' BUILDING CSFW FOR ',2A6,' LYR ',I2.2)
                CALL INTMSG(MSG)
              END IF
              MRCP(IMOD) = 'CSF_'//MODULE(1:TRULEN(MODULE))//'_RCP'
              IF (LCORRECT) THEN
C
C ****  FETCH CSF_'MODULE'_RCP
C
                KEEP_CORRECT(ILYR)=.TRUE.
                MODULE_RCP = MRCP(IMOD)
                CALL EZLOC(MODULE_RCP,LOC)
                IF (LOC.EQ.0) THEN
                  IF(SRC.EQ.'D0') THEN
                    MODULE_RCPFILE = 'D0$CALOR_OFF:'
     &                //'CSF_'//MODULE(1:TRULEN(MODULE))//'.RCP'
                  ELSE
                    MODULE_RCPFILE = 'D0$CALOR_OFF:'//SRC(1:TRULEN(SRC))
     &                //'_CSF_'//MODULE(1:TRULEN(MODULE))//'.RCP'
                  END IF
                  CALL TRNLNM(MODULE_RCPFILE,FILENAME,LFILE)
                  MSG = ' Reading file '//FILENAME(1:LFILE)
                  CALL INTMSG(MSG)
                  CALL INRCP(MODULE_RCPFILE,IER)
                  IF ( IER .NE. 0 ) THEN
                    MSG = ' Cannot open file '//FILENAME(1:LFILE)
                    CALL INTMSG(MSG)
                    CALL ERRMSG('BADOPEN','INRCP',MSG,'F')
                  ENDIF
                END IF
                CALL EZPICK(MODULE_RCP)
                IF(IETA.LT.0) THEN
                  WRITE(PARAM,100)'NORTH',ILYR
                ELSE
                  WRITE(PARAM,100)'SOUTH',ILYR
                END IF
  100           FORMAT('CORRECTION_',A5,'_LYR_',I2.2)
                CALL EZGET(PARAM,CORRECT,IER)
                IF(IER.NE.0) GOTO 1999
                CALL EZRSET
C
C ****  RE_NORMALIZE CORRECTION ARRAY RELATIVE TO ETA & PHI
C
                IF (RENORM)  THEN
                  CALL ERRMSG('RE_NORMALIZE_CORRECT_ARRAY',
     &              'CSF_MAKE_STPFILE','NOT YET DONE','S')
                ENDIF
              ENDIF
C
C ****  BOOK CSFC BANK IF NEEDED
C
              LCSFC = GZCSFC ()
              IF (LCSFC.EQ.0) THEN
                CALL BKCSFC(0,LCSFC)
                STP_LSLINK(NCSFC) = LCSFC
                IC(STP_LSLINK(NCSFC)+2) = ILYR
              ELSE
                LCSFC = LZFIND(IDVSTP,LCSFC,ILYR,2)
                IF (LCSFC.EQ.0) THEN
                  CALL BKCSFC(0,LCSFC)
                  STP_LSLINK(NCSFC) = LCSFC
                  IC(STP_LSLINK(NCSFC)+2) = ILYR
                ELSE
                  STP_LSLINK(NCSFC) = LCSFC
                END IF
              END IF
            END IF
C  W INDEXED IN ILYR FOR CC,EC, W INDEXED IN IETA FOR MG,ICD
            IW = ILYR
            IF ( (IMOD.GE.3).AND.(IMOD.LE.5) ) IW = ABS(IETA)
C
C ****  LOOP OVER PHI
C
            DO IPHI = 1, NPHIL
              IF(CEXIST(IETA,IPHI,ILYR) ) THEN
C
C ****  DETERMINE INDEX INTO CSFC BANK & MULTIPLY A*W*C
C
                I =IPHI+(IETA+NETAL)*NPHIL  ! 1 to 4800
                J =ABS(IETA)-IETA0+1+(IPHI-1)*NETA  !ETA and PHI MAP FOR CORRECT
                IF (LCORRECT) THEN
                  C(STP_LSLINK(NCSFC)+2+I) = CORRECT(J)
                ELSE
                  C(STP_LSLINK(NCSFC)+2+I) = 1.0
                END IF
              END IF
            END DO
            I =ILYR+(IABS(IETA)-1)*NLYRL  ! 1 to 629
            C(STP_LSLINK(NCSFW)+1+I) = A(IMOD)*W(IW)
          END IF
        END DO
      END DO
      DO ILYR = 1, NLYRL
        LCSFC = GZCSFC ()
        LCSFC = LZFIND(IDVSTP,LCSFC,ILYR,2)
        IF((LCSFC.GT.0).AND.(.NOT. KEEP_CORRECT(ILYR))) THEN
          IF(LMSG) THEN
            WRITE(MSG,901)ILYR
  901       FORMAT (' DROPPING UNUSED CSFC BANK FOR LYR ',I2.2)
            CALL INTMSG(MSG)
          END IF
          CALL MZDROP(IXSTP,LCSFC,' ')
        END IF
      END DO
      DO IMOD = 1, 10
        MODULE_RCP = MRCP(IMOD)
        CALL EZLOC(MODULE_RCP,LOC)
        IF(LOC.GT.0)CALL EZDROP(MODULE_RCP)
      END DO
  999 RETURN
 1999 CONTINUE
      CALL EZRSET
      CALL ERRMSG('CSF RCP ERROR','CSFBUILD',MAIN_RCP_FILE,'S')
 2999 RETURN
C#######################################################################
      ENTRY CSFBUILD_MSG(LMSG1) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TURN ON MESSAGES FROM CSFBUILD
C-
C----------------------------------------------------------------------
      LMSG = LMSG1
C----------------------------------------------------------------------
      END
