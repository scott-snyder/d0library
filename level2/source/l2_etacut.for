      SUBROUTINE L2_ETACUT( PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,
     &  EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TOOL to cut on  eta and on delta eta
C-
C-   Inputs  : PARAM_SET_NUMBER : # of parameter set to use
C-             HARDWARE:          mask of set bits for LV1 trigger which started
C-                                  this filter.
C-   Outputs : RESULT_FLAG :      Flag set to TRUE when we want to pass tool
C-                                  under this PARAM_SET_NUMBER
C-             EXTRA_FLAG  :      Set to TRUE when we want to pass event and
C-                                  do no further filtering. (NOT IMPLEMENTED)
C-   Controls:
C-
C-   Created  3-NOV-1992   Andrew G. Brandt
C-   Updated  7-DEC-1992   Meenakshi Narain  Modify for Electrons/photons
C-                                            Use L2EM banks rather than ESUM
C-                                            for these objects
C-   Updated  7-DEC-1992   Andrew G. Brandt  Add -GAPCUT option for muons
C-   Updated  9-JAN-1993   Andrew G. Brandt  Add option of detector eta
C-   Updated  29-JUL-1993   James T. Linnemann   uset GT routines for L2EM
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARAM_SET_NUMBER,HARDWARE
      LOGICAL EXTRA_FLAG,RESULT_FLAG
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra main store
      CHARACTER*80 MSG
C
C Tool parameters
C
      CHARACTER*64 OBJECT
      INTEGER NUM_FOUND
      REAL    ETMIN
      REAL    GAPCUT,ABS_ETA_MIN,ABS_ETA_MAX
C
      INTEGER IP,NPARIN,IER
      LOGICAL EZERROR,OK
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INTEGER NGOT,NCHR,NPASSED
      INTEGER NFOUND(ID_ALL:LAST_TYPE),ID,IFLAG
      REAL ET,ETA_PHYS,ETA_DET,THETA,PHI
      INTEGER I,J,N
      INTEGER NMAX
      PARAMETER(NMAX = 40)    !be generous
      INTEGER IORDER(NMAX),WORK(NMAX)
C
      INTEGER ID_OBJECT, NETA
      REAL ETA_OBJ(NMAX),ETA_OBJ_PASS(NMAX),DELETA,FDUM
      INTEGER L2JETS_CURRENT_ESUM_OBJECT,IDUM,IERR
      LOGICAL CURRENT,PASSED
      PARAMETER( CURRENT = .TRUE.  )  !only current parameter set
      PARAMETER( PASSED = .TRUE. )    !only passed events
C
      LOGICAL OBJECT_HAS
      INTEGER TRULEN
      CHARACTER*6 MUO,PHO,ELE,JET,SUB,ETADET
      PARAMETER(ETADET='ETADET')
      PARAMETER(MUO='MUO')
      PARAMETER(PHO='PHO')
      PARAMETER(ELE='ELE')
      PARAMETER(JET='JET')
C----------------------------------------------------------------------
C...statement function returns true if substring SUB found in OBJECT
      OBJECT_HAS(SUB) = INDEX(OBJECT,SUB(1:TRULEN(SUB))).NE.0
C----------------------------------------------------------------------

C
C Initialize logicals
C
      EXTRA_FLAG = .FALSE.
      RESULT_FLAG = .FALSE.
C
C First, carefully retrieve cuts from RCP
C
      IP = PARAM_SET_NUMBER
      CALL EZPICK('L2_ETACUT') ! downloaded from configuration file (.FILTs)
      OK = .NOT.EZERROR(IER)
      IF (OK) THEN
C
C Is IP consistent with the number of sets which exist?
C
        IF (IER .EQ. 0) CALL EZGET('NUMBER_OF_SETS',NPARIN,IER)
        IF (IER.EQ.0) THEN
          IF ((IP.LE.0).OR.(IP.GT.NPARIN)) THEN
            WRITE(MSG,'(A,I5,A,I5)')' parameter set requested = ',
     &        PARAM_SET_NUMBER, ' but only had ',NPARIN
            CALL ERRMSG('L2_ETACUT','L2_ETACUT',MSG,'F')
            GO TO 999
          ENDIF
C
C Get the parameters from the IPth set
C
          IF (IER.EQ.0) CALL EZGETS('OBJECT',IP,OBJECT,NCHR,IER)
          IF (IER.EQ.0) CALL EZGETA('NUM_FOUND',IP,IP,1,NUM_FOUND,IER)
          IF (IER.EQ.0) CALL EZGETA('ETMIN',IP,IP,1,ETMIN,IER)
          IF (IER.EQ.0) CALL EZGETA('GAPCUT',IP,IP,1,GAPCUT,IER)
          IF (IER.EQ.0)
     &      CALL EZGETA('ABS_ETA_MIN',IP,IP,1,ABS_ETA_MIN,IER)
          IF (IER.EQ.0)
     &      CALL EZGETA('ABS_ETA_MAX',IP,IP,1,ABS_ETA_MAX,IER)
        ENDIF
      ENDIF
      IF (.NOT.OK) THEN
        CALL ERRMSG('L2_ETACUT','L2_ETACUT','Couldn''t find bank','F')
      ELSEIF (IER .NE. 0) THEN      ! Error reading RCP
        CALL ERRMSG('L2_ETACUT','L2_ETACUT_PARAMETERS',
     &          'Couldn''t find parameter','F')
      ENDIF
C
C Use ESUM bank as basis of tool
C
      CALL GTESUM_COUNTS('FILT',NFOUND,IER)
      IF (IER.NE.0) THEN
        WRITE(MSG,'(A,I)')'IER From GTESUM_COUNTS = ',IER
        CALL ERRMSG('GTESUM_COUNTS','L2_ETACUT',MSG,'W')
      ENDIF
C
C Get type of object
C
      IF(OBJECT_HAS(JET)) THEN
        ID_OBJECT=L2JETS_CURRENT_ESUM_OBJECT()
      ELSE IF(OBJECT_HAS(MUO)) THEN
        ID_OBJECT=ID_MUON
      ELSE IF(OBJECT_HAS(ELE)) THEN
        ID_OBJECT=ID_ELECTRON
      ELSE IF(OBJECT_HAS(PHO)) THEN
        ID_OBJECT=ID_PHOTON
      ELSE
        CALL ERRMSG('L2_ETACUT','L2_ETACUT','Object type is not
     +              a jet muon electron or photon' ,'F')
      END IF
C
C Get number of found objects
C
      NGOT = NFOUND(ID_OBJECT)
      IF (ID_OBJECT.EQ.ID_ELECTRON.OR.ID_OBJECT.EQ.ID_PHOTON) THEN
        CALL GTL2EM_COUNT(CURRENT,NGOT,IER)
        IF (IER.NE.0) THEN
          WRITE(MSG,'(A,I)')'IER From GTL2EM_COUNT = ',IER
          CALL ERRMSG('GTL2EM_COUNT','L2_ETACUT',MSG,'W')
        ENDIF
        NPASSED = 0
        DO I = 1,NGOT
          CALL GTL2EM_VALUE(I,28,PASSED,FDUM,IDUM,IER)  !IDUM has IFAILED
          IF (IER.EQ.0) NPASSED = NPASSED + 1
        ENDDO
      ENDIF
C
C Cut on number of objects found
C
      IF(NGOT.LT.NUM_FOUND) GO TO 999
      IF (ID_OBJECT.EQ.ID_ELECTRON.OR.ID_OBJECT.EQ.ID_PHOTON) THEN
        IF(NPASSED.LT.NUM_FOUND) GO TO 999
      ENDIF
C
C Sort ESUM objects  by ET
C
      CALL GTESUM_SORT('FILT',ID_OBJECT,NMAX,IORDER,WORK,IER)
      IF (IER.NE.0) THEN
C...this should only happen if you didn't have enough space reserved (NMAX)
        WRITE(MSG,'(A,I)')'IER From GTESUM_SORT (FILT) = ',IER
        CALL ERRMSG('GTESUM_SORT','L2_ETACUT',MSG,'W')
      ENDIF
      N = 0
C
C Loop over objects
C
      DO I = 1,NGOT
        IF (ID_OBJECT.EQ.ID_ELECTRON.OR.ID_OBJECT.EQ.ID_PHOTON) THEN
          IER = 0
          J = NGOT + 1 - I  !approximately reverse Et ordered
          CALL GTL2EM_VALUE(J,35,PASSED,ET,IDUM,IERR) 
          IF (IERR.NE.0) IER = IERR
          CALL GTL2EM_VALUE(J,37,PASSED,ETA_PHYS,IDUM,IERR)
          IF (IERR.NE.0) IER = IERR
          CALL GTL2EM_VALUE(J,30,PASSED,ETA_DET,IDUM,IERR)
          IF (IERR.NE.0) IER = IERR
        ELSE
          CALL GTESUM('FILT',ID_OBJECT,IORDER(I),ET,ETA_PHYS,
     &      ETA_DET,PHI,IFLAG,IER)
        ENDIF
        IF (IER.NE.0) THEN
C...this should only happen if you asked for more than was claimed
          IF (ID_OBJECT.EQ.ID_ELECTRON.OR.ID_OBJECT.EQ.ID_PHOTON) THEN
            IF (IER.NE.-4) THEN   !but allow failed candidate
              WRITE(MSG,'(A,I)')'TROUBLE IN GTL2EM_VALUE: IER',IER
              CALL ERRMSG('EM_ETACUT','L2_ETACUT',MSG,'W')
            ENDIF
          ELSE
            WRITE(MSG,'(A,I)')'IER From GTESUM = ',IER
            CALL ERRMSG('GTESUM','L2_ETACUT',MSG,'W')
          ENDIF
        ELSE
C
C Cut on ET of object
C
          IF(ET.GE.ETMIN) THEN
C
C Increment counter and save Eta
C
            N=N+1
            IF (N.GT.NMAX) THEN
              WRITE(MSG,'(A,I)')'TOO MANY OBJECTS OF TYPE ',ID_OBJECT
              CALL ERRMSG('NCAND_ETACUT','L2_ETACUT',MSG,'W')
            ELSE
              IF(OBJECT_HAS(ETADET)) THEN
                ETA_OBJ(N)=ETA_DET
              ELSE
                ETA_OBJ(N)=ETA_PHYS     !Default is physics eta
              END IF
            ENDIF
          END IF
        END IF
      ENDDO
C
C Check that the number of objects desired are within specified eta range
C where N is number of objects with ET above threshold
C Save eta of NETA objects that are within eta range
C
      NETA=0
      DO I=1,N
        IF(ABS(ETA_OBJ(I)).GE.ABS_ETA_MIN.AND.
     &    ABS(ETA_OBJ(I)).LT.ABS_ETA_MAX) THEN
          NETA=NETA+1
          ETA_OBJ_PASS(NETA)=ETA_OBJ(I)
        END IF
      END DO
C
C If not enough objects in eta range cut event
C
      IF(NETA.LT.NUM_FOUND) GO TO 999
C
C Check for gap requirement (need at least 2 objects)
C
      IF(GAPCUT.GT.0.) THEN
C
C Loop over pairs and save event if any pair gives gap greater than cut
C
        DO I=1,NETA-1
          DO J=I+1,NETA
            DELETA=ABS(ETA_OBJ_PASS(I)-ETA_OBJ_PASS(J))
            IF(DELETA.GE.GAPCUT) THEN
              RESULT_FLAG=.TRUE.
              GO TO 999
            END IF
          END DO
        END DO
      ELSE IF(GAPCUT.LT.0.) THEN
C
C Loop over pairs and save event if any pair gives gap less than cut
C
        DO I=1,NETA-1
          DO J=I+1,NETA
            DELETA=ABS(ETA_OBJ_PASS(I)-ETA_OBJ_PASS(J))
            IF(DELETA.LE.ABS(GAPCUT)) THEN
              RESULT_FLAG=.TRUE.
              GO TO 999
            END IF
          END DO
        END DO
C
C If gap not required save event that meets simple eta requirement
C
      ELSE
        RESULT_FLAG=.TRUE.
      END IF
  999 CONTINUE
      IF (OK) CALL EZRSET
      RETURN
      END
