      SUBROUTINE GET_NEEP(IP,ID_OBJECT,NUMF,ET,ETA,PHI,POS1,POS2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To collect from databanks the number of
C-                         desired objects, an array of their ET,
C-                         ETA, and PHI, and the positions in the
C-                         arrays of the leading 2 objects in ET
C-   Inputs  : IP - the parameter set number
C-             ID_OBJECT - the object type
C-   Outputs : NUMF - the number of objects
C-             ET, ETA, PHI
C-             POS1, POS2 - index positions of the leading 2 objects
C-   Controls:
C-
C-   Created  20-AUG-1993   Kathy Fatyga
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:L2_MASSCUT_CUTS.INC'
C
C
      REAL ET(NMAX),ETA(NMAX),PHI(NMAX),ETLEAD1,ETLEAD2
      REAL ETASIZE,PHISIZE,EMET,ETA_DET
C
      INTEGER IP,ID_OBJECT,NUMF,POS1,POS2,NGOT,IER,J,I
      INTEGER IDUM,L1ETA,L1PHI,IFLAG,JETP
      INTEGER L2JETS_CURRENT_ESUM_OBJECT,NFOUND(ID_ALL:LAST_TYPE)
      INTEGER L2JETS_CURRENT_PARAM
C
      LOGICAL START
C
      CHARACTER*80 MSG
C
C----------------------------------------------------------------------
C
      ETLEAD1=0.0
      ETLEAD2=0.0
      POS1=0
      POS2=0
C
C object choice is a  photon or electron
C
      IF(ID_OBJECT.EQ.ID_ELECTRON.OR.ID_OBJECT.EQ.ID_PHOTON)THEN
        CALL GTL2EM_COUNT(CURRENT,NGOT,IER)
        IF (IER.NE.0) THEN
          WRITE(MSG,'(A,I)')'IER From GTL2EM_COUNT = ',IER
          CALL ERRMSG('GTL2EM_COUNT','GET_NEEP',MSG,'W')
        ENDIF
        J=1
        DO 50 I=1,NGOT
C   ET corrected for vertex, cluster center, leakage
          CALL GTL2EM_VALUE(I,35,PASSED,ET(J),IDUM,IER)
          IF(IER.NE.0) GOTO 50
C   ETA is corrected for the vertex
          CALL GTL2EM_VALUE(I,37,PASSED,ETA(J),IDUM,IER)
          IF(IER.NE.0) GOTO 50
C   PHI is the corrected detector phi
          CALL GTL2EM_VALUE(I,31,PASSED,PHI(J),IDUM,IER)
          IF(IER.NE.0) GOTO 50
          IF(ET(J).GT.ETLEAD1)THEN
            ETLEAD2=ETLEAD1
            POS2=POS1
            ETLEAD1=ET(J)
            POS1=J
          ELSEIF(ET(J).GT.ETLEAD2)THEN
            ETLEAD2=ET(J)
            POS2=J
          ENDIF
          J=J+1
          IF(J.GT.NMAX)THEN
            NUMF=999
            GOTO 999
          ENDIF
   50   CONTINUE
        NUMF=J-1
C
C   object choice is a  jet
C
      ELSEIF(ID_OBJECT.EQ.L2JETS_CURRENT_ESUM_OBJECT())THEN
        START=.TRUE.
        IER=0
        J=1
C        Get JAUX bank param set number
        JETP=L2JETS_CURRENT_PARAM()
        DO WHILE (IER.EQ.0)
          CALL GTJAUX(START,-1.0,JETP,ET(J),ETA(J),PHI(J),L1ETA,L1PHI,
     &          ETASIZE,PHISIZE,EMET,IER)
          IF(IER.EQ.0)THEN
            IF(ET(J).GT.ETLEAD1)THEN
              ETLEAD2=ETLEAD1
              POS2=POS1
              ETLEAD1=ET(J)
              POS1=J
            ELSEIF(ET(J).GT.ETLEAD2)THEN
              ETLEAD2=ET(J)
              POS2=J
            ENDIF
            J=J+1
            IF(J.GT.NMAX)THEN
              NUMF=999
              GOTO 999
            ENDIF
          ENDIF
        ENDDO
        NUMF=J-1
C
C    object choice is a  muon
C
      ELSEIF(ID_OBJECT.EQ.ID_MUON)THEN
        CALL GTESUM_COUNTS('FILT',NFOUND,IER)
        IF (IER.NE.0) THEN
          WRITE(MSG,'(A,I)')'IER From GTESUM_COUNTS = ',IER
          CALL ERRMSG('GTESUM_COUNTS','GET_NEEP',MSG,'W')
        ENDIF
        NUMF = NFOUND(ID_OBJECT)
        IF(NUMF.GT.NMAX) THEN
          NUMF=999
          GOTO 999
        ENDIF
        DO J=1,NUMF
          CALL GTESUM('FILT',ID_OBJECT,J,ET(J),ETA(J),ETA_DET,PHI(J),
     &       IFLAG,IER)
          IF(ET(J).GT.ETLEAD1)THEN
            ETLEAD2=ETLEAD1
            POS2=POS1
            ETLEAD1=ET(J)
            POS1=J
          ELSEIF(ET(J).GT.ETLEAD2)THEN
            ETLEAD2=ET(J)
            POS2=J
          ENDIF
        ENDDO
      ENDIF
  999 RETURN
      END
