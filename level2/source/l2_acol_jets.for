      SUBROUTINE L2_ACOL_JETS( PARAM_SET_NUMBER, HARDWARE, RESULT_FLAG,
     &  EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Level 2 TOOL to filter out events having any
C-                         jets back-to-back with the Missing Et, or
C-                         with the leading two jets (in Et) themselves
C-                         back-to-back in PHI.
C-
C-   Inputs  : PARAM_SET_NUMBER : # of parameter set to use
C-             HARDWARE:          mask of set bits for the LV1 trigger
C-                                which called this filter.
C-   Outputs : RESULT_FLAG :      Flag set to TRUE to pass the event
C-                                under this PARAM_SET_NUMBER
C-             EXTRA_FLAG  :      Set to TRUE when we want to pass event and
C-                                do no further filtering. (NOT IMPLEMENTED)
C-   Controls:
C-
C-   Created  11-JAN-1994   D.Claes
C-   Updated  24-JAN-1994   Hailin LI   
C-                          Added the Missing Et isolation
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARAM_SET_NUMBER,HARDWARE
      LOGICAL EXTRA_FLAG,RESULT_FLAG
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra main store
      CHARACTER*80 MSG
C
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
C
      INTEGER IP,NPARIN,IER,NJ,NFOUND(ID_ALL:LAST_TYPE),Nmet
      INTEGER NCHR,I,J,POS, POSITION
      INTEGER ID_OBJECT, IFLAG, L1PHI, L1ETA
c      INTEGER L2JETS_CURRENT_ESUM_OBJECT
      integer L2JETS_CURRENT_PARAM
C
      REAL ETASIZE,PHISIZE,EMET,JET_CONE
      REAL ET(10),ETA(10),PHI(10)
      REAL MAX, MET, ETAmet, PHImet, ETA_DET
      REAL DEL_PHI_JETS, DEL_PHI_MET, DEL_PHI_MET_ISO, TEST, DIFF
C
      LOGICAL EZERROR,OK,REQUIRE_EXACT,START
C
C----------------------------------------------------------------------
C
C Initialize output logicals
C
      EXTRA_FLAG = .FALSE.
      RESULT_FLAG = .FALSE.
C
C Retrieve cuts from RCP
      IP = PARAM_SET_NUMBER
      CALL EZPICK('L2_ACOL_JETS') !downloaded from configuration FILT files
      OK = .NOT.EZERROR(IER)
      IF (OK) THEN
C
C Is IP consistent with the number of sets which exist?
C
        CALL EZGET('NUMBER_OF_SETS',NPARIN,IER)
        IF (IER.EQ.0) THEN
          IF ((IP.LE.0).OR.(IP.GT.NPARIN)) THEN
            WRITE(MSG,'(A,I5,A,I5)')' parameter set requested = ',
     &        PARAM_SET_NUMBER, ' but only had ',NPARIN
            CALL ERRMSG('L2_ACOL_PARSET','L2_ACOL_JETS',MSG,'W')
            GO TO 999
          ENDIF
C
C Get the parameters from the IPth set
C
          CALL EZGETA('JET_CONE',IP,IP,1,JET_CONE,IER)
          IF (IER.EQ.0)
     &      CALL EZGETA('DEL_PHI_JETS',IP,IP,1,DEL_PHI_JETS,IER)
          IF (IER.EQ.0)
     &      CALL EZGETA('DEL_PHI_MET',IP,IP,1,DEL_PHI_MET,IER)
          IF (IER.EQ.0)
     &      CALL EZGETA('DEL_PHI_MET_ISO',IP,IP,1,DEL_PHI_MET_ISO,IER)
          IF (IER .NE. 0) THEN      ! Error reading RCP
            CALL ERRMSG('L2_ACOL_PARAMETERS','L2_ACOL_JETS',
     &          'Couldn''t find parameter','F')
          ENDIF
C
        ELSE         !(.NOT.OK)
          CALL ERRMSG('L2_ACOL_JETS_RCP','L2_ACOL_JETS',
     &      'Couldn''t find bank', 'F')
        ENDIF
C
C    Retrieve the position of ETMISS
C
        ID_OBJECT = ID_ETMISS
C
        CALL GTESUM_COUNTS('FILT',NFOUND,IER)
        IF (IER.NE.0) THEN
          WRITE(MSG,'(A,I)')'IER From GTESUM_COUNTS = ',IER
          CALL ERRMSG('GTESUM_COUNTS','L2_ACOL_JETS',MSG,'W')
        ENDIF
        Nmet = NFOUND(ID_ETMISS)
        IF(Nmet.LT.1) THEN
          GOTO 999
        ENDIF
        CALL GTESUM('FILT',ID_OBJECT,Nmet,MET,ETAmet,ETA_DET,PHImet,
     &       IFLAG,IER)
        IF (IER.NE.0) THEN
          WRITE(MSG,'(A,I)')'IER From GTESUM = ',IER
          CALL ERRMSG('GTESUM','L2_ACOL_JETS',MSG,'F')
        ENDIF
C
C  Retrieve number of jets, Et, Eta, Phi, and array positions of the leading 2
C
      ID_OBJECT=L2JETS_CURRENT_PARAM()
C
        MAX=0.0
        POS=0
C
C   Loop over all L2 jets in the event
C   Check each for alignment with Missing Et
C
        START=.TRUE.
        IER=0
        J=1
        DO WHILE (IER.EQ.0)
          CALL GTJAUX(START,JET_CONE,ID_OBJECT,ET(J),ETA(J),
     &      PHI(J),L1ETA,L1PHI,ETASIZE,PHISIZE,EMET,IER)
          IF(IER.EQ.0)THEN
C
C Check for MET alignment with each jet and MET isolation
C
            DIFF = ABS( PHI(J) - PHImet )
            IF (DIFF .GT. PI) DIFF = TWOPI - DIFF
            IF ( DIFF .LT. DEL_PHI_MET_ISO ) GOTO 999 ! MET not isolated
            TEST = ABS( DIFF - PI)
            IF (TEST.LT.DEL_PHI_MET)GOTO 999  ! MEt aligned with jet
C
C Identify the array position of the leading-Et jet
C
            IF(ET(J).GT.MAX)THEN
              MAX=ET(J)
              POS=J
            ENDIF
            J=J+1
          ENDIF
          IF (J.GT.10) GOTO 999
        ENDDO
        NJ=J-1
C
C Check for back-to-back jets
C
        DO I = 1, NJ
          IF (I.NE.POS) THEN
            DIFF = ABS( PHI(POS) - PHI(I) )
            IF (DIFF .GT. PI) DIFF = TWOPI - DIFF
            TEST = ABS( DIFF - PI)
            IF (TEST.LT.DEL_PHI_JETS)GOTO 999 ! Jets are back-to-back
          ENDIF
        ENDDO
C
        RESULT_FLAG = .TRUE.
C
      ENDIF
C
  999 CONTINUE
      IF (OK) CALL EZRSET
      RETURN
      END
