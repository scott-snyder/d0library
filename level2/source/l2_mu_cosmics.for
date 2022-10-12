      SUBROUTINE L2_MU_COSMICS(PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,
     &  EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-      This tool requires Muon_l2 have run previously on the event.
C-                                              
C-   Purpose and Methods : level 2 tool to allow selection of muon which
C-                         passes through specific tracking region or 
C-                         chamber list(s).
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
C-   Created: 23-MAR-1993   Diehl
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PARAM_SET_NUMBER,HARDWARE,NUMBER_OF_SETS,STR_LEN,PSN
      LOGICAL EXTRA_FLAG,RESULT_FLAG
      INCLUDE 'D0$INC:ZEBCOM.INC'               ! zebra main store
      INTEGER IP,AMOD(128),BMOD(128),CMOD(128),OMOD(128)
      INTEGER LMHTT,GZMHTT
      CHARACTER*24 CHARVAR1(128),CHARVAR2(128),REGION(128)

      INTEGER NSAMUS,QUAD,NPT,IFW1,IFW2,IFW3,IFW4    !muot
      REAL XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM  !muot
      REAL XCOSOM,YCOSOM,ZCOSOM,CHSNBV,CHSQBV,MOM,MOMER,ELCAL,ELFE !muot
      REAL SPARE1,SPARE2                                    !muot

      LOGICAL AMODOK,BMODOK,CMODOK,OMODOK,OKSOFAR
      LOGICAL OK,EZERROR,MUATSEL

      INTEGER NMOD,IWADD,NPLN,NWIR,J,IER,NPARIN,I,NTRACKS
C
C----------------------------------------------------------------------
      EXTRA_FLAG = .FALSE.
      RESULT_FLAG = .FALSE.
C
C...first, carefully retrieve cuts from RCP
      PSN = PARAM_SET_NUMBER
      CALL EZPICK('L2_MU_COSMICS')           ! configuration file (.FILTs)
      OK = .NOT.EZERROR(IER)

      IF (OK) THEN
        IF (IER.EQ.0) CALL EZGET_i('NUMBER_OF_SETS',NPARIN,IER)
        IF(PSN.LE.0.OR.PSN.GT.NPARIN) IER = 1
        IF (IER.EQ.0) CALL EZGET_iarr('AMOD',AMOD,IER)
        IF (IER.EQ.0) CALL EZGET_iarr('BMOD',BMOD,IER)
        IF (IER.EQ.0) CALL EZGET_iarr('CMOD',CMOD,IER)
        IF (IER.EQ.0) CALL EZGET_iarr('OMOD',OMOD,IER)
        IF (IER.NE.0) OK = .FALSE.
      ENDIF

      IF(OK) CALL EZGETS('REGION',PSN,REGION(PSN),STR_LEN,IER)
      IF (IER.NE.0) OK = .FALSE.
      IF(OK) CALL EZGETS('CHARVAR1',PSN,CHARVAR1(PSN),STR_LEN,IER)
      IF (IER.NE.0) OK = .FALSE.
      IF(OK) CALL EZGETS('CHARVAR2',PSN,CHARVAR2(PSN),STR_LEN,IER)
      IF (IER.NE.0) OK = .FALSE.

      IF (.NOT.OK) THEN
        CALL ERRMSG('L2_MU_COSMICS','L2_MU_COSMICS', 'BANK BLANK','F')
      ELSEIF (IER .NE. 0) THEN      ! Error reading RCP
        CALL ERRMSG('L2_MU_COSMICS','L2_MU_COSMICS_PARAMETERS',
     &          'Could not find parameter','F')
      ENDIF

      CALL EZRSET

C
C------------------------------------------------------------------------------
      CALL GTMTRH(NTRACKS)
      DO I = 1, NTRACKS
        OKSOFAR = .FALSE.
        CALL GTMUOT(I,NPT,NSAMUS,QUAD,IFW1,IFW2,IFW3,IFW4,
     $    XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     $    YCOSOM,ZCOSOM,CHSQBV,CHSNBV,MOM,MOMER,ELCAL,ELFE,SPARE1,
     $    SPARE2)
        LMHTT = GZMHTT(I)
        AMODOK = .FALSE.
        BMODOK = .FALSE.
        CMODOK = .FALSE.
        OMODOK = .FALSE.
        DO J = 1,NPT
          IWADD = IQ(LMHTT+1+5*(J-1))
          CALL MUADD(IWADD,NMOD,NPLN,NWIR,IER)
          IF(9.GE.AMOD(PSN).OR.AMOD(PSN).GE.308) AMODOK = .TRUE. 
          IF(9.GE.BMOD(PSN).OR.BMOD(PSN).GE.308) BMODOK = .TRUE. 
          IF(9.GE.CMOD(PSN).OR.CMOD(PSN).GE.308) CMODOK = .TRUE. 
          IF(9.GE.OMOD(PSN).OR.OMOD(PSN).GE.308) OMODOK = .TRUE. 
          IF(NMOD.EQ.AMOD(PSN)) AMODOK = .TRUE.
          IF(NMOD.EQ.BMOD(PSN)) BMODOK = .TRUE.
          IF(NMOD.EQ.CMOD(PSN)) CMODOK = .TRUE.
          IF(NMOD.EQ.OMOD(PSN)) OMODOK = .TRUE.
        ENDDO  
        IF(AMODOK.AND.BMODOK.AND.CMODOK.AND.OMODOK) OKSOFAR = .TRUE. 

        IF(REGION(PSN).EQ.'ANY') THEN
          IF(QUAD.GT.1000) OKSOFAR = .FALSE. 
        ELSEIF(REGION(PSN).EQ.'CF') THEN
          IF(QUAD.GT.4) OKSOFAR = .FALSE.
        ELSEIF(REGION(PSN).EQ.'BOTTOM') THEN
          IF(QUAD.NE.4) OKSOFAR = .FALSE.
        ELSEIF(REGION(PSN).EQ.'NOTTOP') THEN
          IF(QUAD.EQ.2) OKSOFAR = .FALSE.
        ELSEIF(REGION(PSN).EQ.'NOTCF') THEN
          IF(QUAD.LE.4) OKSOFAR = .FALSE.
        ELSEIF(REGION(PSN).EQ.'EF') THEN
          IF(QUAD.LT.5.OR.QUAD.GT.12) OKSOFAR = .FALSE.
        ELSEIF(REGION(PSN).EQ.'NORTH') THEN
          IF(QUAD.LT.5.OR.QUAD.GT.8) OKSOFAR = .FALSE.
        ELSEIF(REGION(PSN).EQ.'SOUTH') THEN
          IF(QUAD.LT.9.OR.QUAD.GT.12) OKSOFAR = .FALSE.
        ELSEIF(REGION(PSN).EQ.'SAMUS') THEN
          IF(QUAD.NE.13.OR.QUAD.NE.14) OKSOFAR = .FALSE.
        ELSEIF(REGION(PSN).EQ.'SASBWC') THEN
          IF(.NOT.BTEST(IFW2,10)) OKSOFAR = .FALSE.
        ENDIF

        IF(CHARVAR1(PSN).EQ.'ALIGNMENT') THEN
          OKSOFAR = OKSOFAR.AND.MUATSEL(I)
        ENDIF

        IF(OKSOFAR) RESULT_FLAG = .TRUE.
 
      ENDDO

  999 CONTINUE
      RETURN
      END
