      FUNCTION CAJETS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Does Calorimeter Jet finding
C-
C-   Returned value  : .true. if ALL OK
C-   Inputs  : none
C-   Outputs : none
C-   Controls: Needs CATE bank, CAEP bank, and CAJETS_RCP
C-
C-   Created  14-APR-1989   Rajendran Raja
C-   Updated   2-OCT-1989   Gerald C. Blazey, Harrison B. Prosper
C-     Added algorithm selection switches. Removed call to CJTINI
C-     This should be called in CAL_BEGIN.
C-   Updated  16-JAN-1990   Harrison B. Prosper
C-      Added call to CJET_BUILD_PJET.
C-   Updated  26-MAR-1990   Boaz Klima
C-      Removed PJET code
C-   Updated   2-OCT-1990   Chip Stewart  - modify RCP format
C-   Updated   8-APR-1991   Scott Snyder  - Add EZRSET call.
C-   Updated   4-APR-1991   Nick hadley   - force return to be TRUE
C-   Updated  28-SEP-1992   Norman A. Graf  add on ICD/MG fraction,
C-       CH fraction, number of cells above threshold, and ratio of
C-       hottest to next-hottest cells.
C-   Updated   4-JAN-1993   G.C.Blazey    - Add extra cone algorithms
C-       for L1 bits.
C-   Updated  12-JAN-1993   Alex Smith : Added MUON_JET algorithm
C-   Updated  26-JAN-1993   Harrison B. Prosper, Alex Smith
C-   Updated   3-MAY-1993   Norman A. Graf  Replaced L1BITS With L2NAMES 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CJET_ALGORITHM.INC'
      LOGICAL CAJETS,EZERR
      LOGICAL CJET_CONE,CJET_NEIGHBOR,CJET_USER,CJET_MUON
      LOGICAL DO_CONE,DO_NEIGHBOR,DO_USER,DO_MUON,DO_ANALYSIS
      LOGICAL FIRST,OK,DO_MUON_JFIND,USE_MUJET_DEBUG_TOOLS
      INTEGER IER
      INTEGER I,J,K,L,ID,NID
      LOGICAL NEXT
      CHARACTER NAME*80
      LOGICAL DO_EXTRA,DO_EXTRA_ALG,EXTRA_ALG_TEST,EXTRA_ALG(20)
      LOGICAL L2NAME_PASSED
      INTEGER NL2NAMES
      CHARACTER*32 L2NAMES(128)
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST )THEN                  ! LOCAL INIT
        FIRST = .FALSE.
C
        CALL EZPICK('CAJETS_RCP')       ! SELECT JETS RCP BANK
        IF (EZERR(IER)) THEN
          CALL ERRMSG('CAJETS','CAJETS',
     &      'CAJETS RCP bank not found in CAJETS.','W')
        ELSE
C
C ****  GET EXTRA ALGORITHM PARAMETERS
C
          CALL EZGET('DO_EXTRA',DO_EXTRA,IER)
          IF(IER.NE.0) THEN
            NL2NAMES=0
            CALL ERRMSG('CAJETS','CAJETS',
     &        'CAJETS: No Extra Algorithms Requested.','W')
          END IF
          IF(DO_EXTRA) THEN
            CALL EZ_GET_CHARS('EXTRA_L2NAMES',NL2NAMES,L2NAMES,IER)
            IF(IER.NE.0) THEN
              NL2NAMES=0
              CALL ERRMSG('CAJETS','CAJETS',
     &          'CAJETS: No Extra Algorithm Filter Names Found.','W')
            END IF
          END IF
          IF(NL2NAMES.EQ.0) DO_EXTRA=.FALSE.
C
C ****  READ RUNTIME ALG. SELECTION SWITCHES. ****
C
          CALL EZGET('DO_MUON_JFIND',DO_MUON_JFIND,IER)
          CALL EZGET('USE_MUJET_DEBUG_TOOLS',
     &      USE_MUJET_DEBUG_TOOLS,IER)
          CALL EZGET('DO_ANALYSIS',DO_ANALYSIS,IER)
          NEXT = .TRUE.
          N_ALG = 0
          NID = 1
          DO WHILE (NEXT .AND. (N_ALG.LT.20) )
C
C ***  GET ID of NEXT SWITCH IN RCPFILE WITH GIVEN PREFIX
C
            CALL EZGNXT ('ALGORITHM_',NID,ID)
C
            NEXT = ID.NE.0
C
            IF ( NEXT ) THEN
              CALL EZGETN (ID,NAME,L)
              EXTRA_ALG_TEST = INDEX(NAME,'_XXX').GT.0
              IF( (.NOT.DO_EXTRA) .AND. EXTRA_ALG_TEST) GOTO 1000
              CALL EZGET_VALUE_TYPE(NAME(1:L), ALG_PARAMS,
     &          TYPE_PARAMS,NPARAMS, IER)
            ELSE
              IER = 1
            END IF
C
            IF( IER.NE.0) THEN
              NEXT = .FALSE.
            ELSE
              J = TYPE_PARAMS(1) - 10
              CALL UHTOC(ALG_PARAMS(1),J,ALGORITHM,20)
              DO_CONE            = INDEX(ALGORITHM,'CONE').GT.0
              DO_NEIGHBOR        = INDEX(ALGORITHM,'NEIGHBOR').GT.0
              DO_MUON            = INDEX(ALGORITHM,'MUON').GT.0
              DO_USER            = INDEX(ALGORITHM,'USER').GT.0
C
C ****  SET PARAMETERS
C
              IF ( DO_CONE .OR. DO_NEIGHBOR .OR. DO_MUON 
     &          .OR. DO_USER ) THEN
                N_ALG = N_ALG + 1
                EXTRA_ALG(N_ALG) = EXTRA_ALG_TEST
                CALL CJET_PARAM_SET
     &            (N_ALG,ALGORITHM,ALG_PARAMS,TYPE_PARAMS,NPARAMS,IER)
              END IF
            END IF
 1000       CONTINUE
C
          END DO
C
          CALL EZRSET
C
        END IF
      END IF
C
C ***   EVENT LOOP:  FIRST CHECK L2NAMES.
C
      IF(DO_EXTRA) THEN
        DO_EXTRA_ALG=.FALSE.
        DO I = 1, NL2NAMES
          IF( L2NAME_PASSED(L2NAMES(I)) ) DO_EXTRA_ALG=.TRUE.
        END DO
      END IF
C
      DO I = 1, N_ALG
C
C ****  GET PARAMETERS FOR Ith ALGORITHM AND PUT INTO CJET_ALGORITHM COMMON
C
        CALL CJET_PARAM_GET
     &          (I,ALGORITHM,ALG_PARAMS,TYPE_PARAMS,NPARAMS,IER)
C
        IF (IER.NE.0) GOTO 10
        DO_CONE            = INDEX(ALGORITHM,'CONE').GT.0
        DO_NEIGHBOR        = INDEX(ALGORITHM,'NEIGHBOR').GT.0
        DO_MUON            = INDEX(ALGORITHM,'MUON').GT.0
        DO_USER            = INDEX(ALGORITHM,'USER').GT.0
C
        IF ( DO_CONE ) THEN
          IF (DO_EXTRA) THEN
            IF ( .NOT.EXTRA_ALG(I) )            OK = CJET_CONE()  ! DO CONE
            IF ( EXTRA_ALG(I).AND.DO_EXTRA_ALG) OK = CJET_CONE()  ! DO EXTRA
          ELSE
            OK = CJET_CONE()              ! DO CONE
          END IF
        ELSE IF ( DO_NEIGHBOR ) THEN
          OK = CJET_NEIGHBOR() .OR. OK    ! DO NEAREST NEIGHBOR ALG.
        ELSE IF ( DO_USER ) THEN
          OK = CJET_USER() .OR. OK        ! DO USER algorithm
        ELSE IF ( DO_MUON .AND. DO_MUON_JFIND ) THEN
          OK = CJET_MUON() .OR. OK        ! DO MUON algorithm
          IF (USE_MUJET_DEBUG_TOOLS) CALL CJET_MUDEB_JETS
        ENDIF
   10   CONTINUE
      END DO
C
      CALL CAJETS_ADDON               
C
      IF ( OK .AND. DO_ANALYSIS ) THEN
        CALL CJTANL                     ! CAJETS ANALYSIS ROUTINE
      ENDIF
      CAJETS = .TRUE.                   ! FORCE CAJETS TO RETURN TRUE
C
  999 RETURN
      END
