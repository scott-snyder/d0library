      PROGRAM RGSEARCH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform a random grid search in n-dimensions.
C-   One ntuple, usually the signal ntuple, is used as the supplier of
C-   all possible cuts while the other ntuples contain the data to be
C-   searched.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-APR-1995   Harrison B. Prosper, Chip Stewart
C-   Updated  11-JUL-1995   Harrison B. Prosper, Jeffrey McDonald
C-   Updated  12-JUL-1995   Susan K. Blessing  Allow use of absolute
C-    value, > and < cuts.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:RGSCOM.INC'
C----------------------------------------------------------------------
      INTEGER ICUT, IPAT, IFILE, REPORT, I, STATUS, LUNDUMP
      REAL    CUT(MAXIN),NPASS,WGT,X1(MAXIN),X2(MAXIN),PP
      LOGICAL EZERROR, PASS, DUMP
C----------------------------------------------------------------------
C
C
C ****  Initialize ZEBRA
C
      CALL MZEBRA(0)
      CALL INZSTP
      CALL INPAWC
C
      CALL INRCP ('RGSEARCH_RCP',STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG('RGSEARCH','INRCP','No RGSEARCH_RCP file','F')
      ENDIF
C
      CALL INRCPE('RGSEARCH_RCPE',STATUS)     ! read overwrite file (RCPE)
      IF ( STATUS .EQ. 0 ) THEN
        CALL ERRMSG('RGSEARCH','INRCPE',
     &    'Default RGSEARCH_RCP modified by RGSEARCH_RCPE','W')
      ENDIF
C
C
C *************************************************************
C ****  Get some stuff
C *************************************************************
C
      CALL EZPICK('RGSEARCH_RCP')
      IF ( EZERROR(STATUS) ) THEN
        CALL ERRMSG ('NO_RGSEARCH_RCP','RGSEARCH',
     &    'Cannot pick RGSEARCH_RCP bank','F')
      ENDIF
C
      CALL EZGET_l('DUMP',DUMP,STATUS)
      CALL EZGET_i('DUMPUNIT',LUNDUMP,STATUS)
C
      CALL EZGET('CUT_NTUPLE_ID',CUT_NTUPLE_ID,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG ('NO_CUT_NTUPLE_ID','RGSEARCH',
     &    'Please specify CUT_NTUPLE_ID','F')
      ENDIF
C
      CALL EZGET('INP_NTUPLE_ID',INP_NTUPLE_ID,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG ('NO_INP_NTUPLE_ID','RGSEARCH',
     &    'Please specify INP_NTUPLE_ID','F')
      ENDIF
C
      CALL EZGET('OUT_NTUPLE_ID',OUT_NTUPLE_ID,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        CALL ERRMSG ('NO_OUT_NTUPLE_ID','RGSEARCH',
     &    'Please specify OUT_NTUPLE_ID','F')
      ENDIF
C
      CALL EZGET('REPORT',REPORT,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        REPORT = 100
      ENDIF
C
C
C *************************************************************
C ****  Get input selection defined by Patterns_Input
C ****  and list of files
C *************************************************************
C
      CALL RGS_SELECT
C
      CALL EZRSET
C
C *************************************************************
C ****  Read all cuts into common block /RGSCOM/
C ****  The zero signifies that the file given contains the
C ****  cuts
C *************************************************************
C
      CALL RGS_READFILE(0)
C
      WRITE(6,'(''  '')')
      WRITE(6,'(''  --- Number of CUTS : '',I10)') NCUT
C
C ****  Loop over data-sets
C
      IF ( DUMP ) THEN
        WRITE(LUNDUMP,'(5A16)')
     &        (FIELD(PATTERN_SELECT(I)),I=1,NSELECT)
      ENDIF

      DO IFILE = 1, NFILE
C
C ****  Read all current patterns into common block /RGSCOM/
C
        CALL RGS_READFILE(IFILE)
        WRITE(6,'(/,''  --- Processing file '',A32,I10)')
     &    FILE(IFILE), NPATTERN(IFILE)
C
        IF ( DUMP ) THEN
          WRITE(LUNDUMP,'('' File '',A32,I10)')
     &      FILE(IFILE)(1:32), NPATTERN(IFILE)
        ENDIF
C
C ****  Loop over cuts
C
        TOT_IN(IFILE) = 0
C
        DO ICUT = 1, NCUT
C
          IF ( MOD(ICUT,REPORT) .EQ. 0 ) THEN
            WRITE(6,'(10X,I10)') ICUT
          ENDIF
C
C ****  Get current cut
C
          DO I = 1, NSELECT
            CUT(I) = PATTERN_CUT(PATTERN_SELECT(I),ICUT)
C Check for use of absolute value.
            IF (USE_ABS(I)) CUT(I) = ABS(CUT(I))
          ENDDO
C
C ****  For current cut loop over all patterns of
C ****  current file
C ****  Zero moment counters
C
          DO I = 1, NFIELD
            X1(I) = 0.0
            X2(I) = 0.0
          ENDDO
          NPASS = 0.0
C
          DO IPAT = 1, NPATTERN(IFILE)
            PASS = .TRUE.
C
C ****  Apply cuts
C
            I = 0
            DO WHILE ( PASS .AND. (I .LT. NSELECT))
              I = I + 1
C
              PP = PATTERN_IN(PATTERN_SELECT(I),IPAT)
C Check for use of absolute value.
              IF (USE_ABS(I)) PP = ABS(PP)
C
              IF (LOWER_LIM(I)) THEN
                IF (PP .LT. CUT(I)) THEN
                  PASS = .FALSE.
                END IF
              ELSE IF (UPPER_LIM(I)) THEN
                IF (PP .GT. CUT(I)) THEN
                  PASS = .FALSE.
                END IF
              ELSE
                IF (PP .LT. CUT(I)) THEN
                  PASS = .FALSE.
                ENDIF
              END IF
            ENDDO
C
            WGT = 1.0
            IF ( IWGT.GT.0 ) THEN
              WGT = PATTERN_IN(IWGT,IPAT)
            ENDIF
C
            IF ( PASS ) THEN
C
C ****  Accumulate moments for all fields specified in
C ****  PATTERNS_INPUTS
C
              DO I = 1, NFIELD
                PP = PATTERN_IN(I,IPAT)
                X1(I) = X1(I) + WGT*PP
                X2(I) = X2(I) + WGT*PP*PP
              ENDDO
              NPASS = NPASS + WGT
            ENDIF
C
C ****  Accumulate weights
C
            IF ( ICUT.EQ.1 ) THEN
              TOT_IN(IFILE) = TOT_IN(IFILE) + WGT
            ENDIF
          ENDDO
C
C ****  Store count etc.
C
          PATTERN_OUT(IFILE,ICUT) = NPASS
          IF ( NPASS .GT. 0.0 ) THEN
            DO I = 1, NFIELD
              X1(I) = X1(I)/NPASS
              X2(I) = X2(I)/NPASS
              PATTERN_AVE(I,IFILE,ICUT) = X1(I)
              PATTERN_SIG(I,IFILE,ICUT) = SQRT(ABS(X2(I)-X1(I)*X1(I)))
            ENDDO
          ELSE
            DO I = 1, NFIELD
              PATTERN_AVE(I,IFILE,ICUT) = 0.0
              PATTERN_SIG(I,IFILE,ICUT) = 0.0
            ENDDO
          ENDIF
C
          IF ( DUMP ) THEN
            WRITE(LUNDUMP,*)
     &        ICUT, (CUT(I),I=1,NSELECT),PATTERN_OUT(IFILE,ICUT)
          ENDIF
C
        ENDDO
C
      ENDDO
C
C ****  BOOK and FILL OUTPUT NTUPLE
C
      WRITE(6,'(/,'' Building the output ntuples...'')')
      CALL RGS_CREATE_NTUPLE
      WRITE(6,'('' Done!'')')
C
      STOP
      END
