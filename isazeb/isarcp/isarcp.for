      PROGRAM ISARCP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Run ISAJET from an RCP file
C-                         Based on ISARUN
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   8-NOV-1989   Rajendran Raja
C-   Updated  21-DEC-1990   Harrison B. Prosper
C-      Added optional mass smearing
C-   Updated  22-DEC-1990   Harrison B. Prosper
C-      Added optional mass check
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ISABNK.INC'
      INCLUDE 'D0$INC:ISAUNT.INC'
C
      INCLUDE 'D0$INC:ITAPES.INC'
      INCLUDE 'D0$INC:IDRUN.INC'
      INCLUDE 'D0$INC:ISALNK.INC'
      INCLUDE 'D0$INC:PRIMAR.INC'
      INCLUDE 'D0$INC:PARTCL.INC'
      INCLUDE 'D0$INC:NODCAY.INC'
C
      LOGICAL OK, DO_MASS_DISTRIBUTION,DO_MASS_CHECK
      INTEGER IER
C
      INTEGER IEVTS,IWRT
      INTEGER IFL
      INTEGER NUMEVT
      LOGICAL ISARCP_INI,ISARCP_BEG,ISARCP_EVT,ISARCP_END,ISARCP_EDIT
      INTEGER PRINT_EVENT
C----------------------------------------------------------------------
C
      OK = ISARCP_INI()
      IF(.NOT.OK)CALL ERRMSG('ISARCP','ISARCP',
     &  'CANNOT INITIALIZE ISARCP ','W')
C
      CALL EZGET('DO_MASS_DISTRIBUTION',DO_MASS_DISTRIBUTION,IER)
      CALL EZGET('DO_MASS_CHECK',DO_MASS_CHECK,IER)
      CALL EZGET('OUTPUT_UNIT',ITEVT,IER)
      CALL EZGET('PRINT_EVENT_NUMBER',PRINT_EVENT,IER)
C
      OK = ISARCP_BEG()
      CALL WREVNT(ITEVT)            ! Write output event
C
      CALL EZGET('NUMBER_OF_EVENTS',NUMEVT,IER)
C
      IWRT = 0
      write (*,12), NUMEVT
      DO 11 IEVTS = 1 ,NUMEVT
C
        IF ( DO_MASS_DISTRIBUTION ) THEN
          CALL ISARCP_MASS_DISTRIBUTION
        ENDIF
C
        OK = ISARCP_EVT()
        IF ( OK ) THEN
          IF ( DO_MASS_CHECK ) THEN
            CALL ISARCP_MASS_CHECK(OK)
          ENDIF
          IF ( OK ) THEN
            IF ( ISARCP_EDIT() ) THEN
              CALL WREVNT(ITEVT)            ! Write output event
              IWRT = IWRT + 1
              IF(MOD(IWRT,PRINT_EVENT).EQ.0)
     &          write (*,13), IEVT,IWRT, NUMEVT, IEVTS, ' '
            ELSE
              write (*,13), IEVT,IWRT, NUMEVT, IEVTS, 'CUT'
            END IF
          ENDIF
        ELSE
          CALL ERRMSG('ISARCP','EVENT LOOP',
     &        'ERROR IN ISARCP_EVT','W')
        ENDIF
C
   11 CONTINUE
   12 FORMAT(/8X,I7,' EVENTS ')
   13 FORMAT
     &  (1X,I7,' GENERATED ',1X,I7,' WRITTEN OF ',I7,' Loop ',I7,2X,A5)
      OK = ISARCP_END()                 ! END IT ALL
C
      STOP
      END
