      SUBROUTINE PRRMAS ( PRUNIT, LRMAS, NRMAS, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'RMAS'. 
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout
C-             LRMAS  [I] : Link to RMAS.
C-             NRMAS, CFL are irrelevant as RMAS is unique for an event.
C-             IFL    [I] : Defines the amount of printing:
C-                          0 means full printout.
C-                          1 prints only those events that passed.
C-                          2 prints only those events that didn't pass.
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created  14-APR-1994   Lewis Taylor Goss
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZRMAS.LINK'
      INCLUDE 'D0$PARAMS:RMAS.PARAMS'
C
      INTEGER PRUNIT,LRMAS,NRMAS,IFL,MASS_FAIL,ETAB_FAIL,ETAR_FAIL
      INTEGER NPAR_MAX,PARAM_NUM,OBJECT,NUM_OBJ,RUNNO,EVENTNO
      INTEGER STATUS,TSTATUS,GZRMAS
      REAL MASS,ETABOOST,ET1,ETA1,PHI1,ET2,ETA2,PHI2
      CHARACTER*4 OBJ_TXT
      CHARACTER*(*) CFL
C----------------------------------------------------------------------
      IF( LRMAS .LE. 0 ) LRMAS = GZRMAS()
C
      NPAR_MAX = IQ(LRMAS + MPPARS)
      RUNNO    = IQ(LHEAD + 6)
      EVENTNO  = IQ(LHEAD + 7)
C
C  ***  Print the content of the bank pointed by LRMAS1
C
      WRITE(PRUNIT,*) '********** DUMPING RMAS BANK **********'
      WRITE(PRUNIT,10) 'Run Number',RUNNO,', Event Number',EVENTNO,'.'
  10  FORMAT(A11,X,I7,A14,X,I10,A1)
C
      IF (LRMAS.LE.0) THEN
        WRITE(PRUNIT,*) 'No RMAS bank found.'
      ELSE
        WRITE(PRUNIT,*) 'PAR                 OBJ  NO          ETA   ',
     &    '------- 1 -------  ------- 2 -------'
        WRITE(PRUNIT,*) 'NUM     COMMENT    TYPE  OB   MASS  BOOST  ',
     &    ' ET     ETA   PHI   ET     ETA   PHI'
C
        DO PARAM_NUM = 1, NPAR_MAX
          CALL GTRMAS(PARAM_NUM,STATUS,OBJECT,NUM_OBJ,MASS,ETABOOST,ET1,
     &      ETA1,PHI1,ET2,ETA2,PHI2)
C
          IF (STATUS .GT. 0 ) THEN
            MASS_FAIL = STATUS / 10000
            TSTATUS   = STATUS - MASS_FAIL * 10000
            ETAB_FAIL = TSTATUS / 100
            ETAR_FAIL = TSTATUS - ETAB_FAIL * 100
          END IF
C
          IF (OBJECT .EQ. 1) THEN
            OBJ_TXT = ' Jet'
          ELSEIF (OBJECT .EQ. 2 ) THEN
            OBJ_TXT = ' EM '
          ELSE
            OBJ_TXT = 'Muon'
          ENDIF
C
          IF(STATUS .EQ. 1000000 .AND.(IFL .EQ. 2 .OR. IFL .EQ. 0)) THEN
           WRITE(PRUNIT,20) PARAM_NUM,'Too few objects in the event.   '
   20      FORMAT(X,I3,2X,A32)
          ELSEIF(STATUS.EQ.-1000000.AND.(IFL.EQ. 2 .OR. IFL.EQ. 0)) THEN
           WRITE(PRUNIT,20) PARAM_NUM,'Too many objects in the event.  '
          ELSEIF (STATUS .EQ. 0 .AND. (IFL .EQ. 2 .OR. IFL .EQ. 0)) THEN
           WRITE(PRUNIT,20) PARAM_NUM,'Tool never looked at the event. '
          ELSEIF(STATUS .EQ. -1 .AND. (IFL .EQ. 1 .OR. IFL .EQ. 0)) THEN
            WRITE(PRUNIT,30) PARAM_NUM,'Event Passed',OBJ_TXT,NUM_OBJ,
     &        MASS,ETABOOST,ET1,ETA1,PHI1,ET2,ETA2,PHI2
   30       FORMAT(X,I3,2X,A12,2X,A4,2X,I2,2X,F5.1,2X,F5.3,X,F5.1,2X,
     &        F5.2,2X,F4.2,X,F5.1,2X,F5.2,2X,F4.2)
          ELSEIF(STATUS .GT. 0. .AND. (IFL .EQ. 2 .OR. IFL .EQ. 0)) THEN
          WRITE(PRUNIT,20) PARAM_NUM,'Event didn''t pass tool, because:'
            IF ( MASS_FAIL .NE. 0 ) WRITE(PRUNIT,40) MASS_FAIL,'pairs ',
     &        'failed the mass cut.     '
            IF ( ETAB_FAIL .NE. 0 ) WRITE(PRUNIT,40) ETAB_FAIL,'pairs ',
     &        'failed the eta boost cut.'
            IF ( ETAR_FAIL .NE. 0 ) WRITE(PRUNIT,40) ETAR_FAIL,'pairs ',
     &        'failed the eta range cut.'
   40       FORMAT(6X,I2,X,A6,A25)
            WRITE(PRUNIT,50) 'Leading Pair',OBJ_TXT,NUM_OBJ,MASS,
     &        ETABOOST,ET1,ETA1,PHI1,ET2,ETA2,PHI2
   50       FORMAT(6X,A12,2X,A4,2X,I2,2X,F5.1,2X,F5.3,X,F5.1,2X,F5.2,2X,
     &        F4.2,X,F5.1,2X,F5.2,2X,F4.2)
          ENDIF
        END DO
      ENDIF
  999 RETURN
      END
