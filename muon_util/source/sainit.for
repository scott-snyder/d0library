C+
      SUBROUTINE SAINIT (OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read geometrical constants, calibration
C-                         parameters and minimum time values from
C-                         STP file and DBL3 base.
C-
C-   Inputs  : none.
C-   Outputs : none.
C-   Controls: OK.
C-
C-   Created  15-DEC-1992   Alexander Efimov
C-   Updated  17-DEC-1992   Alexander Efimov
C-   Updated  17-Feb-1993   Alexander Kozelov  Geometry banks must
C-                                       be read only once
C-   Updated  11-MAR-1993   Alexander Kozelov  Move reading of STP file
C-                                        to SCONST routine
C-   Updated   9-JAN-1994   Alexander Efimov
C-   Updated  11-MAR-1994   Alexander Efimov  change INTMSG to ERRMSG
C-   Updated  20-APR-1995   Andrei Mayorov  check validity range for current
C-                                          run to avoid unness. DBL3 calls 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      INCLUDE 'D0$INC:DBSTP.INC/LIST'
      INTEGER LBANK,LKY
      LOGICAL OK
      INTEGER N_STATIONS, N_SECTIONS
      PARAMETER (N_STATIONS=6, N_SECTIONS=6)
      INTEGER N_CRATES
      PARAMETER (N_CRATES=N_STATIONS*N_SECTIONS)
      CHARACTER CALTYPE*40, PATH*40
      INTEGER GZSSAM, GZSELH, GZSMTH, GZSELC, GZSMNT
      INTEGER BKSSAM, BKSELH, BKSMTH
      INTEGER LSSAM,  LSELH,  LSMTH,  LSELC,  LSMNT
      INTEGER NSTA, NSEC, RUN, LRUN, CRATE, NW1, NW2
      INTEGER RUNNO,  J
      CHARACTER*32 MESSID, CALLER
      CHARACTER*80 MESSAG
      INTEGER LOWRUN(N_CRATES,2),HIGHRUN(N_CRATES,2)
      DATA LOWRUN/N_CRATES*0,N_CRATES*0/,HIGHRUN/N_CRATES*0,N_CRATES*0/
      LOGICAL DBL_OPENED
      DATA DBL_OPENED/.FALSE./
      SAVE DBL_OPENED,LOWRUN,HIGHRUN
C
C ****  define current RUN number
C
      OK = .FALSE.
      RUN = RUNNO ()
      CALLER = 'SAINIT'
      MESSID = 'SAINIT: current RUN number'
      WRITE (MESSAG, '(I10)') RUN
      CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
C
C ****  create ZEBRA banks for SAMUS constants
C
      LSSAM = GZSSAM ()
      IF (LSSAM .LE. 0) THEN
        LSSAM = BKSSAM ()             ! SAMUS header bank SSAM
        IF (LSSAM .LE. 0) GO TO 999
      END IF
      LSELH = GZSELH ()
      IF (LSELH .LE. 0) THEN
        LSELH = BKSELH ()             ! electronic constants bank SELH
        IF (LSELH .LE. 0) GO TO 999
      END IF
      LSMTH = GZSMTH ()
      IF (LSMTH .LE. 0) THEN
        LSMTH = BKSMTH ()             ! minimum times bank SMNT
        IF (LSMTH .LE. 0) GO TO 999
      END IF
C
C ****  read electronic calibration constants and t0 values from DBL3
C
      CALTYPE = 'TIMES'
      CALTYPE(17:17) = ' '
      IF(DBL_OPENED) CALL DBCLB_PATH (CALTYPE, 'SAM', PATH)
      DO CRATE = 1, N_CRATES
        NSTA = 1 + (CRATE - 1) / N_SECTIONS
        NSEC = CRATE - (NSTA - 1) * N_SECTIONS
        LSELC = GZSELC (NSTA, NSEC)
C        IF(LSELC.GT.0) THEN
C          LOWRUN=IC(LSELC+4)
C          HIGHRUN=IC(LSELC+5)
C        ELSE
C          LOWRUN=-1
C          HIGHRUN=-1
C      ENDIF
        IF(.NOT.(LOWRUN(CRATE,1).LE.RUN.AND.RUN.LE.HIGHRUN(CRATE,1)) )
     &    THEN
          IF(.NOT.DBL_OPENED) THEN
            CALL DBCLB_INITIALIZE ('DBL3$SAM:DBCALIB$SAM.DAT', ' ', OK)
            IF (.NOT. OK ) THEN
              MESSID = 'SAINIT: error initializing DBL3'
              MESSAG = ' '
              CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
              GO TO 999
            ELSE
              CALL DBCLB_PATH (CALTYPE, 'SAM', PATH)
              DBL_OPENED=.TRUE.
            ENDIF
          END IF
          CALL DBCLB_FETCH_OFFLINE (PATH, RUN, CRATE, LBANK, LKY)
          IF ( .NOT. CALL_DBEND ) THEN        ! fetch failed
            MESSID = 'SAINIT: DBL3 fetch failed'
            WRITE (MESSAG, '('' crate '', I11)') CRATE
            CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
            GO TO 900
          ELSE
            LOWRUN(CRATE,1)=IC(LKY+3)
            HIGHRUN(CRATE,1)=IC(LKY+4)
          END IF
          NW1 = IC(LBANK-1)
          LSELC = GZSELC (NSTA, NSEC)
          NW2 = IC(LSELC-1)
          IF (NW1 .NE. NW2) GO TO 900
          DO J = 1, NW1
            IC(LSELC+J) = IC(LBANK+J)
          END DO
          IF (CRATE .EQ. 1) THEN
            LRUN = IC(LBANK+4)
            MESSID = 'SAINIT: calibration constants from RUN'
            WRITE (MESSAG, '(i10,a,I10,a,i10)')
     &        LRUN,' valid. range:',IC(LKY+3),'-',IC(LKY+4)
            CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
          END IF
        END IF
      END DO
      CALTYPE = 'MINTIME'
      CALTYPE(17:17) = ' '
      IF(DBL_OPENED) CALL DBCLB_PATH (CALTYPE, 'SAM', PATH)
      DO CRATE = 1, N_CRATES
        NSTA = 1 + (CRATE - 1) / N_SECTIONS
        NSEC = CRATE - (NSTA - 1) * N_SECTIONS
        LSMNT = GZSMNT (NSTA, NSEC)
C        IF(LSMNT.GT.0) THEN
C          LOWRUN=IC(LSMNT+4)
C          HIGHRUN=IC(LSMNT+5)
C        ELSE
C          LOWRUN=-1
C          HIGHRUN=-1
C        ENDIF
        IF(.NOT.(LOWRUN(CRATE,2).LE.RUN.AND.RUN.LE.HIGHRUN(CRATE,2)) )
     &    THEN
          IF(.NOT.DBL_OPENED) THEN
            CALL DBCLB_INITIALIZE ('DBL3$SAM:DBCALIB$SAM.DAT', ' ', OK)
            IF (.NOT. OK ) THEN
              MESSID = 'SAINIT: error initializing DBL3'
              MESSAG = ' '
              CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
              GO TO 999
            ELSE
              CALL DBCLB_PATH (CALTYPE, 'SAM', PATH)
              DBL_OPENED=.TRUE.
            ENDIF
          END IF
          CALL DBCLB_FETCH_OFFLINE (PATH, RUN, CRATE, LBANK, LKY)
          IF ( .NOT. CALL_DBEND ) THEN        ! fetch failed
            MESSID = 'SAINIT: DBL3 fetch failed'
            WRITE (MESSAG, '('' crate '', I11)') CRATE
            CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
          ELSE
            LOWRUN(CRATE,2)=IC(LKY+3)
            HIGHRUN(CRATE,2)=IC(LKY+4)
          ENDIF
          NW1 = IC(LBANK-1)
          LSMNT = GZSMNT (NSTA, NSEC)
          NW2 = IC(LSMNT-1)
          IF (NW1 .NE. NW2) GO TO 900
          DO J = 1, NW1
            IC(LSMNT+J) = IC(LBANK+J)
          END DO
          IF (CRATE .EQ. 1) THEN
            LRUN = IC(LBANK+4)
            MESSID = 'SAINIT: times parameters from RUN'
            WRITE (MESSAG, '(i10,a,I10,a,i10)')
     &        LRUN,' valid. range:',IC(LKY+3),'-',IC(LKY+4)
            CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
            CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
          END IF
        END IF
      END DO
      MESSID = 'SAINIT: SAMUS initialisation finished OK'
      MESSAG = ' '
      CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
      OK = .TRUE.
C
  900 CONTINUE
  999 CONTINUE
      IF(DBL_OPENED) THEN
        CALL DBCLB_FINISH
        DBL_OPENED=.FALSE.
      END IF
      RETURN
      END
