      SUBROUTINE TOP_LEPTONS_NTUPLE(NTUSERID,
     &           MORE_NAMES,MORE_QUANS,NUM_MORE,MET_VEC)
C----------------------------------------------------------------------
C-
C-
C-   Purpose and Methods : Makes Top-dileptons + additional
C-   quantities into  an NTUPLE whose Id is obtained from TOP_LEPTONS_RCP
C-
C-   Inputs  :
C-            NTUSERID   = USERID of the calling routine
C-            MORE_NAMES = 8 character tags for additional quantities
C-            MORE_QUANS = additional quantites in ntuple.
C-            NUM_MORE = NUMBER OF additional quantites
C-
C-   Outputs :
C-   Controls:
C
C-   Created  14-SEP-1992   Meenakshi Narain
C-   Modified 16-Nov-1992   HT added - JT
C-   Modified 17-Mar-1993   Changes in routine name for LEPAN
C-   Modified 26-Aug-1993   Added event shape params
C-   Modified  4-Dec-1993   Fix bug in argument lists (JT)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MORE_MAX, TOP_LEPTONS_MAXVAR
      PARAMETER( TOP_LEPTONS_MAXVAR = 200)
      PARAMETER( MORE_MAX =  510 - TOP_LEPTONS_MAXVAR)
C
      INTEGER NTUP_PRIM,NTUP_ID,USER_ID
      INTEGER NUSERS, NTUP_USER, NTUSERID
      INTEGER NTUP_UID(100), NTUP_OFFSET(100)
      CHARACTER*80 NTFILE, NTUP_TITLE, TOP_DIR
      CHARACTER*8 NTUPLE_VARNAMES(512)
      CHARACTER*(*) MORE_NAMES(*)
      CHARACTER*1 STR,STR2,CPTYPE
      INTEGER NUM_MORE
      INTEGER NP, PTYPE, IDP, MAX_CAND
      INTEGER IER,I,NTUPLE_DIM,LENGTH
      INTEGER INEXT, TOP_LEPTONS_NVAR
      INTEGER NUMRUN,NUMEVT,OLD_RUN,OLD_EVENT
      REAL    TOP_LEPTONS_QUAN(TOP_LEPTONS_MAXVAR)
      REAL    NTUPLE_QUANTITIES(512)
      REAL    MORE_QUANS(*),MET_VEC(3)
      LOGICAL DO_NTUPLE, NTUPFILE_OPENED, FREEZE_USERS
      LOGICAL FIRST,LAST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C
C----------------------------------------------------------------------
C
      CALL EVNTID(NUMRUN,NUMEVT)
      IF( FIRST ) THEN
        FIRST = .FALSE.
        OLD_RUN   = NUMRUN
        OLD_EVENT = NUMEVT
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('FILL_NTUPLES',DO_NTUPLE,IER)
        IF(.NOT.DO_NTUPLE) GOTO 999
C
        IF (IER.EQ.0) CALL EZGETS('NTUPE_TOP_DIR',1,TOP_DIR,LENGTH,IER)
        IF (IER.EQ.0) CALL EZGETS('NTUPLE_FILE',1,NTFILE,LENGTH,IER)
        IF (IER.EQ.0) 
     &    CALL EZGETS('NTUPLE_TITLE',1,NTUP_TITLE,LENGTH,IER)
        IF (IER.EQ.0) CALL EZGET('NTUPLE_PRIMARY_ALLOCATION',NTUP_PRIM,
     &    IER)
        IF (IER.EQ.0) CALL EZGET('NTUPLE_ID',NTUP_ID,IER)
        IF (IER.EQ.0) CALL EZGET('USER_ID',USER_ID,IER)
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_NTUPLE',' ','F')
C
        INEXT = 0
        NTUPLE_VARNAMES(INEXT+1) = 'RUN'
        NTUPLE_VARNAMES(INEXT+2) = 'EVENT'
        INEXT = INEXT + 2
C
        NTUPLE_VARNAMES(INEXT+1) = 'EVTYP'
        INEXT = INEXT + 1
        DO PTYPE = 1, 4
          MAX_CAND = 3
          IF (PTYPE.EQ.1) CPTYPE = 'L'
          IF (PTYPE.EQ.2) CPTYPE = 'J'
          IF (PTYPE.EQ.3) CPTYPE = 'N'
          IF (PTYPE.EQ.4) CPTYPE = 'P'
          IF (PTYPE.EQ.2) MAX_CAND = 5
          DO IDP = 1, MAX_CAND
            WRITE(STR,'(I1)') IDP
            IF (PTYPE.EQ.1) THEN
              NTUPLE_VARNAMES(INEXT+1) = 'ID'//CPTYPE//STR
              INEXT = INEXT + 1
            ENDIF
            NTUPLE_VARNAMES(INEXT+1) = 'PX'//CPTYPE//STR
            NTUPLE_VARNAMES(INEXT+2) = 'PY'//CPTYPE//STR
            NTUPLE_VARNAMES(INEXT+3) = 'PZ'//CPTYPE//STR
            INEXT = INEXT + 3
            IF (PTYPE.EQ.1) THEN
              NTUPLE_VARNAMES(INEXT+1) = 'P'//CPTYPE//STR
              NTUPLE_VARNAMES(INEXT+2) = 'PT'//CPTYPE//STR
            ELSE
              NTUPLE_VARNAMES(INEXT+1) = 'E'//CPTYPE//STR
              NTUPLE_VARNAMES(INEXT+2) = 'ET'//CPTYPE//STR
            END IF
            NTUPLE_VARNAMES(INEXT+3) = 'TH'//CPTYPE//STR
            INEXT = INEXT + 3
            NTUPLE_VARNAMES(INEXT+1) = 'ETA'//CPTYPE//STR
            NTUPLE_VARNAMES(INEXT+2) = 'PHI'//CPTYPE//STR
            INEXT = INEXT + 2
            IF (PTYPE.EQ.1) THEN
              DO NP = 1, 5
                WRITE(STR2,'(I1)') NP
                NTUPLE_VARNAMES(INEXT+NP) = 'IS'//STR2//CPTYPE//STR
              END DO
              INEXT = INEXT + 5
            ENDIF
          ENDDO
        END DO
        NTUPLE_VARNAMES(INEXT+1) = 'HT'
        INEXT = INEXT + 1
        NTUPLE_VARNAMES(INEXT+1) = 'SPHER'
        INEXT = INEXT + 1
        NTUPLE_VARNAMES(INEXT+1) = 'PLAN'
        INEXT = INEXT + 1
        NTUPLE_VARNAMES(INEXT+1) = 'GSPHER'
        INEXT = INEXT + 1
        NTUPLE_VARNAMES(INEXT+1) = 'GAPLAN'
        INEXT = INEXT + 1
        NTUPLE_VARNAMES(INEXT+1) = 'GY'
        INEXT = INEXT + 1
        NTUPLE_VARNAMES(INEXT+1) = 'EMAXSH'
        INEXT = INEXT + 1
        NTUPLE_VARNAMES(INEXT+1) = 'ETMAXSH'
        INEXT = INEXT + 1
        NTUPLE_VARNAMES(INEXT+1) = 'EFOURSH'
        INEXT = 0
        NTUPLE_QUANTITIES(INEXT+1) = NUMRUN
        INEXT = INEXT + 1
        NTUPLE_QUANTITIES(INEXT+1) = NUMEVT
        INEXT = INEXT + 1
        CALL TOP_LEPTONS_NTUPLE_LEPAN(TOP_LEPTONS_NVAR,TOP_LEPTONS_QUAN,
     &                                MET_VEC)
        CALL UCOPY(TOP_LEPTONS_QUAN,NTUPLE_QUANTITIES(INEXT+1),
     &             TOP_LEPTONS_NVAR)
        INEXT = INEXT + TOP_LEPTONS_NVAR
        NTUPLE_DIM       = INEXT
C
        NUSERS = 0
C
      END IF
C
      IF (.NOT.DO_NTUPLE) GOTO 999
C
C **** Assume last event... fill all info and exit
C
      IF (NTUSERID.LT.0) THEN
        CALL DHDIR(' ','//PAWC',IER,' ')
        IF (NTUPFILE_OPENED) THEN
          CALL NTUPLE_FILL(TOP_DIR,NTUP_ID,NTUPLE_QUANTITIES,IER)
          IF(IER.NE.0) 
     &      CALL ERRMSG('Error filling Ntuple',TOP_DIR,' ','W')
        ENDIF
        LAST = .TRUE.
        GOTO 999
      ENDIF
      IF (LAST) GOTO 999
C
C ****  NTUPLE file and variable handling
C
      IF (NUMRUN.EQ.OLD_RUN.AND.NUMEVT.EQ.OLD_EVENT) THEN
C
C ****  Keep on accumulating information to be filled in NTUPLE
C
        IF (.NOT.NTUPFILE_OPENED) THEN
          IF ( NTUPLE_DIM+NUM_MORE .GT. TOP_LEPTONS_NVAR+MORE_MAX ) THEN
            CALL ERRMSG('TOP_LEPTONS','TOP_LEPTONS_NTUPLE',
     &        'TOO MANY ADDITIONAL NAMES ','W')
            RETURN
          ENDIF
C
C ****  Set user ID and get NTUPLE_DIMension on first event
C
          IF (.NOT.FREEZE_USERS) THEN
            NUSERS = NUSERS + 1
            NTUP_UID(NUSERS)    = NTUSERID
            NTUP_OFFSET(NUSERS) = NTUPLE_DIM
          END IF
          INEXT      = NTUPLE_DIM
          NTUP_USER  = NUSERS
          NTUPLE_DIM = NTUPLE_DIM + NUM_MORE
C
C ****  Fill user NTUP TAGS
C
          DO I = 1, NUM_MORE
            NTUPLE_VARNAMES(INEXT+I)   = MORE_NAMES(I)
          END DO
        ELSE
C
C ****  Find user 
C
          NTUP_USER = 0
          DO I = 1, NUSERS
            IF (NTUSERID.EQ.NTUP_UID(I)) THEN
              INEXT = NTUP_OFFSET(I)
              NTUP_USER = I
              GOTO 100
            END IF
          END DO
  100     CONTINUE
          IF (NTUP_USER.EQ.0) THEN
            CALL ERRMSG('TOP_LEPTONS','TOP_LEPTONS_NTUPLE',
     &        'USER ID IS INVALID --- NOT INTIALIZED ','W')
            RETURN
          ENDIF
        ENDIF
C
C ****  Fill user NTUP quantities
C
        DO I = 1, NUM_MORE
          NTUPLE_QUANTITIES(INEXT+I) = MORE_QUANS(I)
        END DO
C
      ELSE IF (NUMRUN.NE.OLD_RUN.OR.NUMEVT.NE.OLD_EVENT) THEN
C
C ****  OPEN NTUPLE FILE, on the second event or if not yet opened
C
        IF (.NOT.NTUPFILE_OPENED) THEN
          NTUPFILE_OPENED = .TRUE.
          FREEZE_USERS    = .TRUE.
          CALL NTUPLE_FILE_OPEN(USER_ID,.TRUE.,NTFILE,NTUP_PRIM,
     &      TOP_DIR,IER)
          IF(IER.NE.0)CALL ERRMSG('Error opening Ntuple File',TOP_DIR,
     &      NTFILE,'F')
          CALL NTUPLE_SET_ID(NTUP_ID,1)
          CALL DHDIR(' ','//PAWC',IER,' ')
          CALL NTUPLE_BOOK(TOP_DIR,NTUPLE_DIM,NTUPLE_VARNAMES,
     &      NTUP_TITLE,NTUP_ID,IER)
          IF(IER.NE.0)CALL ERRMSG('Error booking Ntuple',TOP_DIR,' ',
     &      'F')
        ENDIF
C
C ****  Fill the ntuple with info collected for the previous event
C
        CALL DHDIR(' ','//PAWC',IER,' ')
        CALL NTUPLE_FILL(TOP_DIR,NTUP_ID,NTUPLE_QUANTITIES,IER)
        IF(IER.NE.0) 
     &   CALL ERRMSG('Error filling Ntuple',TOP_DIR,' ','W')
C
C ****  Now start collecting info for the current event
C
        DO I = 1, NTUPLE_DIM
          NTUPLE_QUANTITIES(I) = 0.
        END DO
        INEXT = 0
        OLD_RUN   = NUMRUN 
        OLD_EVENT = NUMEVT
        NTUPLE_QUANTITIES(INEXT+1) = NUMRUN
        INEXT = INEXT + 1
        NTUPLE_QUANTITIES(INEXT+1) = NUMEVT
        INEXT = INEXT + 1
        CALL TOP_LEPTONS_NTUPLE_LEPAN(TOP_LEPTONS_NVAR,
     &    TOP_LEPTONS_QUAN,MET_VEC)
        CALL UCOPY(TOP_LEPTONS_QUAN,NTUPLE_QUANTITIES(INEXT+1),
     &             TOP_LEPTONS_NVAR)
        INEXT = INEXT + TOP_LEPTONS_NVAR
C
C ****  find the userid and fill ntuple quantities at the appropriate place
C
        NTUP_USER = 0
        DO I = 1, NUSERS
          IF (NTUSERID.EQ.NTUP_UID(I)) THEN
            INEXT = NTUP_OFFSET(I)
            NTUP_USER = I
            GOTO 101
          END IF
        END DO
  101   CONTINUE
        IF (NTUP_USER.EQ.0) THEN
          CALL ERRMSG('TOP_LEPTONS','TOP_LEPTONS_NTUPLE',
     &      'USER ID IS INVALID --- NOT INTIALIZED ','W')
          RETURN
        ENDIF
C
C ****  NOW fill user quantities
C
        DO I = 1, NUM_MORE
          NTUPLE_QUANTITIES(INEXT+I) = MORE_QUANS(I)
        END DO
C
      ENDIF
C
  999 RETURN
      END
