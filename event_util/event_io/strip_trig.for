      FUNCTION STRIP_TRIG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns FALSE (rejects the event) for
C-                         all triggers except those selected in
C-                         STRIP_RCP.  One can require one of a list of
C-                         L1 trigger bits, one of a list of L2 trigger
C-                         bits, one of either set, or one of both
C-                         sets.  Also calls a dummy routine, USER_STRIP,
C-                         in which the user could add further selection
C-                         based on the header if desired.
C-
C-   Entry point:  STRIP_TRIG_INI   Sets flag to read header first and
C-                                  reject before reading whole event;
C-                                  reads RCP file for selection criteria
C-
C-   Entry point:  GET_STRIP_SUM    Return number of events rejected
C-
C-   Entry point:  STRIP_DONE(SET_DONE)    Used to tell STRIP_TRIG has
C-                                         already been called for this event
C-
C-   Returned value  : TRUE for selected trigger types
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: Which events are processed
C-
C-   Created  26-MAY-1992   K. Wyatt Merritt
C-   Updated   5-OCT-1992   K. Wyatt Merritt  Change it to use names instead
C-                                            of numbers
C-   UPDATED   1-JAN-1994   H. Greenlee
C-       Add DO_HIST flag, Micro blank cut.
C-   Updated  17-FEB-1994   K. Wyatt Merritt  Fix bug in using L2 numbers 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL STRIP_TRIG,STRIP_TRIG_INI,GET_STRIP_SUM,STRIP_DONE
      LOGICAL FIRST,USER_ONLY,LP,DONE,SET_DONE
      LOGICAL L1BIT_PASSED,L2BIT_PASSED
      LOGICAL USE_HEADER,STRIP_ON_L1,STRIP_ON_L2,STRIP_ON_BOTH
      LOGICAL PASS_L1,PASS_L2,PASS_USER,OK,MATCH_WILD,USE_NUMBERS
      LOGICAL DO_HIST, DO_MICRO_BLANK_CUT
      LOGICAL MICRO_BLANK_TRGR, MICRO_BLANK_HEAD, MICRO_BLANK_FLAG
      LOGICAL MICRO_BLANK          ! Entry point in AND_OR_TERMS.FOR
C
      INTEGER L1_BIT_REQ(32),L1BITS(32)
      INTEGER L2_BIT_REQ(128),L2BITS(128)
      INTEGER IMASK,I,J,NBITS
      INTEGER STATUS
      INTEGER NPASS,NREJECTED,NREJ_USER,NREJ_L1,NREJ_L2,
     &  NREJ_MICROBLANK
      INTEGER N1,N2,N3,N4,N5,N6
      INTEGER NTRIGON,NFILTON,LEN1,TBIT_ON(32),FBIT_ON(128)
      INTEGER NTSTRING_REQ,NFSTRING_REQ
      INTEGER NDIAG
      INTEGER IDUM1,IDUM2,IDUM3
      INTEGER TRIGSUM(32),FILTSUM(128)
      INTEGER TSUM(32),FSUM(128)
C
      CHARACTER*80 MSG
      CHARACTER*32 TNAME_ON(32),FNAME_ON(128)
      CHARACTER*32 TSTRING(32),FSTRING(128)
      CHARACTER*32 TSTRING_REQ(32),FSTRING_REQ(128)
      CHARACTER*32 SEARCH_STRING
      CHARACTER*1 CDUM
C
      DATA FIRST /.TRUE./
      DATA NPASS,NREJECTED,NREJ_USER,NREJ_L1,NREJ_L2 / 0,0,0,0,0 /
C----------------------------------------------------------------------
      IF(DONE) THEN       ! already called for header record
        STRIP_TRIG=.TRUE.
        GOTO 999
      ENDIF
      PASS_L1 = .TRUE.
      PASS_L2 = .TRUE.
C
C   Does this event pass?
C
      IF(DO_HIST)THEN
        CALL HCDIR('//PAWC/TRIGBITS',' ')
        CALL HIST_TRIG1(10)
        CALL HIST_TRIG2(20)
      ENDIF
      IF (.NOT.USE_NUMBERS)THEN
C*************************************************************************
C
C  This section uses trig/filt names
C
        CALL GTTSUM(NTRIGON,TBIT_ON,TNAME_ON,NFILTON,FBIT_ON,FNAME_ON)
C       IF (NDIAG.LE.50) CALL PRTSUM(6,IDUM1,IDUM2,CDUM,IDUM3)
C
C*************************************************************************
        IF (STRIP_ON_L1) THEN
          PASS_L1 = .FALSE.
          DO I = 1,NTSTRING_REQ
            SEARCH_STRING = TSTRING_REQ(I)
            DO J = 1,NTRIGON
              OK = MATCH_WILD(TNAME_ON(J),SEARCH_STRING)
              PASS_L1 = PASS_L1 .OR. OK
              IF ( OK ) THEN
                TRIGSUM(TBIT_ON(J)) = TRIGSUM(TBIT_ON(J)) + 1
              ENDIF
            ENDDO
          ENDDO
        ENDIF
C
C  Calculate whether the event passes a selection on L2 trigger name
C
        IF (STRIP_ON_L2) THEN
          PASS_L2 = .FALSE.
          DO I = 1,NFSTRING_REQ
            SEARCH_STRING = FSTRING_REQ(I)
            DO J = 1,NFILTON
              OK = MATCH_WILD(FNAME_ON(J),SEARCH_STRING)
              PASS_L2 = PASS_L2 .OR. OK
              IF ( OK ) THEN
                FILTSUM(FBIT_ON(J)) = FILTSUM(FBIT_ON(J)) + 1
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ELSE
C-
C- Strip on trigger/filter number
C-
        IF (STRIP_ON_L1) THEN
          PASS_L1 = .FALSE.
          DO I = 1,32
            IF (L1_BIT_REQ(I).NE.0) THEN
              J = I - 1
              LP = L1BIT_PASSED(J)
              PASS_L1 = PASS_L1 .OR. LP
            ENDIF
          ENDDO
        ENDIF
C
        IF(STRIP_ON_L2)THEN
          PASS_L2 = .FALSE.
          DO I = 1,128
            IF (L2_BIT_REQ(I).NE.0) THEN
              J = I - 1
              LP = L2BIT_PASSED(J)
              PASS_L2 = PASS_L2 .OR. LP
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C-
C- Level 1 statistics
C-
      IF ( .NOT. PASS_L1 ) THEN
        NREJ_L1 = NREJ_L1 + 1
        IF(DO_HIST)CALL HIST_TRIG1(101)
      ELSE
        IF(DO_HIST)CALL HIST_TRIG1(100)
      ENDIF
C-
C- Level 2 statistics
C-
      IF ( .NOT. PASS_L2 ) THEN
        NREJ_L2 = NREJ_L2 + 1
        IF(DO_HIST)CALL HIST_TRIG2(201)
      ELSE
        IF(DO_HIST)CALL HIST_TRIG2(200)
      ENDIF
C-
C- Combine level 1/level 2
C-
      IF (STRIP_ON_BOTH) THEN
        STRIP_TRIG = PASS_L1 .AND. PASS_L2
      ELSE
        STRIP_TRIG = PASS_L1 .OR. PASS_L2
      ENDIF
C-
C- Micro_blank cut
C-
      IF(DO_MICRO_BLANK_CUT.AND.LHEAD.NE.0)THEN
        MICRO_BLANK_HEAD = BTEST(IQ(LHEAD+30),0)
        MICRO_BLANK_TRGR = MICRO_BLANK(STATUS)
        IF(STATUS.EQ.0)THEN
          MICRO_BLANK_FLAG = .NOT.MICRO_BLANK_TRGR
        ELSE
          MICRO_BLANK_FLAG = .NOT.MICRO_BLANK_HEAD
        ENDIF
      ELSE
        MICRO_BLANK_FLAG = .TRUE.
      ENDIF
      IF(.NOT.MICRO_BLANK_FLAG)NREJ_MICROBLANK = NREJ_MICROBLANK + 1
      STRIP_TRIG = STRIP_TRIG .AND. MICRO_BLANK_FLAG
C
C   User routine can provide substitute rejection code (USER_ONLY .T.)
C   or additional rejection code (USER_ONLY .F.).  Default in the library
C   returns L = .T., USER_ONLY = .F.
C
      CALL USER_STRIP(PASS_USER,USER_ONLY)
      IF (USER_ONLY) THEN
        STRIP_TRIG = PASS_USER
      ELSE
        STRIP_TRIG = PASS_USER .AND. STRIP_TRIG
      ENDIF
      IF ( .NOT. PASS_USER ) THEN
        NREJ_USER = NREJ_USER + 1
        IF(DO_HIST)THEN
          CALL HIST_TRIG1(301)
          CALL HIST_TRIG2(401)
        ENDIF
      ELSE
        IF(DO_HIST)THEN
          CALL HIST_TRIG1(300)
          CALL HIST_TRIG2(400)
        ENDIF
      ENDIF
C
      IF ( STRIP_TRIG ) THEN
        NPASS = NPASS + 1
        IF(DO_HIST)THEN
          CALL HIST_TRIG1(500)
          CALL HIST_TRIG2(600)
        ENDIF
      ELSE
        NREJECTED = NREJECTED + 1
        IF(DO_HIST)THEN
          CALL HIST_TRIG1(501)
          CALL HIST_TRIG2(601)
        ENDIF
      ENDIF
C
      IF(DO_HIST)CALL HCDIR('//PAWC',' ')
      GOTO 999
C
C
      ENTRY STRIP_DONE(SET_DONE)
      STRIP_DONE=.TRUE.
      DONE=SET_DONE
  999 RETURN
C--------------------------------------------------------------------------
C
C****   Initialization entry point
C
      ENTRY STRIP_TRIG_INI()
C
      STRIP_TRIG_INI = .TRUE.
      IF(DO_HIST)CALL HIST_TRIG_BOOK
C
C
C     Extract the strip request parameters from STRIP_RCP
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL VZERO(L1_BIT_REQ,32)
        CALL VZERO(L2_BIT_REQ,128)
        CALL INRCP('STRIP_RCP',STATUS)
        IF (STATUS .NE. 0) THEN
          CALL ERRMSG('D0USER','STRIP_TRIG','Unable to open STRIP_RCP',
     &      'F')
        ENDIF
        CALL EZPICK('STRIP_RCP')
        CALL EZGET_l('USE_HEADER',USE_HEADER,STATUS)
        CALL EZGET_l('DO_HIST',DO_HIST,STATUS)
        IF(STATUS.NE.0)THEN
          DO_HIST = .TRUE.
          CALL ERRMSG('D0USER','STRIP_TRIG',
     &      'DO_HIST RCP parameter missing -- set to .TRUE.', 'W')
          STATUS = 0
        ENDIF
        IF(STATUS.EQ.0)CALL EZGET_l('DO_MICRO_BLANK_CUT',
     &    DO_MICRO_BLANK_CUT,STATUS)
        IF(STATUS.NE.0)THEN
          DO_MICRO_BLANK_CUT = .FALSE.
          CALL ERRMSG('D0USER','STRIP_TRIG',
     &      'DO_MICRO_BLANK_CUT RCP parameter missing -- set to .FALSE',
     &      'W')
          STATUS = 0
        ENDIF
        CALL EZGET('STRIP_ON_L1',STRIP_ON_L1,STATUS)
        CALL EZGET('STRIP_ON_L2',STRIP_ON_L2,STATUS)
        CALL EZGET('STRIP_ON_BOTH',STRIP_ON_BOTH,STATUS)
        IF(.NOT.STRIP_ON_L1 .OR. .NOT.STRIP_ON_L2)
     &    STRIP_ON_BOTH = .TRUE.
        IF ( USE_HEADER ) THEN
          CALL EVTIN_HEADER(.TRUE.) ! Enable the reading of the header first
                                    ! to allow early rejection
        ENDIF
        CALL EZGET('USE_NUMBERS',USE_NUMBERS,STATUS)
C
C---- DEFAULT TO USING BIT NUMBERS INSTEAD FOR NAMES IF PARAMETER
C     USE_NUMBERS DOES NOT EXIST, FOR BACKWARD COMPATIBILITY WITH
C     OLD RCP FILES.
C----------------------------------------------------------------
        IF (STATUS .NE. 0 .OR. USE_NUMBERS) THEN
C
C ---  This initializes the stream descriptions using bit numbers.
C
          USE_NUMBERS = .TRUE.
          IF (STRIP_ON_L1) THEN
C
C ****  L1 bits which are requested
            CALL EZGETA('L1BITS',0,0,0,NBITS,STATUS)
            IF ( STATUS .NE. 0 ) THEN
              CALL ERRMSG('D0USER','STRIP-TRIG','Unable to read L1BITS',
     &          'F')
            ELSE
              CALL EZGET_i('L1BITS',L1BITS,STATUS)
            ENDIF
          ENDIF
          DO I = 1,NBITS
            J = L1BITS(I)
            L1_BIT_REQ(J+1) = 1
          ENDDO
C
          IF (STRIP_ON_L2) THEN
C
C ****  L2 bits which are requested
            CALL EZGETA('L2BITS',0,0,0,NBITS,STATUS)
            IF ( STATUS .NE. 0 ) THEN
              CALL ERRMSG('D0USER','STRIP-TRIG','Unable to read L2BITS',
     &          'F')
            ELSE
              CALL EZGET_iarr('L2BITS',L2BITS,STATUS)
            ENDIF
          ENDIF
          DO I = 1,NBITS
            J = L2BITS(I)
            L2_BIT_REQ(J+1) = 1
          ENDDO
          GO TO 600
        ENDIF
C
C     This is the setup for using trigger and filter names to strip
C
        IF (STRIP_ON_L1) THEN
          CALL EZ_GET_CHARS('L1NAMES',NTSTRING_REQ,TSTRING_REQ,
     &        STATUS)
        ENDIF
        IF (STRIP_ON_L2) THEN
          CALL EZ_GET_CHARS('L2NAMES',NFSTRING_REQ,FSTRING_REQ,
     &        STATUS)
        ENDIF
        IF (NTSTRING_REQ .NE. 0)
     &      WRITE (6,8765) NTSTRING_REQ,
     &      (TSTRING_REQ(I),I=1,NTSTRING_REQ)
        IF (NFSTRING_REQ .NE. 0)
     &      WRITE (6,8766) NFSTRING_REQ,
     &      (FSTRING_REQ(I),I=1,NFSTRING_REQ)
 8765   FORMAT(1X,'L1 TRIGS',1X,I3,1X,2(A32,1X),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32))
 8766   FORMAT(1X,'L2 TRIGS',1X,I3,1X,2(A32,1X),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32),/,
     &    2(1X,A32))
C
  600   CALL EZRSET
      ENDIF
C
 1999 RETURN
C--------------------------------------------------------------------------
C
C****   Statistics entry point
C
      ENTRY GET_STRIP_SUM(N1,N2,N3,N4,N5,N6)
C
      GET_STRIP_SUM = .TRUE.
C
      N1 = NPASS
      N2 = NREJECTED
      N3 = NREJ_L1
      N4 = NREJ_L2
      N5 = NREJ_MICROBLANK
      N6 = NREJ_USER
C
 2999 RETURN
      END
 
