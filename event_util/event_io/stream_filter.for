      FUNCTION STREAM_FILTER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Acts like package STRIP_TRIG (q.v.), except that 
C-                         one can have multiple lists of triggers to select,
C-                         each list being directed to a different output
C-                         stream.  The output streams are subsets of STA or
C-                         DST.  That is, if you select output stream STA in
C-                         D0USER, and direct the output file to ALLxxx.X_STA01,
C-                         then define four streams in STREAM_FILTER.RCP,
C-                         for example MIN,QCD,EMF,MUF, then the triggers 
C-                         defined for stream MIN will go to the output file
C-                         ALLxxx.X_STA01_MIN, and so forth.
C-
C-   Entry point:  STREAM_FILTER_INI   Sets flag to read header first and
C-                                  reject before reading whole event;
C-                                  reads RCP file for selection criteria
C-
C-   Entry point:  GET_STREAM_SUM    Return number of events rejected
C-
C-   Entry point:  STREAM_DONE(SET_DONE)    Used to flag that STREAM_FILTER has
C-                                         already been called for this event
C-
C-   Returned value  : TRUE for selected trigger types
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: Which events are processed
C-
C-   Created  26-MAY-1992   K. Wyatt Merritt
C-   Updated   6-JUL-1992   K. Wyatt Merritt  Add multiple lists for different
C-                                            output streams. 
C-   Updated  16-SEP-1992   K. Wyatt Merritt  Allow for using names instead
C-                                            of bit numbers for the triggers 
C-   Updated   3-MAY-1993   K. Wyatt Merritt  Eliminate histograms; return
C-                                            .TRUE. for package even if no
C-                                            event passes so other filter
C-                                            packages can be used downstream of
C-                                            it
C-   Updated  26-JUN-1993   K. Wyatt Merritt   Added exact match option
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER NMAX      ! Maximum number of output streams allowed
      PARAMETER (NMAX = 40)
C
      LOGICAL STREAM_FILTER, STREAM_FILTER_INI, GET_STREAM_SUM
      LOGICAL STREAM_DONE
      LOGICAL FIRST, WRITE_IT, LP, DONE, SET_DONE
      LOGICAL PASS_L1, PASS_L2, PASS_USER, PASS
      LOGICAL L1BIT_PASSED, L2BIT_PASSED
      LOGICAL PACKAGE_OFF,USE_HEADER, USE_NUMBERS
      LOGICAL STRIP_ON_L1(NMAX), STRIP_ON_L2(NMAX), STRIP_ON_BOTH(NMAX)
      LOGICAL USER_ONLY(NMAX), EXACT_MATCH(NMAX), LVAL(5)
      LOGICAL OK, MATCH_WILD
C
      INTEGER L1_BIT_REQ(32, NMAX), L1BITS(32), L1TEMP(1)
      INTEGER L2_BIT_REQ(128, NMAX), L2BITS(128), L2TEMP(1)
      INTEGER I, J, K, NBITS
      INTEGER STATUS
      INTEGER NSTREAM
      INTEGER NPASS(NMAX), NREJECTED(NMAX)
      INTEGER NREJ_USER(NMAX), NREJ_L1(NMAX), NREJ_L2(NMAX)
      INTEGER NS, N1, N2, N3, N4, N5
      INTEGER ISTREAM(NMAX)
      INTEGER ISTART
      INTEGER NTRIGON, NFILTON, TBIT_ON(32), FBIT_ON(128)
      INTEGER NTSTRING_REQ(NMAX), NFSTRING_REQ(NMAX)
      INTEGER TRIGSUM(NMAX, 32), FILTSUM(NMAX, 128)
      INTEGER TSUM(32), FSUM(128)
      INTEGER NELE
C
      CHARACTER*5 STREAM_NAME(NMAX)
      CHARACTER*10 ARRAY_NAME
      CHARACTER*80 MSG
      CHARACTER*32 TNAME_ON(32), FNAME_ON(128)
      CHARACTER*32 TSTRING(32), FSTRING(128)
      CHARACTER*32 TSTRING_REQ(32, NMAX), FSTRING_REQ(128, NMAX)
      CHARACTER*32 SEARCH_STRING
C
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (DONE) THEN       ! already called for header record
        STREAM_FILTER = .TRUE.
        GOTO 999
      ENDIF
C
      STREAM_FILTER= .TRUE.   ! Make it always true, to use with other 
C                             ! types of filter, e.g. in OMNI_FILTER
C
      IF (PACKAGE_OFF) RETURN ! Flag to allow package to be linked in
                              ! without making any streams
      IF (USE_NUMBERS) GO TO 100
C*************************************************************************
C
C  This section uses trig/filt names
C
      CALL GTTSUM(NTRIGON,TBIT_ON,TNAME_ON,NFILTON,FBIT_ON,FNAME_ON)
C
C*************************************************************************
C   Loop over the streams defined in the rcp file
C
  100 DO K = 1 , NSTREAM
        IF (USE_NUMBERS) GO TO 200
C*************************************************************************
C
C  Calculate whether the event passes a selection on L1 trigger name
C
        IF (STRIP_ON_L1(K)) THEN
          PASS_L1 = .FALSE.
          DO I = 1,NTSTRING_REQ(K)
            SEARCH_STRING = TSTRING_REQ(I,K)
            DO J = 1,NTRIGON
              IF (EXACT_MATCH(K)) THEN
                OK = TNAME_ON(J).EQ.SEARCH_STRING
              ELSE
                OK = MATCH_WILD(TNAME_ON(J),SEARCH_STRING)
              ENDIF
              PASS_L1 = PASS_L1 .OR. OK
              IF ( OK ) THEN
                TRIGSUM(K,TBIT_ON(J)) = TRIGSUM(K,TBIT_ON(J)) + 1
              ENDIF
            ENDDO
          ENDDO
          IF ( .NOT. PASS_L1 ) THEN
            NREJ_L1(K) = NREJ_L1(K) + 1
          ENDIF
        ENDIF
C
C  Calculate whether the event passes a selection on L2 trigger name
C
        IF (STRIP_ON_L2(K)) THEN
          PASS_L2 = .FALSE.
          DO I = 1,NFSTRING_REQ(K)
            SEARCH_STRING = FSTRING_REQ(I,K)
            DO J = 1,NFILTON
              IF (EXACT_MATCH(K)) THEN
                OK = FNAME_ON(J).EQ.SEARCH_STRING
              ELSE
                OK = MATCH_WILD(FNAME_ON(J),SEARCH_STRING)
              ENDIF
              PASS_L2 = PASS_L2 .OR. OK
              IF ( OK ) THEN
                FILTSUM(K,FBIT_ON(J)) = FILTSUM(K,FBIT_ON(J)) + 1
              ENDIF
            ENDDO
          ENDDO
          IF ( .NOT. PASS_L2 ) THEN
            NREJ_L2(K) = NREJ_L2(K) + 1
          ENDIF
        ENDIF
C
        GO TO 300
C
C*************************************************************************
C   This section uses trigger bit numbers instead of names
C
  200   CONTINUE
C
C  Calculate whether the event passes a selection on L1 trigger bit
C
        IF (STRIP_ON_L1(K)) THEN
          PASS_L1 = .FALSE.
          DO I = 1,32
            IF (L1_BIT_REQ(I,K).NE.0) THEN
              J = I - 1
              LP = L1BIT_PASSED(J)
              PASS_L1 = PASS_L1 .OR. LP
            ENDIF
          ENDDO
          IF ( .NOT. PASS_L1 ) THEN
            NREJ_L1(K) = NREJ_L1(K) + 1
          ENDIF
        ENDIF
C
C
C  Calculate whether the event passes a selection on L2 trigger bit
C
        IF (STRIP_ON_L2(K)) THEN
          PASS_L2 = .FALSE.
          DO I = 1,128
            IF (L2_BIT_REQ(I,K) .NE. 0) THEN
              J = I - 1
              LP = L2BIT_PASSED(J)
              PASS_L2 = PASS_L2 .OR. LP
            ENDIF
          ENDDO
          IF ( .NOT. PASS_L2 ) THEN
            NREJ_L2(K) = NREJ_L2(K) + 1
          ENDIF
        ENDIF
C
C*************************************************************************
C   User selection and final logic common to both names and numbers versions
C
  300   CONTINUE
        IF ( STRIP_ON_L1(K) .AND. .NOT.STRIP_ON_L2(K) ) THEN
          PASS = PASS_L1
        ELSE IF ( .NOT.STRIP_ON_L1(K) .AND. STRIP_ON_L2(K)) THEN
          PASS = PASS_L2
        ELSE IF ( STRIP_ON_L1(K) .AND. STRIP_ON_L2(K)) THEN
          IF ( STRIP_ON_BOTH(K) ) THEN
            PASS = PASS_L1 .AND. PASS_L2
          ELSE
            PASS = (PASS_L1 .OR. PASS_L2)
          ENDIF
        ELSE
          PASS = .TRUE.
        ENDIF
C
C   User routine can provide substitute rejection code (USER_ONLY .T.)
C   or additional rejection code (USER_ONLY .F.).  Default in the library
C   returns PASS_USER = .T., USER_ONLY = .F., and therefore has no
C   effect on the selections.
C
        IF ( USER_ONLY(K) ) THEN
          CALL USER_STREAM_FILTER(STREAM_NAME(K), PASS_USER,
     &      USER_ONLY(K))
        ELSE
          IF (PASS) THEN
            CALL USER_STREAM_FILTER(STREAM_NAME(K),
     &        PASS_USER, USER_ONLY(K))
            IF ( .NOT. PASS_USER ) THEN
              NREJ_USER(K) = NREJ_USER(K) + 1
            ENDIF
          ENDIF
        ENDIF
C
C  Decide whether or not to keep the event in this stream based on
C  the combination of flags from the rcp file; remember PASS_USER
C  from the library routine is always true! Notice that if all flags
C  for a stream are false (STRIP_ON_L1,STRIP_ON_L2,USER_ONLY) that
C  stream will keep ALL events!!
C
        IF ( USER_ONLY(K) ) THEN
          WRITE_IT = PASS_USER
        ELSE
          WRITE_IT = PASS .AND. PASS_USER
        ENDIF
C
        IF ( WRITE_IT ) THEN
          NPASS(K) = NPASS(K) + 1
          CALL EVSET_STREAM(ISTREAM(K))
        ELSE
          NREJECTED(K) = NREJECTED(K) + 1
        ENDIF
C
      ENDDO
C
      GOTO 999
C
C
      ENTRY STREAM_DONE(SET_DONE)
      STREAM_DONE=.TRUE.
      DONE=SET_DONE
  999 RETURN
C--------------------------------------------------------------------------
C
C****   Initialization entry point
C
      ENTRY STREAM_FILTER_INI()
C
      STREAM_FILTER_INI = .TRUE.
C
C
C     Extract the stream-dependent trigger lists from STREAM_FILTER_RCP
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP('STREAM_FILTER_RCP',STATUS)
        IF (STATUS .NE. 0) THEN
          CALL ERRMSG('STREAM_FILTER','STREAM_FILTER_INI',
     &      'Unable to open STREAM_FILTER_RCP','F')
        ENDIF
        CALL EZPICK('STREAM_FILTER_RCP')
        PACKAGE_OFF = .FALSE.
        CALL EZGET_l('PACKAGE_OFF',PACKAGE_OFF,STATUS)
        IF (PACKAGE_OFF) THEN
          CALL ERRMSG('STREAM_FILTER','STREAM_FILTER_INI',
     &      'WARNING: STREAM_FILTER package is turned OFF','W')
          RETURN
        ENDIF
        CALL VZERO(NPASS,NMAX)
        CALL VZERO(NREJECTED,NMAX)
        CALL VZERO(NREJ_L1,NMAX)
        CALL VZERO(NREJ_L2,NMAX)
        CALL VZERO(NREJ_USER,NMAX)
        CALL VZERO(TRIGSUM,NMAX*32)
        CALL VZERO(FILTSUM,NMAX*128)
        CALL EZGET('USE_HEADER',USE_HEADER,STATUS)
        IF ( USE_HEADER ) THEN
          CALL EVTIN_HEADER(.TRUE.) ! Enable the reading of the header first
                                    ! to allow early rejection
        ENDIF
        CALL EZGETA('STREAM_DESCRIP', 0, 0, 0, NELE, STATUS)
        NSTREAM = NELE/7
        IF (NSTREAM.LT.1 .OR. NSTREAM.GT.NMAX) THEN
          WRITE (MSG,1000) NSTREAM
 1000     FORMAT(' Invalid value for NSTREAM: ',I4)
          CALL ERRMSG('STREAM_FILTER','STREAM_FILTER_INI',MSG,'F')
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
          CALL VZERO(L1TEMP,32*NMAX)
          CALL VZERO(L2TEMP,128*NMAX)
          DO K = 1 , NSTREAM
            ISTART = (K-1)*7 + 1
            CALL EZGETC('STREAM_DESCRIP',ISTART,5,STREAM_NAME(K),STATUS)
            CALL EVGET_STREAM(STREAM_NAME(K),ISTREAM(K))
            CALL EZGETA_iarr('STREAM_DESCRIP',ISTART+2,ISTART+6,1,LVAL,
     &        STATUS)
            STRIP_ON_L1(K) = LVAL(1)
            STRIP_ON_L2(K) = LVAL(2)
            STRIP_ON_BOTH(K) = LVAL(3)
            USER_ONLY(K) = LVAL(4)
            IF (STRIP_ON_L1(K)) THEN
              WRITE (ARRAY_NAME,103) STREAM_NAME(K)(1:3)
  103         FORMAT(A3,'_L1BITS')
              CALL EZGETA(ARRAY_NAME,0,0,0,NBITS,STATUS)
              CALL EZGET_iarr(ARRAY_NAME,L1BITS,STATUS)
              DO I = 1,NBITS
                J = L1BITS(I)
                L1_BIT_REQ(J+1,K) = 1 
              ENDDO
            ENDIF
            IF (STRIP_ON_L2(K)) THEN
              WRITE (ARRAY_NAME,104) STREAM_NAME(K)(1:3)
  104         FORMAT(A3,'_L2BITS')
              CALL EZGETA(ARRAY_NAME,0,0,0,NBITS,STATUS)
              CALL EZGET_i(ARRAY_NAME,L2BITS,STATUS)
              DO I = 1,NBITS
                J = L2BITS(I)
                L2_BIT_REQ(J+1,K) = 1 
              ENDDO
            ENDIF
          ENDDO
          GO TO 600
        ENDIF
C
C ---  This initializes the stream descriptions using trig/filt names.
C
        DO K = 1 , NSTREAM
          ISTART = (K-1)*7 + 1
          CALL EZGETC('STREAM_DESCRIP',ISTART,5,STREAM_NAME(K),STATUS)
          CALL EVGET_STREAM(STREAM_NAME(K),ISTREAM(K))
          CALL EZGETA_iarr('STREAM_DESCRIP',ISTART+2,ISTART+6,1,LVAL
     &         ,STATUS)
          STRIP_ON_L1(K) = LVAL(1)
          STRIP_ON_L2(K) = LVAL(2)
          STRIP_ON_BOTH(K) = LVAL(3)
          USER_ONLY(K) = LVAL(4)
          EXACT_MATCH(K) = LVAL(5)
          IF (EXACT_MATCH(K)) THEN
            WRITE (MSG,6000) STREAM_NAME(K)
 6000       FORMAT(' WARNING: Exact trigger matches required for',
     &        ' stream ',A5)
            CALL ERRMSG('STREAM_FILTER','STREAM_FILTER_INI',
     &        MSG,'W')
          ENDIF
          IF (STRIP_ON_L1(K)) THEN
            WRITE (ARRAY_NAME,101) STREAM_NAME(K)(1:3)
  101       FORMAT(A3,'_TNAMES')
            CALL EZ_GET_CHARS(ARRAY_NAME,NTSTRING_REQ(K),TSTRING,
     &        STATUS)
            DO I = 1,NTSTRING_REQ(K)
              TSTRING_REQ(I,K) = TSTRING(I)
            ENDDO
          ENDIF
          IF (STRIP_ON_L2(K)) THEN
            WRITE (ARRAY_NAME,102) STREAM_NAME(K)(1:3)
  102       FORMAT(A3,'_FNAMES')
            CALL EZ_GET_CHARS(ARRAY_NAME,NFSTRING_REQ(K),FSTRING,
     &        STATUS)
            DO I = 1,NFSTRING_REQ(K)
              FSTRING_REQ(I,K) = FSTRING(I)
            ENDDO
          ENDIF
        ENDDO
C
C Setup common to using numbers or names
C
  600   CONTINUE
        CALL EVTWOS_MULT(.TRUE.)
        CALL EZRSET
      ENDIF
C
 1999 RETURN
C--------------------------------------------------------------------------
C
C****   Statistics entry point
C
      ENTRY GET_STREAM_SUM(NS,N1,N2,N3,N4,N5,TSUM,FSUM)
C
      GET_STREAM_SUM = .TRUE.
C
      N1 = NPASS(NS)
      N2 = NREJECTED(NS)
      N3 = NREJ_L1(NS)
      N4 = NREJ_L2(NS)
      N5 = NREJ_USER(NS)
      DO I = 1,32
        TSUM(I) = TRIGSUM(NS,I)
      ENDDO
      DO I = 1,64
        FSUM(I) = FILTSUM(NS,I)
      ENDDO
C
 2999 RETURN
      END
