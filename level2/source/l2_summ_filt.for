      LOGICAL FUNCTION L2_SUMM_FILT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Look at existing L2 bits and set counts if possible
C-
C-   Inputs  : the event and filter script
C-   Outputs : L2_SUMM_FILT= .TRUE. if want to write this event
C              /FILTER_COM/ counts updated
C               FILT bank with SET , TRIED, PASSED, UNBIASED bits
C-   Controls: None
C-
C-   Created 4-JUN-1992   James T. Linnemann
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FILTER_COM.INC'
      INCLUDE 'D0$PARAMS:L2_TYPE.DEF'
      INCLUDE 'D0$INC:FILTER_TIMING.INC'
      INCLUDE 'D0$INC:FSUM.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL SCRIPT_PASS,INSIST_PASS,FORCE_FAILURE,WRITE_EVENT,
     &  WRITE_BIT,TRY_EVENT,PASS_EVENT,UNBIAS_EVENT
      INTEGER I,J,K,M,N,T,JINDE,SET_BIT(4),TRIED_BIT(4),PASSED_BIT(4),
     &  UNBIASED_BIT(4),IPASS
      BYTE LAST_USED(2,0:127) ! last tool and parameter set to run on each bit
      INTEGER LFILT,GZFILT,PAR_SET,LFRES,GZFRES,TLAST
      CHARACTER*4 OLDPATH
      LOGICAL L2BIT_SET,L2BIT_TRIED,L2BIT_PASSED,L2BIT_UNBIASED
C----------------------------------------------------------------------
      L2_SUMM_FILT = .FALSE.
      TRY_EVENT = .FALSE.     !not tried
      PASS_EVENT = .FALSE.     !no filter passed
      WRITE_EVENT = .FALSE.   !not decided to write yet
      UNBIAS_EVENT = .FALSE.   !none written unfiltered yet

      CALL VZERO(LAST_USED,64)
      LFRES = GZFRES()           ! get it just before using it
      CALL UCOPY(IQ(LFRES+2),LAST_USED,64)  !retreive info about last tool used

      DO K= 0,127 !script number 0-127
        IF(L2BIT_SET(K)) THEN
          FILTER_SET_COUNT(K)=FILTER_SET_COUNT(K)+1 ! L1 bit for script K on
        ENDIF
        I=K/32+1            !word 1-4  (128 bits)
        J=MOD(K,32)         !bit in word
        IF (L2BIT_TRIED(K)) THEN
C
C...  FILTER_COUNT(try/pass,tool_in_script,script); tool 0 is whole script
          FILTER_COUNT(1,0,K)=FILTER_COUNT(1,0,K)+1     !script K tried
          TRY_EVENT = .TRUE.    !one script tried
        ENDIF
C... now test other bits (in fact ignoring the TRIED and SET bits)
        SCRIPT_PASS= L2BIT_PASSED(K)
        INSIST_PASS=.FALSE.   ! true if tool insists on passing event
        IF(SCRIPT_PASS.OR.INSIST_PASS) THEN
          WRITE_BIT = .TRUE.    !write any stream controlled by this bit
          PASS_EVENT = .TRUE.   !the event was really passed
          FILTER_COUNT(2,0,K)=FILTER_COUNT(2,0,K)+1 !script K passes
        ELSE
          WRITE_BIT = .FALSE.   !won't write unless unbiased
        ENDIF
        IF(L2BIT_UNBIASED(K)) THEN
          WRITE_BIT = .TRUE.    !write any stream controlled by this bit
          UNBIAS_EVENT = .TRUE. !this event is marked and passed
          WRITE_UNFILT_COUNT(K) = WRITE_UNFILT_COUNT(K) + 1
        ENDIF
C
C...here save the true results of level 2 filtering, as seen by outside world
        IF (WRITE_BIT) THEN
          WRITE_EVENT = .TRUE. !L2 bits start out on, if their L1 bit was on
          FILTER_WRITE_COUNT(K) = FILTER_WRITE_COUNT(K) + 1
        ENDIF
C
C...now try to reconstruct tool-level information; will fail if tool numbers
C   differ from those in the ONLINE L2TOOL.DAT.
C     Also, if the last tool used in a script actually appears in the script
C     more than once, it is ASSUMED that the script ended at the first call
C
        IF (L2BIT_TRIED(K)) THEN
          TLAST = LAST_USED(1,K)      !last tool used
C
C...FILTER_SCRIPT(tool_NUM/par_set,nth_tool_of_script,script)
          M=1
          T = FILTER_SCRIPT(1,M,K)  !tool number (in the L2TOOL.DAT where run)
          DO WHILE (T.NE.0)
            FILTER_COUNT(1,M,K)=FILTER_COUNT(1,M,K)+1   !Mth tool tries
            IF ( (T.EQ.TLAST).OR.(FILTER_SCRIPT(1,M+1,K).EQ.0) ) THEN
              IF(SCRIPT_PASS)FILTER_COUNT(2,M,K)=FILTER_COUNT(2,M,K)+1
              T = 0  !end of script; force failure of loop test
            ELSE
              FILTER_COUNT(2,M,K)=FILTER_COUNT(2,M,K)+1 !not last tool => passed
              M=M+1                     !which tool in script (serial order)
              T = FILTER_SCRIPT(1,M,K)  !tool number
            ENDIF
          ENDDO
        ENDIF
      ENDDO
  999 CONTINUE
      L2_SUMM_FILT = WRITE_EVENT  ! decision to write
      IF (TRY_EVENT) FILT_EVENTS(1) = FILT_EVENTS(1) + 1
      IF (WRITE_EVENT) FILT_EVENTS(2) = FILT_EVENTS(2) + 1
      IF (PASS_EVENT) FILT_EVENTS(3) = FILT_EVENTS(3) + 1
      IF (UNBIAS_EVENT) FILT_EVENTS(4) = FILT_EVENTS(4) + 1
      RETURN
      END
