      FUNCTION L2_COMPARE_FILT(LUN,PRTLEV,SET,TRIED,PASSED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      compare two FILT bank SET, TRIE, and PASSED words of 128 bits
C-      
C-      does NOT compare the UNBIASED bits!!!!!
C-      
C-   Inputs  : LUN     logical unit number to dump complaints
C-             PRTLEV I   set it to zero, errmsg are printed out
C-             SET  I[4,2] set bits
C-             TRIED  I[4,2] set bits
C-             PASSED I[4,2] set bits (MODE 0) or passed bits (MODE 1)
C-   Outputs : L2_COMPARE_FILT [L] TRUE if they are the same
C-
C-   INTERNAL:  COUNT I["COND",128] contains tabulated results.
C-              COUNT will be initialized to zero first time, and
C-              be printed out with a call to L2_COMPARE_FILT_OUT
C-   Controls:
C-
C-   Created   9-APR-1992   Drew Baden
C-   Updated  14-JUL-1994   Lewis Taylor Goss--corrected various bugs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LUN,PRTLEV,PASSED(4,2),SET(4,2),TRIED(4,2)
      LOGICAL L2_COMPARE_FILT,L2_COMPARE_FILT_OUT
C
      INTEGER COND
      PARAMETER (COND = 11)
      INTEGER COUNT(COND,128)
      INTEGER I,J,BIT
      LOGICAL BTEST,THIS_BIT,FIRST
      LOGICAL O_SET,O_TRIED,O_PASSED,OSET(128),OTRIED(128),OPASSED(128)
      LOGICAL S_SET,S_TRIED,S_PASSED,SSET(128),STRIED(128),SPASSED(128)
      LOGICAL ATALL(128)
      CHARACTER*80 MSG
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        DO I=1,128
          ATALL(I) = .FALSE.
          DO J=1,COND
            COUNT(J,I) = 0
          ENDDO
        ENDDO
        FIRST = .FALSE.
      ENDIF
C
      L2_COMPARE_FILT = .TRUE.
C
C     to make coding easier, unpack the SET and PASSED bits, which
C     come to you packed via SET and PASSED (which are just copied
C     from the FILT bank directly).   interestingly enough, if you
C     screw up the following tight little loop of code, NOTHING
C     DOWNSTREAM HAS ANY MEANING!!!
C     
C     NOTE:  O stands for "original", S stands for "simulation"
C     and it is assumed that the linear chains always start with
C     the "original" (i.e. data)
C
      BIT = 0
      DO I=1,4
        DO J=1,32
          BIT = BIT + 1
          OSET(BIT) = BTEST(SET(I,2),J)
          OTRIED(BIT) = BTEST(TRIED(I,2),J)
          OPASSED(BIT) = BTEST(PASSED(I,2),J)
          SSET(BIT) = BTEST(SET(I,1),J)
          STRIED(BIT) = BTEST(TRIED(I,1),J)
          SPASSED(BIT) = BTEST(PASSED(I,1),J)
        ENDDO
      ENDDO
C
C     now, check SET bits and PASSED bits.  the SET bits ENABLE specific 
C     Level 2 scripts (or trigger algorithms), and are set if a Level 1
C     requirement (aka trigger) was satisfied.  the PASSED bits tells
C     you if the Level 2 script (aka trigger) passed.  
C
      DO BIT=1,128
        COUNT(1,BIT) = COUNT(1,BIT) + 1
        O_SET = OSET(BIT)
        O_TRIED = OTRIED(BIT)
        O_PASSED = OPASSED(BIT)
        S_SET = SSET(BIT)
        S_TRIED = STRIED(BIT)
        S_PASSED = SPASSED(BIT)
        THIS_BIT = .TRUE.
        IF (O_SET.OR.O_TRIED.OR.O_PASSED.OR.
     &      S_SET.OR.S_TRIED.OR.S_PASSED) ATALL(BIT) = .TRUE.
C
C       we should always have a SET bit on for the PASSED bit to be on,
C       AND we should always have a TRIED bit on for the SET bit on.
C       THIS is the first logical thing to check
C       
        IF (O_SET.AND.(.NOT.O_TRIED)) THEN
          WRITE(MSG,'(''DATA/FILT bank SET bit '',I3,
     &      '' .T. with TRIED bit .F.'')') bit
          IF (PRTLEV.LT.1)
     &      CALL ERRMSG('L2CMPFLTDIOT','L2_COMPARE_FILT',MSG,'E')
          THIS_BIT = .FALSE.
          COUNT(2,BIT) = COUNT(2,BIT) + 1
        ENDIF
        IF (O_PASSED.AND.(.NOT.O_SET)) THEN
          WRITE(MSG,'(''DATA/FILT bank PASSED bit '',I3,
     &      '' .T. with SET bit .F.'')') bit
          IF (PRTLEV.LT.1)
     &      CALL ERRMSG('L2CMPFLTDIOS','L2_COMPARE_FILT',MSG,'E')
          THIS_BIT = .FALSE.
          COUNT(3,BIT) = COUNT(3,BIT) + 1
        ENDIF
        IF (S_SET.AND.(.NOT.S_TRIED)) THEN
          WRITE(MSG,'(''SIMULATION/FILT bank SET bit '',I3,
     &      '' .T. with TRIED bit .F.'')') bit
          IF (PRTLEV.LT.1)
     &      CALL ERRMSG('L2CMPFLTSIOT','L2_COMPARE_FILT',MSG,'E')
          THIS_BIT = .FALSE.
          COUNT(4,BIT) = COUNT(4,BIT) + 1
        ENDIF
        IF (S_PASSED.AND.(.NOT.S_SET)) THEN
          WRITE(MSG,'(''SIMULATION/FILT bank PASSED bit '',I3,
     &      '' .T. with SET bit .F.'')') bit
          IF (PRTLEV.LT.1)
     &      CALL ERRMSG('L2CMPFLTSIOS','L2_COMPARE_FILT',MSG,'E')
          THIS_BIT = .FALSE.
          COUNT(5,BIT) = COUNT(5,BIT) + 1
        ENDIF

C       bail out?
C
        IF (THIS_BIT) THEN
C
C         ok, now we know things are kosher - see if this trigger 
C         compares favorably (or not)
C       
          IF (O_SET.AND.S_SET) THEN
C
C           both bits set, this script was enabled for this event - 
C           can check passed bits
C
            IF (O_PASSED.AND.S_PASSED) THEN
C
C             looks good - nothing really to do here since I am only
C             interested in the errors
C
              COUNT(6,BIT) = COUNT(6,BIT) + 1
C           
            ELSE IF (.NOT.(O_PASSED.OR.S_PASSED)) THEN
C
C             this is also OK - both bits enabled, both scripts failed!!!
C
              COUNT(7,BIT) = COUNT(7,BIT) + 1
            ELSE IF (O_PASSED.AND.(.NOT.S_PASSED)) THEN
C 
C             data passed but simulation did not - this could NOT be
C             due to prescaling because both SET bits were ON!!!
C
              COUNT(8,BIT) = COUNT(8,BIT) + 1
              THIS_BIT = .FALSE.
            ELSE IF ((.NOT.O_PASSED).AND.S_PASSED) THEN
C
C             data failed but simulation did not
C
              COUNT(9,BIT) = COUNT(9,BIT) + 1
              THIS_BIT = .FALSE.
            ENDIF
C
          ELSE IF (O_SET.AND.(.NOT.S_SET)) THEN
C
C           original set but not simulation - probably a L1SIM problem
C           BUT we don't need to worry about the PASSED bits cause
C           they WON'T agree in any case
C 
            COUNT(10,BIT) = COUNT(10,BIT) + 1
            THIS_BIT = .FALSE.
          ELSE IF (S_SET.AND.(.NOT.O_SET)) THEN
C
C           simulation set but not original - probably due to prescaling
C           BUT we don't need to worry about the PASSED bits cause
C           they WON'T agree in any case
C
            COUNT(11,BIT) = COUNT(11,BIT) + 1
            THIS_BIT = .FALSE.
          ENDIF
        ENDIF
C
C       now what do we do?
C
        IF (.NOT.THIS_BIT) L2_COMPARE_FILT = .FALSE.
      ENDDO
C
      RETURN
C
      ENTRY L2_COMPARE_FILT_OUT(LUN)
C
C     outputs results from tabulation (COUNT) onto LUN
C
      WRITE(LUN,'('' ======> L2_COMPARE_FILT ouput <======'')')
C 
      WRITE(LUN,'('' KEY:  S/T means Set  bit .T. / Try bit .F.'',/,
     &            ''       P/S means Pass bit .T. / Set bit .F.'',/,
     &            ''       D*S means Data .T. / Simu .F.'',/,
     &            ''       S*D means Simu .T. / Data .F.'')')
      WRITE(LUN,'(''                 INCONSISTENCIES      '',
     &  ''          -----Disagrees------'',/,
     &  ''               --DATA--   --SIMU--  --Agrees---  '',
     &  ''Pass bit   Set bit'',/,
     &  ''Script  Tries  S/T  P/S   S/T  P/S  Passes Fail  '',
     &  ''D*S  S*D   D*S  S*D'')')
      I = 1
      DO BIT=1,128
        IF (MOD(I,20).EQ.0) THEN
          WRITE(LUN,'(''                 INCONSISTENCIES      '',
     &  ''          -----Disagrees------'',/,
     &  ''               --DATA--   --SIMU--  --Agrees---  '',
     &  ''Pass bit   Set bit'',/,
     &  ''Script  Tries  S/T  S/P   S/T  S/P  Passes Fail  '',
     &  ''D*S  S*D   D*S  S*D'')')
          ENDIF
        IF (ATALL(BIT)) THEN
          WRITE(LUN,'(I5,I8,2I5,1X,2I5,1X,2I6,2I5,1X,2I5)')
     &        BIT,(COUNT(J,BIT),J=1,11)
          I = I + 1
        ENDIF
      ENDDO
C
      RETURN
      END
