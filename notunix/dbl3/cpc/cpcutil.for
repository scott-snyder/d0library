      SUBROUTINE CPC_INIT
!!!      OPTIONS /EXTEND_SOURCE    (NOT NECESSARY ANYMORE   SA)
c************************************************************************
c*
c*       CPCUTIL
c*
c*       Odds and ends used by CPC
c*
c************************************************************************
c************************************************************************
c*
c*       SUBR. CPC_INIT
c*
c*       General CPC initialization. Not called by user.
c*
c************************************************************************
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INCLUDE 'D0$INC:CLUSCOM.INC'
      LOGICAL FIRST /.TRUE./
c*
      IF (.NOT.FIRST) RETURN
c**        init value for IC event flag
      IC__EF = -1
      IF (FIRST) FIRST = .FALSE.
      END
      SUBROUTINE CPC_FINISH
c************************************************************************
c*
c*       SUBR. CPC_FINISH
c*
c*       Pre-exit cleanup. Should be called by all processes using CPC
c*       just before program termination.
c*
c************************************************************************
      INTEGER SYS$DEQ
      INCLUDE 'D0$PARAMS:CPCERR.PARAMS'
      INCLUDE 'D0$INC:INFOCACHE.INC'
      INCLUDE 'D0$INC:CLUSCOM.INC'
      INCLUDE '($SSDEF)'
      INCLUDE '($LCKDEF)'
c*
      IFLAG = LCK$M_DEQALL
      ISTAT = SYS$DEQ(,,,%VAL(IFLAG))
      IF (ISTAT.NE.SS$_NORMAL) CALL LIB$SIGNAL (%VAL(ISTAT))
      NCACHE=0
      NCCOM=0
      END
c************************************************************************
      SUBROUTINE CPC_UCOPY(ISRC,IDEST,N)
      INTEGER ISRC(*), IDEST(*)
      DO I=1,N
        IDEST(I)=ISRC(I)
      ENDDO
      END
c************************************************************************
      FUNCTION LST (S)
C
C CERN PROGLIB# M507    LENOCC          .VERSION KERNFOR  4.10  850320
C ORIG. MARCH 1985
C
C-    FUNCTION TO RETURN THE LENGTH OF A CHARACTER VARIABLE UP TO AND
C-    NOT INCLUDING ANY TRAILING BLANKS.
C
      CHARACTER*(*) S
      PARAMETER   (NCHWD=4)
      CHARACTER    BLANK*(NCHWD)
      DATA  BLANK / '    ' /
      LENDEF = LEN(S)
      NWFULL = LENDEF / NCHWD
      NCFULL = NWFULL * NCHWD
      NTRAIL = LENDEF - NCFULL
      IF (NTRAIL.EQ.0)       GO TO 21

C--                ARE ALL TRAILING CHARACTERS BLANK

      DO 12  I=LENDEF,NCFULL+1,-1
   12 IF (S(I:I).NE.' ')     GO TO 49
      IF (NWFULL.EQ.0)       GO TO 29

C--                STEP OVER TRAILING FULL WORDS

   21 DO 24  JW=NWFULL,1,-1
   24 IF (S((JW-1)*NCHWD+1:JW*NCHWD).NE.BLANK) GO TO 31
   29 LST = 0
      RETURN

C--                FIND LAST CHARACTER IN FULL WORD

   31 DO 34  I=JW*NCHWD,(JW-1)*NCHWD+1,-1
   34 IF (S(I:I).NE.' ')     GO TO 49
   49 LST = I
      RETURN
      END
c************************************************************************
      SUBROUTINE L3_WAIT(SEC)
c************************************************************************
c*
c*       SUBR. L3_WAIT
c*
c*       Same as LIB$WAIT, EXCEPT:
c*       LIB$WAIT can FREEZE UP when it is interrupted by an AST. ie
c*       upon returning to LIB$WAIT after the AST, the process goes
c*       into an infinite hibernate instead of waking after the
c*       prescribed interval.
c*       L3_WAIT does not have this problem.
c*
c*       1/5/89:
c*       L3_WAIT as first written does, though, have another problem
c*       also exhibited by LIB$WAIT: suddenly the duration of the wait
c*       in a polling loop for instance goes from the specified value
c*       to zero. So, I am replacing SCHDWK/HIBER with SETIMR/WAITFR.
c*       eg. [WENAUS.MUDAS.CPC]CLUREAD,CLUWRITE.
c*
c*       Also, protected against simultaneous normal/AST execution.
c*
c*       NOTE that an event flag is grabbed and held on the first call
c*       to each of the normal and AST routines.
c*
c************************************************************************
      REAL SEC
      LOGICAL LIB$AST_IN_PROG
      IF (LIB$AST_IN_PROG()) THEN
        CALL L3_WAIT_A(SEC)
      ELSE
        CALL L3_WAIT_N(SEC)
      ENDIF
      END
      SUBROUTINE L3_WAIT_A(SEC)
      INCLUDE '($LIBDEF)'
      INTEGER ITIME(2), IFLAG, ISTAT, IEF, IOP, ITID
      REAL SEC
      INTEGER LIB$CVTF_TO_INTERNAL_TIME, SYS$SCHDWK, SYS$HIBER
      EXTERNAL LIB$K_DELTA_SECONDS_F
c*
C        IFLAG=%LOC(LIB$K_DELTA_SECONDS_F)
C        CALL LIB$CVTF_TO_INTERNAL_TIME(IFLAG,SEC,ITIME)
C        ISTAT = SYS$SCHDWK(,,ITIME,)
C        IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
C        ISTAT= SYS$HIBER()
C        IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
c*
      INTEGER SYS$SETIMR, SYS$WAITFR
      INTEGER SYS$CANTIM, LIB$GET_EF, LIB$FREE_EF
      LOGICAL FIRST /.TRUE./
      COMMON /C_L3_WAIT/ ITID
      DATA ITID /2847843/
c*
      IF (FIRST) THEN
        FIRST = .FALSE.
        IOP = %LOC(LIB$K_DELTA_SECONDS_F)
        ISTAT = LIB$GET_EF(IEF)
        IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ENDIF
      ISTAT = LIB$CVTF_TO_INTERNAL_TIME(IOP,SEC,ITIME)
      IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT = SYS$SETIMR(%VAL(IEF),ITIME,,%VAL(ITID),)
      IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT = SYS$WAITFR(%VAL(IEF))
      IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      END
c*
      SUBROUTINE L3_WAIT_N(SEC)
      INCLUDE '($LIBDEF)'
      INTEGER ITIME(2), IFLAG, ISTAT, IEF, IOP, ITID
      REAL SEC
      EXTERNAL LIB$K_DELTA_SECONDS_F
      INTEGER LIB$CVTF_TO_INTERNAL_TIME, SYS$SETIMR, SYS$WAITFR
      INTEGER SYS$CANTIM, LIB$GET_EF, LIB$FREE_EF
      LOGICAL FIRST /.TRUE./
      COMMON /C_L3_WAIT/ ITID
c*
      IF (FIRST) THEN
        FIRST = .FALSE.
        IOP = %LOC(LIB$K_DELTA_SECONDS_F)
        ISTAT = LIB$GET_EF(IEF)
        IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ENDIF
      ISTAT = LIB$CVTF_TO_INTERNAL_TIME(IOP,SEC,ITIME)
      IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT = SYS$SETIMR(%VAL(IEF),ITIME,,%VAL(ITID),)
      IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      ISTAT = SYS$WAITFR(%VAL(IEF))
      IF (.NOT.ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
      END
c************************************************************************
      SUBROUTINE L3_CANWAIT
      INTEGER ITID, SYS$CANTIM, ISTAT
      COMMON /C_L3_WAIT/ ITID
c*
c**        Cancel L3_WAIT timer
c*
      ISTAT = SYS$CANTIM(%VAL(ITID),)
      IF (ISTAT.NE.1) CALL LIB$SIGNAL(%VAL(ISTAT))
      END
