      REAL FUNCTION UDST_VALUE(GRP,TAG,NUM,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return value of word in uDST identified by
C-                          GRP (group name), TAG and NUM
C-
C-   Returned value  : value of uDST word (0 if IER>0)
C-   Inputs  : GRP - name of group in which to search for TAG
C-                   (if empty string then all groups are searched)
C-             TAG - tag of variable (as listed in TAGS_MAP.RCP)
C-             NUM - index of object
C-   Outputs : IER - error flag
C-                 = 0 if no error condition
C-                 = 1 if specified group name (GRP) was not found
C-                 = 2 if specified tag name (TAG) was not found
C-                 = 3 if there were less than NUM objects found
C-
C-   Created   7-SEP-1993   Ulrich Heintz
C-   Updated   9-MAR-1994   Ulrich Heintz  fixed link protection
C-   Updated  11-AUG-1994   Ian Adam  handle multiple versions of UDST 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$PARAMS:UDST_DIMENSIONS.PARAMS'
      INTEGER I,IDMAX,JR,JEV,IR,IEV,K,IER,NUM,KMAX,KMIN,RUNNO,EVONUM
      INTEGER NDIMG1(IGRP),MAP,GET_INDEX,NWORD(IGRP),XPTR(IGRP)
      INTEGER IUDST,GZUDST,LUDST
      REAL    XDATA(NGRP)
      CHARACTER*4 XGRP1(IGRP),GRP
      CHARACTER*8 XTAGS1(NGRP,IGRP)
      CHARACTER*(*) TAG
      LOGICAL FIRST
      DATA    FIRST/.TRUE./
      INTEGER UDST_VERSION,LAST_VERSION/999/
C----------------------------------------------------------------------
      IER=0
      UDST_VALUE=0.

      LUDST = GZUDST()
      IF (LUDST.LE.0) THEN
        CALL ERRMSG('NO UDST BANK','UDST_VALUE',' ','W')
        GOTO 999
      ENDIF

      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL READ_UTAG_BANK(IDMAX,NDIMG1,XTAGS1,XGRP1,IGRP,NGRP)
        CALL GRLINK('UDST_VALUE',IUDST)
        LAST_VERSION = IQ(LUDST+1)
      ENDIF
C
C CHECK VERSION

      UDST_VERSION = IQ(LUDST+1)
      IF (UDST_VERSION.NE.LAST_VERSION) THEN
        CALL READ_UTAG_BANK(IDMAX,NDIMG1,XTAGS1,XGRP1,IGRP,NGRP)
        LAST_VERSION = UDST_VERSION
      ENDIF

      IF(GRP.EQ.' ')THEN
        KMIN=1
        KMAX=IDMAX
      ELSE
        KMAX=0
        DO I=1,IDMAX
          IF(XGRP1(I).EQ.GRP)KMAX=I
        ENDDO
        KMIN=KMAX
      ENDIF
      IF(KMAX.EQ.0)THEN
        CALL ERRMSG('group not found','UDST_VALUE',GRP,'W')
        IER=1
        GOTO 999
      ENDIF
      DO K=KMIN,KMAX
        MAP=GET_INDEX(NDIMG1(K),XTAGS1(1,K),TAG)
        IF(MAP.GT.0)GOTO 100
      ENDDO
  100 IF(MAP.LE.0)THEN
        CALL ERRMSG('tag not found','UDST_VALUE',TAG,'W')
        IER=2
        GOTO 999
      ELSE
        IR=RUNNO()
        IEV=EVONUM()
        IF(IR.NE.JR.OR.IEV.NE.JEV)THEN
          CALL READ_UDST_BANK(NWORD,XDATA,XPTR,IGRP,NGRP)
          LRLINK(IUDST)=GZUDST()
          DO I=1,IDMAX
            XPTR(I)=XPTR(I)-LRLINK(IUDST)
          ENDDO
          JR=IR
          JEV=IEV
        ENDIF
        IF(NUM .GT. NWORD(K)/NDIMG1(K))THEN
          CALL ERRMSG('object not found','UDST_VALUE',' ','W')
          IER = 3
          GOTO 999
        ENDIF
        IF(XPTR(K).EQ.XPTR(K+1))THEN
          CALL ERRMSG('object not found','UDST_VALUE',' ','W')
          IER = 3
          GOTO 999
        ELSE
          UDST_VALUE=Q(LRLINK(IUDST)+XPTR(K)+(NUM-1)*NDIMG1(K)+MAP)
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
