*VAXFOR HINLIB
      SUBROUTINE HINLIB
C
C     ******************************************************************
C     *                                                                *
C     *       INSERT THE HISTOGRAM IN THE COMMON BLANK                 *
C     *       CHANGE ALL THE POINTERS IN THE LIBRARY                   *
C     *       INSERT IN THE LIBRARY THE FEATURES OF THE                *
C     *       HISTOGRAM . MODIFIED R.RAJA SEP 87                       *
C     *                                                                *
C     ******************************************************************
C
      COMMON//B(5)
      DIMENSION IB(5)
      EQUIVALENCE (B(1),IB(1))
C
      INTEGER NUMHST,IFFILL,IDMAX,IHSTAD(1)
      EQUIVALENCE (IB(1),NUMHST),(IB(2),IFFILL)
      EQUIVALENCE (IB(3),IDMIN),(IB(4),IDMAX)
      EQUIVALENCE (IB(5),IHSTAD(1))
C
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,
     +       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,
     +       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,
     +       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2
C
      COMMON/HTAPIO/IOUT,IERMES,LDISC
C
C     ------------------------------------------------------------------
C
C
C             UPDATE OF POINTERS
C
      NRHIST=NRHIST+1
      NX    =NX2-NWLIB+1
      NI    = NX1+1
      IF (NX    .LT. NI)                    GO TO  30
C
      IF ( NUMHST.EQ.0 ) THEN  !ONLY BUMP POINTERS IF IN NON-BLOCK
C                              ! BOOKING MODE
        DO  10 I=NI,NX,NWLIB
          IF (IB(I) .LT. 0 )                    GO TO  10
          IB(I) =IB(I)+NWLIB
   10   CONTINUE
C
C             SHIFT HISTOGRAMS ONLY IN NON-BLOCK BOOKING MODE
C
        NX   =NEWHIS-NX2
        CALL UCOPY2 (B(NX2),B(NX2+NWLIB),NX)
C
      ELSEIF ( NRHIST.GT.NUMHST ) THEN
        WRITE(IOUT,11)NRHIST,NUMHST
   11   FORMAT(' BLOCK BOOKING MODE . NUMBER OF BOOKED HISTOGRAMS ',I8,
     +          /,' GREATER THAN THE DECLARED NUMBER ',I8)
        CALL SCRAP !GRACEFUL EXIT
      ENDIF
C
      IF (IDBADD .EQ. NX2)                  GO TO  30
C
C             SHIFT OF LIBRARY
C             UPDATE OF LIBRARY'S ORDER
C
      NX     =NX2-IDBADD
      CALL UCOPY2 (B(IDBADD),B(IDBADD+NWLIB),NX)
      NX     =NX2-NWLIB
      DO  20 I=IDBADD,NX,NWLIB
        J      =I+NWLIB-1
        IF (IB(I+NWLIB+1) .LT. 0)             GO TO  20
        K      =IB(I+NWLIB+1)+IB(I+NWLIB+2)-1
        IB(K)  =IB(K)+1
   20 IB(  J)=IB(J+NWLIB)
C
C              UPDATE LIBRARY
C
   30 CONTINUE
      IF(NUMHST.EQ.0)NEWHIS =NEWHIS+NWLIB       !ONLY BUMP NEWHIS IF NOT
C                                               ! in block book
      NX2         =NX2   +NWLIB
      IB(NX2-1)   =ID
      IB(  IDBADD)=ID
      IF ( IFFILL.NE.0.AND.NUMHST.NE.0) THEN
        IF ( ID.GE.IDMIN.AND.ID.LE.IDMAX ) THEN
          IHSTAD(ID-IDMIN+1)=IDBADD
        ELSE
          WRITE(IOUT,42)ID,IDMIN,IDMAX
   42     FORMAT(' ROUTINE HINLIB. FAST FILL MODE. ID ',I8,
     +           ' OUT OF RANGE',2I8)
          CALL SCRAP !GRACEFUL EXIT          
        ENDIF
      ENDIF
      IB(IDBADD+1)=NEWHIS
      IB(IDBADD+2)=NWHIST
      DO 40 I=1,NWFLAG
   40 IB(IDBADD+I+2)=0
      LFHIST=NEWHIS
      NEWHIS      =NEWHIS+NWHIST
      IB(NEWHIS-1)=(IDBADD-NX1+1)/NWLIB +1
C
      IDHOLD=0
      IDLAST=0
      LID   =0
      RETURN
      END
