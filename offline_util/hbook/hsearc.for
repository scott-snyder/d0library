*VAXFOR HSEARC
      SUBROUTINE HSEARC(IDCONT)
C
C     ******************************************************************
C     *                                                                *
C     *       BINARY SEARCH IN THE ADDRESS TABLE OF THE IDENT  ID      *
C     *       IDBADD CONTAINS THE ADRESS OF ID IF ID EXIST             *
C     *       IF ID DOES NOT EXIST IDBADD IS THE ADRESS OF THE ID      *
C     *       IMMEDIATLY GREATER THAN ID                               *
C     *       IF(IDCONT=2 ) ID INEXISTING                              *
C     *                                                                *
C     ******************************************************************
C
      COMMON//B(5)
      DIMENSION IB(5)
      EQUIVALENCE (B(1),IB(1))
C
      INTEGER NUMHST,IFFILL,IDMIN,IDMAX,IHSTAD(1)
      EQUIVALENCE (IB(1),NUMHST),(IB(2),IFFILL)
      EQUIVALENCE (IB(3),IDMIN) , (IB(4),IDMAX)
      EQUIVALENCE (IB(5),IHSTAD(1))
C
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,
     +       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,
     +       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,
     +       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2
C
C     ------------------------------------------------------------------
C
      IDCONT=1
      IF(ID.EQ.LID)RETURN
      LID=ID
      IF ( IFFILL.GT.0 .AND. NUMHST.GT.0) THEN   !FAST FILL
C VERY FAST IDFINDER. NO SAFETY. NO BINARY SEARCH.
        IF ( ID.GE.IDMIN.AND.ID.LE.IDMAX ) THEN
          IDBADD=IHSTAD(ID-IDMIN+1)
          IF ( IDBADD.EQ.0 ) THEN
            IDBADD=NX1+(NRHIST)*NWLIB !NEXT AVAILABLE ADDRESS
            IDLAST=0
            LID=0
            IDCONT=2   !ID NOT DEFINED
          ENDIF
          RETURN
        ELSE
          IDCONT=2  !BAD ID
          WRITE(IOUT,5)ID
    5     FORMAT(' ***bad value of ID in Hsearc*** ',I8)
          RETURN
        ENDIF
      ELSE
        IDBADD=NX1
        LUP=NRHIST+1
        LDOWN=0
C
   10   IF(LUP-LDOWN.LE.1)GO TO 40
        LMEAN=(LUP+LDOWN)/2
        IDBADD=NX1+NWLIB*(LMEAN-1)
        IF(ID-IB(IDBADD))20,99,30
   20   LUP=LMEAN
        GO TO 10
C
   30   LDOWN=LMEAN
        IDBADD=IDBADD+NWLIB
        GO TO 10
C
   40   IDCONT=2
        IDLAST=0
        LID=0
   99   RETURN
      ENDIF
      END
