      SUBROUTINE HFFF1(ID1,X,W)
C
C     ******************************************************************
C     *                                                                *
C     *       Ultra FAST FILLING ROUTINE FOR 1-DIM HIST                *
C     *       IFFILL must be set in call to HFAST. R.Raja 23-Sep-87    *
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
      COMMON/HPFILL/IFPROX,IFWPRX,UNDER,OVER,BWID
C
C     ------------------------------------------------------------------
C
      IF(IFFILL.EQ.0)RETURN      !LOSE 1 STATEMENT FOR SAFETY.
      IF(ID1.EQ.IDLAST)GO TO 30
C
   10 ID=ID1
      LID=ID1
      IDBADD=IHSTAD(ID1-IDMIN+1)  !ID HAS TO EXIST. NO SAFETY!!
C
   20 IDLAST=ID1
      IFWPRX=IB(IDBADD+1)+2
      IF=IFWPRX+IB(IFWPRX-2)
      IFPROX=IF+IB(IF)-2
      UNDER=B(IFWPRX)
      OVER=B(IFWPRX+1)
      BWID=B(IFWPRX+2)
C
C             INCREMENT NUMBER OF ENTRIES AND COMPUTE CHANNEL NUMBER
C
   30 IB(IFPROX)=IB(IFPROX)+1
      IF(X.LT.UNDER)GO TO 40
      IF(X.GE.OVER)GO TO 50
      ICHAN=(X-UNDER)*BWID+IFPROX+2
      B(ICHAN)=B(ICHAN)+W
      RETURN
C
C             UNDERFLOW
C
   40 ICHAN=IFPROX+1
      B(ICHAN)=B(ICHAN)+W
      RETURN
C
C             OVERFLOW
C
   50 ICHAN=IB(IFWPRX-1)+IFPROX+2
      B(ICHAN)=B(ICHAN)+W
      RETURN
      END
