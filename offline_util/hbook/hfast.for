      SUBROUTINE HFAST(NUMHST,IFFILL,IDMIN,IDMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-   To declare ahead the number of histograms one wants to book.
C    Saves a lot of computing time and preserves the standard Hbook
C    format. Should be called ONCE immediately after HLIMIT
C
C-   Inputs  : NUMHST = 0    OLD HBOOK operation will result.
C              NUMHST .gt.0  Will Book ahead Library space
C              for NUMHST ID's (2D plots as well as 1D Hists)
C              and will prevent Shifting of old hists as
C              booking proceeds.
C
C              IFFILL = 0   Histogram Filling will occur as
C              before. HSEARC will determine the Address of the
C              histogram by binary search.
C              IFFILL .gt. 0 will cause Fast Filling to occur.
C              If IFFILL. Gt. 0. IDMAX has to be declared
C              greater than the maximum ID value used.IDMIN is
C              the minimum value of ID used.
C
C-   Outputs : NEWHIS , the pointer to the next ID is calculated
C              assuming there are NUMHST id's.
C              Storage space is reserved in Blank common.
C              NX0 is bumped to make room for NUMHST,IFFILL, IDMAX
C              and Histogram ID space IHSTAD
C-
C-   Created  22-SEP-1987   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMHST,IFFILL,IDMIN,IDMAX,IHSTAD(1)
C
      REAL B
      INTEGER ID,IDBADD,LID,IDLAST,IDHOLD,NBIT,NBITCH
      INTEGER NCHAR,NX0,NX1,NX2,INTER,INDEX,LAST,LIMIT
      INTEGER LFIEL,NEWHIS,NRLENG,NWLIB,NWFLAG,NBFLAG,NWSTAT,NRHIST
      INTEGER IDISC,LFHIST,LLHIST,NWHIST,IERR,NV,NRDIS,IA2
      INTEGER IT,NIDS
C
      COMMON//B(5)
      INTEGER IB(5)
      EQUIVALENCE (B(1),IB(1))
C
C
      COMMON/HFLAG /ID    ,IDBADD,LID   ,IDLAST,IDHOLD,NBIT  ,NBITCH,
     +       NCHAR ,NX0   ,NX1   ,NX2   ,INTER ,INDEX ,LAST  ,LIMIT ,
     +       LFIEL ,NEWHIS,NRLENG,NWLIB ,NWFLAG,NBFLAG,NWSTAT,NRHIST,
     +       IDISC ,LFHIST,LLHIST,NWHIST,IERR  ,NV    ,NRDIS ,IA2
C
C----------------------------------------------------------------------
C
      CALL HMACHI
C
      IF(IDMIN.LE.0)IDMIN=1
      IF(IDMAX.LE.0)IDMAX=1
      NIDS=IDMAX-IDMIN+1
      IF (NUMHST.GT.0.AND.IFFILL.GT.0)THEN
        IF( NIDS.LT.NUMHST ) THEN
          IDMAX=IDMIN+NUMHST-1
          WRITE(6,123)NIDS,NUMHST,IDMAX
  123     FORMAT(' NUMBER OF EXPECTED IDS ( IDMAX-IDMIN+1)',
     &    I7,' IS LESS THAN NUMHST ',I9,/,
     & '  IDMAX BEING CHANGED TO ',I8)
        ENDIF
      ENDIF
      IB(1)=NUMHST
      IB(2)=IFFILL
      IB(3)=IDMIN
      IB(4)=IDMAX
      IT=0
      IF(IFFILL.GT.0)IT=1
      IF(IT.NE.0)CALL VZERO(B(5),NIDS)   !IN CASE OF REUSE
C
      NX0=5+NIDS*IT   !1ST FREE SPACE FOR HBOOK. 1ST 4 ARE
C                      !NUMHST,IFFILL,IDMIN AND IDMAX.
      NX1=NX0
      NX2=NX0
      NEWHIS=NX0+NWLIB*NUMHST       !POINTER TO NEXT HISTOGRAM ENTRY
      IF(NUMHST.NE.0)CALL VZERO(B(NX0),NWLIB*NUMHST) !IN CASE OF REUSE
      NRHIST=0      !RESETTING NUMBER OF HISTOGRAMS
C
  999 RETURN
      END
