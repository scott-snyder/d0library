*CMZ :          08/01/91  14.33.49  by  Federico Carminati
*CMZ :  3.14/14 20/06/90  17.39.33  by  Rene Brun
*-- Author :
      SUBROUTINE GGET (LUN,KEYSU,NUKEYS,IDENT,IER)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Routine to read in data structures                       *
C.    *                                                                *
C.    *       LUN      Logical unit number                             *
C.    *       KEYSU    Keywords to select data structures              *
C.    *       NKEYS    Number of keywords                              *
C.    *       IER      Error flag                                      *
C.    *                                                                *
C.    *    ==>Called by : <USER>, UGINIT,GUKINE                        *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      COMMON/GCDRAW/NUMNOD,MAXNOD,NUMND1,LEVVER,LEVHOR,MAXV,IPICK,
     + MLEVV,MLEVH,NWCUT,JNAM,JMOT,JXON,JBRO,JDUP,JSCA,JDVM,JPSM,
     + JNAM1,JMOT1,JXON1,JBRO1,JDUP1,JSCA1,JULEV,JVLEV,
     + LOOKTB(16),
     + GRMAT0(10),GTRAN0(3),IDRNUM,GSIN(41),GCOS(41),SINPSI,COSPSI,
     + GTHETA,GPHI,GPSI,GU0,GV0,GSCU,GSCV,NGVIEW,
     + ICUTFL,ICUT,CTHETA,CPHI,DCUT,NSURF,ISURF,
     + GZUA,GZVA,GZUB,GZVB,GZUC,GZVC,PLTRNX,PLTRNY,
     + LINATT,LINATP,ITXATT,ITHRZ,IPRJ,DPERS,ITR3D,IPKHIT,IOBJ,LINBUF,
     + MAXGU,MORGU,MAXGS,MORGS,MAXTU,MORTU,MAXTS,MORTS,
     + IGU,IGS,ITU,ITS,NKVIEW,IDVIEW,
     + NOPEN,IGMR,IPIONS,ITRKOP,IHIDEN,
     + DDUMMY(18)
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      COMMON/QUEST/IQUEST(100)
C
      CHARACTER*4 KLEY(22)
      CHARACTER*4 KEYSU(1)
      DIMENSION KEYS(22),IUHEAD(2)
      DIMENSION KSEL(14),LKEY(22),LKNUM(22),LINK(14),JLINK(17)
      EQUIVALENCE (JLINK(1),JDIGI)
      SAVE IFIRST,LKEY
      DATA LINK/7,6,13,16,8,10,2,9,3,15,5,17,4,1/
      DATA KLEY/'PART','MATE','TMED','VOLU','ROTM','SETS','DRAW','RUNG'
     +         ,'INIT','INIT','INIT','INIT','INIT','INIT','INIT','INIT'
     +         ,'HEAD','KINE','KINE','JXYZ','HITS','DIGI'/
      DATA LKNUM/1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,9,10,11,12,13,14/
      DATA IFIRST/0/
C.
C.    ------------------------------------------------------------------
C.
      IF(IFIRST.EQ.0)THEN
         IFIRST=1
         CALL UCTOH(KLEY,LKEY,4,88)
      ENDIF
C
      IDENT=-1
      IER    = 0
      NKEYS=IABS(NUKEYS)
      IF (NKEYS.LE.0)                                 GO TO 99
      CALL UCTOH(KEYSU,KEYS,4,4*NKEYS)
C
      IF(NUKEYS.LT.0)THEN
        I1=1
        I2=15
        K1=1
        K2=7
      ELSE
        I1=18
        I2=22
        K1=10
        K2=14
      ENDIF
C
      NKT=0
      DO 10 K=K1,K2
  10  KSEL(K)=0
      DO 20 I=I1,I2
      N=LKNUM(I)
      DO 20 IK=1,NKEYS
      IF(KEYS(IK).EQ.LKEY(I))THEN
         KSEL(N)=1
         NKT=NKT+1
      ENDIF
  20  CONTINUE
      IF(NKT.EQ.0)GO TO 99
      NUH=2
C
C               Go for next start of event data structure
C
      IF(NUKEYS.LT.0)THEN
         IF(JRUNG.NE.0)CALL MZDROP(IXCONS,JRUNG,' ')
         CALL FZIN(LUN,IXCONS,JRUNG,1,'E',NUH,IUHEAD)
         IF(IQUEST(1).GT.2)GO TO 90
         IDIV=IXCONS
      ELSE
         IF(JHEAD.NE.0)CALL MZDROP(IXDIV,JHEAD,' ')
         CALL FZIN(LUN,IXDIV,JHEAD,1,'E',NUH,IUHEAD)
         IF(IQUEST(1).GT.2)GO TO 90
         IDIV=IXDIV
      ENDIF
C
      IDENT= IUHEAD(1)
      NK   = IUHEAD(2)
      IF(NK.LE.0)GO TO 99
      IF(NK.GT.10)GO TO 99
      DO 30 I=1,NK
C
C              Read next header
C
         NUH=2
         CALL FZIN(LUN,IDIV,0,0,'S',NUH,IUHEAD)
         IF(IQUEST(1).GT.2)GO TO 90
         KS=IUHEAD(1)
         IF(KS.LE.0)GO TO 30
         IF(KS.GT.14)GO TO 30
         IF(KSEL(KS).EQ.0)GO TO 30
         IL=LINK(KS)
         IF(JLINK(IL).NE.0)CALL MZDROP(IDIV,JLINK(IL),' ')
C
C              Read pending data structure
C
         CALL FZIN(LUN,IDIV,JLINK(IL),1,'A',NUH,IUHEAD)
         IF(IQUEST(1).GT.2)GO TO 90
  30  CONTINUE
C
C             Fill header bank
C             Reconstruct NKVIEW,NVOLUM,NVERTX,NTRACK
C             Reconstruct NMATE, NROTM, NTMED, NPART
C
      IF(NUKEYS.LT.0)THEN
         IF(KSEL(1).NE.0.AND.JPART.GT.0) NPART=IQ(JPART-2)
         IF(KSEL(2).NE.0.AND.JMATE.NE.0) NMATE=IQ(JMATE-2)
         IF(KSEL(3).NE.0.AND.JTMED.NE.0) THEN
            CALL UCOPY(Q(JTMED+1),CUTGAM,10)
            NTMED=IQ(JTMED-2)
         ENDIF
         IF(KSEL(4).NE.0.AND.JVOLUM.GT.0) THEN
            NVOLUM=0
            DO 40 J=1, IQ(JVOLUM-2)
               IF(LQ(JVOLUM-J).EQ.0) GO TO 50
               NVOLUM=NVOLUM+1
  40        CONTINUE
  50        CONTINUE
         END IF
         IF(KSEL(5).NE.0.AND.JROTM.GT.0) NROTM=IQ(JROTM-2)
         IF(KSEL(7).NE.0.AND.JDRAW.GT.0) NKVIEW=IQ(JDRAW-2)
      ENDIF
C
      IF(JHEAD.GT.0)THEN
         IDRUN=IQ(JHEAD+1)
         IDEVT=IQ(JHEAD+2)
      ENDIF
C
      IF(KSEL(10).GT.0)THEN
         NVERTX=0
         NTRACK=0
         IF(JVERTX.GT.0)NVERTX=IQ(JVERTX+1)
         IF(JKINE .GT.0)NTRACK=IQ(JKINE +1)
      ENDIF
      GO TO 99
C
C             Error, EOF,etc
C
  90  IER=IQUEST(1)
C
  99  RETURN
      END
