      SUBROUTINE DZSURV (CHTEXT,IXDIV,LBANK)
      SAVE NWDES
      PARAMETER(NBKHDQ=1,NBKCTQ=9,NBKOHQ=NBKHDQ+NBKCTQ,NBKJMQ=12)
      PARAMETER(MBKLNQ=0,JBKSKQ=1,NBKSKQ=16)
      PARAMETER(MBKIOQ=MBKLNQ,JBKIOQ=NBKSKQ+1,NBKIOQ=16)
      PARAMETER(JBKUSQ=1,NBKUSQ=18,JBKSYQ=NBKUSQ+1,NBKSYQ=32-NBKUSQ)
      PARAMETER(JDVBFQ=1,NDVBFQ=1,IDVFWQ=0,IDVBWQ=1)
      PARAMETER(IDVUSQ=1,IDVLGQ=2,IDVPAQ=3,IDVSYQ=4)
      PARAMETER(JDVIDQ=1,NDVIDQ=20)
      PARAMETER(JDVUSQ=21,JDVLGQ=22,JDVPAQ=23,JDVSYQ=24)
      PARAMETER(MDVLWQ=1,MDVHGQ=2)
      PARAMETER (MARE1Q=-1      ,MARE2Q=MARE1Q-1,MARE3Q=MARE2Q-1,
     X           MARE4Q=MARE3Q-1,MARE5Q=MARE4Q-1,MARE6Q=MARE5Q-1,
     X           MARE7Q=MARE6Q-1,MBKD1Q=MARE7Q-1,MBKD2Q=MBKD1Q-1,
     X           MBKU1Q=MBKD2Q-1,MBKU2Q=MBKU1Q-1,MBKU3Q=MBKU2Q-1,
     X           MBKU4Q=MBKU3Q-1,MBKX1Q=MBKU4Q-1,MBKX2Q=MBKX1Q-1,
     X           MBKX3Q=MBKX2Q-1,MCHV1Q=MBKX3Q-1,MFOR1Q=MCHV1Q-1,
     X           MFOR2Q=MFOR1Q-1,MFOR3Q=MFOR2Q-1,MIOP1Q=MFOR3Q-1,
     X           MSHO1Q=MIOP1Q-1,MSHO2Q=MSHO1Q-1,MSHP1Q=MSHO2Q-1,
     X           MSNA1Q=MSHP1Q-1,MSNA2Q=MSNA1Q-1,MSNA3Q=MSNA2Q-1,
     X           MSNA4Q=MSNA3Q-1,MSNA5Q=MSNA4Q-1,MSNA6Q=MSNA5Q-1,
     X           MSNA7Q=MSNA6Q-1,MSNA8Q=MSNA7Q-1,
     X           MSUR1Q=MSNA8Q-1,MVER1Q=MSUR1Q-1,MVER2Q=MVER1Q-1,
     X           MVER3Q=MVER2Q-1,MVER4Q=MVER3Q-1,MVER5Q=MVER4Q-1,
     X           MVER6Q=MVER5Q-1,MVER7Q=MVER6Q-1,MVER8Q=MVER7Q-1,
     X           MVER9Q=MVER8Q-1,MVE10Q=MVER9Q-1)
      PARAMETER      (IQDROP=25, IQMARK=26, IQCRIT=27, IQSYSX=28)
      COMMON /QUEST/ IQUEST(100)
      COMMON /ZVFAUT/IQVID(2),IQVSTA,IQVLOG,IQVTHR(2),IQVREM(2,6)
      COMMON /ZEBQ/  IQFENC(4), LQ(100)
                              DIMENSION    IQ(92),        Q(92)
                              EQUIVALENCE (IQ(1),LQ(9)), (Q(1),IQ(1))
      COMMON /MZCA/  NQSTOR,NQOFFT(16),NQOFFS(16),NQALLO(16), NQIAM
     +,              LQATAB,LQASTO,LQBTIS, LQWKTB,NQWKTB,LQWKFZ
     +,              MQKEYS(3),NQINIT,NQTSYS,NQM99,NQPERM,NQFATA,NQCASE
     +,              NQTRAC,MQTRAC(48)
                                       EQUIVALENCE (KQSP,NQOFFS(1))
      COMMON /MZCB/  JQSTOR,KQT,KQS,  JQDIVI,JQDIVR
     +,              JQKIND,JQMODE,JQDIVN,JQSHAR,JQSHR1,JQSHR2,NQRESV
     +,              LQSTOR,NQFEND,NQSTRU,NQREF,NQLINK,NQMINR,LQ2END
     +,              JQDVLL,JQDVSY,NQLOGL,NQSNAM(6)
                                       DIMENSION    IQCUR(16)
                                       EQUIVALENCE (IQCUR(1),LQSTOR)
      COMMON /MZCC/  LQPSTO,NQPFEN,NQPSTR,NQPREF,NQPLK,NQPMIN,LQP2E
     +,              JQPDVL,JQPDVS,NQPLOG,NQPNAM(6)
     +,              LQSYSS(10), LQSYSR(10), IQTDUM(22)
     +,              LQSTA(21), LQEND(20), NQDMAX(20),IQMODE(20)
     +,              IQKIND(20),IQRCU(20), IQRTO(20), IQRNO(20)
     +,              NQDINI(20),NQDWIP(20),NQDGAU(20),NQDGAF(20)
     +,              NQDPSH(20),NQDRED(20),NQDSIZ(20)
     +,              IQDN1(20), IQDN2(20),      KQFT, LQFSTA(21)
                                       DIMENSION    IQTABV(16)
                                       EQUIVALENCE (IQTABV(1),LQPSTO)
              DIMENSION  LQLUP(99), LQLORG(98),IQWIDN(97),IQWIDH(96)
     +,                  IQWNL(95), IQWNS(94), IQWND(93)
            EQUIVALENCE (LQLUP(8),  LQLORG(7), IQWIDN(6), IQWIDH(5)
     +,                  IQWNL(4),  IQWNS(3),  IQWND(2), IQ(1))
      COMMON /MZCN/  IQLN,IQLS,IQNIO,IQID,IQNL,IQNS,IQND, IQNX,IQFOUL
      COMMON /ZBCD/  IQNUM2(11),IQLETT(26),IQNUM(10),   IQPLUS,IQMINS
     +,              IQSTAR,IQSLAS,IQOPEN,IQCLOS,IQDOLL,IQEQU, IQBLAN
     +,              IQCOMA,IQDOT, IQNUMB,IQAPO, IQEXCL,IQCOLO,IQQUOT
     +,              IQUNDE,IQCLSQ,IQAND, IQAT,  IQQUES,IQOPSQ,IQGREA
     +,              IQLESS,IQREVE,IQCIRC,IQSEMI,IQPERC,  IQLOWL(26)
     +,              IQCROP,IQVERT,IQCRCL,IQNOT, IQGRAV,  IQILEG
     +,              NQHOL0,NQHOLL(95)
      PARAMETER (MPOSAQ= 1 ,MPOSBQ= 2 ,MPOSCQ= 3 ,MPOSDQ= 4 ,MPOSEQ= 5
     X          ,MPOSFQ= 6 ,MPOSGQ= 7 ,MPOSHQ= 8 ,MPOSIQ= 9 ,MPOSJQ=10
     X          ,MPOSKQ=11, MPOSLQ=12 ,MPOSMQ=13 ,MPOSNQ=14 ,MPOSOQ=15
     X          ,MPOSPQ=16, MPOSQQ=17 ,MPOSRQ=18 ,MPOSSQ=19 ,MPOSTQ=20
     X          ,MPOSUQ=21, MPOSVQ=22 ,MPOSWQ=23 ,MPOSXQ=24 ,MPOSYQ=25
     X          ,MPOSZQ=26                                            )
      COMMON /ZUNIT/ IQREAD,IQPRNT,IQPR2,IQLOG,IQPNCH,IQTTIN,IQTYPE
      COMMON /ZUNITZ/IQDLUN,IQFLUN,IQHLUN,  NQUSED
      PARAMETER(NOFMTQ=0)
      PARAMETER(MSYSFQ=5,MSYSDQ=7,MSYSKQ=8,MSYSPQ=9)

      COMMON /DZC1/  IFLOPT(26),
     +               JDROP,LN,LS,LX,IO,NL,NS,ND,
     +               IBASE,LBASE,NDW,JDFD,JD,JTYP

      PARAMETER (NSTMXQ=16,NDVMXQ=20,LNULL=0,NOFLIQ=8,NCHEKQ=-7)

      PARAMETER (MCQSIQ=8,MCQLSQ=2,MCQLGQ=15,MCQLTQ=19,MCQLIQ=87)
      CHARACTER CQSTAK*13,CQINFO*40
      PARAMETER (NLICHQ=130,NSTCHQ=8,NDVCHQ=8,NBKCHQ=4 )
      CHARACTER CQLINE*(NLICHQ),CQMAP(10)*(NLICHQ)
      CHARACTER CQSTOR*(NSTCHQ),CQDIV*(NDVCHQ),CQID*(NBKCHQ)
      COMMON /DZC1CH/ CQSTOR,CQDIV,CQID,CQMAP,CQSTAK,CQINFO
      EQUIVALENCE (CQLINE,CQMAP)

      CHARACTER   CDUMMQ*(*)
      PARAMETER ( CDUMMQ = ' ' )
      DIMENSION       NEWID(10)

      CHARACTER CHTEXT*(*)

      CHARACTER CHROUT*(*)
      PARAMETER (CHROUT = 'DZSURV')


      DATA  NWDES  / 4 /


      CQSTAK = CHROUT//'/'
      IQUEST(1) = 0

      CALL MZSDIV (IXDIV,-1)


      IF (LBANK.EQ.0)                                      GO TO 999
      IF (JBIT(IQ(LBANK+KQS),IQDROP).EQ.1)                 GO TO 999

      LSTART = LBANK

      CALL MZCHLS(NCHEKQ,LSTART)
      IF (IQFOUL.NE.0)                                     GO TO 91
      IDSTR  = IQID


      IF (CHTEXT.NE.CDUMMQ.AND.CHTEXT.NE.'-')        THEN
          CALL ZPAGE(IQPRNT,7)
          CQMAP(1)        = ' '
          CQMAP(2)        = ' DZSURV --- '
          CQMAP(2)(13:99) = CHTEXT
          WRITE(CQMAP(2)(100:),'('' ST= '',2A4,''  LSTART= '',I8)')
     X      NQPNAM(KQT+1),NQPNAM(KQT+2),LSTART
          CALL DZTEXT(0,CDUMMQ,2)
      ENDIF

      IF (CHTEXT.NE.'-') THEN
          CALL ZPAGE(IQPRNT,5)
          CQMAP(1) = ' '
          CQMAP(2) = '   NWCUM     NW   WBK  NBK    IDENTIFIER(S)'
          CQMAP(3) = ' '
          CALL DZTEXT(0,CDUMMQ,3)
      ENDIF

      MAXALL = 0
      DO 20 I=1,NDVMXQ
          IF(I.LE.JQDVLL.OR.I.GE.JQDVSY)
     I    MAXALL = MAXALL + LQEND(KQT+I) - LQSTA(KQT+I)
   20 CONTINUE
      if (MAXALL.lt.500000) maxall = 500000
      NBCUM    = 0
      NWCUM    = 0
      NEWNW    = 0
      NEWND    = 0
      NEWNBK   = 0
      NEWRIN   = IQBLAN
      NEWLOW   = 99
      NEWUP    = 1
      NEWNID   = 0
      NEWID(1) = 0
      NEWLEV   = 1
      LEVELH   = 0
      LTAB1    = NQOFFT(1)
      LEND1    = LQEND(LTAB1+1)
      LWORK    = LQWKTB
      NWORK    = NQWKTB
      NLVMAX   = NWORK/NWDES
      LDESC    = LWORK - NWDES
      LKBCD    = IQBLAN
      LGO      = LSTART
      L        = LSTART

                                                           GO TO 52
   31 NEWLEV = NEWLEV - 1

      IF (CHTEXT.NE.'-') CALL DZTEXT(1,CDUMMQ,1)

      IF (NEWLEV.EQ.1)                                     GO TO 9991
      LDESC = LDESC - NWDES
   34 IF (LQ(LDESC).EQ.LQ(LDESC+1))                        GO TO 31
      LQ(LDESC) = LQ(LDESC) + 1
   39 CALL VBLANK (IQUEST(2),3)
      CALL USET   (LQ(LDESC),IQUEST,2,4)
      IQUEST(1) = IQMINS
      CALL URIGHT (IQUEST,1,4)
      CALL UBUNCH (IQUEST,LKBCD,4)
      NEWNW    = 0
      NEWND    = 0
      NEWNBK   = 0
      NEWRIN   = IQBLAN
      NEWLOW   = 99
      NEWUP    = 1
      NEWNID   = 0
      NEWID(1) = 0
      LEVELH   = 1
      LDESH    = LWORK
      LGO      = LSTART
                                                           GO TO 42
   41 LEVELH      = LEVELH + 1
      LDESH       = LDESH  + NWDES
   42 JFOLL       = LQ(LDESH)
      LQ(LDESH+2) = LGO
      LQ(LDESH+3) = LGO
      LM          = LGO
                                                           GO TO 46
   43 NEWRIN = IQLETT(MPOSRQ)
   44 IF (LEVELH.EQ.0)                                     GO TO 71
   45 LM = LQ(LM+KQS)
      IF (LM.EQ.0)                                         GO TO 47
      IF (LM.EQ.LQ(LDESH+3))                               GO TO 47
   46 CALL MZCHLS (NCHEKQ,LM)
      IF (IQFOUL.NE.0)                                     GO TO 91
      IF (IQNS-JFOLL.LT.0)                                 GO TO 45
      LQ(LDESH+2) = LM
      K   = LM - JFOLL
      LGO = LQ(K+KQS)

      IF (LGO.EQ.0)                                        GO TO 45
      IF (LEVELH+1.LT.NEWLEV)                              GO TO 41
      L = LGO
                                                           GO TO 52
   47 LEVELH = LEVELH - 1
      IF (LEVELH.EQ.0)                                     GO TO 71
      LDESH = LDESH - NWDES
      JFOLL = LQ(LDESH)
      LM    = LQ(LDESH+2)
                                                           GO TO 45
   51 K = L
      L = LQ(K+KQS)
      IF (L.EQ.0)                                          GO TO 44
      IF (L.EQ.LGO)                                        GO TO 43
   52 CALL MZCHLS(NCHEKQ,L)
      IF (IQFOUL.NE.0)                                     GO TO 91
      NEWNBK = NEWNBK + 1
      N      = NBKOHQ + IQNL + IQND
      NEWND  = MAX(NEWND,N)
      NEWNW  = NEWNW + N
      IF (NEWNW.GE.MAXALL)                                 GO TO 71
      IF (IQID.EQ.NEWID(1))                                GO TO 57
      IF (NEWNID.EQ.0)                                     GO TO 56
      IF (NEWNID.EQ.10)                                    GO TO 57
      IF (IUCOMP(IQID,NEWID,NEWNID).NE.0)                  GO TO 57
   56 NEWNID = NEWNID + 1
      NEWID(NEWNID) = IQID
   57 IF (IQNS.EQ.0)                                       GO TO 51
      N = MIN(NEWLOW-1,IQNS)
      DO 62 J=1,N
          IF (LQ(L-J+KQS).NE.0)        THEN
              NEWLOW = J
                                                           GO TO 64
          ENDIF
   62 CONTINUE
   64 JA = MAX(NEWLOW,NEWUP) + 1
      DO 65 J=JA,IQNS
          IF (LQ(L-J+KQS).NE.0)  NEWUP=J
   65 CONTINUE

                                                           GO TO 51
   71 IF (NEWNBK.EQ.0)                                     GO TO 75
      NBCUM = NBCUM + NEWNBK
      NWCUM = NWCUM + NEWNW
      CALL VBLANK(IQUEST,NEWLEV)
      IQUEST(NEWLEV) = LKBCD
      NEWNID = MIN(NEWNID,21-NEWLEV)
      IF (CHTEXT.NE.'-') THEN
          WRITE(CQLINE,'(1X,2I7,I6,I5,2(1X,A1),20(1X,A4))')
     W                   NWCUM,NEWNW,NEWND,NEWNBK,NEWRIN
     W,                  (IQUEST(J),J=1,NEWLEV)
     W,                  (NEWID(J), J=1,NEWNID)
          CALL DZTEXT(0,CDUMMQ,1)
      ENDIF

      IF (NWCUM.GT.MAXALL) THEN
          WRITE(CQINFO,'(I10,''/'',I10)') NWCUM,MAXALL
          CALL DZTEXT(MSHO1Q,CDUMMQ,0)
          IQUEST(1) = 1
                                                           GO TO 999
      ENDIF

      IF (NEWLOW.GE.64)                                    GO TO 75
      IF (NEWLEV.GE.NLVMAX)                                GO TO 74
      NEWLEV = NEWLEV + 1
      LDESC  = LDESC  + NWDES
      LQ(LDESC)   = NEWLOW
      LQ(LDESC+1) = MAX(NEWLOW,NEWUP)
                                                           GO TO 39
   74 CALL DZTEXT(MSUR1Q,CDUMMQ,0)
   75 IF (NEWLEV.NE.1)                                     GO TO 34
                                                           GO TO 9991
   91 IQUEST(1) = LGO
      IQUEST(2) = K
      IQUEST(3) = L
      CALL ZFATAM (CHROUT)


 9991 IF (CHTEXT.NE.'-') THEN
          WRITE(CQMAP,'(1X,/,'' DZSURV --- The structure supported by'',
     +    '' bank '',A4,'' at '',I10,'' in store '',2A4,'' occupies '',
     +    I10,'' words in '',I6,'' banks '')')
     +    IDSTR,LSTART,NQPNAM(KQT+1),NQPNAM(KQT+2),NWCUM,NBCUM
          CALL DZTEXT(0,CDUMMQ,2)
      ENDIF
      IQUEST(11) = IDSTR
      IQUEST(12) = LSTART
      IQUEST(13) = NQPNAM(KQT+1)
      IQUEST(14) = NQPNAM(KQT+2)
      IQUEST(15) = NWCUM
      IQUEST(16) = NBCUM
  999 RETURN
      END
