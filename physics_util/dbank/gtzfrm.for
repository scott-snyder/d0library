      SUBROUTINE GTZFRM(IXSTOR,LBANK,CHFORM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the Store and Bank address,
C-                         Routine returns the CHFORM format descriptor.
C-
C-   Inputs  : IXSTOR,LBANK store and Bank address of bank in question
C-   Outputs : CHFORM character string containing Format descriptor
C-   Controls:
C-
C-   Created  12-APR-1989   Rajendran Raja by modifying the Zebra routine
C-                          DZFORM
C-
C----------------------------------------------------------------------
      PARAMETER      (IQDROP=25, IQMARK=26, IQCRIT=27, IQSYSX=28)
      COMMON /QUEST/ IQUEST(100)
      COMMON /ZEBQ/  IQFENC(4), LQ(100)
      DIMENSION    IQ(92),        Q(92)
      EQUIVALENCE (IQ(1),LQ(9)), (Q(1),IQ(1))
      INCLUDE 'D0$INC:MZCA.INC'
      INCLUDE 'D0$INC:MZCB.INC'
      INCLUDE 'D0$INC:MZCC.INC'
      COMMON /MZCN/  IQLN,IQLS,IQNIO,IQID,IQNL,IQNS,IQND, IQNX,IQFOUL
      COMMON /ZUNIT/ IQREAD,IQPRNT,IQPR2,IQLOG,IQPNCH,IQTTIN,IQTYPE
      COMMON /ZUNITZ/IQDLUN,IQFLUN,IQHLUN,  NQUSED
      EQUIVALENCE (LQFORM,LQSYSS(5))
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
      CHARACTER   CHROUT*(*),CHFORM*(*)
      PARAMETER (CHROUT = 'DZFORM')
      CQSTAK = CHROUT//'/'
      IQUEST(1) = 0
      IF (LBANK.EQ.0)    THEN
        IF (LQFORM.EQ.0)                                 GO TO 999
        CQMAP(1) = ' '
        CQMAP(2) =' DZFORM -- List of the IO characteristics '//
     X             'known to ZEBRA'
        CQMAP(3) = ' ========================================'//
     X             '=============='
        CALL DZTEXT(0,CDUMMQ,3)
        LIOD   = LQ(KQSP+LQFORM-2)
        LID    = LQFORM
        DO 200 I = 1,2
          LIX    = LQ(KQSP+LID-1)
          NWID   = IQ(KQSP+LID+1)
          IF (NWID.GT.0) THEN
            CALL SORTZV
     X            (IQ(KQSP+LID+4),LQ(LQWKTB),MIN(NWID,NQWKTB),0,0,0)
          ENDIF
          DO 100 J = 0,NWID-1
            CQLINE = ' '
            INDXA  = LQ(LQWKTB+J)
            IXIOD  = IQ(KQSP+LIX+INDXA)
            NWIO   = JBYT(IQ(KQSP+LIOD+IXIOD+1),7,5) - 1
            CALL MZIOCR(IQ(KQSP+LIOD+IXIOD+1))
            IF(IQUEST(1).LT.0) THEN
              WRITE(CQINFO,'(A4)') IQ(KQSP+LID+INDXA+3)
              CALL DZTEXT(MFOR1Q,CDUMMQ,0)
              GO TO 999
            ELSE
              IQUEST(1) = 0
            ENDIF
            WRITE(CQLINE(1:37),
     X            '('' Format for ID='',A4,'' is NWIO=1/2+'',I2,1X)')
     X             IQ(KQSP+LID+INDXA+3),NWIO
            CALL DZFOR1(CHFORM)
  100     CONTINUE
          LID=LQ(KQSP+LID)
  200   CONTINUE
      ELSE
        IF (IXSTOR.NE.NCHEKQ) THEN
          CALL MZSDIV(IXSTOR,-1)
        ENDIF
        CALL MZCHLS(NCHEKQ,LBANK)
        IF(IQFOUL.NE.0) THEN
          CALL DZBKDV(LBANK)
          IF (IQUEST(1).NE.0)                          GO TO 999
          WRITE(CQINFO,'(A,''/'',I10,'','',I4)') CQDIV,LBANK,IQFOUL
          CALL DZTEXT(MFOR2Q,CDUMMQ,0)
          GO TO 999
        ENDIF
        CALL MZIOCR(LQ(KQS+IQLN))
        IF(IQUEST(1).LT.0) THEN
          CALL DZBKDV(LBANK)
          IF (IQUEST(1).NE.0)                          GO TO 999
          WRITE(CQINFO,'(A,''/'',I10,'','',A4)') CQDIV,LBANK,IQID
          CALL DZTEXT(MFOR3Q,CDUMMQ,0)
          GO TO 999
        ELSE
          IQUEST(1) = 0
        ENDIF
        CQLINE = ' '
        WRITE(CQLINE(1:37),
     X    '('' DZFORM for ID='',A4,'' is NWIO=1/2+'',I2,1X)')IQID,IQNIO
        CALL DZFOR1(CHFORM)
      ENDIF
  999 RETURN
      END
