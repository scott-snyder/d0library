
*CMZ :  3.14/14 20/06/90  17.35.43  by  Rene Brun
*-- Author :
      SUBROUTINE GSMIXT(IMAT,NAMATE,A,Z,DENS,NLMAT,WMAT)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Defines mixture OR COMPOUND IMAT as composed by          *
C.    *       THE BASIC NLMAT materials defined by arrays A,Z and WMAT *
C.    *                                                                *
C.    *       If NLMAT.GT.0 then WMAT contains the PROPORTION BY       *
C.    *       WEIGTHS OF EACH BASIC MATERIAL IN THE MIXTURE.           *
C.    *                                                                *
C.    *       If NLMAT.LT.0 then WMAT contains the number of atoms     *
C.    *       of a given kind into the molecule of the COMPOUND        *
C.    *       In this case, WMAT in output is changed to relative      *
C.    *       weigths.                                                 *
C.    *                                                                *
C.    *       nb : the radiation length is computed according          *
C.    *            the EGS manual slac-210 uc-32 June-78               *
C.    *                           formula  2-6-8 (37)                  *
C.    *                                                                *
C.    *    ==>Called by : <USER>, UGEOM                                *
C.    *       Authors    R.Brun, M.Maire  *********                    *
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
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      COMMON/GCMZFO/IOMATE,IOPART,IOTMED,IOSEJD,IOSJDD,IOSJDH,IOSTAK
     +             ,IOMZFO(13)
C
      INTEGER       IOMATE,IOPART,IOTMED,IOSEJD,IOSJDD,IOSJDH,IOSTAK
     +             ,IOMZFO
C
      DIMENSION WMAT(1),A(1),Z(1)
      CHARACTER*(*) NAMATE
      CHARACTER*20 NAME
      DATA ALR2AV , AL183 / 1.39621E-03  ,  5.20948 /
C.
C.    ------------------------------------------------------------------
C.
      IF (IMAT.LE.0)GO TO 99
      IF(JMATE.LE.0)THEN
         CALL MZBOOK(IXCONS,JMATE,JMATE,1,'MATE',NMATE,NMATE,0,3,0)
         IQ(JMATE-5)=0
         NMATE = 0
      ENDIF
      NMATE = MAX0(IMAT,NMATE)
      NZMAT = IQ(JMATE-2)
      IF(IMAT.GT.NZMAT)THEN
         CALL MZPUSH(IXCONS,JMATE,IMAT-NZMAT,0,'I')
      ENDIF
      IF(LQ(JMATE-IMAT).GT.0) CALL MZDROP(IXCONS,LQ(JMATE-IMAT),' ')
      CALL MZBOOK(IXCONS,JMA,JMATE,-IMAT,'MATE',20,20,11,IOMATE,0)
      CALL MZBOOK(IXCONS,JPROB,JMA,-4,'MAPR',0,0,40,3,0)
C
      NAME=NAMATE
      NAME=NAMATE
      NCH=LENOCC(NAME)
      IF(NCH.GT.0)THEN
         IF(NAME(NCH:NCH).EQ.'$')NAME(NCH:NCH)=' '
      ENDIF
      CALL UCTOH(NAME,IQ(JMA+1),4,20)
C
C             Store mixture parameters
C             and parameter for Pair/Brems and
C             Photoelectric routines
C
      NLM    = IABS(NLMAT)
      IF (NLM.LE.0)GO TO 90
      CALL MZBOOK(IXCONS,JMIXT,JMA,-5,'MAMI',2,2,4*NLM,3,0)
      CALL MZBOOK(IXCONS,JMI1,JMIXT,-1,'MAM1',0,0,10,3,0)
      JMA = LQ(JMATE- IMAT)
      IQ(JMIXT-5)=IMAT
      IQ(JMI1-5)=IMAT
C
C             Compute proportion by weigths in the compound
C
      IF(NLMAT.LT.0) THEN
         AMOL   = 0.
         ZMOL   = 0.
         DO 10 I= 1,NLM
         AMOL   = AMOL + WMAT(I)*A(I)
         ZMOL   = ZMOL + WMAT(I)*Z(I)
   10    CONTINUE
         DO 20 I= 1,NLM
         WMAT(I)= WMAT(I)*A(I) / AMOL
   20    CONTINUE
      ENDIF
C
C             Compute effective mixture parameters
C
      AEFF   = 0.
      ZEFF   = 0.
      RADINV = 0.
      DO 40 I = 1,NLM
         AEFF   = AEFF + WMAT(I)*A(I)
         ZEFF   = ZEFF + WMAT(I)*Z(I)
         ZC     = Z(I)
         ALZ    = LOG(ZC)/3.
         XINV   = ZC*(ZC+GXSI(ZC))*(AL183-ALZ-GFCOUL(ZC))/A(I)
         RADINV = RADINV + WMAT(I)*XINV
         Q(JMIXT+3*NLM+I)=XINV
         Q(JMIXT + 2* NLM + I) = WMAT(I)
         Q(JMIXT + NLM + I) = Z(I)
         Q(JMIXT + I) = A(I)
   40 CONTINUE
      RADINV = ALR2AV * DENS * RADINV
      RADEFF = 1. / RADINV
      CALL GHMIX(A,WMAT,NLM,AHEFF)
      ABSEFF=10000.*AHEFF/(6.022*DENS*GHSIGM(5.,8,AHEFF))
C
      Q(JMA + 6) = AEFF
      Q(JMA + 7) = ZEFF
      Q(JMA + 8) = DENS
      Q(JMA + 9) = RADEFF
      Q(JMA + 10) = ABSEFF
      Q(JMA + 11) = NLM
      Q(JMI1 + 1) = AHEFF
      IF(NLMAT.GT.0)THEN
         Q(JMI1 + 2) = AEFF
         Q(JMI1 + 3) = ZEFF
      ELSE
         Q(JMI1 + 2) = AMOL
         Q(JMI1 + 3) = ZMOL
      ENDIF
      GO TO 99
C
  90  CHMAIL=' ***** GSMIXT ERROR. MIXTURE WITH NO COMPONENTS'
      CALL GMAIL(0,0)
C
  99  RETURN
      END
