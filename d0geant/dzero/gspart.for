      SUBROUTINE GSPART(IPART,NAPART,ITRTYP,AMASS,CHARGE,TLIFE,
     +            UBUF,NWBUF)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Store particle parameters                                *
C.    *                                                                *
C.    *    ==>Called by : <USER>, GPART                                *
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
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      COMMON/GCMZFO/IOMATE,IOPART,IOTMED,IOSEJD,IOSJDD,IOSJDH,IOSTAK
     +             ,IOMZFO(13)
C
      INTEGER       IOMATE,IOPART,IOTMED,IOSEJD,IOSJDD,IOSJDH,IOSTAK
     +             ,IOMZFO
C
      DIMENSION UBUF(1)
      INTEGER NZPAR
      CHARACTER*(*) NAPART
      CHARACTER*20 NAME
C.
C.    ------------------------------------------------------------------
C.
      IF(IPART.LE.0)GO TO 99
      IF(JPART.LE.0)THEN
         CALL MZBOOK(IXCONS,JPART,JPART,1,'PART',NPART,NPART,0,3,0)
         IQ(JPART-5)=0
         NPART = 0
      ENDIF
      NPART = MAX0(IPART,NPART)
      NZPAR = IQ(JPART-2)
      IF(IPART.GT.NZPAR)THEN
         CALL MZPUSH(IXCONS,JPART,IPART-NZPAR,0,'I')
      ENDIF
      IF(LQ(JPART-IPART).GT.0)CALL MZDROP(IXCONS,LQ(JPART-IPART),' ')
      CALL MZBOOK(IXCONS,JPA,JPART,-IPART,'PART',2,2,NWBUF+9,IOPART,0)
C
      NAME=NAPART
      NCH=LENOCC(NAME)
      IF(NCH.GT.0)THEN
         IF(NAME(NCH:NCH).EQ.'$')NAME(NCH:NCH)=' '
      ENDIF
      CALL UCTOH(NAME,IQ(JPA+1),4,20)
C
      Q(JPA + 6) = ITRTYP
      Q(JPA + 7) = AMASS
      Q(JPA + 8) = CHARGE
      Q(JPA + 9) = TLIFE
      IF(NWBUF.GT.0)CALL UCOPY(UBUF,Q(JPA+10),NWBUF)
C
  99  RETURN
      END
