*CMZ :  3.14/16 14/09/90  10.10.24  by  Rene Brun
*-- Author :
      SUBROUTINE GLTRAC
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. GLTRAC                                                *
C.    *                                                                *
C.    *   Extracts next track from stack JSTAK and prepares commons    *
C.    *    /GCTRAK/, /GCKINE/ and /GCVOLU/                             *
C.    *                                                                *
C.    *   Called by : GTREVE                                           *
C.    *   Authors   : R.Brun, F.Bruyant                                *
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
      COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
     +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      COMMON/GCONST/PI,TWOPI ,PIBY2,DEGRAD,RADDEG,CLIGHT ,BIG,EMASS
      COMMON/GCONSX/EMMU,PMASS,AVO
C
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
*
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
C
      COMMON/GCTMED/NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,DMAXMS
     +      ,DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD
C
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     +  ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     +  ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     +  ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     +  ,NLVSAV,ISTORY
C
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      DIMENSION RNDM(5)
      DOUBLE PRECISION P2,GETOTD,GEKIND
C.
C.    ------------------------------------------------------------------
*
* *** Extract next track from stack JSTAK
*
      ISTAK = IQ(JSTAK+1)
      IQ(JSTAK+1) = ISTAK -1
      JST = JSTAK +NWSTAK*IQ(JSTAK+1) +3
      ITRA   = IQ(JST+1)
      IF (ITRA.LT.0) THEN
        ITRA = -ITRA
      ELSE
        ISTAK = 0
      ENDIF
      IPART  = IQ(JST+2)
      DO 60 I = 1,3
        VERT(I) = Q(JST+3+I)
        PVERT(I) = Q(JST+6+I)
   60 CONTINUE
      TOFG   = Q(JST+10)
      SAFETY = Q(JST+11)
      UPWGHT = Q(JST+12)
*
* *** Prepare tracking parameters
*
      P2 = PVERT(1)**2+PVERT(2)**2+PVERT(3)**2
      IF (P2.EQ.0.) THEN
        VECT(7) = 0.
      ELSE
        VECT(7) = SQRT(P2)
      ENDIF
      DO 80 I = 1,3
        VECT(I) = VERT(I)
        IF (VECT(7).NE.0.) THEN
          VECT(I+3) = PVERT(I)/VECT(7)
        ELSE
          IF (I.EQ.3) THEN
            VECT(6)   = 1.
          ELSE
            VECT(I+3) = 0.
          ENDIF
        ENDIF
   80 CONTINUE
*
*  ** Reload Particle characteristics, ALWAYS!! to fix overwrite of
*  **  particle mass for particles with two-body decays
*
      JPA = LQ(JPART-IPART)
      DO 90 I = 1,5
        NAPART(I) = IQ(JPA+I)
   90 CONTINUE
      ITRTYP = Q(JPA+6)
      AMASS  = Q(JPA+7)
      CHARGE = Q(JPA+8)
      TLIFE  = Q(JPA+9)
      IUPD   = 0
      IPAOLD = IPART
*
      GETOTD = SQRT(P2+AMASS*AMASS)
      GEKIND = GETOTD -AMASS
      GETOT  = GETOTD
      GEKIN  = GEKIND
      CALL GEKBIN
      SLENG  = 0.
      NSTEP  = 0
      NTMSTO = NTMSTO +1
      NTMULT = NTMSTO
      ISTORY = 0
*
*  ** Initialize interaction probabilities
*
      IF (ITRTYP.EQ.1) THEN
*      Gammas
        CALL GRNDM(RNDM,5)
        ZINTPA = -LOG(RNDM(1))
        ZINTCO = -LOG(RNDM(2))
        ZINTPH = -LOG(RNDM(3))
        ZINTPF = -LOG(RNDM(4))
        ZINTRA = -LOG(RNDM(5))
      ELSE IF (ITRTYP.EQ.2) THEN
*       Electrons
        CALL GRNDM(RNDM,3)
        ZINTBR = -LOG(RNDM(1))
        ZINTDR = -LOG(RNDM(2))
        ZINTAN = -LOG(RNDM(3))
      ELSE IF (ITRTYP.EQ.3) THEN
*       Neutral hadrons
        CALL GRNDM(RNDM,2)
        SUMLIF = -CLIGHT*TLIFE*LOG(RNDM(1))
        ZINTHA = -LOG(RNDM(2))
      ELSE IF (ITRTYP.EQ.4) THEN
*       Charged hadrons
        CALL GRNDM(RNDM,3)
        SUMLIF = -CLIGHT*TLIFE*LOG(RNDM(1))
        ZINTHA = -LOG(RNDM(2))
        ZINTDR = -LOG(RNDM(3))
      ELSE IF (ITRTYP.EQ.5) THEN
*       Muons
        CALL GRNDM(RNDM,5)
        SUMLIF = -CLIGHT*TLIFE*LOG(RNDM(1))
        ZINTBR = -LOG(RNDM(2))
        ZINTPA = -LOG(RNDM(3))
        ZINTDR = -LOG(RNDM(4))
        ZINTMU = -LOG(RNDM(5))
      ENDIF
*
*   * Prepare common /GCVOLU/ and structure JGPAR, if needed
*
      IF (NJTMAX.LE.0) CALL GMEDIA (VECT, NUMED)
      INFROM = 0
*                                                             END GLTRAC
      END
