*CMZ :          18/07/91  22.44.13  by  Federico Carminati
*CMZ :  3.14/14 10/07/90  08.42.15  by  Rene Brun
*-- Author :
      SUBROUTINE GINVOL (X, ISAME)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. GINVOL (X, ISAME*)                                    *
C.    *                                                                *
C.    *   Checks if particle at point X has left current volume/medium *
C.    *   If so, returns ISAME = 0 and prepares information useful to  *
C.    *    identify the new volume entered.                            *
C.    *   Otherwise, returns ISAME = 1                                 *
C.    *                                                                *
C.    *   Note : INGOTO is set by GTNEXT, to transmit the information  *
C.    *       on the one volume which has limited the step SNEXT,      *
C.    *       >0 : INth content                                        *
C.    *       =0 : current volume                                      *
C.    *       <0 : -NLONLY, with NLONLY defined as the first 'ONLY'    *
C.    *           level up in the tree for the 'NOT-ONLY' volume       *
C.    *           where the point X is found to be.                    *
C.    *                                                                *
C.    *   Called by : GNEXT, GTELEC, GTHADR, GTMUON, GTNEXT            *
C.    *   Authors   : S.Banerjee, R.Brun, F.Bruyant                    *
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
      COMMON/GCONST/PI,TWOPI ,PIBY2,DEGRAD,RADDEG,CLIGHT ,BIG,EMASS
      COMMON/GCONSX/EMMU,PMASS,AVO
C
      COMMON/GCPOLY/IZSEC,IPSEC
      INTEGER IZSEC,IPSEC
C
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)
C
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
C
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)
C
      COMMON/GCVOL2/NLEVE2,NAMES2(15),NUMB2(15),
     +LVOL2(15),LIND2(15),INFRO2,NLDEV2(15),LINMX2(15),
     +GTRAN2(3,15),GRMAT2(10,15),GONLY2(15),GLX2(15)
      INTEGER NLEVE2,NAMES2,NUMB2,LVOL2,LIND2,INFRO2,NLDEV2,LINMX2
      REAL GTRAN2,GRMAT2,GONLY2,GLX2
C
C.
      DIMENSION X(*)
      REAL      XC(3), XT(3), X0(3)
      INTEGER   IDTYP(3,12)
C.
      DATA  IDTYP / 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 2, 3, 1,
     +              2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 4, 3, 1, 1, 1,
     +              2, 3, 1, 2, 3, 1/
C.
C.    ------------------------------------------------------------------
*
      ISAME = 0
      JVO   = LQ(JVOLUM-LVOLUM(NLEVEL))
*
* *** Get coordinates of point X in reference system of current volume
*
      IF (GRMAT(10,NLEVEL).EQ.0.) THEN
         DO 19 I = 1,3
            XC(I) = X(I) -GTRAN(I,NLEVEL)
   19    CONTINUE
      ELSE
*       (later, code in line)
         CALL GTRNSF (X, GTRAN(1,NLEVEL), GRMAT(1,NLEVEL), XC)
      ENDIF
*
      IF (INGOTO.GT.0) THEN
*
* ***   Entrance in content INGOTO predicted by GTNEXT
*
         JIN   = LQ(JVO-INGOTO)
         IVOT  = Q(JIN+2)
         JVOT  = LQ(JVOLUM-IVOT)
         JPAR  = LQ(JGPAR-NLEVEL-1)
*
         IROTT = Q(JIN+4)
         IF (IROTT.EQ.0) THEN
            DO 49 I = 1,3
               XT(I) = XC(I) -Q(JIN+4+I)
   49       CONTINUE
         ELSE
*          (later, code in line)
            CALL GITRAN (XC, Q(JIN+5), IROTT, XT)
         ENDIF
*
*   *   Check if point is in content
*
         CALL GINME ( XT, Q(JVOT+2), Q(JPAR+1), IYES)
         IF (IYES.NE.0) THEN
*
*          If so, prepare information for volume retrieval, and return
*
            NLEVIN = NLEVEL +1
            LVOLUM(NLEVIN) = IVOT
            NAMES(NLEVIN)  = IQ(JVOLUM+IVOT)
            NUMBER(NLEVIN) = Q(JIN+3)
            LINDEX(NLEVIN) = INGOTO
            LINMX(NLEVIN)  = Q(JVO+3)
            GONLY(NLEVIN)  = Q(JIN+8)
            IF (LQ(LQ(JVOLUM-IVOT)).EQ.0) THEN
               NLDEV(NLEVIN) = NLDEV(NLEVEL)
            ELSE
               NLDEV(NLEVIN) = NLEVIN
            ENDIF
            CALL GTRMUL (GTRAN(1,NLEVEL), GRMAT(1,NLEVEL), Q(JIN+5),
     +                   IROTT, GTRAN(1,NLEVIN), GRMAT(1,NLEVIN))
            GO TO 999
         ENDIF
      ENDIF
*
* *** Check if point is within current volume (test has to be done even
*      if INGOTO is non zero, because of possible corner effects)
*
      JPAR = LQ(JGPAR-NLEVEL)
*
      CALL GINME (XC, Q(JVO+2), Q(JPAR+1), IYES)
      IF (IYES.EQ.0) THEN
         NLEVIN = NLEVEL -1
         GO TO 999
      ENDIF
*
      NLEVIN = NLEVEL
      INFR   = INFROM
      NLONLY = NLEVMX
*
* *** Point is in current volume, check contents if any
*
  100 IF (Q(JVO+3).EQ.0.) GO TO 300
      NIN = Q(JVO+3)
      IF (NIN.LT.0) GO TO 200
*
* ***   Case with contents positioned
*
         ISEARC = Q(JVO+1)
         IF (ISEARC.LT.0) THEN
*
*  **      Contents are ordered by GSORD, select neighbours
*
            JSB = LQ(LQ(JVO-NIN-1))
            IAX = Q(JSB+1)
            NSB = Q(JSB+2)
            IF (IAX.LE.3) THEN
               IDIV = LOCATF (Q(JSB+3), NSB, XC(IAX))
            ELSE
               CALL GFCOOR (XC, IAX, CX)
               IDIV = LOCATF (Q(JSB+3), NSB, CX)
            ENDIF
            IF (IDIV.LT.0) IDIV = -IDIV
            IF (IDIV.EQ.0) THEN
               IF (IAX.NE.6) GO TO 300
               IDIV = NSB
            ELSE IF (IDIV.EQ.NSB) THEN
               IF (IAX.NE.6) GO TO 300
            ENDIF
            JSC0 = LQ(JVO-NIN-2)
            NCONT = IQ(JSC0+IDIV)
            IF (NCONT.LE.0) GO TO 300
            JSCV = LQ(JSC0-IDIV)
            ICONT = 1
            GO TO 120
         ELSE
            JNEAR = LQ(JVO-NIN-1)
            IF (ISEARC.EQ.0) THEN
               IF (INFR.GT.0) THEN
                  JIN = LQ(JVO-INFR)
                  IF (LQ(JIN-1).NE.0) JNEAR = LQ(JIN-1)
               ENDIF
            ELSE
               CALL GUNEAR (ISEARC, 1, XC, JNEAR)
               IF (IQ(JNEAR+1).EQ.0) GO TO 300
            ENDIF
            JNEAR = JNEAR +1
            NNEAR = IQ(JNEAR)
            INEAR = 1
         ENDIF
*
*  **   For each selected content in turn, check if point is in content
*
  110    IN = IQ(JNEAR+INEAR)
         IF (IN.GT.0) GO TO 150
         GO TO 190
*
  120    IN = IQ(JSCV+ICONT)
*
  150    IF (IN.EQ.INGOTO) GO TO 190
         JIN   = LQ(JVO-IN)
         IVOT  = Q(JIN+2)
         JVOT  = LQ(JVOLUM-IVOT)
         IROTT = Q(JIN+4)
*
         IF (JBIT(IQ(JVOT),2).NE.0) THEN
*          (case with JVOLUM structure locally developed)
            JPAR = LQ(LQ(JVOLUM-LVOLUM(NLDEV(NLEVIN))))
            DO 169 ILEV = NLDEV(NLEVIN),NLEVIN
               IF (IQ(JPAR+1).EQ.0) THEN
                  IF (ILEV.EQ.NLEVIN) THEN
                     JPAR = LQ(JPAR-IN)
                  ELSE
                     JPAR = LQ(JPAR-LINDEX(ILEV+1))
                  ENDIF
                  IF (JPAR.EQ.0) GO TO 170
               ELSE IF (IQ(JPAR-3).GT.1) THEN
                  JPAR = LQ(JPAR-LINDEX(ILEV+1))
               ELSE
                  JPAR = LQ(JPAR-1)
               ENDIF
  169       CONTINUE
            JPAR = JPAR +5
            NPAR = IQ(JPAR)
            GO TO 180
         ENDIF
*       (normal case)
  170    NPAR = Q(JVOT+5)
         IF (NPAR.EQ.0) THEN
            JPAR = JIN +9
            NPAR = Q(JPAR)
         ELSE
            JPAR = JVOT +6
         ENDIF
*
*   *   Check if point is in current content
*
  180    IF (IROTT.EQ.0) THEN
            DO 189 I = 1,3
               XT(I)   = XC(I) -Q(JIN+4+I)
  189       CONTINUE
         ELSE
*          (later, code in line)
            CALL GITRAN (XC, Q(JIN+5), IROTT, XT)
         ENDIF
*
         CALL GINME (XT, Q(JVOT+2), Q(JPAR+1), IYES)
         IF (IYES.NE.0) THEN
*
*  *       If so, prepare information for volume retrieval
*
            INGOTO = IN
            NLEVIN = NLEVIN + 1
            LQ(JGPAR-NLEVIN) = JPAR
            IQ(JGPAR+NLEVIN) = NPAR
            LVOLUM(NLEVIN)   = IVOT
            NAMES(NLEVIN)    = IQ(JVOLUM+IVOT)
            NUMBER(NLEVIN)   = Q(JIN+3)
            LINDEX(NLEVIN)   = IN
            LINMX(NLEVIN)    = NIN
            GONLY(NLEVIN)    = Q(JIN+8)
            IF (LQ(LQ(JVOLUM-IVOT)).EQ.0) THEN
               NLDEV(NLEVIN) = NLDEV(NLEVIN-1)
            ELSE
               NLDEV(NLEVIN) = NLEVIN
            ENDIF
            CALL GTRMUL (GTRAN(1,NLEVIN-1),GRMAT(1,NLEVIN-1), Q(JIN+5),
     +                   IROTT, GTRAN(1,NLEVIN), GRMAT(1,NLEVIN))
            IF (GONLY(NLEVIN).NE.0.) GO TO 400
            INFR = 0
            JVO  = LQ(JVOLUM-IVOT)
            GO TO 100
         ENDIF
*
  190    IF (ISEARC.LT.0) THEN
            IF (ICONT.EQ.NCONT) GO TO 300
            ICONT = ICONT +1
            GO TO 120
         ELSE
            IF (INEAR.EQ.NNEAR) GO TO 300
            INEAR = INEAR +1
            GO TO 110
         ENDIF
*
 200  CONTINUE
*
*  *    Case with content defined by division
*
         JDIV   = LQ(JVO-1)
         ISH    = Q(JVO+2)
         IAXIS  = Q(JDIV+1)
         IVOT   = Q(JDIV+2)
         JVOT   = LQ(JVOLUM-IVOT)
         IF (NLEVIN.LT.NLDEV(NLEVIN)) THEN
            JPARM = 0
         ELSE
*          (case with structure JVOLUM locally developed)
            JPARM = LQ(LQ(JVOLUM-LVOLUM(NLDEV(NLEVIN))))
            IF (NLEVIN.EQ.NLDEV(NLEVIN)) GO TO 215
            DO 210 ILEV = NLDEV(NLEVIN), NLEVIN-1
               IF (IQ(JPARM+1).EQ.0) THEN
                  JPARM = LQ(JPARM-LINDEX(ILEV+1))
                  IF (JPARM.EQ.0) GO TO 215
               ELSE IF (IQ(JPARM-3).GT.1) THEN
                  JPARM = LQ(JPARM-LINDEX(ILEV+1))
               ELSE
                  JPARM = LQ(JPARM-1)
               ENDIF
               IF (ILEV.EQ.NLEVIN-1) THEN
                  NDIV = IQ(JPARM+1)
                  ORIG =  Q(JPARM+2)
                  SDIV =  Q(JPARM+3)
               ENDIF
  210       CONTINUE
            GO TO 220
         ENDIF
*        (normal case)
  215    NDIV = Q(JDIV+3)
         ORIG = Q(JDIV+4)
         SDIV = Q(JDIV+5)
*
  220    IDT  = IDTYP(IAXIS,ISH)
         IF (IDT.EQ.1) THEN
*
*  *       Division along X, Y or Z axis
*
            XTT = XC(IAXIS)
            IF (ISH.EQ.10) THEN
               IF (IAXIS.NE.3) THEN
                  XTT = XTT - Q(JPAR+IAXIS+4) * XC(3)
                  IF (IAXIS.EQ.1) THEN
                     YT  = XC(2) - Q(JPAR+6) * XC(3)
                     XTT = XTT - Q (JPAR+4) * YT
                  ENDIF
               ENDIF
            ENDIF
            IN  = (XTT - ORIG) /SDIV + 1
         ELSE IF (IDT.EQ.2) THEN
*
*  *       Division along R axis
*
            R = XC(1)**2 + XC(2)**2
            IF (ISH.EQ.9) R = R + XC(3)**2
            R = SQRT(R)
            IF (ISH.EQ.5.OR.ISH.EQ.6.OR.ISH.EQ.9) THEN
               IN  = (R - ORIG) /SDIV + 1
            ELSE IF (ISH.EQ.7.OR.ISH.EQ.8) THEN
               DR  = 0.5 * (Q(JPAR+4) - Q(JPAR+2)) / Q(JPAR+1)
               RMN = 0.5 * (Q(JPAR+4) + Q(JPAR+2)) + DR * XC(3)
               DR  = 0.5 * (Q(JPAR+5) - Q(JPAR+3)) / Q(JPAR+1)
               RMX = 0.5 * (Q(JPAR+5) + Q(JPAR+3)) + DR * XC(3)
               STP = (RMX - RMN) / NDIV
               IN  = (R - RMN) / STP + 1
            ELSE
               IF (ISH.EQ.12) THEN
                  IPT = JPAR + 1
               ELSE
                  IPT = JPAR + 2
               ENDIF
               IF (IZSEC.GT.0) THEN
                  IPT = IPT + 3 * IZSEC
               ELSE
                  NZ  = Q(IPT+2)
                  DO 221 IZ = 1, NZ-1
                     IF ((XC(3)-Q(IPT+3*IZ))*(XC(3)-Q(IPT+3*IZ+3))
     +                                                   .LE.0.) THEN
                        IZSEC = IZ
                        IPT   = IPT + 3 * IZSEC
                        GO TO 222
                     ENDIF
  221             CONTINUE
                  IN  = 0
                  GO TO 230
               ENDIF
  222          POR1 = (Q(IPT+3) - XC(3)) / (Q(IPT+3) - Q(IPT))
               POR2 = (XC(3) - Q(IPT)) / (Q(IPT+3) - Q(IPT))
               RMN  = POR1 * Q(IPT+1) + POR2 * Q(IPT+4)
               RMX  = POR1 * Q(IPT+2) + POR2 * Q(IPT+5)
               IF (ISH.EQ.11) THEN
                  NPDV = Q(JPAR+3)
                  DPH  = Q(JPAR+2) / NPDV
                  IF (IPSEC.LE.0) THEN
                     IF (XC(1).NE.0..OR.XC(2).NE.0.) THEN
                        PHI  = RADDEG * ATAN2 (XC(2), XC(1))
                     ELSE
                        PHI  = 0.0
                     ENDIF
                     PH0 = PHI-Q(JPAR+1)
                     SG = SIGN(1.0,PH0)
                     PH0 = MOD( ABS(PH0), 360.0 )
                     IF(SG.LE.0.0) PH0=360.0-PH0
                     IPSEC= PH0/DPH + 1
                  ENDIF
                  PH   = DEGRAD * (Q(JPAR+1) + (IPSEC - 0.5) * DPH)
                  R    = XC(1) * COS(PH) + XC(2) * SIN(PH)
               ENDIF
               STP  = (RMX - RMN) / NDIV
               IN   = (R - RMN) / STP + 1
            ENDIF
         ELSE IF (IDT.EQ.3) THEN
*
*  *       Division along Phi Axis
*
            IF (XC(1).NE.0..OR.XC(2).NE.0.) THEN
               PHI = RADDEG * ATAN2 (XC(2), XC(1))
            ELSE
               PHI = 0.0
            ENDIF
            SG = SIGN(1.0,PHI-ORIG)
            DPHO = MOD ( ABS(PHI-ORIG), 360.)
            IF (SG.LE.0.) DPHO = 360.-DPHO
            IN = DPHO/SDIV +1
         ELSE IF (IDT.EQ.4) THEN
*
*  *       Division along Theta axis
*
            IF (XC(3).NE.0.0) THEN
               RXY  = SQRT (XC(1)**2 + XC(2)**2)
               THET = RADDEG * ATAN (RXY/XC(3))
               IF (THET.LT.0.0) THET = THET + 180.
            ELSE
               THET = 90.
            ENDIF
            IN  = (THET - ORIG) / SDIV + 1
         ENDIF
*
  230    IF (IN.GT.NDIV) IN = 0
         IF (IN.GT.0) THEN
*
*  *       Prepare information for volume retrieval, and return
*
            NL1  = NLEVIN + 1
            IF (JPARM.NE.0) THEN
               IF (IQ(JPARM-3).GT.1) THEN
                  JPAR = LQ(JPARM-IN)
               ELSE
                  JPAR = LQ(JPARM-1)
               ENDIF
               JPAR = JPAR + 5
               NPAR = IQ(JPAR)
            ELSE
               NPAR = Q(JVOT+5)
               JPAR = JVOT + 6
            ENDIF
*
            IF (IDT.EQ.1) THEN
               DO 231 I = 1, 3
                  X0(I) = 0.0
  231          CONTINUE
               X0(IAXIS) = ORIG + (IN - 0.5) * SDIV
               IF (ISH.EQ.4.OR.(ISH.EQ.10.AND.IAXIS.NE.1)) THEN
                  CALL GCENT (IAXIS, X0)
               ENDIF
               IF (GRMAT(10,NLEVIN).EQ.0.0) THEN
                  DO 232 I = 1, 3
                     GTRAN(I,NL1) = GTRAN(I,NLEVIN) + X0(I)
  232             CONTINUE
                  DO 233 I = 1, 10
                     GRMAT(I,NL1) = GRMAT(I,NLEVIN)
  233             CONTINUE
               ELSE
                  CALL GTRMUL (GTRAN(1,NLEVIN), GRMAT(1,NLEVIN), X0,
     +                         0, GTRAN(1,NL1), GRMAT(1,NL1))
               ENDIF
*
            ELSE IF (IDT.EQ.3.OR.IDT.EQ.4) THEN
               IF (IDT.EQ.3) THEN
                  PH0  = DEGRAD * (ORIG + (IN - 0.5) * SDIV)
                  CPHR = COS (PH0)
                  SPHR = SIN (PH0)
               ELSE
                  PH0  = 0.0
                  CPHR = 1.0
                  SPHR = 0.0
               ENDIF
               DO 234 I = 1, 3
                  GTRAN(I  ,NL1) = GTRAN(I,NLEVIN)
                  GRMAT(I  ,NL1) = GRMAT(I,NLEVIN)*CPHR
     +                           + GRMAT(I+3,NLEVIN)*SPHR
                  GRMAT(I+3,NL1) = GRMAT(I+3,NLEVIN)*CPHR
     +                           - GRMAT(I,NLEVIN)*SPHR
                  GRMAT(I+6,NL1) = GRMAT(I+6,NLEVIN)
  234          CONTINUE
               IF (PH0.EQ.0.0.AND.GRMAT(10,NLEVIN).EQ.0.0) THEN
                  GRMAT(10,NL1) = 0.0
               ELSE
                  GRMAT(10,NL1) = 1.0
               ENDIF
*
            ELSE
               DO 235 I = 1, 3
                  GTRAN(I,NL1) = GTRAN(I,NLEVIN)
  235          CONTINUE
               DO 236 I = 1, 10
                  GRMAT(I,NL1) = GRMAT(I,NLEVIN)
  236          CONTINUE
            ENDIF
*
            NLEVIN = NL1
            LVOLUM(NLEVIN) = IVOT
            NAMES(NLEVIN)  = IQ(JVOLUM+IVOT)
            NUMBER(NLEVIN) = IN
            LINDEX(NLEVIN) = IN
            LINMX(NLEVIN)  = NDIV
            GONLY(NLEVIN)  = GONLY(NLEVEL)
            IF (LQ(LQ(JVOLUM-IVOT)).EQ.0) THEN
               NLDEV(NLEVIN) = NLDEV(NLEVIN-1)
            ELSE
               NLDEV(NLEVIN) = NLEVIN
            ENDIF
            LQ(JGPAR-NLEVIN) = JPAR
            IQ(JGPAR+NLEVIN) = NPAR
            IF (GONLY(NLEVIN).NE.0.) GO TO 400
            INFR = 0
            JVO  = LQ(JVOLUM-IVOT)
            GO TO 100
         ENDIF
*
  300 IF (GONLY(NLEVIN).EQ.0.) THEN
*
* ***   Case with current volume NOT-ONLY
*
         IF (NLONLY.EQ.NLEVMX) THEN
            CALL GSCVOL
            NLONLY = NLEVIN
  305       NLONLY = NLONLY -1
            IF (GONLY(NLONLY).EQ.0.) GO TO 305
         ELSE IF (NLEVIN.GT.NLEVEL) THEN
            GO TO 400
         ELSE IF (NLEVIN.EQ.NLEVEL) THEN
            DO 309 NL = NLONLY+1,NLEVEL
               IF (LINDEX(NL).LT.LIND2(NL)) GO TO 400
               IF (LINDEX(NL).NE.LIND2(NL)) GO TO 310
  309       CONTINUE
         ENDIF
*
  310    NLEVIN = NLEVIN -1
         JVO = LQ(JVOLUM-LVOLUM(NLEVIN))
         NIN = Q(JVO+3)
         IF (NIN.LT.0) GO TO 310
         NLSTO  = NLEVIN
*
         IF (GRMAT(10,NLEVIN).EQ.0.) THEN
            DO 319 I = 1,3
               XC(I) = X(I) -GTRAN(I,NLEVIN)
  319       CONTINUE
         ELSE
            CALL GTRNSF (X, GTRAN(1,NLEVIN), GRMAT(1,NLEVIN), XC)
         ENDIF
*
         JNEAR = LQ(LQ(JVO-LINDEX(NLEVIN+1))-1)
         IF (JNEAR.EQ.0) JNEAR = LQ(JVO-NIN-1)
         JNEAR = JNEAR +1
         NNEAR = IQ(JNEAR)
         IOK   = 0
         DO 329 INEAR = 1,NNEAR
            IF (IQ(JNEAR+INEAR).EQ.LINDEX(NLEVIN+1))
     +                          IQ(JNEAR+INEAR) = -IQ(JNEAR+INEAR)
            IF (IQ(JNEAR+INEAR).LE.0) GO TO 329
            IF (IOK.EQ.0) IOK = INEAR
            JIN  = LQ(JVO-IQ(JNEAR+INEAR))
            IF (Q(JIN+8).NE.0.) GO TO 329
            JN = LQ(LQ(JVO-INEAR)-1)
            IF (JN.EQ.0) JN = LQ(JVO-NIN-1)
            JN = JN +1
            N  = IQ(JN)
            DO 328 I = 1,N
               IF (IQ(JN+I).EQ.LINDEX(NLEVIN+1)) IQ(JN+I) = -IQ(JN+I)
  328       CONTINUE
  329    CONTINUE
         IF (IOK.NE.0) THEN
            INEAR  = IOK
            ISEARC = 0
            GO TO 110
         ENDIF
*
         IF (NLEVIN.GT.NLONLY) THEN
*          Restore JNEAR members before going up the tree
            DO 359 IN = 1,NIN
               JNEAR = LQ(LQ(JVO-IN)-1)
               IF (JNEAR.EQ.0) JNEAR = LQ(JVO-NIN-1)
               JNEAR = JNEAR +1
               NNEAR = IQ(JNEAR)
               DO 358 INEAR = 1,NNEAR
                  IF (IQ(JNEAR+INEAR).LT.0)
     +                   IQ(JNEAR+INEAR) = -IQ(JNEAR+INEAR)
  358          CONTINUE
  359       CONTINUE
         ENDIF
      ENDIF
*
      ISAME = 1
*
  400 IF (NLONLY.NE.NLEVMX) THEN
*       Restore JNEAR members in current tree branch
  410    JVO = LQ(JVOLUM-LVOLUM(NLSTO))
         NIN = Q(JVO+3)
         DO 459 IN = 1,NIN
            JNEAR = LQ(LQ(JVO-IN)-1)
            IF (JNEAR.EQ.0) JNEAR = LQ(JVO-NIN-1)
            JNEAR = JNEAR +1
            NNEAR = IQ(JNEAR)
            DO 458 INEAR = 1,NNEAR
               IF (IQ(JNEAR+INEAR).LT.0)
     +                IQ(JNEAR+INEAR) = -IQ(JNEAR+INEAR)
  458       CONTINUE
  459    CONTINUE
         IF (NLSTO.GT.NLONLY) THEN
            NLSTO = NLSTO -1
            GO TO 410
         ENDIF
*
         CALL GFCVOL
         IF (NJTMAX.GT.0) THEN
            NLEVIN = -NLONLY
         ELSE
            NLEVIN = NLONLY
         ENDIF
*
      ENDIF
*                                                             END GINVOL
  999 END
