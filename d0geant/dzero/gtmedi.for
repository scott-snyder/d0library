*CMZ :          18/07/91  22.45.40  by  Federico Carminati  
*CMZ :  3.14/14 09/07/90  11.59.15  by  Rene Brun   
*-- Author :    
      SUBROUTINE GTMEDI (X, NUMED)  
C.  
C.    ******************************************************************    
C.    *                                                                *    
C.    *   Finds in which volume/medium the point X is, and updates the *    
C.    *    common /GCVOLU/ and the structure JGPAR accordingly.        *    
C.    *                                                                *    
C.    *   NUMED returns the tracking medium number, or 0 if point is   *    
C.    *         outside the experimental setup.                        *    
C.    *                                                                *    
C.    *   Note : For INWVOL = 2, INFROM set to a positive number is    *    
C.    *      interpreted by GTMEDI as the number IN of the content     *    
C.    *      just left by the current track within the mother volume   *    
C.    *      where the point X is assumed to be.                       *    
C.    *                                                                *    
C.    *   Called by : GTRACK                                           *    
C.    *   Authors   : S.Banerjee, R.Brun, F.Bruyant, A.McPherson       *    
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
      PARAMETER (MAXMEC=30) 
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)   
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG  
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL 
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN 
     + ,NLVSAV,ISTORY   
C   
      PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)    
      COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,   
     +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)    
C   
      COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),    
     +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),  
     +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3) 
C   
C.  
      DIMENSION  X(*)   
      REAL       X0(3), XC(3), XT(3)    
      REAL       XTEMP(3)
      INTEGER    IDTYP(3,12)    
      DOUBLE PRECISION TEMP,RNDIV,RSDIV 
C.  
      DATA  IDTYP / 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 2, 3, 1,    
     +              2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 4, 3, 1, 1, 1,    
     +              2, 3, 1, 2, 3, 1/   
C.  
C.    ------------------------------------------------------------------    
*   
* *** Get coordinates of point X in reference system of current volume  
*   
   10 IF (GRMAT(10,NLEVEL).EQ.0.) THEN  
         DO 19 I = 1,3  
            XC(I) = X(I) -GTRAN(I,NLEVEL)   
   19    CONTINUE   
      ELSE  
C       (Coded in line for D0GEANT: K. Wyatt Merritt; 7/26/91)
CCCCCCC  CALL GTRNSF (X, GTRAN(1,NLEVEL), GRMAT(1,NLEVEL), XC)  
        DO  I=1,3   
          XTEMP(I)=X(I)-GTRAN(I,NLEVEL)
        ENDDO
        DO  I=1,3   
          PTR = 3*I
          XC(I)=XTEMP(1)*GRMAT(PTR-2,NLEVEL)
     &      +XTEMP(2)*GRMAT(PTR-1,NLEVEL)+XTEMP(3)*GRMAT(PTR,NLEVEL)
        ENDDO
      ENDIF 
*   
      JVO  = LQ(JVOLUM-LVOLUM(NLEVEL))  
      IF (INGOTO.GT.0) THEN 
*   
* ***   Entrance in content INGOTO predicted by GTNEXT  
*   
         JIN   = LQ(JVO-INGOTO) 
         IVOT  = Q(JIN+2)   
         JVOT  = LQ(JVOLUM-IVOT)    
         JPAR = LQ(JGPAR-NLEVEL-1)  
*   
         PTR = JIN + 4
         IROTT = Q(PTR)   
         IF (IROTT.EQ.0) THEN   
            DO 49 I = 1,3   
               XT(I) = XC(I) -Q(PTR+I)    
   49       CONTINUE    
         ELSE   
*           (Coded in line for D0GEANT: K. Wyatt Merritt; 7/26/91)
************ CALL GITRAN (XC, Q(JIN+5), IROTT, XT) *
           DO I = 1 , 3
             XTEMP(I) = XC(I) - Q(PTR+I)
           ENDDO
           JR = LQ(JROTM - IROTT)
           DO I = 1 , 3
             PTR = JR + 3*I
             XT(I) = XTEMP(1)*Q(PTR-2) 
     &             + XTEMP(2)*Q(PTR-1)
     &             + XTEMP(3)*Q(PTR)
           ENDDO
         ENDIF  
*   
*   *   Check if point is in content    
*   
         CALL GINME (XT, Q(JVOT+2), Q(JPAR+1), IYES)    
         IF (IYES.NE.0) THEN    
*   
*          If so, prepare information for volume retrieval, and return  
*   
            NL1 = NLEVEL +1 
            LVOLUM(NL1) = IVOT  
            NAMES(NL1)  = IQ(JVOLUM+IVOT)   
            NUMBER(NL1) = Q(JIN+3)  
            LINDEX(NL1) = INGOTO    
            LINMX(NL1)  = Q(JVO+3)  
            GONLY(NL1)  = Q(JIN+8)  
            IF (LQ(LQ(JVOLUM-IVOT)).EQ.0) THEN  
               NLDEV(NL1) = NLDEV(NLEVEL)   
            ELSE    
               NLDEV(NL1) = NL1 
            ENDIF   
            CALL GTRMUL (GTRAN(1,NLEVEL), GRMAT(1,NLEVEL), Q(JIN+5),    
     +                   IROTT, GTRAN(1,NL1), GRMAT(1,NL1)) 
            NLEVEL = NL1    
            DO 69 I = 1,3   
               XC(I) = XT(I)    
   69       CONTINUE    
            JVO    = JVOT   
            INGOTO = 0  
            GO TO 80    
         ENDIF  
      ENDIF 
*   
      JPAR = LQ(JGPAR-NLEVEL)   
      CALL GINME (XC, Q(JVO+2), Q(JPAR+1), IYES)    
      IF (IYES.NE.0) GO TO 80   
*   
*  ** Point not in current volume, go up the tree   
*   
      IF (NLEVEL.GT.1) THEN 
         INFROM = LINDEX(NLEVEL)    
         INGOTO = 0 
         NLEVEL = NLEVEL -1 
         GO TO 10   
      ELSE  
*   
*   *   Point is outside setup  
*   
         NUMED = 0  
         GO TO 999  
      ENDIF 
*   
*  ** Point is in current volume    
*   
   80 IF (GONLY(NLEVEL).NE.0..AND.Q(JVO+3).EQ.0.) GO TO 990 
*   
      NLONLY = NLEVMX   
      NLMAX  = 0    
*   
*  **  Check contents, if any   
*   
  100 JVO = LQ(JVOLUM-LVOLUM(NLEVEL))   
      IF (Q(JVO+3).EQ.0.) GO TO 300 
      NIN = Q(JVO+3)    
      IF (NIN.LT.0) GO TO 200   
*   
*   *   Case with contents defined by Position  
*   
      JNEAR = LQ(JVO-NIN-1) 
      IF (INWVOL.EQ.2) THEN 
         IF (INFROM.NE.0) THEN  
            JIN   = LQ(JVO-INFROM)  
            IF (LQ(JIN-1).NE.0) JNEAR = LQ(JIN-1)   
         ENDIF  
         IF (IQ(JNEAR+2).EQ.0) GO TO 300    
      ENDIF 
      ISEARC = Q(JVO+1) 
      IF (ISEARC.LT.0) THEN 
*   
*       Prepare access list when contents have been ordered by GSORD    
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
         IF (ISEARC.GT.0) THEN  
            CALL GUNEAR (ISEARC, 1, XC, JNEAR)  
            IF (IQ(JNEAR+1).EQ.0) GO TO 300 
         ENDIF  
         JNEAR  = JNEAR +1  
         NNEAR  = IQ(JNEAR) 
         INEAR  = 1 
      ENDIF 
*   
  110 IN = IQ(JNEAR+INEAR)  
      IF (IN.GT.0) GO TO 150    
      GO TO 190 
*   
  120 IN = IQ(JSCV+ICONT)   
*   
*     For each selected content in turn, check if point is in   
*   
  150 IF (IN.EQ.INGOTO) GO TO 190   
      JIN  = LQ(JVO-IN) 
      IVOT = Q(JIN+2)   
      JVOT = LQ(JVOLUM-IVOT)    
      IF (IAND(IQ(JVOT),2).NE.0) THEN   
*       (case with JVOLUM structure locally developed)  
         JPAR = LQ(LQ(JVOLUM-LVOLUM(NLDEV(NLEVEL))))    
         DO 169 ILEV = NLDEV(NLEVEL), NLEVEL    
            IF (IQ(JPAR+1).EQ.0) THEN   
               IF (ILEV.EQ.NLEVEL) THEN 
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
  169    CONTINUE   
         JPAR = JPAR +5 
         NPAR = IQ(JPAR)    
         GO TO 180  
      ENDIF 
*      (normal case)    
  170 NPAR = Q(JVOT+5)  
      IF (NPAR.EQ.0) THEN   
         JPAR = JIN +9  
         NPAR = Q(JPAR) 
      ELSE  
         JPAR = JVOT +6 
      ENDIF 
*   
  180 PTR = JIN + 4
      IROTT  = Q(PTR) 
      IF (IROTT.EQ.0) THEN  
         DO 181 I = 1,3 
  181     XT(I) = XC(I) -Q(PTR+I) 
      ELSE  
*           (Coded in line for D0GEANT: K. Wyatt Merritt; 7/26/91)
************ CALL GITRAN (XC, Q(JIN+5), IROTT, XT) *
        DO I = 1 , 3
          XTEMP(I) = XC(I) - Q(PTR+I)
        ENDDO
        JR = LQ(JROTM - IROTT)
        DO I = 1 , 3
          PTR = JR + 3*I
          XT(I) = XTEMP(1)*Q(PTR-2) 
     &          + XTEMP(2)*Q(PTR-1)
     &          + XTEMP(3)*Q(PTR)
        ENDDO
      ENDIF 
      CALL GINME (XT, Q(JVOT+2), Q(JPAR+1), IYES)   
      IF (IYES.NE.0) THEN   
*   
*       Volume found at deeper level    
*   
         INGOTO = IN    
         NL1    = NLEVEL    
         NLEVEL = NLEVEL +1 
         LVOLUM(NLEVEL) = IVOT  
         NAMES(NLEVEL)  = IQ(JVOLUM+IVOT)   
         NUMBER(NLEVEL) = Q(JIN+3)  
         LINDEX(NLEVEL) = IN    
         LINMX(NLEVEL)  = NIN   
         GONLY(NLEVEL)  = Q(JIN+8)  
         IF (LQ(LQ(JVOLUM-IVOT)).EQ.0) THEN 
            NLDEV(NLEVEL) = NLDEV(NL1)  
         ELSE   
            NLDEV(NLEVEL) = NLEVEL  
         ENDIF  
         CALL GTRMUL (GTRAN(1,NL1), GRMAT(1,NL1), Q(JIN+5), IROTT,  
     +                GTRAN(1,NLEVEL), GRMAT(1,NLEVEL)) 
         IQ(JGPAR +NLEVEL) = NPAR   
         LQ(JGPAR -NLEVEL) = JPAR   
*   
         INFROM = 0 
         INGOTO = 0 
         DO 189 I = 1,3 
            XC(I) = XT(I)   
  189    CONTINUE   
         GO TO 100  
      ENDIF 
*   
  190 IF (ISEARC.LT.0) THEN 
         IF (ICONT.EQ.NCONT) GO TO 300  
         ICONT = ICONT +1   
         GO TO 120  
      ELSE  
         IF (INEAR.EQ.NNEAR) GO TO 300  
         INEAR = INEAR +1   
         GO TO 110  
      ENDIF 
*   
*   *   Case with contents defined by division  
*   
  200 JDIV  = LQ(JVO-1) 
      ISH   = Q(JVO+2)  
      IAXIS = Q(JDIV+1) 
      IVOT  = Q(JDIV+2) 
      JVOT  = LQ(JVOLUM-IVOT)   
      IF (NLEVEL.LT.NLDEV(NLEVEL)) THEN 
         JPAR = 0   
      ELSE  
*       (case with structure JVOLUM locally developped) 
         JPAR = LQ(LQ(JVOLUM-LVOLUM(NLDEV(NLEVEL))))    
         IF (NLEVEL.EQ.NLDEV(NLEVEL)) GO TO 250 
         DO 249 ILEV = NLDEV(NLEVEL), NLEVEL-1  
            IF (IQ(JPAR+1).EQ.0) THEN   
               JPAR = LQ(JPAR-LINDEX(ILEV+1))   
               IF (JPAR.EQ.0) GO TO 250 
            ELSE IF (IQ(JPAR-3).GT.1) THEN  
               JPAR = LQ(JPAR-LINDEX(ILEV+1))   
            ELSE    
               JPAR = LQ(JPAR-1)    
            ENDIF   
            IF (ILEV.EQ.NLEVEL-1) THEN  
               NDIV  = IQ(JPAR+1)   
               RNDIV = 1./NDIV  
               ORIG  =  Q(JPAR+2)   
               SDIV  =  Q(JPAR+3)   
               RSDIV = 1./SDIV  
            ENDIF   
  249    CONTINUE   
         GO TO 260  
      ENDIF 
*      (normal case)    
  250 NDIV  = Q(JDIV+3) 
      RNDIV = 1./NDIV   
      ORIG  = Q(JDIV+4) 
      SDIV  = Q(JDIV+5) 
      RSDIV = 1./SDIV   
*   
  260 IDT = IDTYP(IAXIS,ISH)    
      IF (IDT.EQ.1) THEN    
*   
*      Division along X, Y or Z axis    
*   
         XTT = XC(IAXIS)    
         IF (ISH.EQ.10) THEN    
            IF (IAXIS.NE.3) THEN    
               XTT = XTT - Q(LQ(JGPAR-NLEVEL)+IAXIS+4) * XC(3)  
               IF (IAXIS.EQ.1) THEN 
                  YT  = XC(2) - Q(LQ(JGPAR-NLEVEL)+6) * XC(3)   
                  XTT = XTT - Q(LQ(JGPAR-NLEVEL)+4) * YT    
               ENDIF    
            ENDIF   
         ENDIF  
         IN = (XTT -ORIG)/SDIV +1   
      ELSE IF (IDT.EQ.2) THEN   
*   
*       Division along R axis   
*   
         R = XC(1)**2 + XC(2)**2    
         IF (ISH.EQ.9) R = R + XC(3)**2 
         R = SQRT (R)   
         IF (ISH.EQ.5.OR.ISH.EQ.6.OR.ISH.EQ.9) THEN 
            IN = (R - ORIG) / SDIV + 1  
         ELSE IF (ISH.EQ.7.OR.ISH.EQ.8) THEN    
            IPAR = LQ(JGPAR-NLEVEL) 
            TEMP = 1./Q(IPAR+1) 
            DR   = 0.5 * (Q(IPAR+4) - Q(IPAR+2)) * TEMP 
            RMN  = 0.5 * (Q(IPAR+4) + Q(IPAR+2)) + DR * XC(3)   
            DR   = 0.5 * (Q(IPAR+5) - Q(IPAR+3)) * TEMP 
            RMX  = 0.5 * (Q(IPAR+5) + Q(IPAR+3)) + DR * XC(3)   
            STP  = (RMX - RMN) * RNDIV  
            IN   = (R - RMN) / STP + 1  
         ELSE   
            IPAR = LQ(JGPAR-NLEVEL) 
            IF (ISH.EQ.12) THEN 
               IPT = IPAR + 1   
            ELSE    
               IPT = IPAR + 2   
            ENDIF   
            IF (IZSEC.GT.0) THEN    
               IPT = IPT + 3 * IZSEC    
            ELSE    
               NZ  = Q(IPT+2)   
               DO 261 IZ = 1, NZ-1  
                PTR = IPT + 3*IZ
                IF((XC(3)-Q(PTR))*(XC(3)-Q(PTR+3)).LE.0.) THEN
                     IZSEC = IZ 
                     IPT   = IPT + 3 * IZSEC    
                     GO TO 262  
                  ENDIF 
  261          CONTINUE 
               IN  = 0  
               GO TO 265    
            ENDIF   
  262       POR1 = (Q(IPT+3) - XC(3)) / (Q(IPT+3) - Q(IPT)) 
            POR2 = (XC(3) - Q(IPT)) / (Q(IPT+3) - Q(IPT))   
            RMN  = Q(IPT+1) * POR1 + Q(IPT+4) * POR2    
            RMX  = Q(IPT+2) * POR1 + Q(IPT+5) * POR2    
            IF (ISH.EQ.11) THEN 
               DPH  = Q(IPAR+2) / AINT(Q(IPAR+3))   
               IF (IPSEC.LE.0) THEN 
                  IF (XC(1).NE.0..OR.XC(2).NE.0.) THEN  
                     PHI  = RADDEG * ATAN2 (XC(2), XC(1))   
                  ELSE  
                     PHI  = 0.0 
                  ENDIF 
                  PH0  = MOD (PHI-Q(IPAR+1)+360., 360.) 
                  IPSEC= PH0/DPH + 1    
               ENDIF    
               PH   = DEGRAD * (Q(IPAR+1) + (IPSEC - 0.5) * DPH)    
               R    = XC(1) * COS(PH) + XC(2) * SIN(PH) 
            ENDIF   
            STP = (RMX - RMN) * RNDIV   
            IN  = (R - RMN) / STP + 1   
         ENDIF  
      ELSE IF (IDT.EQ.3) THEN   
*   
*       Division along Phi axis 
*   
         IF (XC(1).NE.0..OR.XC(2).NE.0.) THEN   
            PHI = RADDEG * ATAN2 (XC(2), XC(1)) 
         ELSE   
            PHI = 0.    
         ENDIF  
         IN  = MOD (PHI-ORIG+360., 360.) * RSDIV + 1    
      ELSE IF (IDT.EQ.4) THEN   
*   
*       Division along Theta axis   
*   
         IF (XC(3).NE.0.0) THEN 
            RXY  = SQRT (XC(1)**2 + XC(2)**2)   
            THET = RADDEG * ATAN (RXY/XC(3))    
            IF (THET.LT.0.0)  THET = THET + 180.0   
         ELSE   
            THET = 90.0 
         ENDIF  
         IN   = (THET - ORIG) * RSDIV + 1   
      ENDIF 
*   
  265 IF (IN.GT.NDIV) IN = 0    
      IF (IN.LE.0) GO TO 300    
*   
      IF (JPAR.NE.0) THEN   
         IF (IQ(JPAR-3).GT.1) THEN  
            JPAR = LQ(JPAR-IN)  
         ELSE   
            JPAR = LQ(JPAR-1)   
         ENDIF  
         JPAR = JPAR + 5    
         NPAR = IQ(JPAR)    
      ELSE  
         NPAR = Q(JVOT+5)   
         JPAR = JVOT + 6    
      ENDIF 
*   
*      Volume found at deeper level 
*   
      NL1    = NLEVEL   
      NLEVEL = NLEVEL +1    
      LVOLUM(NLEVEL) = IVOT 
      NAMES(NLEVEL)  = IQ(JVOLUM+IVOT)  
      NUMBER(NLEVEL) = IN   
      LINDEX(NLEVEL) = IN   
      LINMX(NLEVEL)  = NDIV 
      GONLY(NLEVEL)  = GONLY(NL1)   
      IF (LQ(LQ(JVOLUM-IVOT)).EQ.0) THEN    
         NLDEV(NLEVEL) = NLDEV(NL1) 
      ELSE  
         NLDEV(NLEVEL) = NLEVEL 
      ENDIF 
*   
      IF (IDT.EQ.1) THEN    
         DO 275 I = 1, 3    
            X0(I) = 0.0 
  275    CONTINUE   
         X0(IAXIS) = ORIG + (IN - 0.5) * SDIV   
         IF (ISH.EQ.4.OR.(ISH.EQ.10.AND.IAXIS.NE.1)) THEN   
            CALL GCENT (IAXIS, X0)  
         ENDIF  
*       The following trannsformation could be skipped if Q(JVOT+3)=0.  
         DO 276 I = 1, 3    
            XC(I) = XC(I) - X0(I)   
  276    CONTINUE   
         IF (GRMAT(10,NL1).EQ.0.0) THEN 
            DO 277 I = 1, 3 
               GTRAN(I,NLEVEL) = GTRAN(I,NL1) + X0(I)   
  277       CONTINUE    
            DO 278 I = 1, 10    
               GRMAT(I,NLEVEL) = GRMAT(I,NL1)   
  278       CONTINUE    
         ELSE   
            CALL GTRMUL (GTRAN(1,NL1), GRMAT(1,NL1), X0, 0, 
     +                   GTRAN(1,NLEVEL), GRMAT(1,NLEVEL))  
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
         DO 279 I = 1, 3    
            GTRAN(I  ,NLEVEL) = GTRAN(I,NL1)    
            GRMAT(I  ,NLEVEL) = GRMAT(I,NL1)*CPHR + GRMAT(I+3,NL1)*SPHR 
            GRMAT(I+3,NLEVEL) = GRMAT(I+3,NL1)*CPHR - GRMAT(I,NL1)*SPHR 
            GRMAT(I+6,NLEVEL) = GRMAT(I+6,NL1)  
  279    CONTINUE   
*       The following trannsformation could be skipped if Q(JVOT+3)=0.  
         XTT   = XC(1) * CPHR + XC(2) * SPHR    
         XC(2) = XC(2) * CPHR - XC(1) * SPHR    
         XC(1) = XTT    
         IF (PH0.EQ.0.0.AND.GRMAT(10,NL1).EQ.0.0) THEN  
            GRMAT(10,NLEVEL) = 0.0  
         ELSE   
            GRMAT(10,NLEVEL) = 1.0  
         ENDIF  
         IF (ISH.EQ.11) IPSEC = 1   
*   
      ELSE  
         DO 280 I = 1, 3    
            GTRAN(I,NLEVEL) = GTRAN(I,NL1)  
  280    CONTINUE   
         DO 281 I = 1, 10   
            GRMAT(I,NLEVEL) = GRMAT(I,NL1)  
  281    CONTINUE   
      ENDIF 
*   
      IQ(JGPAR+NLEVEL) = NPAR   
      LQ(JGPAR-NLEVEL) = JPAR   
*   
      INFROM = 0    
      GO TO 100 
*   
* *** Point is in current volume/medium, and not in any content 
*   
  300 IF (GONLY(NLEVEL).EQ.0.) THEN 
*   
*  **   Lowest level is 'NOT ONLY'  
*   
         IF (NLONLY.EQ.NLEVMX) THEN 
            NLONLY = NLEVEL -1  
  305       IF (GONLY(NLONLY).EQ.0.) THEN   
               NLONLY = NLONLY -1   
               GO TO 305    
            ENDIF   
         ENDIF  
*   
         IF (NLEVEL.GT.NLMAX) THEN  
            CALL GSCVOL 
            NLMAX = NLEVEL  
         ENDIF  
*   
*   *   Go up the tree up to a volume with positioned contents  
*   
  310    NLEVEL = NLEVEL -1 
         JVO    = LQ(JVOLUM-LVOLUM(NLEVEL)) 
         NIN    = Q(JVO+3)  
         IF (NIN.LT.0) GO TO 310    
         NLSTO  = NLEVEL    
*   
         IF (GRMAT(10,NLEVEL).EQ.0.) THEN   
            DO 319 I = 1,3  
               XC(I) = X(I) -GTRAN(I,NLEVEL)    
  319       CONTINUE    
         ELSE   
C       (Coded in line for D0GEANT: K. Wyatt Merritt; 7/26/91)
CCCCCCC  CALL GTRNSF (X, GTRAN(1,NLEVEL), GRMAT(1,NLEVEL), XC)  
           DO  I=1,3   
             XTEMP(I)=X(I)-GTRAN(I,NLEVEL)
           ENDDO
           DO  I=1,3   
             PTR = 3*I
             XC(I)=XTEMP(1)*GRMAT(PTR-2,NLEVEL)
     &         +XTEMP(2)*GRMAT(PTR-1,NLEVEL)+XTEMP(3)*GRMAT(PTR,NLEVEL)
           ENDDO
         ENDIF  
*   
         JNEAR = LQ(LQ(JVO-LINDEX(NLEVEL+1))-1) 
         IF (JNEAR.EQ.0) JNEAR = LQ(JVO-NIN-1)  
         JNEAR = JNEAR +1   
         NNEAR = IQ(JNEAR)  
         IOK   = 0  
         DO 329 INEAR = 1,NNEAR 
            IF (IQ(JNEAR+INEAR).EQ.LINDEX(NLEVEL+1))    
     +                          IQ(JNEAR+INEAR) = -IQ(JNEAR+INEAR)  
            IF (IQ(JNEAR+INEAR).LE.0) GO TO 329 
            IF (IOK.EQ.0) IOK = INEAR   
            JIN = LQ(JVO-IQ(JNEAR+INEAR))   
            IF (Q(JIN+8).NE.0.) GO TO 329   
            JN = LQ(LQ(JVO-INEAR)-1)    
            IF (JN.EQ.0) JN = LQ(JVO-NIN-1) 
            JN = JN +1  
            N  = IQ(JN) 
            DO 328 I = 1,N  
               IF (IQ(JN+I).EQ.LINDEX(NLEVEL+1)) IQ(JN+I) = -IQ(JN+I)   
  328       CONTINUE    
  329    CONTINUE   
         IF (IOK.NE.0) THEN 
            INEAR  = IOK    
            ISEARC = 0  
            GO TO 110   
         ENDIF  
*   
         IF (NLEVEL.GT.NLONLY) THEN 
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
            GO TO 310   
         ENDIF  
      ENDIF 
*   
      IF (NLONLY.LT.NLEVMX) THEN    
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
         IF (NLEVEL.LE.NLMAX) CALL GFCVOL   
         JVO = LQ(JVOLUM-LVOLUM(NLEVEL))    
      ENDIF 
*   
  990 NUMED = Q(JVO+4)  
*                                                             END GTMEDI    
  999 END   
