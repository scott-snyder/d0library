      SUBROUTINE UPCATD(KCATD,ISEC,IETA,IPHI,ETA,PHI,DELETA,E)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpacks the packed words of CATD bank.
C-
C-   Inputs  : KCATD [I] - an address of the packed word
C-                         ( to get this address, use GTCATD )
C-             ISEC  [I] - give 1 for EM, 2 for HAD or TOTAL
C-   Outputs : IETA  [I] - detector eta index
C-             IPHI  [I] - detector phi index
C-              ETA  [F] - nominal eta
C-              PHI  [F] - nominal phi
C-            DELETA [F] - delta_eta = true_eta - nominal_eta
C-              E(4) [F] - Ex, Ey, Ez and E of the tower
C-   Controls:
C-
C-   Created  20-DEC-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCATD.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER PACTWR
      INTEGER KCATD1,IE,ISIGN,IDLETA,IENER,NV
      INTEGER JBYT,JBIT, ISTAT
      INTEGER KCATD,ISEC,IETA,IPHI
      REAL    ETA,PHI,DELETA,E(4)
      REAL    EUNIT,DEUNIT,ENER,ETANOM,ET
      REAL    XC,YC,ZC,ZOT,ZV(10),DZ(10),THETAN,THETAT
      REAL    SMALL
      PARAMETER( SMALL = 1.0E-5 )
C
      DATA    EUNIT,DEUNIT / .1, .01/
C----------------------------------------------------------------------
C-
C--- unpack IETA, IPHI, IDELTA_ETA and IENERGY of the packed tower word.
C-
      KCATD1 = KCATD
      PACTWR = IQ(KCATD1)
      IE   = JBYT(PACTWR, 1, 7)
      IPHI = JBYT(PACTWR, 8, 7)
      ISIGN  = JBIT(PACTWR,15)
      IDLETA = JBYT(PACTWR,16, 4)
      IF (ISIGN .EQ. 1)   IDLETA = -1*IDLETA
      DELETA = IDLETA * DEUNIT
      IF (IE .LE. NETAL) THEN
        IETA = IE - NETAL - 1
      ELSE
        IETA = IE - NETAL
      ENDIF
      IENER = JBYT(PACTWR,20,13)
      ENER  = IENER * EUNIT
C-
C--- get vertex information
C-
      CALL ZVERTE(NV,ZV,DZ)
      IF(NV.EQ.0) THEN
        CALL ERRMSG('CATD','CATDFL',
     &              'NO_VERTICES - Z set to 0','W')
        ZV(1)=0.0
      ENDIF
C-
      IF (ISEC .EQ. 1) THEN
        CALL CELXYZ(IETA,IPHI, 1,XC,YC,ZC,ISTAT)
      ELSE
        CALL CELXYZ(IETA,IPHI,11,XC,YC,ZC,ISTAT)
      ENDIF
      IF(ISTAT .EQ. 0) THEN
        PHI = ATAN2(YC,XC+SMALL)
        IF (PHI .LT. 0.) PHI =PHI + TWOPI
        ZC  = ZC - ZV(1)
        ZOT = (ZC+SMALL)/(SQRT(XC*XC+YC*YC+ZC*ZC)+SMALL)
        THETAN = ACOS(ZOT)
        ETANOM = -ALOG(TAN(THETAN/2.)+SMALL)
        ETA    = DELETA + ETANOM
        THETAT = 2.*(ATAN(EXP(-ETA)))
        ET     = ENER*SIN(THETAT)
        E(1)   = ET*COS(PHI)
        E(2)   = ET*SIN(PHI)
        E(3)   = ENER*COS(THETAT)
        E(4)   = ENER
      ENDIF
C-
  999 RETURN
      END
