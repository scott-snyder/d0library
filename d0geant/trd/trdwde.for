      FUNCTION TRDWDE(E,EPL1,EPL2,X1,X2,N,GAMMA,SIG)
C       ---------===
C
C  ANALYTICAL FORMULA FOR RADIATION TRANSITION PRODUCTION
C  FROM:C.W. FABJAN AND W. STRUCZINKSKI;PHYS. LETT. 57B,483 (1975)
C
      IMPLICIT NONE
C
      INTEGER IFOIS,N,K
      REAL E,EPL1,EPL2,X1,X2,GAMMA,SIG,E1,EP1,EP2,XE1,XE2,TAU,HQCE
      REAL V,D1,B0,ABE,AN0,N0,ESUM,TPIN,C1,SU,SUM,TRDWDE,BEXP
C
      DATA  HQCE / 3.9465717E-08  /
      DATA IFOIS/0/
C
      IFOIS=IFOIS+1
C
C      IF(IFOIS.LE.1)WRITE(16, 9000)
 9000 FORMAT(' ENTER TRDWDE',/,' ----- ----')
      E1  = (E)
      EP1 = (EPL1)
      EP2 = (EPL2)
      XE1 = (X1)
      XE2 = (X2)
C
C      WRITE(16, 6341)E1,EP1,EP2,XE1,XE2
C      PRINT 6341,E1,EP1,EP2,XE1,XE2
 6341 FORMAT(' E1,EP1,EP2,XE1,XE2',5G10.4)
      TAU = XE2/XE1
C      WRITE(16, 6342)TAU,HQCE
 6342 FORMAT(' TAU,HQCE',2G10.4)
      V = XE1*(EP1**2-EP2**2)/(E1*HQCE)
      D1 = -TAU*V
      B0 = ((XE1+XE2)/GAMMA**2 + (XE1*EP1**2+XE2*EP2**2)/E1**2)
     +      *(E1/HQCE)
C
C       CALCULATE B*EXP
C
      ABE = N*SIG
C      WRITE(16, 604)N,SIG,ABE
C      PRINT 604,N,SIG,ABE
  604 FORMAT(' N',I4,'SIG,ABE',2G10.4)
      BEXP=(1-EXP(-ABE))/SIG
C
C       CALCULATE THE SUM
C
      AN0 = (1.+B0/6.2831853)
      N0  = IFIX(AN0)
      ESUM = 0.
C
C      WRITE(16, *)' AVANT LA BOUCLE 100,N0= ',N0
      DO     100     K = N0,10000
        TPIN = 6.2831853*K
        C1 = TPIN - D1
        SU = (TPIN-B0)/(C1**2*(TPIN-V)**2)
        SUM = SU*(1.-COS(C1/(1.+TAU)))
        ESUM = ESUM + SUM
        IF(SU.LT.ESUM/20000.)     GO TO 110
  100 CONTINUE
C           PINC=-2.
  110 TRDWDE  = 4. / 137.036 * BEXP * (1.+TAU)**2. * V**2 * ESUM
C      WRITE(LOUT,*)'BEXP',BEXP,' V',V,'ESUM',ESUM,'TRDWDE',TRDWDE
C      IF(IFOIS.LE.1)WRITE(16, 9001)
 9001 FORMAT(' EXIT TRDWDE',/,' ---- ----')
      RETURN
      END
