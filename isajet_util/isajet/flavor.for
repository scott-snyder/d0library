CDECK  ID>, FLAVOR. 
      SUBROUTINE FLAVOR(ID,IFL1,IFL2,IFL3,JSPIN,INDEX)
C
C          This subroutine unpacks the IDENT code ID=+/-IJKL
C
C          Mesons--
C          I=0, J<=K, +/- is sign for J
C          ID=110 for PI0, ID=220 for ETA, etc.
C
C          Baryons--
C          I<=J<=K in general
C          J<I<K for second state antisymmetric in (I,J), eg. L = 2130
C
C          Other--
C          ID=1,...,6 for quarks
C          ID=9 for gluon
C          ID=10 for photon
C          ID=11,...,16 for leptons
C          ID=20 for KS, ID=-20 for KL
C
C          I=21...26 for left scalar quarks
C          I=29 for gluino
C          I=30 for Z1SS
C          I=31...36 for left scalar leptons
C          I=39 for W1SS
C          I=40 for Z2SS
C          I=41...46 for right scalar quarks
C          I=49 for W2SS
C          I=50 for Z3SS
C          I=51...56 for right scalar leptons
C          I=60 for Z4SS
C
C          ID=80 for W+
C          ID=81,...,89 for Higgs mesons
C          ID=90 for Z0
C
C          Diquarks--
C          ID=+/-IJ00, I<J for diquark composed of I,J.
C
C          INDEX is a sequence number used internally
C
C          Ver. 7.03: Make more robust by returning INDEX = 0 for
C          bad ID codes. Does not check for valid baryons, e.g.,
C          uuu with J = 1/2. Test on LABEL(1:3) = 'ERR' for this.
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:NQLEP.DEF'
      REAL      AMLEP(100)
      INTEGER ID,IFL1,IFL2,IFL3,JSPIN,INDEX
      INTEGER I,J,K,IDABS
C
      IDABS=IABS(ID)
      IF(IDABS.GT.NQLEP-1.AND.IDABS.LT.80) GO TO 400
      IF(IDABS.GT.90.AND.IDABS.LE.100) GO TO 400
C          Quarks: ID < 100
      IF(IDABS.LT.100) GO TO 200
      I=IDABS/1000
      J=MOD(IDABS/100,10)
      K=MOD(IDABS/10,10)
      JSPIN=MOD(IDABS,10)
      IF(I.EQ.9.OR.J.EQ.9.OR.K.EQ.9) GO TO 400
      IF(JSPIN.GT.1) GO TO 400
C          Mesons: 100 < ID < 1000
      IF(IDABS.LT.1000) GO TO 100
C          Diquarks: ID > 1000 but K = 0
      IF(K.EQ.0.AND.JSPIN.EQ.0) GO TO 300
C          Baryons
C          Only X,Y baryons are QQX, QQY, Q=U,D,S.
      IF(I.GT.K.OR.J.GT.K.OR.J.EQ.0) GO TO 400
      IF(K.GT.6.AND.(I.GT.3.OR.J.GT.3)) GO TO 400
      IFL1=ISIGN(I,ID)
      IFL2=ISIGN(J,ID)
      IFL3=ISIGN(K,ID)
      IF(K.LE.6) THEN
        INDEX=MAX0(I-1,J-1)**2+I+MAX0(I-J,0)+(K-1)*K*(2*K-1)/6
     1  +109*JSPIN+36*NMES+NQLEP+11
      ELSE
        INDEX=MAX0(I-1,J-1)**2+I+MAX0(I-J,0)+9*(K-7)+91
     1  +109*JSPIN+36*NMES+NQLEP+11
      ENDIF
      RETURN
C          Mesons
100   CONTINUE
      IF(J.GT.K) GO TO 400
      IF(J.EQ.K.AND.ID.LT.0) GO TO 400
      IFL1=0
      IFL2=ISIGN(J,ID)
      IFL3=ISIGN(K,-ID)
      INDEX=J+K*(K-1)/2+36*JSPIN+NQLEP
      INDEX=INDEX+11
      RETURN
C          Quarks, leptons, etc
200   CONTINUE
      IFL1=0
      IFL2=0
      IFL3=0
      JSPIN=0
      INDEX=IDABS
      IF(IDABS.LT.20) RETURN
C          Define INDEX=20 for KS, INDEX=21 for KL
      INDEX=IDABS+1
      IF(ID.EQ.20) INDEX=20
C          INDEX=NQLEP+1,...,NQLEP+11 for W+, Higgs, Z0
      IF(IDABS.LT.80) RETURN
      INDEX=NQLEP+IDABS-79
      RETURN
C          Diquarks
300   IF(JSPIN.GT.0.OR.I.GT.J) GO TO 400
      IF(I.GT.6.OR.J.GT.6) GO TO 400
      IFL1=ISIGN(I,ID)
      IFL2=ISIGN(J,ID)
      IFL3=0
      JSPIN=0
      INDEX=109*NBARY+36*NMES+NQLEP+11+I+J*(J-1)/2
      RETURN
C          Error
400   CONTINUE
      IFL1=0
      IFL2=0
      IFL3=0
      JSPIN=0
      INDEX=0
      RETURN
      END
