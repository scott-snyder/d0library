      REAL FUNCTION ERRJN(IDUM)

C...JetNet function calculate ERRor.

C...Returns the error function.
C...The error measure is selected by MSTJN(4).

      INCLUDE 'D0$INC:JNDAT1.INC'
      PARAMETER(MAXV=2000,MAXM=150000)
      COMMON /JNINT1/ O(MAXV),A(MAXV),D(MAXV),T(MAXV),DT(MAXV),
     &                W(MAXM),DW(MAXM),NSELF(MAXM),NTSELF(MAXV),
     &                G(MAXM+MAXV),ODW(MAXM),ODT(MAXV),ETAV(MAXM+MAXV)
      COMMON /JNINT2/ M(0:10),MV0(11),MM0(11),NG(10),NL,IPOTT,
     &                ER1,ER2,SM(10),ICPON
      SAVE /JNDAT1/,/JNINT1/,/JNINT2/

      REAL PWT

      PWT = 10**(PARJN(40))
      ERR=0.0
      IF (MSTJN(4).EQ.0) THEN
C...Summed square error:

        DO 100 I=1,M(NL)
          OUTVAL=O(JNINDX(NL,I,0))
          ERR=ERR+0.5*PWT*(OUT(I)-OUTVAL)**2
          OUT(I)=OUTVAL
100     CONTINUE

      ELSEIF (MSTJN(4).EQ.1) THEN
C...Cross Entropy error:

        DO 110 I=1,M(NL)
          OUTVAL=O(JNINDX(NL,I,0))
          ERR=ERR-(OUT(I)*LOG(OUTVAL)+(1.-OUT(I))*LOG(1.-OUTVAL))
          OUT(I)=OUTVAL
110     CONTINUE

      ELSEIF (MSTJN(4).GE.2) THEN
C...Kullback error:

        DO 120 I=1,M(NL)
          OUTVAL=O(JNINDX(NL,I,0))
          ERR=ERR+OUT(I)*LOG(OUT(I)/OUTVAL)
          OUT(I)=OUTVAL
120     CONTINUE

      ELSEIF (MSTJN(4).EQ.-1) THEN
C...Log-squared error:

        DO 130 I=1,M(NL)
          OUTVAL=O(JNINDX(NL,I,0))
          ERR=ERR-0.5*LOG(1.-(OUT(I)-OUTVAL)**2)
          OUT(I)=OUTVAL
130     CONTINUE

      ELSE IF ((MSTJN(4).LT.-1).AND.(MSTJN(4).GT.-10)) THEN
C...Summed power=-mstjn(4) error:
        J = -MSTJN(4)
        DO 135 I=1,M(NL)
          OUTVAL=O(JNINDX(NL,I,0))
          ERR=ERR+0.5*(OUT(I)-OUTVAL)**J
          OUT(I)=OUTVAL
135     CONTINUE

      ELSE IF ((MSTJN(4).LT.-10).AND.(MSTJN(4).GT.-20)) THEN
C...Summed chisquare + bkg tail supressing term
C   term =  sum bkg (low target outval) (1/(out))**power
C   power = mstjn(4)+10:
        J = ABS(MSTJN(4)+10)
        DO 136 I=1,M(NL)
          OUTVAL=O(JNINDX(NL,I,0))
          ERR=ERR+0.5*PWT*(OUT(I)-OUTVAL)**2
          IF(OUT(I).LT.(0.5)) THEN
            ERR=ERR+(OUTVAL-1.01)**(-J)
          END IF
          OUT(I)=OUTVAL
136     CONTINUE

      ELSEIF ((MSTJN(4).LT.-20).AND.(MSTJN(4).GT.-30)) THEN
C
C...add term to square error: chip 
C...   term is the sum(background)/sum(signal) about out(I) cut at X
C...   background is outval < 0.5
C...   signal     is outval > 0.5
C
        X = -float(MSTJN(4))/100.0
C
        NB = 0
        NS = 0
        DO 140 I=1,M(NL)
          OUTVAL=O(JNINDX(NL,I,0))
          ERR=ERR+0.5*PWT*(OUT(I)-OUTVAL)**2
          IF(OUTVAL.GT.X) THEN
            IF(OUT(I).LT.(0.5)) THEN
              NB = NB + 1
            ELSE
              NS = NS + 1
            END IF
          END IF
          OUT(I)=OUTVAL
  140   CONTINUE
        ERR = ERR+FLOAT(NB)-FLOAT(NS)
      ELSE 
        WRITE(6,150)MSTJN(4)
  150   FORMAT(' BAD CHOICE OF ERROR MEASURE : MSTJN(4)=',I5)
      ENDIF

      ERRJN=ERR

      RETURN
      END
