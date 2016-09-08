      SUBROUTINE TRDUNP(LL,YAUX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack the TRD information laying in ZEBRA
C-                          bank TRDH and TLYR
C-
C-   Inputs  :LL =Address of the bank in TLYR
C-   Outputs :YAUX(I)=dE/dT in bin I,I from 1 to NFADC
C-
C-   Created  14-DEC-1987   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER I,IFOIS,J,K,LL,LTRDH,MASK,NBITF,NWORDF
      INTEGER IAUX,IPROV,NERR
      REAL FACT,ORIG,S
      REAL YAUX(1),YPROV
      LOGICAL FIRST
      DATA IFOIS/0/,FIRST/.TRUE./,NERR/0/
C----------------------------------------------------------------------
C
C
C  THE FIRST TIME FIND IN BANK TRDH, THE NUMBER OF BITS PER FADC CHANNEL
C  AND THE NUMBER OF CHANNELS IN ONE 32 BITS WORD
      IF(FIRST)THEN
        LTRDH=LQ(LL+1)!Address of the supporting bank
        IF(LTRDH.LE.0)THEN
          NERR=NERR+1
          IF(NERR.LE.20)
     +        CALL ERRMSG('Wrong adress of Zebra bank TRDH ','TRUNP'
     +        ,' ' ,'W')
          GO TO 999
        END IF
        NBITF=IQ(LTRDH+2)
        NWORDF=IQ(LTRDH+3)
        MASK=0
        DO 5 I=1,NBITF
    5   MASK=MASK+2**(I-1)
        NERR=0
        FIRST=.FALSE.
      END IF
      IFOIS=IFOIS+1
      IF(LL.LE.0)THEN
        WRITE(LOUT,*)'ERROR IN ADDRESS OF ZEBRA BANK TLYR '
        GO TO 999
      END IF
      S=0.
      ORIG = MOD(IQ(LL+1),256)
      I=IQ(LL+1)/256
      FACT = FLOAT(I)/100.
C      print*,' dans trdunp,fact,orig',fact,orig
      K=0
      DO 20 I=2,IQ(LL-1)-2
        DO 10 J=1,NWORDF
          K=K+1
          IAUX=iand(ISHFT(IQ(LL+I),-NBITF*(J-1)),MASK)
          IPROV=iand(IBITS(IQ(LL+I),NBITF*(J-1),NBITF),MASK)
          YAUX(K)=IPROV
          IF(YAUX(K).LT.ORIG)THEN
            PRINT*,' PROBLEM_TRD :IN TRDUNP,YAUX',YAUX(K),
     +        ' INF A ORIG=',ORIG
          END IF
          YAUX(K)=(YAUX(K)-ORIG)/FACT
C          S=S+YAUX(K)
   10   CONTINUE
   20 CONTINUE
C      PRINT*,' DANS TRDUNP,S',S
      IF (IFOIS.LE.2 .AND. PTRD.GE.8) THEN
        WRITE (LOUT,*) ' IN TRDUNP.  FACT',FACT,' ORIG',ORIG,' YAUX :'
C        WRITE (LOUT,*) (YAUX(I),I=1,K)
      END IF
  999 RETURN
      END
