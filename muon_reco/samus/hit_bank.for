      SUBROUTINE HIT_BANK(NTR_ALL,NTR,PTR,N_N,N_S)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-MAR-1991   O.Eroshin
C-
C----------------------------------------------------------------------
C     IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER GZSARD
      INTEGER NTR(2),NST,IPL,NRD
      INTEGER LSAHH,LSAMH,LSATH,LSARD,GZSAHH,GZSAMH,SATREK,S_ROAD,
     &  V_TRACK,GZGMUH
      INTEGER    STATION(3),SECTION(3,3),SPARK(100,2)
      REAL    TRACK(3,100,2),PTR(100,2)
C----------------------------------------------------------------------
C      EQUIVALENCE (NTR(1),NTR_N),(NTR(2),NTR_S)
C----------------------------------------------------------------------
C
      CALL VZERO(NTR,2)
      NTR_N = 0
      NTR_S = 0
C
C     LGMUH = LQ(LHEAD-16)
      LGMUH = GZGMUH()
C-----
      NTR_ALL = IQ(LGMUH+1)
C-----
      N_S = 0
      N_N = 0
      DO II=1,NTR_ALL
        IF (II.EQ.1) LGSAT = LQ(LGMUH-1)
        IF (II.NE.1) LGSAT = LQ(LGSAT)
          DIF = SQRT(Q(LGSAT+6)**2+Q(LGSAT+7)**2+Q(LGSAT+8)**2)
          IF (Q(LGSAT+8).LT.0.)         THEN
            IF (IQ(LGSAT+2).EQ.5) CALL HF1(50,-DIF,1.)
            IF (IQ(LGSAT+2).EQ.6) CALL HF1(50, DIF,1.)
                                        ELSE
            IF (IQ(LGSAT+2).EQ.5) CALL HF1(51,-DIF,1.)
            IF (IQ(LGSAT+2).EQ.6) CALL HF1(51, DIF,1.)
          END IF
          CALL HF1(46,ATAN2(Q(LGSAT+7),Q(LGSAT+6)+0.0001)*180/3.1415927,
     &      1.)
          DF = SQRT(Q(LGSAT+6)**2+Q(LGSAT+7)**2)
          CALL HF1(40,ATAN2(DF,Q(LGSAT+8)+0.0001)*180/3.1415927,1.)
          CALL HF1(41,ATAN2(DF,Q(LGSAT+8)+0.0001)*180/3.1415927,1.)
        IF (Q(LGSAT+8).LT.0) N_N = N_N+1
        IF (Q(LGSAT+8).GT.0) N_S = N_S+1
        IF (IQ(LGSAT+2).EQ.5.OR.IQ(LGSAT+2).EQ.6) THEN
          CALL VZERO(STATION,3)
          CALL VZERO(SECTION,9)
          ID = (IQ(LGSAT-1)-10)/5
D          IF (Q(LGSAT+8).LT.0) TYPE *,' TRACK: ',II,'  SP:',ID
          DO I=1,ID
            IS = Q(LGSAT+10+5*(I-1)+1)
            IN = Q(LGSAT+10+5*(I-1)+2)
            IW = Q(LGSAT+10+5*(I-1)+3)
            IF (IS.GT.3) IS=IS-3
            IF (IN.GT.3) IN=IN-3
D            IF (Q(LGSAT+8).LT.0) TYPE *,' ST/SC/WR: ',IS,IN,IW
            STATION(IS)    = 1
            SECTION(IN,IS) =1
          END DO
          DO I=1,3
            STATION(I) = SECTION(1,I)+SECTION(2,I)+SECTION(3,I)
          END DO
          CALL HF1(47,ATAN2(Q(LGSAT+7),Q(LGSAT+6)+0.0001)*180/3.1415927,
     &      1.)
          DF = SQRT(Q(LGSAT+6)**2+Q(LGSAT+7)**2)
          CALL HF1(42,ATAN2(DF,Q(LGSAT+8)+0.0001)*180/3.1415927,1.)
          CALL HF1(43,ATAN2(DF,Q(LGSAT+8)+0.0001)*180/3.1415927,1.)
          IF (Q(LGSAT+8).LT.0.0)                THEN
            NTR_N = NTR_N+1
            TRACK(1,NTR_N,1) = -Q(LGSAT+6)/DIF
            TRACK(2,NTR_N,1) = -Q(LGSAT+7)/DIF
            TRACK(3,NTR_N,1) = -Q(LGSAT+8)/DIF
            SPARK(NTR_N,1)   = STATION(1)+STATION(2)+STATION(3)
            IF (STATION(2).LT.2.OR.STATION(3).LT.2) SPARK(NTR_N,1)=-
     &        IABS(SPARK(NTR_N,1))
            IF (STATION(1)+STATION(2)+STATION(3).LT.5) SPARK(NTR_N,1)=-
     &        IABS(SPARK(NTR_N,1))
            IF (IQ(LGSAT+2).EQ.5) PTR(NTR_N,1) = -DIF
            IF (IQ(LGSAT+2).EQ.6) PTR(NTR_N,1) =  DIF
            CALL HF1(52,PTR(NTR_N,1),1.)
                                                ELSE
            NTR_S = NTR_S+1
            TRACK(1,NTR_S,1) = Q(LGSAT+6)/DIF
            TRACK(2,NTR_S,1) = Q(LGSAT+7)/DIF
            TRACK(3,NTR_S,1) = Q(LGSAT+8)/DIF
            SPARK(NTR_S,2)   = STATION(1)+STATION(2)+STATION(3)
            IF (STATION(2).LT.2.OR.STATION(3).LT.2) SPARK(NTR_S,2)=-
     &        IABS(SPARK(NTR_S,2))
            IF (STATION(1)+STATION(2)+STATION(3).LT.5) SPARK(NTR_S,2)=-
     &        IABS(SPARK(NTR_S,2))
            IF (IQ(LGSAT+2).EQ.5) PTR(NTR_S,2) = -DIF
            IF (IQ(LGSAT+2).EQ.6) PTR(NTR_S,2) =  DIF
            CALL HF1(53,PTR(NTR_S,2),1.)
          END IF
        END IF
      END DO
      CALL HF1(4,FLOAT(N_N),1.)
      CALL HF1(5,FLOAT(N_S),1.)
C----------------------------------------------------------------------
      DO I=1,2
        NTR(I) = 0
        IF (I.EQ.1) NT=NTR_N
        IF (I.EQ.2) NT=NTR_S
        IF (NT.GT.0)                    THEN
          DO II=1,NT
            IF (SPARK(II,I).GT.0) NTR(I) = NTR(I)+1
          END DO
        END IF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
