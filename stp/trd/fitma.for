      SUBROUTINE FITMA (LT,ICASE,IANG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the bank TMAA,TMAB,TMAC: Tables of the 
C-                         ratio of prob. for electrons/prob. for pions
C-                         Total energy/Nb. of clusters.
C-                         from  CERN calibration runs.
C-                         To be used to compute likelihood Etot/NB. of clust.
C-                         Cluster threshold =0 . From file MANEx10.DAT(IANG=1)
C-                         or MANEx20.DAT (IANG=2) with x=A,B,C corresponding
C-                         to cluster threshold of 0,30,60 resp.
C-
C-   Inputs  : LT= Address of the bank
C-             IANG=1 For 50 degrees,= 2 For 90 degrees
C-             ICASE= 1,2,or 3  for the 3 cluster thresholds
C-   Outputs :
C-   Controls:
C-
C-   Created  30-AUG-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,ICH,ID,IANG,ICASE,J,JUNIT,LT,K,KI,NBETOT,NBCLUS,NBECL
      CHARACTER*26 CAR26,DFILE
      CHARACTER*1 CAR1,char1(3)
      REAL EEL,EPI,S1,S2,VNORM
      LOGICAL OK
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      DATA CHAR1/'A','B','C'/
      DATA JUNIT/16/
C
      NBETOT=100
      NBCLUS=10
      IC(LT+1) = 0     !   Bank Type
      IC(LT+2) = 0     !   Status
      IC(LT+3) = 0     !   Quality factor
      IC(LT+4) = 0     !   Lowest Valid Run Number
      IC(LT+5) = 1000  !   Highest Valid Run Number
      IC(LT+6) = 0     !   Run Number Used to Generate
      IC(LT+7) = 10789 !   Date Generated
      IC(LT+8) =  0    !   Time Generated
      IC(LT+9) =  1    !   Type of Run Generated for
       C(LT+10)=  0.0  !   Version Number
      IC(LT+11)= 10*100+NBETOT
       C(LT+12)= -1*100-15
       C(LT+13)= 15    !   Step in Etot
C----------------------------------------------------------------------
      WRITE(CAR1,'(I1)')IANG
      DFILE = 'MANE'//CHAR1(ICASE)//CAR1//'0'
      CALL D0OPEN(JUNIT, DFILE, 'IF', OK)
      IF(.NOT.OK)GO TO 180
      READ(JUNIT,'(10I4)')J,(IWS(I),I=1,J)
      READ(JUNIT,'(A26)') CAR26
      K=LT
      NBECL=NBETOT*NBCLUS
      DO 88 ICH=1,3
        S1=0.
        S2=0.
        DO 25 ID=1,2
          J=(ID-1)*NBECL
          READ(JUNIT,'(10I7)') (IWS(I+J),I=1,NBECL)
   25   CONTINUE
        KI=K+13
C  COMPUTE PROBABILITY FUNCTIONS
        DO 40 I=1,NBECL
          EPI=FLOAT(IWS(I))
          EEL=FLOAT(IWS(I+NBECL))
          EPI=AMAX1(0.01,EPI)
          EEL=AMAX1(0.01,EEL)
          K=K+1
            C(KI+I)= EEL/EPI
          IF(EEL.EQ.0.01 .AND. EPI.EQ.0.01)C(KI+I)=0.
          S1=EPI+S1
          S2=EEL+S2
   40   CONTINUE
        VNORM=0.
        IF(S1*S2.NE.0.) VNORM=S1/S2
C        PRINT*,' VNORM',VNORM
C        PRINT*,'ICH',ICH,'KI',KI
C        PRINT*,' AVANT NORM.'
C        PRINT'(10F7.3)',(C(KI+I),I=1,NBETOT)
        DO 50 I=1,NBECL
          C(KI+I)=C(KI+I)*VNORM
          IF(C(KI+I).EQ.0.)C(KI+I)=1.
   50   CONTINUE
C        PRINT*,' APRES NORM.'
   88 CONTINUE
      CLOSE (JUNIT)
C      WRITE(3,*)' DANS FITMA,NB. DE POINTS',K-LT
C      WRITE(3,'(10F7.3)')(C(13+I),I=LT,K)
      GO TO 999
  180 CALL INTMSG(' ERROR OPENING TRD FILE '//DFILE)
  999 RETURN
      END
