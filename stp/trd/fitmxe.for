      SUBROUTINE FITMXE (LT,IK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the bank TMXE: Tables of the ratio of
C_                    Total energy deposit for electrons and pions .
C                     From CERN calibration runs.
C                     To be used to compute likelihood on Total Energy.
C                     Corresponds to file MAXE10.DAT and MAXE20.DAT
C                     (For angle =50 and 90 resp.)
C-
C-   Inputs  : LT= Address of the bank
C-             IK=1 For 50 degrees,= 2 For 90 degrees
C-   Outputs :
C-   Controls:
C-
C-   Created  30-AUG-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,ICH,ID,IET,IK,J,JUNIT,LT,K,KI
      CHARACTER*26 CAR26,DFILE
      CHARACTER*1 CAR1
      REAL EEL,EPI,S1,S2,VNORM
      LOGICAL OK
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      DATA JUNIT/16/
C
C      WRITE(3,*)' DANS FITMXE,LT,IK',LT,IK
      IET=100
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
       IC(LT+11)= IET  !   Nb. of steps (NSTEP=100 for CERN calib.)
        C(LT+12)= -15  !   Origin
        C(LT+13)= 15   !   Step
C----------------------------------------------------------------------
      WRITE(CAR1,'(I1)')IK
      DFILE = 'MAXE'//CAR1//'0'
      CALL D0OPEN(JUNIT, DFILE, 'IF', OK)
      IF(.NOT.OK)GO TO 180
      READ(JUNIT,'(10I4)')J,(IWS(I),I=1,J)
C      PRINT*,'NB. DE RUNS',J,(IWS(I),I=1,J)
      READ(JUNIT,'(A26)') CAR26
C      PRINT*,CAR26
      K=LT
      DO 88 ICH=1,3
        S1=0.
        S2=0.
        DO 25 ID=1,2
          J=(ID-1)*IET
          READ(JUNIT,'(10I7)') (IWS(I+J),I=1,IET)
   25   CONTINUE
        KI=K+13
C  COMPUTE PROBABILITY FUNCTIONS
        DO 40 I=1,IET
          EPI=FLOAT(IWS(I))
          EEL=FLOAT(IWS(I+IET))
          EPI=AMAX1(EPI,0.01)
          EEL=AMAX1(EEL,0.01)
          K=K+1
          C(KI+I)= EEL/EPI
          IF(EEL.EQ.0.01 .AND. EPI.EQ.0.01)C(KI+I)=0.
C          PRINT*,' DANS FITMXE,I,KI',I,KI,'EEL,EPI',EEL,EPI,'C',C(KI+I)
          S1=EPI+S1
          S2=EEL+S2
   40   CONTINUE
        VNORM=0.
        IF(S1*S2.NE.0.) VNORM=S1/S2
C        PRINT*,' VNORM',VNORM
C        PRINT*,'ICH',ICH,'KI',KI
C        PRINT*,' AVANT NORM.'
C        PRINT'(10F7.3)',(C(KI+I),I=1,IET)
        DO 50 I=1,IET
          C(KI+I)=C(KI+I)*VNORM
          IF(C(KI+I).EQ.0.)C(KI+I)=1.
   50   CONTINUE
C        PRINT*,' APRES NORM.'
   88 CONTINUE
      CLOSE (JUNIT)
C      WRITE(3,*)' DANS FITMXE,K,LT',K,LT,'NB. DE POINTS',K-LT
C      WRITE(3,'(10F7.3)')(C(13+I),I=LT,K)
C      CALL DZSHOW(' A LA FIN DE FITMXE',IDVSTP,LT,'SLV',0,0,0,0)
      GO TO 999
  180 CALL INTMSG(' ERROR OPENING TRD FILE '//DFILE)
  999 RETURN
      END
