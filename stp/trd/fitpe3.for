      SUBROUTINE FITPE3 (LT,IK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the bank TPE3: Table of "probabilities"
C-                         for pions and electrons using the TOTAL ENERGY
C-                         with the condition that 3 chambers are hit
C-                         Has been obtained integrating the total energy
C-                         summed over 3 chambers for pions and electrons
C                          Corresponds to PRETOTAL310 and PRETOTAL320
C                          (50 and 90 degrees resp.)
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
      INTEGER WSI(2),WSS(2)
      CHARACTER*26 CAR26,DFILE
      CHARACTER*1 CAR1
      REAL VNORM,OR,PAS,S,S1,S2,VSUM
      LOGICAL OK
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      DATA JUNIT/16/
C
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
C----------------------------------------------------------------------
      WRITE(CAR1,'(I1)')IK
      DFILE = 'PRETOTAL3'//CAR1//'0'
      CALL D0OPEN(JUNIT, DFILE, 'IF', OK)
      IF(.NOT.OK)GO TO 180
      READ(JUNIT,'(10I4)')J,(IWS(I),I=1,J)
C      PRINT*,'NB. DE RUNS',J,(IWS(I),I=1,J)
      DO 88 ID=1,2
C       ID=1 PIONS,ID=2 ELECTRONS
        READ(JUNIT,'(A26)') CAR26 !read the title
C      PRINT*,CAR26
        S=0.
        READ(JUNIT,'(F10.5,I10,F10.5)')OR,IET,PAS
        IC(LT+11)= IET   !   Nb. of steps 
         C(LT+12)= or    !   Origin
         C(LT+13)= pas   !   Step
        READ(JUNIT,'(10I7)')wsi(id),(IWS(I),I=1,iet),
     &        wss(Id)
C        PRINT*,' LECTURE'
C        PRINT'(10I7)',wsi(id),(IWS(I),I=1,iet), wss(Id)
        CALL VFLOAT(IWS,WS,IET)
        S1=WSI(ID)+WSS(ID)
        S2=VSUM(WS,IET)
        S=S1+S2
C       PRINT*,'WS'
C        PRINT'(10F7.1)',(WS(I),I=1,IET)
C        PRINT*,'S1,S2,S ',S1,S2,S
        S2=WSI(ID)
        S1=WSI(ID)
        J=(2-ID)*IET+13+LT
        DO 40 I=1,IET
          VNORM=WS(I)
          S1=S1+WS(I)
          WS(I)=1.-S2/S
          C(J+I)=1.-S2/S
C          PRINT*,I,' VNORM',VNORM,'S1,S',S1,S,' WS',WS(I)
          S2=S1
  40    CONTINUE
C        PRINT*,' ID',ID,'J',J
C        PRINT'(10F6.3)',(WS(I),I=1,IET)
        K=0
   88 CONTINUE
      CLOSE (JUNIT)
C      PRINT*,' NB. DE POINTS',K
C      PRINT'(10F7.3)',(C(13+I),I=1,2*iet)
      GO TO 999
  180 CALL INTMSG(' ERROR OPENING TRD FILE '//DFILE)
  999 RETURN
      END
