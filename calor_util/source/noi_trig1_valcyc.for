      SUBROUTINE NOI_TRIG1_VALCYC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used for pileup.
C-                         Calculate bucket occupancies
C-                         Calculate weights of past and future events
C-                                   for level 1.
C-   Inputs  :
C-   Outputs : WEIGHT(0,KMAX) array of event weights
C-                          stored in NOISY.INC
C-             WEIGHT(0) = weight of signal event
C-                   (1-KMAX) = weight of past, present, and future
C-                             events
C-             IWBUCK(1-KMAX) = bucket ID of events
C-             KFIN = Number of pileup events given non-zero weight
C-             KPRES = Number of interactions in current bucket
C-   Controls:
C-
C-   Created   6-JAN-1992   Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      INTEGER INARAY(KMAX)
      REAL RANARY(KMAX),VEIGHT(KMAX)
      INTEGER II,JJ,KK,IVBUCK(KMAX)
      INTEGER IHIT,NOI_HIT
      REAL ARG,RSEED,RNDM
C      REAL ASURV,ATRY,VERT_EFF
C      DATA ASURV,ATRY,VERT_EFF/3*0.0/
C
C#####EXECUTION BEGINS HERE######
C
      DO KK=1,KMAX
        WEIGHT(KK)=0.0
        VEIGHT(KK)=0.0
        IVBUCK(KK)=-1000
        IWBUCK(KK)=-1000
      ENDDO
C.....NOW START TO FILL WEIGHTS
      KK=0
C
C.....PRESENT
C
      KPRES=0
      IF(DO_PRESENT .OR. DO_PRESENT1)THEN
        JJ=80
   10   CONTINUE
        IHIT=NOI_HIT()
C        ATRY=ATRY+1.0
        DO II=1,IHIT
C         VAX VERSION:
C          ARG=RAN(RAN_VALUE)
C         RNDM:
          RSEED=II
          ARG=RNDM(RSEED)
C!           TYPE*,' DOUBLE VTX?',ARG,DOUBLE_VERTEX
          IF(ARG.GE.DOUBLE_VERTEX)GO TO 10
        ENDDO
C        ASURV=ASURV+1.0
C        IF(ATRY.NE.0.0)VERT_EFF=ASURV/ATRY
        KPRES=IHIT
        DO II=1,IHIT
          IF(KK.GE.KMAX)GO TO 50
          KK=KK+1
          IVBUCK(KK)=JJ
          IF(DO_PRESENT1)THEN
            VEIGHT(KK)=FRACTION(JJ)
          ENDIF
        ENDDO
      ENDIF                 ! PRESENT
C
C.....FUTURE
C
      IF(DO_FUTURE1)THEN
        CALL ERRMSG('NOISY','NOI_TRIG1_VALCYC',
     &   'TRIG LEV 1 Future pileup not implemented') 
      ENDIF
      IF(DO_FUTURE)THEN
        DO JJ=81,100
          IF(FRACTION(JJ).NE.0.0)THEN
            IHIT=NOI_HIT()
C!             TYPE*,' CROSSING,HITS',JJ,IHIT
            DO II =1,IHIT
              IF(KK.GE.KMAX)GO TO 50
              KK=KK+1
              IVBUCK(KK)=JJ
C
C              VEIGHT(KK)=FRACTION(JJ)
C
C!               TYPE*,'          KK,WEIGHT      ',KK,WEIGHT(KK)
            ENDDO
          ENDIF
        ENDDO
      ENDIF            ! FUTURE
   20 CONTINUE
C
C.....PAST
C
      IF(DO_PAST1)THEN
        CALL ERRMSG('NOISY','NOI_TRIG1_VALCYC',
     &   'TRIG LEV 1 PAST pileup not implemented') 
      ENDIF
      IF(DO_PAST)THEN
        DO JJ=79,1,-1
          IF(FRACTION(JJ).NE.0.0)THEN
            IHIT=NOI_HIT()
C!           TYPE*,' CROSSING,HITS',JJ,IHIT
            DO II=1,IHIT
              IF(KK.GE.KMAX)GO TO 50
              KK=KK+1
              IVBUCK(KK)=JJ
C
C              VEIGHT(KK)=FRACTION(JJ)
C
C!              TYPE*,'          KK,WEIGHT     ',KK,WEIGHT(KK)
            ENDDO
          ENDIF
        ENDDO
      ENDIF         !PAST
   50 KFIN=KK
C
C.... NOW PUT the weights in random order
C
      DO JJ=1,NUMBPILE
C       VAX VERSION:
C        RANARY(JJ)=RAN(RAN_VALUE)
C       RNDM:
        RSEED=JJ
        RANARY(JJ)=RNDM(RSEED)
      ENDDO
      CALL SORTZV(RANARY,INARAY,NUMBPILE,1,0,0)
      DO JJ=1,NUMBPILE
        WEIGHT(JJ)=VEIGHT(INARAY(JJ))
        IWBUCK(JJ)=IVBUCK(INARAY(JJ))
      ENDDO
C
C.....SIGNAL
C
      WEIGHT(0)=1.0
      IWBUCK(0)=80
C
C     DONE FILING ARRAY WEIGHT
C
c 
      RETURN
      END
