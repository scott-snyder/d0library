      SUBROUTINE NOI_TRIG2_VALCYC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used for pileup for trig level 1 and 2.
C-                         Calculate weights of past and future events
C-                             for level 2.
C-
C-   Inputs  : IWBUCK(1-KMAX) = Bucket ID of events
C-   Outputs : WEIGHT(0,KMAX) array of event weights
C-                          stored in NOISY.INC
C-             WEIGHT(0) = weight of signal event
C-                   (1-KMAX) = weight of past, present, and future
C-                             events
C-   Controls:
C-
C-   Created   6-JAN-1992   Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      INTEGER KK
C
C#####EXECUTION BEGINS HERE######
C
      DO KK=1,NUMBPILE
        IF(IWBUCK(KK).GT.0)THEN
          WEIGHT(KK)=FRACTION(IWBUCK(KK))
        ELSE
          WEIGHT(KK)=0.0
        ENDIF
      ENDDO
C
      WEIGHT(0)=1.0
C
C     DONE FILING ARRAY WEIGHT
C
      RETURN
      END
