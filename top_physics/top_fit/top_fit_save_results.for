      SUBROUTINE TOP_FIT_SAVE_RESULTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SAVE RESULTS INTO NTUPLE/ZEBRA ETC
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-FEB-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:EVENT_QUAN_2C.INC'
      INCLUDE 'D0$INC:FIT_QUAN_2C.INC'
      INCLUDE 'D0$INC:KINE_FIT.INC'
      INTEGER I,IER
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      DOUBLE PRECISION    CHSQMN
      INTEGER IMIN
C
      INTEGER NCMBMX
      PARAMETER( NCMBMX = 120)
      REAL BUFFER(100,NCMBMX)
      INTEGER NCOMB
      SAVE BUFFER
      REAL    RTOT,DTOT
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_FIT_RCP')
        CALL DO_HBOOK('TOP_FIT_NTUPLE')
        CALL EZRSET
      ENDIF
      BUFFER(1,COMBNUM) = RUNC
      BUFFER(2,COMBNUM) = EVENTC
      BUFFER(3,COMBNUM) = COMBNUM
      DO I = 1 , NFIT
        CALL MNPOUT(I,FIT_NAMES(I),FIT_VAL(I),FIT_ERR(I),
     &    FIT_LOLIM(I), FIT_HILIM(I),IER)
      ENDDO

      BUFFER(4,COMBNUM) = FIT_VAL(1)  !FITTED MASS
      BUFFER(5,COMBNUM) = CHISQC
      BUFFER(6,COMBNUM) = PROBC
      BUFFER(7,COMBNUM) = 0.0  !Maxprob filled in at end
      BUFFER(8,COMBNUM) = FEDM  !Distance from minimum
      BUFFER(9,COMBNUM) = ISTAT  !status of fit
      BUFFER(10,COMBNUM) = ISR_LIKE
      BUFFER(11,COMBNUM) = FSR_LIKE
C
      CALL TOP_FIT_CALCULATE_WEIGHTS(FIT_VAL(1),TOP_LEPTON_F,
     &  TOP_HADRON_F,RWT,DWT)
C
      BUFFER(12,COMBNUM) = RWT
      BUFFER(13,COMBNUM) = DWT
C
      CALL UCOPYDS(XMEAS,BUFFER(14,COMBNUM),17)
      CALL UCOPYDS(XPRED,BUFFER(31,COMBNUM),17)
      CALL UCOPYDS(STRFN,BUFFER(48,COMBNUM),17)
      CALL UCOPYDS(FIT_VAL,BUFFER(65,COMBNUM),15)
      RETURN
C
      ENTRY TOP_FIT_WRITE_NTUPLE
C
      NCOMB=COMBNUM
      CHSQMN=1.0E8
      IMIN = 0
      DTOT = 0.
      RTOT = 0.
      DO I = 1 , NCOMB
        IF ( CHSQMN.GT.BUFFER(5,I) ) THEN
          CHSQMN = BUFFER(5,I)
          IMIN = I
        ENDIF
        RTOT = RTOT + BUFFER(12,I)
        DTOT = DTOT + BUFFER(13,I)
      ENDDO
C
C
C ****  NORMALIZING DG AND RR WEIGHTS AMONG COMBINATIONS
C
      DO I = 1 , NCOMB
        BUFFER(12,I) = BUFFER(12,I)/RTOT
        BUFFER(13,I) = BUFFER(13,I)/DTOT
      ENDDO
C
      BUFFER(7,IMIN) = 1.0  !MIN CHISQ
C
      DO I = 1 , NCOMB
        CALL DO_HFN('FIT2C',100,BUFFER(1,I))
      ENDDO
C
  999 RETURN
      END
