      SUBROUTINE WMU_ISAJ(PTMU,ETAMU,PHIMU,IDMU)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   3-NOV-1992   Darien R. Wood
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INCLUDE 'D0$LINKS:IZISAJ.LINK'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL PTMU,ETAMU,PHIMU
      INTEGER LISV1,LISP1,LISAJ,ID_PARENT,IDV,ID,IDMU
      REAL P(4),X,Y,Z,ETA,TH,PHI
C----------------------------------------------------------------------
      PTMU = 0.
      ETAMU = 0.
      PHIMU = 0.
      IDMU = 0
      LISV1=0
C
  100 CALL GTISV1(LISV1,LISV1,IDV,P,X,Y,Z) ! loop over vertices
      IF(LISV1.GT.0) THEN 
        LISP1=LISV1-IZISP1
C
  200   CALL GTISP1(LISP1,LISP1,ID,P,PHI,TH,ETA) ! loop over particles
        IF(LISP1.GT.0) THEN 
          IF(ABS(ID).EQ.14)   THEN ! check for muon
            LISAJ = LQ(LISP1-3)
            IF(LISAJ.GT.0) THEN
              ID_PARENT = IQ(LISAJ+1)
              IF(ABS(ID_PARENT).EQ.80) THEN ! must come from W+-
                PTMU = SQRT(P(1)**2+P(2)**2)
                ETAMU = ETA
                PHIMU = PHI
                IDMU = ID
              ENDIF
            ENDIF  
          ENDIF
C
          GOTO 200
        ENDIF
C
        GOTO 100
      ENDIF
C
  999 RETURN
      END
