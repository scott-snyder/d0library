      SUBROUTINE ZMU_ISAJ(PTMU1,PTMU2,ETAMU1,ETAMU2,PHIMU1,PHIMU2,IDMU1,
     &  IDMU2)
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
      REAL PTMU1,ETAMU1,PHIMU1
      REAL PTMU2,ETAMU2,PHIMU2
      INTEGER LISV1,LISP1,LISAJ,ID_PARENT,IDV,ID,IDMU1,IDMU2
      REAL P(4),X,Y,Z,ETA,TH,PHI
C----------------------------------------------------------------------
      PTMU1 = 0.
      ETAMU1 = 0.
      PHIMU1 = 0.
      IDMU1 = 0
      PTMU2 = 0.
      ETAMU2 = 0.
      PHIMU2 = 0.
      IDMU2 = 0
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
              IF(ABS(ID_PARENT).EQ.90) THEN ! must come from Z0
                IF(ID.GT.0.) THEN
                  PTMU1 = SQRT(P(1)**2+P(2)**2)
                  ETAMU1 = ETA
                  PHIMU1 = PHI
                  IDMU1 = ID
                ELSE
                  PTMU2 = SQRT(P(1)**2+P(2)**2)
                  ETAMU2 = ETA
                  PHIMU2 = PHI
                  IDMU2 = ID
                ENDIF
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
