      SUBROUTINE CGET_PHI(EX,WY,ICELL,LO_HI,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :This routine returns the PHI cell corresponding 
C-                        to the given X,Y coordinates.
C-
C-   Inputs  : The X,Y coordinates of the point.
C-   Outputs : PHI (and an error message) for the X,Y pair
C-   Controls: None
C-
C-   Created  31-OCT-1990   John M. Balderston 
C-   Modified  7-FEB-1991   J.B.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      REAL QUANT,EX,WY
      REAL PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC'
      INTEGER ICELL,IPHIC               ! the return value
      INTEGER ERROR,QUAD,ZROFLG         ! Error message,Quadrant,X/Y=0
      REAL CEN_TAN_PHI(16)
      REAL TANPHI(16)
      INTEGER MIN, MID, MAX, LO_HI      ! running indexes
      LOGICAL FIRST
      MAX=INT(NPHIL/4.)
      MIN=1
      ERROR=0
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
        DO 100 IPHIC = 1,MAX-1
          TANPHI(IPHIC) = TAN(IPHIC*TWOPI/NPHIL)
          CEN_TAN_PHI(IPHIC)=TAN(IPHIC*TWOPI/NPHIL-PI/NPHIL)
  100   CONTINUE
        TANPHI(MAX)=999.
        CEN_TAN_PHI(MAX)=TAN(HALFPI-PI/NPHIL)
        FIRST=.FALSE.
      ENDIF
      QUAD=0
      QUANT=0
C-
C--------! Determine which quandrant the point is in !--------
C-
      IF ((EX.GT.0).AND.(WY.GT.0)) THEN
        QUAD=1
        GO TO 150
      ELSEIF((EX.LT.0).AND.(WY.GT.0)) THEN
        EX=-EX
        QUAD=2
        GO TO 150
      ELSEIF((EX.LT.0).AND.(WY.LT.0)) THEN
        QUAD=3
        GO TO 150
      ELSEIF((EX.GT.0).AND.(WY.LT.0)) THEN
        WY=-WY
        QUAD=4
        GO TO 150
      END IF
C-
C----! Check that neither X nor Y or both are zero !---------
C-
      IF ((EX.EQ.0).AND.(WY.EQ.0)) THEN
        ZROFLG=1
        ERROR=4
        GO TO 300
      ELSEIF((EX.EQ.0).AND.(WY.LT.0)) THEN
        ZROFLG=2       
        QUAD=3
        ERROR=2
        GO TO 300
      ELSEIF((EX.EQ.0).AND.(WY.GT.0)) THEN
        QUAD=1
        ZROFLG=2
        ERROR=2
        GO TO 300
      END IF
  150 CONTINUE
C-
C--------! Next determine which base cell the point is in !---------
C-
      QUANT=WY/EX
      IF (QUANT.LT.TANPHI(1)) THEN
         MAX=1
         GO TO 300
      ELSEIF(QUANT.GT.TANPHI(15)) THEN  ! 15 = (NPHIL/4)-1
         MAX=16
         GO TO 300
      END IF
      DO 200 WHILE((MAX-MIN).GT.1)
        MID=INT((MAX+MIN)/2.)
        IF (QUANT.LT.TANPHI(MID)) THEN
          MAX=MID
        ELSE
          MIN=MID
        END IF
  200 CONTINUE
  300 CONTINUE
      IF (ZROFLG.EQ.1) THEN
        MAX=8
      ELSEIF(ZROFLG.EQ.2) THEN
        MAX=16
      END IF
      ICELL=MAX
      IF (QUANT.EQ.TANPHI(MAX)) THEN
       ERROR=1
      END IF
C-
C----------! Return the actual cell !-------------
C-
      IF (QUAD.EQ.1) THEN
       IF (QUANT.LT.CEN_TAN_PHI(ICELL)) THEN
        LO_HI=0
       ELSE
        LO_HI=1
       ENDIF
       GO TO 999
      ELSEIF(QUAD.EQ.2) THEN
       IF (QUANT.LT.CEN_TAN_PHI(ICELL)) THEN
        LO_HI=1
       ELSE
        LO_HI=0
       ENDIF
       ICELL=33-ICELL
       EX=-EX
       GO TO 999
      ELSEIF(QUAD.EQ.3) THEN
       IF (QUANT.LT.CEN_TAN_PHI(ICELL)) THEN
        LO_HI=0
       ELSE
        LO_HI=1
       ENDIF
       ICELL=32+ICELL
       GO TO 999
      ELSEIF(QUAD.EQ.4) THEN
       IF (QUANT.LT.CEN_TAN_PHI(ICELL)) THEN
        LO_HI=1
       ELSE
        LO_HI=0
       ENDIF
       ICELL=65-ICELL
       WY=-WY
      END IF
  999 RETURN
      END
