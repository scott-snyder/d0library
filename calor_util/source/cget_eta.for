      SUBROUTINE CGET_ETA(AR,ZE,ICELL,LO_HI,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :This routine returns the ETA cell corresponding
C-                        to the space point X,Y,Z
C-
C-   Inputs  : The values r & z for comparison :(r/z) vs lookup table
C-   Outputs : The ETA cell corresponding to the point
C-   Controls: None
C-
C-   Created  31-OCT-1990   John M. Balderston 
C-   Modified  7-FEB-1991   J.B.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE 
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER I,IETA,ETA_IND,IERR
      REAL C_L,D_L,C_H,D_H,HI_CEN_ETA
      REAL CENETA,DELETA
      REAL AR,ZE,ALPHA,CHI,TANTH
      INTEGER ICELL,LO_HI               ! the return value
      INTEGER ERROR,ZESIGN,ZROFLG       ! Error message & pos./neg. Z,Z=0?
      INTEGER MIN, MID, MAX             ! running indexes
      REAL TANTHETA(NETAL)
      REAL CEN_TAN(NETAL-1)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
        DO 100 ICELL = 1,NETAL
         IF(ICELL.EQ.NETAL) THEN
            CALL CALETA(ICELL,CENETA,DELETA,IERR)
            TANTHETA(ICELL)=TAN(2.*ATAN(EXP(-(CENETA+DELETA/2.))))
         ELSEIF((ICELL.GT.32).AND.(ICELL.LT.NETAL)) THEN
            CALL CALETA(ICELL,CENETA,DELETA,IERR)
            TANTHETA(ICELL)=TAN(2.*ATAN(EXP(-(CENETA+DELETA/2.))))
            C_L=CENETA
            D_L=DELETA
            CALL CALETA(ICELL+1,CENETA,DELETA,IERR)
            C_H=CENETA
            D_H=DELETA
            HI_CEN_ETA=((C_L+D_L/2.)+(C_H+D_H/2.))/2.
            CEN_TAN(ICELL)=TAN(2.*ATAN(EXP(-HI_CEN_ETA)))
         ELSE
            CALL CALETA(ICELL,CENETA,DELETA,IERR)
            TANTHETA(ICELL)=TAN(2.*ATAN(EXP(-(CENETA+DELETA/2.))))
            CEN_TAN(ICELL)=TAN(2.*ATAN(EXP(-CENETA)))
         ENDIF
100     CONTINUE
        FIRST=.FALSE.
        ENDIF
      MAX=37
      MIN=1
      ERROR=0
      ZESIGN=0
      ZROFLG=0
C-
C-----! First check sign of Z !--------------
C-
      IF (ZE.EQ.0) THEN
        ZROFLG=1
        ERROR=1
        GO TO 200
      END IF
      IF (ZE.LT.0) THEN
        ZE=-ZE
        ZESIGN=1
      END IF
C-
C-----! Get base cell !----------------------
C-
      TANTH=AR/ZE
      IF (TANTH.GT.TANTHETA(1)) THEN
         MAX=1
         GO TO 200
      ELSEIF(TANTH.LT.TANTHETA(37)) THEN
         MAX=NETAL
         GO TO 200
      ELSE
       DO 150 I=1,NETAL
        MID=INT((MAX+MIN)/2.)
        IF (TANTH.LT.TANTHETA(MID)) THEN
          MIN=MID
        ELSE
          MAX=MID
        END IF
        IF((MAX-MIN).EQ.1) GOTO 200
  150   CONTINUE
      ENDIF  
  200 CONTINUE
      IF (ZROFLG.EQ.1) THEN
        MAX=NETAL
      END IF
      ICELL=MAX
      IF (TANTH.GT.CEN_TAN(MAX).AND.ZESIGN.EQ.0) LO_HI=0
      IF (TANTH.LT.CEN_TAN(MAX).AND.ZESIGN.EQ.0) LO_HI=1
      IF (TANTH.GT.CEN_TAN(MAX).AND.ZESIGN.EQ.1) LO_HI=1
      IF (TANTH.LT.CEN_TAN(MAX).AND.ZESIGN.EQ.1) LO_HI=0
      IF (ZESIGN.EQ.1) THEN
        ICELL=-ICELL
        ZE=-ZE
      END IF
  999 RETURN
      END
