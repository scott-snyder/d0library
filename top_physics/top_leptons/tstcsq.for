      INTEGER FUNCTION TSTCSQ(DET_ETA,CHISQ,LEVEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  xxxxxxxxx  Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  LEVEL
      REAL     ETAD,DET_ETA,CHISQ
C----------------------------------------------------------------------
      TSTCSQ = 1
      IF (LEVEL.EQ.99) THEN
        GOTO 998
      ENDIF
      ETAD = DET_ETA
      IF (ABS(ETAD).LE.25.) THEN        
        IF (CHISQ.GT.200) GOTO 999
        IF (LEVEL.EQ.2) THEN
          IF (ABS(ETAD).LE.13.) THEN        
*           ! modified for CC only.  5/17/93
            IF (CHISQ.GT.100) GOTO 999  
          ENDIF
        ENDIF
        IF (LEVEL.EQ.3) THEN
*           9/26/93
          IF (CHISQ.GT.100) GOTO 999  
        ENDIF
      ELSE IF (ABS(ETAD).LE.34.) THEN
        IF (CHISQ.GT.60) GOTO 999
      ELSE
        IF (CHISQ.GT.35) GOTO 999
      ENDIF
  998 CONTINUE
      TSTCSQ = 0
  999 RETURN
      END
