C----------------------------------------------------------------------
      LOGICAL FUNCTION T0HITS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines if there are hits in the fiber
C-                         detector
C-
C-   Inputs       : None
C-   Outputs      : None
C-   Return value : .True.  - if there are FADC pulses
C-                  .False. - otherwise
C-   Controls     : None
C-
C-   Created  14-MAY-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:T0DREC.INC'
      INCLUDE      'D0$PARAMS:T0D.PARAMS'
      INTEGER       NEMPTY, LEMPTY(NFADC), I
      INTEGER       LT0RW, GZT0RW
C----------------------------------------------------------------------
      NEMPTY =  NFADC
      T0HITS = .FALSE.
      LT0RW  =  GZT0RW(0)
      IF (LT0RW .LE. 0) GO TO 999
      NEMPTY =  0
      T0HITS = .TRUE.
C
    1 CONTINUE
      IF ( LT0RW .LE. 0 ) THEN
C
C ****  Second pass: dropping of the empty banks listed in LEmpty
C
        IF ( (NEMPTY .GT. 0) .AND. (NEMPTY .LT. NFADC) ) THEN
          DO I = 1,NEMPTY
            CALL MZDROP(IXCOM,LEMPTY(I),' ')
          END DO
          CALL ZPRESS(IXCOM,GZT0RW(0))
        ELSE IF ( NEMPTY .EQ. NFADC ) THEN
          CALL MZDROP(IXCOM,GZT0RW(0),'L')
          T0HITS = .FALSE.
        END IF
        GO TO 999
      ELSE IF ( IQ(LT0RW+2) .EQ. 0 ) THEN
C
C ****  First pass: filling of LEmpty with the addresses of empty banks
C
        NEMPTY = NEMPTY + 1
        LEMPTY(NEMPTY) = LT0RW
      END IF
      LT0RW = LQ(LT0RW)
      GO TO 1
C----------------------------------------------------------------------
 999  IF (L_HIST) CALL HFILL(2,FLOAT(NFADC-NEMPTY),0.,1.)
      RETURN
      END
