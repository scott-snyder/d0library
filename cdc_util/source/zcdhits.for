C----------------------------------------------------------------------
      SUBROUTINE ZCDHITS(HIT_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines if there are hits in the fiber
C-                         detector
C-
C-   Inputs       : None
C-   Outputs      : HIT_FLAG
C-                      .True.  - if there are FADC pulses
C-                      .False. - otherwise
C-   Controls     : None
C-
C-   Created  14-MAY-1992   Gregory L. Landsberg
C-   Updated  16-OCT-1995   Freddie Landry  changed from logical function T0HITS
C-                                          to subroutine ZCDHITS
C-                                          added HIT_FLAG
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:ZCDREC.INC'
      INCLUDE      'D0$PARAMS:ZCD.PARAMS'
      INTEGER       NEMPTY, LEMPTY(NFADC), I
      INTEGER       LZDRW, GZZDRW
      LOGICAL HIT_FLAG
C----------------------------------------------------------------------
      NEMPTY =  NFADC
      HIT_FLAG = .FALSE.
      LZDRW  =  GZZDRW(0)
      IF (LZDRW .LE. 0) GO TO 999
      NEMPTY =  0
      HIT_FLAG = .TRUE.
C
    1 CONTINUE
      IF ( LZDRW .LE. 0 ) THEN
C
C ****  Second pass: dropping of the empty banks listed in Lempty
C
        IF ( (NEMPTY .GT. 0) .AND. (NEMPTY .LT. NFADC) ) THEN
          DO I = 1,NEMPTY
            CALL MZDROP(IXCOM,LEMPTY(I),' ')
          END DO
          CALL ZPRESS(IXCOM,GZZDRW(0))
        ELSE IF ( NEMPTY .EQ. NFADC ) THEN
          CALL MZDROP(IXCOM,GZZDRW(0),'L')
          HIT_FLAG = .FALSE.
        END IF
        GOTO 999
      ELSE IF ( IQ(LZDRW+2) .EQ. 0 ) THEN
C
C ****  First pass: filling of Lempty with the addresses of empty banks
C
        NEMPTY = NEMPTY + 1
        LEMPTY(NEMPTY) = LZDRW
      END IF
      LZDRW = LQ(LZDRW)
      GO TO 1
C----------------------------------------------------------------------
  999 IF (L_HIST) CALL HFILL(2,FLOAT(NFADC-NEMPTY),0.,1.)
      RETURN
      END
