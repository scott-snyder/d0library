      SUBROUTINE FGET_INFO(HALF,UNIT,QDRT,SCTR,WIRE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call GETPAR for FDC hardware
C-
C-   Inputs  : HALF,UNIT,QDRT,SCTR,WIRE
C-   Outputs :
C-   Controls:
C-
C-   Created  20-FEB-1990   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER N
      INTEGER HALF,UNIT,QDRT,SCTR,WIRE
C
      LOGICAL FDEXST
C
C----------------------------------------------------------------------
C
      N = 0
C
    2 CALL GETPAR(1,' Enter Half (0 or 1) > ','I',HALF)
      IF (HALF.LT.0.OR.HALF.GT.1) THEN
        CALL INTMSG(' Illegal entry. Valid Range 0 to 1.  Reenter. ')
        GO TO 2
      ENDIF
   21 CALL GETPAR(1,' Enter Unit (0 for Theta, 1 for Phi) > ','I',
     &    UNIT)
      IF (UNIT.LT.0.OR.UNIT.GT.1) THEN
        CALL INTMSG(' Illegal entry. Valid range 0 to 1.  Reenter. ')
        GO TO 21
      END IF
      IF (UNIT.EQ.0) THEN
   22   CALL GETPAR(1,' Enter Quadrant (0-7) > ','I',QDRT)
        IF (QDRT.LT.0.OR.QDRT.GT.7) THEN
          CALL INTMSG(' Illegal entry. Valid range 0 to 7.  Reenter. ')
          GO TO 22
        END IF
      ELSE
        QDRT = 0
      END IF
   23 CALL GETPAR(1,' Enter Sector (0-5 or 0-35) > ','I',SCTR)
      IF (UNIT.EQ.0) THEN
        IF (SCTR.LT.0.OR.SCTR.GT.5) THEN
          CALL INTMSG(' Illegal entry. Valid range 0 to 5.  Reenter. ')
          GO TO 23
        END IF
      ELSE
        IF (SCTR.LT.0.OR.SCTR.GT.35) THEN
          CALL INTMSG(' Illegal entry. Valid range 0 to 35.  Reenter. ')
          GO TO 23
        END IF
      END IF
   24 CALL GETPAR(1,' Enter Wire (0-9 or 0-15) > ','I',WIRE)
      IF (UNIT.EQ.0) THEN
        IF (WIRE.LT.0.OR.WIRE.GT.9) THEN
          CALL INTMSG(' Illegal entry. Valid range 0 to 9.  Reenter. ')
          GO TO 24
        END IF
      ELSE
        IF (WIRE.LT.0.OR.WIRE.GT.15) THEN
          CALL INTMSG(' Illegal entry. Valid range 0 to 15.  Reenter. ')
          GO TO 24
        END IF
      END IF
C
      IF (.NOT.FDEXST(HALF,UNIT,QDRT,SCTR)) THEN
        N = N + 1
        IF (N.GT.3) THEN
          CALL INTMSG(' Three strikes.  You will have to start over.')
          HALF = -1
          GO TO 999
        ELSE
          CALL INTMSG(
     &      ' That combination is not being read out.  Try again.')
        END IF
        GO TO 2
      END IF
C
  999 RETURN
      END
