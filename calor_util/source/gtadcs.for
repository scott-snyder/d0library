      SUBROUTINE GTADCS(TASK,NUM,VALUE)
C---------------------------------------------------------------------
C-                                                                   -
C-     Routine to extract average values and sigmas from ZEBWRK
C-     for a particular channel NUM-
C-
C-     INPUT:
C-
C-     TASK = 1 (pedestals); >1 (gains)
C-     NUM  - channel IDentifier
C-
C-     OUTPUT:
C-                                                                   -
C-     NPNTS - number of data points
C-     VALUE(1:4) = PULSER amplitude, PULSER sigma,AVR PH, SIGMA
C-
C-     AZ Feb.22, 1987, RR DEC 87
C-                                                                   -
C---------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBWRK.INC'
      INCLUDE 'D0$INC:STADDR.INC'
      INCLUDE 'D0$LINKS:IZSTEP.LINK'
C
      INTEGER TASK,NUM,ID
      INTEGER LST,I,LADCS,IBAD
      REAL VALUE(*),XNRM
C
C--   Initialize
C
      CALL UZERO(VALUE,1,4)
C
C--   Zebra STEP and ADCS banks
C
      CALL NUM_TO_ADDR(NUM,ID,1)
      ID=ISHFT(ID,-1)
C
      VALUE(3)=ACMOM1(ID)    !Mean of distrib
C
      IF(TASK.EQ.1)THEN
        VALUE(4)=ACMOM2(ID)  !Sigma of distrib for peds
      ELSE
        XNRM=ACMOM0(ID)
        IF(XNRM.GT.0.0)THEN
          VALUE(4)=ACMOM2(ID)/SQRT(XNRM)      !Error in mean FOR GAINS
        ELSE
          VALUE(4)=0.
        ENDIF
      ENDIF
C
   11 IF(TASK.EQ.1) GOTO 99
C
C--   Only if GAINS mode here
C
      LST=LW(LWRKH-IZSTEP)
    1 IF(LST.LE.0) GO TO 99
      IF(STEPN(ID).NE.IW(LST+2)) GO TO 12
C correct Step found
      VALUE(1)=FLOAT(IW(LST+17))   !Pulser Amplitude in funny units
      VALUE(2)=FLOAT(IW(LST+18))   !Pulser Sigma
      GO TO 99
   12 LST=LW(LST)
      IF(LST.GT.0) GOTO 1
C
   99 RETURN
      END
