      FUNCTION NOI_SHAPE(TIME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize NOISY package
C-       RETURNS A REMAINING FRACTION OF AMPLITUDE AT TIME T
C-       IN MICROSECONDS,  WHERE T=0.0 IS PEAK OF PULSE, AND
C-       T IS RUNNING IN RIGHT DIRECTION (+==FORWARD)
C
C-
C-   Inputs  :  TIME = time at which fraction is calculated
C-   Outputs :  NOI_SHAPE = fraction
C-   Controls:
C-       Relevant RCP parameters:
C-              T_SAMP = base to peak sampling time
C-              USE_EXPSIG = if T, use simple exponential model
C-                           of pulse SHAPE
C-                   PULSE SHAPE IS TRIANGULAR RISE TO 1.0 IN
C-                   TIME T_SAMP, THEN EXPONENTIAL
C-                   FALL WITH TIME CONSTANT T_DECAY.
C-                           if F, then:
C-                   SHAPE IS CALCULATED INSTEAD BY
C-                   QUADRATIC INTERPOLATION FROM SPICE
C-                   OUTPUT LOOKUP TABLES
C-
C-              T_DECAY = Decay time in simple exponential model
C-
C_              All times are in microseconds
C-
C-   Created   5-SEP-1991   Peter Nemethy and Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
C
C
      INTEGER II,IS
      REAL THL,THP,THH,SLP,SLH,SLL,DELH,DELL
      REAL NOI_SHAPE,NOI_FTQUAD
      REAL TIME
      REAL XPK,UPK,TPK,APK
      REAL THRAP
      DATA THRAP/0.05/
C
C########EXECUTION BEGINS HERE#######
C
      NOI_SHAPE=0.0
C.......DECIDE BETWEEN EXPONENTIAL AND ACTUAL PULSE SHAPE
      IF(USE_EXPSIG)THEN   !........EXPONENTIAL MODEL
        IF(TIME.GE.0.0)THEN
          NOI_SHAPE=EXP(-TIME/T_DECAY)
        ELSE
          IF(TIME.GT.-T_SAMP)NOI_SHAPE=1.0+TIME/T_SAMP
        ENDIF
        GOTO 999
      ELSE                 !.......ACTUAL SHAPE MODEL FROM SPICE
        IF(TIME.GT.TP(1).AND.TIME.LT.TP(50))THEN
          DO II=2,50
            IS=0
            IF(TP(II).GE.TIME)THEN
C...............DECIDE BETWEEN LINEAR AND QUAD FIT.
              IF(II.LT.3.OR.(ABS(AP(II)).LT.THRAP.AND.
     +                          ABS(AP(II-1)).LT.THRAP))THEN
                NOI_SHAPE=AP(II-1)+(AP(II)-AP(II-1))*(TIME-TP(II-1))/
     +                         (TP(II)-TP(II-1))
C#######              IF(II.LT.50)TYPE 569, II,TIME,NOI_SHAPE
C  569           FORMAT(' LIN: II,TIME,SHAPE',I3,6F9.4)
                GOTO 999
              ENDIF
C.....................QUAD FIT:
              IF(II.LT.50)THEN
C.......................PICK THE BEST INTERVAL FOR QUAD INTERPOLATION
                SLP=(AP(II)-AP(II-1))/(TP(II)-TP(II-1))
                SLH=(AP(II+1)-AP(II))/(TP(II+1)-TP(II))
                SLL=(AP(II-1)-AP(II-2))/(TP(II-1)-TP(II-2))
                THP=57.*ATAN(SLP)
                THH=57.*ATAN(SLH)
                THL=57.*ATAN(SLL)
                DELH=ABS(THH-THP)
                DELL=ABS(THL-THP)
                IF(DELH.GT.DELL)IS=1
C                 type *,'THL THP THH:',THL,THP,THH
C                 type *, 'dell delh',dell,delh
              ENDIF
              NOI_SHAPE=AP(II+IS-2)+NOI_FTQUAD(AP(II+IS-2),
     +              AP(II+IS-1),AP(II+IS),TP(II+IS-2),
     +              TP(II+IS-1),TP(II+IS),TIME,XPK,UPK)
              TPK=-999.9
              APK=0.0
              IF(XPK.NE.-999.9)THEN
                TPK=TP(II+IS-2)+XPK
                APK=AP(II+IS-2)+UPK
              ENDIF
C#####  IF(II.LT.50)
C#####     +    TYPE 570,II,IS,TIME,NOI_SHAPE,TPK,APK
C  570         FORMAT(' QD:II,IS,TIME,SHAPE',2I3,2F9.4,
C     +          ' PK T AND A:',2F9.4)
              GOTO 999
            ENDIF
          ENDDO
        ENDIF
        GOTO 999
      ENDIF
  999 RETURN
      END
