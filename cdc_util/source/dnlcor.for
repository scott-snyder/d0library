      SUBROUTINE DNLCOR (WIRE,LAYER,DRFTP,DRFTM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Makes nonlinearity correction NLC to +/- phi
C-                         hits DRFTP, DRFTM.
C-                         NLC = A0+A2*(ABS(DRFT)-A1)**2  Inner wires
C-                         NLC = 6th order polynomial   Outer wires
C-   Inputs  : LAYER,WIRE
C-             DRFTP,DRFTM (drift distance +/- phi regions of drift cell)
C-   Outputs : DRFTP,DRFTM (corrected)
C-   Controls: 
C-
C-   Created   1-OCT-1992   Domenico Pizzuto
C-   Updated  31-DEC-1992   Qizhong Li-Demarteau  added EZERROR and EZRSET 
C-
C----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'

C- Minimum starting drift distance for inner wires in which to begin
C- nonlinearity corrections
      REAL MINCST
      PARAMETER (MINCST = 2.5)

      INTEGER LAYER,LAY,WIRE,WIR,SID,ERR,MAXLAY
      INTEGER LDNL,LDNLO,GZDNLI,GZDNLO,IPDNL,I
      INTEGER IER
      REAL NLC,A0,A1,A2,T1,YSTI (0:1,5,0:3)
      REAL DRFTP,DRFTM,DDP,DDM,Y
      LOGICAL FIRST
      LOGICAL EZERROR
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
       FIRST = .FALSE.
       CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DNLCOR',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
       CALL EZGET ('MAXLAY',MAXLAY,ERR)
       CALL EZRSET
C- Find starting drift distance for applying corrections
        DO 10 LAY = 0, MAXLAY
         LDNL  = GZDNLI (LAY)
         IPDNL = LDNL+2
          DO 15 WIR = 1, MXSENS-1
           DO 20 SID = 0, 1
            A0 = C (IPDNL+1)
            A1 = C (IPDNL+2)
            A2 = C (IPDNL+3)
            T1 = -A0/A2
             IF (T1.GT.0.) THEN
              T1 = A1-SQRT (T1)
              YSTI (SID,WIR,LAY) = MAX (MINCST,T1)
             ELSE
              YSTI (SID,WIR,LAY) = MAX (MINCST,A1)
             END IF
            IPDNL = IPDNL+IC (LDNL+2)
   20      CONTINUE
   15     CONTINUE
   10   CONTINUE
      END IF

      DDP  = ABS (DRFTP)
      DDM  = ABS (DRFTM)

       IF (WIRE.GT.0.AND.WIRE.LT.MXSENS) THEN        ! Inner wire
        LDNL  = GZDNLI (LAYER)
         IF (DDP.GE.YSTI (0,WIRE,LAYER)) THEN        ! + phi side
          NLC = 0.0
          IPDNL = LDNL+IC (LDNL+2)*IC (LDNL+1)*(WIRE-1)+2
          NLC   = C (IPDNL+1)+C (IPDNL+3)*(DDP-C (IPDNL+2))**2
          DRFTP = DRFTP-NLC
         END IF
         IF (DDM.GE.YSTI (1,WIRE,LAYER)) THEN        ! - phi side
          NLC = 0.0
          IPDNL = LDNL+IC (LDNL+2)*(IC (LDNL+1)*(WIRE-1)+1)+2
          NLC   = C (IPDNL+1)+C (IPDNL+3)*(DDM-C (IPDNL+2))**2
          DRFTM = DRFTM+NLC
         END IF
       ELSE                                          ! Outer wire
        WIR   = WIRE/MXSENS
        LDNL  = GZDNLO (LAYER)
        IPDNL = LDNL+IC (LDNL+2)*IC (LDNL+1)*WIR+2
        NLC   = C (IPDNL+1)
        Y     = DDP
         DO 30 I = 2, IC (LDNL+2)                    ! + phi side
          NLC = NLC+C (IPDNL+I)*Y
          Y   = Y*DDP
   30    CONTINUE
        DRFTP = DRFTP-NLC
        IPDNL = LDNL+IC (LDNL+2)*(IC (LDNL+1)*WIR+1)+2
        NLC   = C (IPDNL+1)
        Y     = DDM
         DO 35 I = 2, IC (LDNL+2)                     ! - phi side
          NLC = NLC+C (IPDNL+I)*Y
          Y   = Y*DDM
   35    CONTINUE
        DRFTM = DRFTM+NLC
       END IF

  999 RETURN
      END
