C----------------------------------------------------------------------
      SUBROUTINE T0RWFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book bank T0RW, which is the down link bank
C-                         from T0HT or previous T0RW bank. T0RW bank
C-                         contains compressed information about raw
C-                         FADC pulses
C-
C-   Inputs  : None
C-   Outputs : Bank created
C-
C-   Created  2-MAY-1992   Gregory L. Landsberg
C-   Updated  12-MAY-1993   Qizhong Li-Demarteau  fixed the zebra format 
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
C
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:T0DLNT2.INC'
      INCLUDE      'D0$LINKS:IZT0RW.LINK'
      INCLUDE      'D0$PARAMS:T0D.PARAMS'
C
      INTEGER       ISETVN, MAXDATA
      INTEGER       GZT0HT, IC, IS, IHIT
      INTEGER       MPT0RW(5)
      INTEGER       IADDR, I, J, K, LP, LD, NPLS
      PARAMETER   ( MAXDATA = 512 )
      INTEGER       DATA(MAXDATA)
      LOGICAL       L_FIRST
C
C      DATA          MPT0RW  /4HT0RW,0,0,2,0/
      DATA          MPT0RW  /4HT0RW,0,0,2,1/
      DATA          L_FIRST /.TRUE./
C
      SAVE          L_FIRST
C----------------------------------------------------------------------
C      IF (L_FIRST) THEN
C        CALL MZFORM('T0RW','2I /1B 1I 1F',MPT0RW(5))
C        L_FIRST=.FALSE.
C      END IF
C
      DO 1 IADDR =  T0DADR,T0DADR+31
        IF ((IADDR .GE. T0DADR+16) .AND. (IADDR .LE. T0DADR+27)) GO TO 1
        CALL ZDEXPD(1, IADDR, DATA)
        I    = 1
        NPLS = 0
        LD   = 2
        DO WHILE (DATA(I) .NE. 0)
          NPLS = NPLS + 1
          LP   = DATA(I)
          IF ((LP/4)*4.NE.LP)
     &      CALL ERRMSG('T0D','T0RWFL','Length of pulse <> 4*k','S')
          LD = LD + LP/4 + 1
          I  = I  + LP + 2
          IF (I .GT. MAXDATA)
     &      CALL ERRMSG('T0D','T0RWFL','Length of data > MaxData','S')
        END DO
        MPT0RW(4) = LD             ! Length of the bank
C
        IF (LT0RW .EQ. 0) THEN     ! First FADC. Book from T0HT
          LT0HT = GZT0HT()
          IF (LT0HT .LE. 0) CALL BKT0HT( LT0HT )
          CALL MZLIFT( IXMAIN, LT0RW, LT0HT, -IZT0RW, MPT0RW, 0 )
          IQ( LT0RW - 5 ) = 1
        ELSE                       ! Next track. Book from previous
          CALL MZLIFT( IXMAIN, LT0RW, LT0RW, 0, MPT0RW, 0 )
        END IF
        IQ( LT0RW + 1 ) = IADDR
        IQ( LT0RW + 2 ) = NPLS
        IQ( LT0RW )     = ISETVN(IQ(LT0RW),0)
        I = 1
        J = 3
        DO WHILE (DATA(I).NE.0)
          LP = DATA(I)
          IQ( LT0RW + J ) = IOR(ISHFT(DATA(I+1),16),LP)
          DO K = 1,LP/4
            IQ( LT0RW + J + K ) = IOR(IOR(ISHFT(DATA(I+4*K+1),24),
     &                                    ISHFT(DATA(I+4*K  ),16)),
     &                                    IOR(ISHFT(DATA(I+4*K-1),8),
     &                                    DATA(I+4*K-2)))
          END DO
          J = J + LP/4 + 1
          I = I + LP   + 2
        END DO
    1 CONTINUE
C----------------------------------------------------------------------
      RETURN
      END
