C----------------------------------------------------------------------
      SUBROUTINE BKT0HT( LT0HT_0 )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the banks up to T0HT
C-
C-   Inputs  : None
C-   Outputs : Pointer to the new bank T0HT
C-
C-   Created  26-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:T0DLNT2.INC'
      INCLUDE      'D0$LINKS:IZT0HT.LINK'
      INTEGER       LHITS, LT0HT_0, NWDSEC, NWDCDA, ISETVN, LDSEC, I, J
      INTEGER       MPT0HT(5), ISEC, ILAY, NHIT, NHITC
      INTEGER       GZHITS, GZT0HT, GZHSTR, GZDSEC
      LOGICAL       L_FIRST
C
      DATA          L_FIRST  / .TRUE. /
      DATA          MPT0HT / 0, 4, 3, 8, 0 /
C
      SAVE          L_FIRST
C----------------------------------------------------------------------
      IF ( L_FIRST ) THEN
        L_FIRST = .FALSE.
        CALL UCTOH( 'T0HT', MPT0HT(1), 4, 4 )
        CALL MZFORM('T0HT', '2I 6B', MPT0HT(5) )
      ENDIF
C
C ****  Test for HEAD bank, abort if doesn't exist
C
      IF ( LHEAD .EQ. 0 ) THEN
        CALL ERRMSG('T0D','BKT0HT','HEAD bank does not exist','S')
        RETURN
      ENDIF
C
C ****  Book HITS bank, if needed
C
      LHITS = GZHITS()
      IF (LHITS.LE.0) CALL BKHITS(LHITS)
C
C ****  Book T0HT bank
C
      LT0HT = GZT0HT()
      IF ( LT0HT .EQ. 0 ) THEN
        CALL MZLIFT ( IXMAIN, LT0HT, LHITS, -IZT0HT, MPT0HT, 0 )
        LT0DH = 0
        LT0FH = 0
      ENDIF
      LQ(LT0HT - 4) = GZHSTR()        ! Reference Link to latest History
      IQ(LT0HT)     = ISETVN(IQ(LT0HT),0)
C
      DO I = 0,5
        IF (I .LT. 4) THEN
          ISEC = 30
          ILAY = I
        ELSE
          ISEC = 31
          ILAY = 2*(I-4)
        END IF
        LDSEC = GZDSEC(ISEC,ILAY)
        NHITC = 0
        IF (LDSEC .GT. 0) THEN
          LDSEC = LDSEC + 4
          DO J = 0,6
            NHIT = IQ(LDSEC+J)
            IF (NHIT .GT. 15) NHIT = 15
            NHITC = IOR(NHITC,ISHFT(NHIT,4*J))
          END DO
        END IF
        IQ(LT0HT+3+I) = NHITC
      END DO
C
      RETURN
      END
