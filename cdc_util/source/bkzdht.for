C----------------------------------------------------------------------
      SUBROUTINE BKZDHT( LZDHT_0 )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the banks up to ZDHT
C-
C-   Inputs  : None
C-   Outputs : Pointer to the new bank ZDHT
C-
C-   Created  26-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:ZCDLN2.INC'
      INCLUDE      'D0$LINKS:IZZDHT.LINK'
      INTEGER       LZDHT_0, NWDSEC, NWDCDA, ISETVN, LDSEC, I, J
      INTEGER       MPZDHT(5), ISec, ILay, NHit, NHitC
      INTEGER       GZHITS, GZZDHT, GZHSTR, GZDSEC
      LOGICAL       L_First
C
      DATA          L_First  / .True. /
      DATA          MPZDHT / 0, 4, 3, 8, 0 /
C
      SAVE          L_First
C----------------------------------------------------------------------
      IF ( L_First ) THEN
        L_First = .False.
        CALL UCTOH( 'ZDHT', MPZDHT(1), 4, 4 )
        CALL MZFORM('ZDHT', '2I 6B', MPZDHT(5) )
      ENDIF
C
C ****  Test for HEAD bank, abort if doesn't exist
C
      IF ( LHEAD .EQ. 0 ) THEN
        CALL ERRMSG('ZCD','BKZDHT','HEAD bank does not exist','S')
        RETURN
      ENDIF
C
C ****  Book HITS bank, if needed
C
      LHITS = GZHITS()
      IF (LHITS.LE.0) CALL BKHITS(LHITS)
C
C ****  Book ZDHT bank
C
      LZDHT = GZZDHT()
      IF ( LZDHT .EQ. 0 ) THEN
        CALL MZLIFT ( IXMAIN, LZDHT, LHITS, -IZZDHT, MPZDHT, 0 )
        LZDDH = 0
        LZDFH = 0
      ENDIF
      LQ(LZDHT - 4) = GZHSTR()        ! Reference Link to latest History
      IQ(LZDHT)     = ISETVN(IQ(LZDHT),0)
C
      DO I = 0,5
        IF (I .LT. 4) THEN
          ISec = 30
          ILay = I
        ELSE
          ISec = 31
          ILay = 2*(I-4)
        END IF
        LDSEC = GZDSEC(ISec,ILay)
        NHitC = 0
        IF (LDSEC .GT. 0) THEN
          LDSEC = LDSEC + 4
          DO J = 0,6
            NHit = IQ(LDSEC+J)
            IF (NHit .GT. 15) NHit = 15
            NHitC = IOR(NHitC,ISHFT(NHit,4*J))
          END DO
        END IF
        IQ(LZDHT+3+I) = NHitC
      END DO
C
      RETURN
      END
