C----------------------------------------------------------------------
      SUBROUTINE ZDRWFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book bank ZDRW, which is the down link bank
C-                         from ZDHT or previous ZDRW bank. ZDRW bank
C-                         contains compressed information about raw
C-                         FADC pulses
C-
C-   Inputs  : None
C-   Outputs : Bank created
C-
C-   Created  2-MAY-1992   Gregory L. Landsberg
C-   Updated   9-OCT-1995   Freddie Landry  changed name from T0RWFL to ZDRWL
C-                                          replace T0 with ZD or T0D with ZCD
C-                                          everywhere
C-                                          collecting from channels 1-10 from
C-                                          both FADC's rather then channels
C-                                          used by T0D
C-   Updated  16-NOV-1995   Norman A. Graf  Added check on maximum pulse
C-                                          length after noticing problems 
C-                                          with bad FADCs.
C-   Updated  17-NOV-1995   Norman A. Graf  Added checks for CDD1 bank.
C-                                          Copy ZDHT struture from FILT
C-                                          to RECO if CDD1 absent and 
C-                                          hitfinding done in L2.
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
C
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:ZCDLN2.INC'
      INCLUDE      'D0$LINKS:IZZDRW.LINK'
      INCLUDE      'D0$LINKS:IZCDD1.LINK'
      INCLUDE      'D0$PARAMS:ZCD.PARAMS'
C
      INTEGER       ISETVN, MAXDATA
      INTEGER       LCDD1, GZZDHT,  GZHITS
      INTEGER       MPZDRW(5)
      INTEGER       IADDR, I, J, K, LP, LD, NPLS, MAX_PULSE_LENGTH,IERR
      PARAMETER   ( MAXDATA = 512 )
      INTEGER       DATA(MAXDATA)
      LOGICAL       L_FIRST
      CHARACTER*4 PATH
C
      DATA          MPZDRW  /4HZDRW,0,0,2,0/
      DATA          L_FIRST /.TRUE./
C
      SAVE          L_FIRST
C
C----------------------------------------------------------------------
      IF (L_FIRST) THEN
        CALL MZFORM('ZDRW','2I -B',MPZDRW(5))
        L_FIRST=.FALSE.
        CALL EZPICK('ZCD_RCP')
        CALL EZGET('MAX_PULSE_LENGTH',MAX_PULSE_LENGTH,IERR)
        CALL EZRSET
      END IF
C
C ****  Check for CDD1 bank
C
      LCDD1 = LQ(LHEAD-IZCDD1)
      IF ( LCDD1 .LE. 0 ) THEN
        LHITS = GZHITS()
C
C ****  If no CDD1, check under FILT for ZDHT banks created in Level 2
C
        CALL PATHGT(PATH)
        IF ( PATH .NE. 'FILT' ) CALL PATHST('FILT')
        LZDHT = GZZDHT()
        IF ( LZDHT .GT. 0 ) THEN
C
C ****  If banks created in level 2, copy over to RECO path
C
          CALL MZCOPY(IXMAIN,LZDHT,IXMAIN,LHITS,-8,'L')
        ENDIF
C
C ****  Reset conditions and return
C
        CALL PATHST(PATH)
        RETURN
      ENDIF
C
C** looping from channel 1 of our first FADC to channel 10 of our second

      DO 1 IADDR =  ZCDADR+1,ZCDADR+26
C** Don't fill any banks for channels 11-15 of our first FADC and channel 0 of
C** our second FADC.  They're not connected to any ZCD modules.
        IF ((IADDR .GE. ZCDADR+11) .AND. (IADDR .LE. ZCDADR+16)) GO TO 1

C** Collect FADC raw data from CDD1 bank
        CALL ZDEXPD(1, IADDR, DATA)

        I    = 1
        NPLS = 0
        LD   = 2
        DO WHILE (DATA(I) .NE. 0)
          LP   = DATA(I)
          IF(LP.LT.MAX_PULSE_LENGTH) NPLS = NPLS + 1
          IF ((LP/4)*4.NE.LP)
     &      CALL ERRMSG('ZCD','ZDRWFL','Length of pulse <> 4*k','S')
          LD = LD + LP/4 + 1
          I  = I  + LP + 2
          IF (I .GT. MAXDATA)
     &      CALL ERRMSG('ZCD','ZDRWFL','Length of data > MaxData','S')
        END DO
        MPZDRW(4) = LD             ! Length of the bank
C
        IF (LZDRW .EQ. 0) THEN     ! First FADC. Book from ZDHT
          LZDHT = GZZDHT()
          IF (LZDHT .LE. 0) CALL BKZDHT( LZDHT )
          CALL MZLIFT( IXMAIN, LZDRW, LZDHT, -IZZDRW, MPZDRW, 0 )
          IQ( LZDRW - 5 ) = 1
        ELSE                       ! Next bank. Book from previous
          CALL MZLIFT( IXMAIN, LZDRW, LZDRW, 0, MPZDRW, 0 )
        END IF
        IQ( LZDRW + 1 ) = IADDR
        IQ( LZDRW + 2 ) = NPLS
        IQ( LZDRW )     = ISETVN(IQ(LZDRW),0)
        I = 1
        J = 3
        DO WHILE (DATA(I).NE.0)
          LP = DATA(I)
          IF(LP.LT.MAX_PULSE_LENGTH) THEN
            IQ( LZDRW + J ) = IOR(ISHFT(DATA(I+1),16),LP)
            DO K = 1,LP/4
              IQ( LZDRW + J + K ) = IOR(IOR(ISHFT(DATA(I+4*K+1),24),
     &                                    ISHFT(DATA(I+4*K  ),16)),
     &                                    IOR(ISHFT(DATA(I+4*K-1),8),
     &                                    DATA(I+4*K-2)))
            END DO
          ENDIF
          J = J + LP/4 + 1
          I = I + LP   + 2
        END DO
    1 CONTINUE
C----------------------------------------------------------------------
      RETURN
      END
