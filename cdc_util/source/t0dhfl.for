C----------------------------------------------------------------------
      SUBROUTINE T0DHFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book bank T0DH, which is the down link bank
C-                         from T0HT or previous T0DH bank. T0DH bank
C-                         contains compressed information for the track
C-                         in T0TD bank which is pointed by LT0TZ
C-
C-   Inputs  : LT0TZ - pointer to the T0TD track (in COMMON block)
C-   Outputs : bank created
C-
C-   Created  27-APR-1992   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:T0DLNT2.INC'
      INCLUDE      'D0$LINKS:IZT0DH.LINK'
      INTEGER       ISETVN
      INTEGER       LDTRK, LDTTH, GZT0HT, GZDTRK, NHIT, IC, IS, IHIT
      INTEGER       MPT0DH(5), LDHIT, NDHIT, GZDHIT, NWORD, J, IWORD
      LOGICAL       L_FIRST
C
      DATA          MPT0DH /4HT0DH,0,0,2,0/
      DATA          L_FIRST  /.TRUE./
C
      SAVE L_FIRST, LDHIT
C----------------------------------------------------------------------
      IF (L_FIRST) THEN
        CALL MZFORM('T0DH','2I /1B 1I 1F',MPT0DH(5))
        L_FIRST=.FALSE.
      END IF
C
      IF ( LT0TD .LE. 0 ) THEN
        CALL ERRMSG('T0D','T0DHFL','T0TD bank does not exist','S')
        RETURN
      END IF
C
      NHIT = IQ(LT0TD+3)
      MPT0DH(4) = NHIT*3 + 2     ! Length of the bank
C
      IF (LT0DH .EQ. 0) THEN     ! First track. Book from T0HT
        LDHIT = GZDHIT()
        IF (LDHIT .LE. 0) THEN
          CALL ERRMSG('T0D','T0DHFL','T0TD bank does not exist','S')
          RETURN
        END IF
        NDHIT = IQ(LDHIT+2)
        NWORD = IQ(LDHIT+3)
        LT0HT = GZT0HT()
        IF (LT0HT .LE. 0) CALL BKT0HT( LT0HT )
        CALL MZLIFT( IXMAIN, LT0DH, LT0HT, -IZT0DH, MPT0DH, 0 )
        IQ( LT0DH - 5 ) = 1
      ELSE                       ! Next track. Book from previous
        CALL MZLIFT( IXMAIN, LT0DH, LT0DH, 0, MPT0DH, 0 )
      END IF
C
      IQ(LT0HT+2) = IQ(LT0HT+2) + 1   ! Updates T0DH hits bank counter
      IQ(LT0DH)   = ISETVN(IQ(LT0DH),0)
      IQ(LT0DH+1) = NHIT              ! Number of hits per track
      IQ(LT0DH+2) = 3                 ! Number of words per hit
C
C ****  Book T0DH bank
C
      LDHIT       = GZDHIT()
      LDTRK = GZDTRK(IQ(LT0TD+1))
      LDTTH = LQ(LDTRK-1)
      DO IC=1,NHIT
        IHIT = IQ(LDTTH+1)
        J = 4
        DO WHILE ( J .LT. NWORD*NDHIT+4 )
          IWORD = IQ(LDHIT+J)
          IF ((IWORD.AND.'0003FFFF'X).EQ.IHIT) THEN
            IQ(LT0DH+3*IC)   = IWORD
            IQ(LT0DH+3*IC+1) = IQ(LDHIT+J+1)
            Q (LT0DH+3*IC+2) = Q(LDTTH+2)
            J = NWORD*NDHIT + 4
          END IF
          J = J + NWORD
        END DO
        IF (J .EQ. NWORD*NHIT+4) THEN
          CALL ERRMSG('T0D','T0DHFL','Missing hit in the DHIT bank','S')
        END IF
        LDTTH = LDTTH + 2
      END DO
C----------------------------------------------------------------------
      RETURN
      END
