      LOGICAL FUNCTION PSRWFL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill PSRW bank, based upon T0RWFL
C-                         by Gregory Landsberg
C-
C-   Returned value  : TRUE - successful
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-OCT-1995   Hailin Li
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$LINKS:IZPSHT.LINK'
      INCLUDE 'D0$LINKS:IZPSRW.LINK'
      INCLUDE 'D0$PARAMS:PRESHOWER.PARAMS'
      CHARACTER*4 PATH
      INTEGER IADDR,DATA(LFADC),LCDD1
      INTEGER I,J,K,NPLS,LD,LP,LPSRW,LUPGD
      INTEGER LPSHT,GZPSHT,GZUPGD
C----------------------------------------------------------------------
      PSRWFL = .FALSE.
C
      LCDD1 = LQ(LHEAD-IZCDD1)
      IF ( LCDD1 .LE. 0 ) THEN
        CALL PATHGT(PATH)
        IF ( PATH .NE. 'RECO' ) CALL PATHST('RECO')
        LUPGD = GZUPGD()
        IF ( LUPGD .LE. 0 ) CALL BKUPGD(LUPGD)
        IF ( LUPGD .LE. 0 ) GOTO 20
C
        CALL PATHGT(PATH)
        IF ( PATH .NE. 'FILT' ) CALL PATHST('FILT')
        LPSHT = GZPSHT()
        IF ( LPSHT .LE. 0 ) GOTO 20
        LPSRW = LQ(LPSHT-IZPSRW)
        IF ( LPSRW .LE. 0 ) THEN
          CALL ERRMSG('NO CDD1 and PSRW under FILT','PSRWFL',' ','S')
        ELSE
          CALL MZCOPY(IXMAIN,LPSHT,IXMAIN,LUPGD,-IZPSHT,'L')
          PSRWFL = .TRUE.
        ENDIF
C
C***    Reset conditions and return
C
   20   CONTINUE
        CALL PATHST(PATH)
        RETURN
      ENDIF
C
      DO 100 IADDR =  PSADR,PSADR+NFADC-1
        CALL ZDEXPD(ITYPE, IADDR, DATA)
C
        I    = 1
        NPLS = 0
        LD   = 2
        DO WHILE (DATA(I) .NE. 0)
          NPLS = NPLS + 1
          LP   = DATA(I)
          IF ((LP/4)*4.NE.LP) THEN
            CALL ERRMSG('mod(length of pulse,4) != 0','PSRWFL',' ','S')
          ENDIF
          LD = LD + LP/4 + 1
          I  = I  + LP + 2
          IF (I .GT. LFADC) THEN
            CALL ERRMSG('Length of data > LFADC(512)','PSRWFL',' ','S')
            RETURN
          ENDIF
        END DO
C
        IF ( NPLS .EQ. 0 ) GOTO 100
C
        CALL BKPSRW(LD,LPSRW)
        IQ( LPSRW - 5 ) = IADDR - PSADR + 1 ! FADC channel
        IQ( LPSRW + 1 ) = IADDR
        IQ( LPSRW + 2 ) = NPLS
C
        I = 1
        J = 3
        DO WHILE ( DATA(I) .NE. 0 )
          LP = DATA(I)
          IQ( LPSRW + J ) = IOR(ISHFT(DATA(I+1),16),LP)
          DO K = 1,LP/4
            IQ( LPSRW + J + K ) = IOR(IOR(ISHFT(DATA(I+4*K+1),24),
     &                                    ISHFT(DATA(I+4*K  ),16)),
     &                                    IOR(ISHFT(DATA(I+4*K-1),8),
     &                                    DATA(I+4*K-2)))
          END DO
          J = J + LP/4 + 1
          I = I + LP   + 2
        END DO
C
  100 CONTINUE
C
      PSRWFL = .TRUE.
      RETURN
      END
