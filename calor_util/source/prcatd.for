      SUBROUTINE PRCATD ( PRUNIT, LCATD, NCATD, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print on unit PRUNIT the content of one or more
C-              banks 'CATD'.
C-
C-   Inputs  : PRUNIT [I] : Unit number for printout.
C-             LCATD  [I] : Pointer to the bank.
C-             NCATD  [I] : Dummy.
C-             CFL    [C*]: Dummy.
C-             IFL    [I] : Dummy.
C-   Outputs : on unit PRUNIT
C-   Controls: none
C-
C-   Created   9-DEC-1991 Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCATD.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C
      INTEGER PRUNIT, LCATD, NCATD, IFL
      INTEGER LCATD1, GZCATD, NEMTWR,NHDTWR,NMUTWR
      INTEGER I,J,IE,IP,IDLETA,NDATA,IEMEND,IHDEND,ICHK
      INTEGER IETA,LINE,HDPNT,EMLIM,IENER,NBEG,NEND
      INTEGER BVN,ISIGN
      INTEGER IPNTEM,IPNTHD,IPNTMU
      INTEGER PACTWR
      INTEGER JBYT,JBIT
      REAL    ENERGY,DELETA,EUNIT,DEUNIT
      REAL    ETMNEM,ETMNHD,EMNMUO
      CHARACTER*(*) CFL
C
      DATA EUNIT,DEUNIT /.1, .01/
C----------------------------------------------------------------------
      LCATD1 = LCATD
      IF( LCATD .LE. 0 ) THEN
        LCATD1 = GZCATD()
        NDATA  = IQ(LCATD1-1)
        BVN    = IQ(LCATD1+1)
      ELSE
        GOTO 990
      ENDIF
C
C  ***  Print the content of the bank pointed by LCATD1
C
      CALL GTCATD (LCATD1,IPNTEM,IPNTHD,IPNTMU,NEMTWR,NHDTWR,
     &                         NMUTWR,ETMNEM,ETMNHD,EMNMUO)

      WRITE( PRUNIT, 1000 ) BVN,ETMNEM,ETMNHD,NEMTWR,NHDTWR,NMUTWR
 1000 FORMAT(1H0,'************************************************'/
     &  ' * CATD : PACKED CALORIMETER TOWER BANK FOR DST *'/
     &  ' ************************************************'/
     &  ' BANK VSN =',I6,' : CUT Et(GeV) for EM Tower =',F4.1,
     &  ' and HAD Tower =',F4.1/
     &  1X,I6,' EM TOWERS :',I6,' HAD TOWERS :',I6,
     &  ' TOTAL TOWERS ASSO. with PMUO')
      WRITE( PRUNIT, 1010 )
 1010 FORMAT(1X,80('='))
      WRITE( PRUNIT, 1015 )
 1015 FORMAT(11X,'EM TOWERS',17X,':',14X,'HAD TOWERS')
      WRITE( PRUNIT, 1016 )
 1016 FORMAT(1X,80('-'))
      WRITE( PRUNIT, 1020 )
 1020 FORMAT(3X,' IETA ',' IPHI ','IDLETA',2X,'ENERGY(GeV) ',
     &       5X,' IETA ',' IPHI ','IDLETA',2X,'ENERGY(GeV) ',
     &       3X,'LINE#')
C-
C--- PRINT PACKED TOWERS DATA
C-
      LINE = 0
      NBEG = IPNTEM  + 1
      NEND = IPNTMU - 1
      DO 100 I = NBEG,NEND
C--- HAD
        LINE  = LINE + 1
        HDPNT = IPNTHD + LINE
        IF (HDPNT .LE. NEND) THEN
          PACTWR = IQ(LCATD1+HDPNT)
          IE  = JBYT(PACTWR, 1, 7)
          IP  = JBYT(PACTWR, 8, 7)
          ISIGN  = JBIT(PACTWR,15)
          IDLETA = JBYT(PACTWR,16, 4)
          IF (ISIGN .EQ. 1)   IDLETA = -1*IDLETA
          DELETA = IDLETA * DEUNIT
          IF (IE .LE. NETAL) THEN
            IETA = IE - NETAL - 1
          ELSE
            IETA = IE - NETAL
          ENDIF
          IENER  = JBYT(PACTWR,20,13)
          ENERGY = IENER * EUNIT
          WRITE(PRUNIT,1100) IETA,IP,DELETA,ENERGY,LINE
          IHDEND = 0
        ELSE
          WRITE(PRUNIT,1110) LINE
          IHDEND = 1
        ENDIF
C--- EM
        EMLIM = IPNTHD - 1
        IF (I .LE. EMLIM) THEN
          PACTWR = IQ(LCATD1+I)
          IE  = JBYT(PACTWR, 1, 7)
          IP  = JBYT(PACTWR, 8, 7)
          ISIGN  = JBIT(PACTWR,15)
          IDLETA = JBYT(PACTWR,16, 4)
          IF (ISIGN .EQ. 1)   IDLETA = -1*IDLETA
          DELETA = IDLETA * DEUNIT
          IF (IE .LE. NETAL) THEN
            IETA = IE - NETAL - 1
          ELSE
            IETA = IE - NETAL
          ENDIF
          IENER  = JBYT(PACTWR,20,13)
          ENERGY = IENER * EUNIT
          WRITE(PRUNIT,1200) IETA,IP,DELETA,ENERGY
          IEMEND = 0
        ELSE
          IEMEND = 1
        ENDIF
        ICHK = IEMEND*IHDEND
        IF (ICHK .EQ. 1) THEN
          WRITE(PRUNIT,1300)
          GO TO 200
        ENDIF
  100 CONTINUE
C-
C--- PRINT PACKED TOWERS DATA ASSOCIATED WITH PMUO
C-
  200 NBEG = IPNTMU + 1
      IF (NBEG .GT. NDATA) GO TO 999
      LINE = 0
      WRITE( PRUNIT, 1250 ) EMNMUO
      WRITE( PRUNIT, 1010 )
      WRITE( PRUNIT, 1260 )
      WRITE( PRUNIT, 1016 )
      WRITE( PRUNIT, 1020 )
      DO 300 I = NBEG,NDATA
C--- TOTAL ONLY
        LINE  = LINE + 1
        IF (I .LE. NDATA) THEN
          PACTWR = IQ(LCATD1+I)
          IE  = JBYT(PACTWR, 1, 7)
          IP  = JBYT(PACTWR, 8, 7)
          ISIGN  = JBIT(PACTWR,15)
          IDLETA = JBYT(PACTWR,16, 4)
          IF (ISIGN .EQ. 1)   IDLETA = -1*IDLETA
          DELETA = IDLETA * DEUNIT
          IF (IE .LE. NETAL) THEN
            IETA = IE - NETAL - 1
          ELSE
            IETA = IE - NETAL
          ENDIF
          IENER  = JBYT(PACTWR,20,13)
          ENERGY = IENER * EUNIT
          WRITE(PRUNIT,1100) IETA,IP,DELETA,ENERGY,LINE
        ENDIF
  300 CONTINUE
C-
  999 RETURN
 1100 FORMAT(39X,2I6,2X,F4.2,2X,F6.1,9X,I4)
 1110 FORMAT(74X,I4)
 1200 FORMAT(1H+,1X,2I6,2X,F4.2,2X,F6.1)
 1250 FORMAT(' FOLLOWING TOWERS ARE ASSOCIATED WITH PMUO :',
     &       ' CUT E(GeV) for Total Tower =',F4.1)
 1260 FORMAT(37X,':',14X,'TOTAL TOWERS')
 1300 FORMAT(1H+,80('='))
C
C  *** Error : Linear without bank pointer
C
  990 WRITE( PRUNIT, 2000 )
 2000 FORMAT(/' ** PRCATD ** could not find the bank CATD !!! ')
      GOTO 999
C-
      END
