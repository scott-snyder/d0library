      SUBROUTINE GTPPHO(IPHO, E, ET, SIG, THETA, ETA, PHI, CONE_NRG,
     &  IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   Returns the PPHO information
C-
C-   Inputs  :
C-              IPHO = which photon candidate you want
C-   Outputs :
C-              E(4) = Px, Py, Pz, Etot
C-              ET
C-              SIG(4) = PXSIG**2, PYSIG**2, PTSIG, HMTE TRUNCATED CHISQ
C-              THETA = theta of photon
C-              ETA = eta of photon
C-              PHI = phi of photon
C-              CONE_NRG(5) = 1 -- EM energy outside central tower
C-                            2 -- total energy in core cone
C-                            3 -- total energy in isolation cone
C-                            4 -- EM energy in core cone
C-                            5 -- EM energy in isolation cone
C-              IER = 0 OK
C-                   -4 No PPHOs found
C-                   -5 Requested PPHO not found
C-                  -13 MDST bank not found and path is set to 'MDST'
C-                  -14 Energy correction not done
C-   Controls:
C-
C-
C-      Return total number of PPHO BANKS
C-
C-      ENTRY GTPPHO_TOTAL(NUM_PHO, IER)
C-          NUM_PHO   [I]  Number of PPHO banks
C-          IER       [I]  0 = ok
C-                        -13= no MDST bank and path is set to 'MDST'
C
C-   Created  20-JUN-1991   Andrew J. Milder
C-   Updated  19-DEC-1992   Andrew J. Milder  New MDST format, added
C-                          H-matrix chi-squared. Note this is not a
C-                          change in the argument list, as SIG was
C-                          always dimensioned as 4.
C-   Modified 18-SEP-1993   R. Astur "Make offical and mdst compatible",
C-   Updated   8-OCT-1993   Marc Paterno  Reset GET_UNCORRECTED to FALSE after
C-                                        every call
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPPHO.LINK'
      INCLUDE 'D0$LINKS:IZHMTP.LINK'
      CHARACTER*4 PATH
      INTEGER N_AUX_PPHO_WORDS
      PARAMETER( N_AUX_PPHO_WORDS = 8)
      REAL    E(4), SIG(4), THETA, ETA, PHI, CONE_NRG(5), ET
      INTEGER IPHO, NUM_PHO, IER, LPPHO, GZPPHO, NZBANK, JPHO, LMDST
      INTEGER GZMDST, NUM_PPHO, NREP, IOFF, LHMTP
C: Entry point
      INTEGER LVCOR, I
      LOGICAL GET_UNCORRECTED
      SAVE GET_UNCORRECTED
      DATA GET_UNCORRECTED /.FALSE./
C----------------------------------------------------------------------
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') THEN
        IER = 0
        LMDST = GZMDST()
        IF (LMDST .LE. 0) THEN
          IER = -13
          GOTO 999
        ENDIF
        NUM_PHO=IQ(LMDST+18)
        IF ( NUM_PHO .LE. 0 ) THEN
          IER = -4
          GOTO 999
        ENDIF
        IF( IPHO.GT.NUM_PHO) THEN
          IER = -5
          GOTO 999
        ENDIF
        NREP=IQ(LMDST+17)
        IOFF=IQ(LMDST+19)+(IPHO-1)*NREP-1
        LPPHO = LMDST + IOFF
        SIG(4) = Q(LPPHO+NREP-N_AUX_PPHO_WORDS)
      ELSE
        IER = 0
        JPHO = 1
        LPPHO = GZPPHO()
        IF (LPPHO.LE.0) THEN
          IER = -4
          GOTO 999
        ENDIF
        DO WHILE (LPPHO .GT. 0)
          IF (IPHO .EQ. JPHO) THEN
            GOTO 100
          ELSE
            LPPHO = LQ(LPPHO)
            JPHO = JPHO + 1
          ENDIF
        ENDDO
        IER = -5
        GOTO 999
  100   CONTINUE
        LHMTP = LQ(LPPHO-IZHMTP)
        IF (LHMTP.GT.0) THEN
          SIG(4) = Q(LHMTP+7)
        ELSE
          SIG(4) = -100.
        ENDIF
      ENDIF
C
      E(1) = Q(LPPHO+3)
      E(2) = Q(LPPHO+4)
      E(3) = Q(LPPHO+5)
      E(4) = Q(LPPHO+6)
      ET   = Q(LPPHO+7)
C
C: Return corrected energies or uncorrected
C
      IF ( PATH .NE. 'MDST' .AND.  (.NOT. GET_UNCORRECTED)
     &   .AND. LQ( LPPHO - 4 ) .EQ. 0 ) THEN
C        IER = -14           ! Correction was not done
      ELSEIF ( PATH .NE. 'MDST' .AND. GET_UNCORRECTED .AND. LQ( LPPHO -
     &  4 ) .GT. 0 ) THEN
        LVCOR = LQ( LPPHO - 4 )
        ET = (1. - Q(LVCOR+2+4)/E(4) )*ET
        DO I = 1, 4
          E(I) = E(I) - Q( LVCOR + 2 + I )
        ENDDO
      ENDIF
      SIG(1) = Q(LPPHO+11)
      SIG(2) = Q(LPPHO+12)
      SIG(3) = Q(LPPHO+13)
      THETA = Q(LPPHO+8)
      ETA = Q(LPPHO+9)
      PHI = Q(LPPHO+10)
      CONE_NRG(1) = Q(LPPHO+14)
      CONE_NRG(2) = Q(LPPHO+15)
      CONE_NRG(3) = Q(LPPHO+16)
      CONE_NRG(4) = Q(LPPHO+17)
      CONE_NRG(5) = Q(LPPHO+18)
  999 GET_UNCORRECTED = .FALSE.
      RETURN
C*************************************************
C  Entry point to get total number of PPHO banks
C*************************************************
      ENTRY GTPPHO_TOTAL(NUM_PPHO, IER)
C
      CALL PATHGT(PATH)
      IF ( PATH.EQ.'MDST' ) THEN
        LMDST=GZMDST()
        IF( LMDST.LE.0 ) THEN
          NUM_PPHO=0
          IER=-13
        ELSE
          NUM_PPHO=IQ(LMDST+18)
          IER=0
        ENDIF
      ELSE
        LPPHO = GZPPHO()
        IF ( LPPHO .GT. 0 ) THEN
          NUM_PPHO = NZBANK(IXCOM, LPPHO)
          IER = 0
        ELSE
          NUM_PPHO = 0
          IER = 0
        ENDIF
      ENDIF
C
      RETURN
C*************************************************
C Entry point to get Uncorrected energy of photon
C************************************************
      ENTRY GTPPHO_UNCORRECTED
      GET_UNCORRECTED = .TRUE.
      RETURN
      END
