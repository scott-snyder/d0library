      SUBROUTINE GTPELC(IELC,E,ET,SIG,THETA,ETA,PHI,CONE_NRG,DIST,
     &  NUM_TRCKS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the PELC information
C-
C-   Inputs  :  IELC = which electron candidate you want
C-   Outputs :
C-              E(4) = Px, Py, Pz, Etot
C-              ET
C-              SIG(4) = PXSIG**2, PYSIG**2,PTSIG,H-matrix TRUNCATED chi-sq.
C-              THETA = theta of electron
C-              ETA = eta of electron
C-              PHI = phi of electron
C-              CONE_NRG(5) = 1 -- EM energy outside central tower
C-                            2 -- total energy in core cone
C-                            3 -- total energy in isolation cone
C-                            4 -- EM energy in core cone
C-                            5 -- EM energy in isolation cone
C-              DIST = distance of closest approach of central track
C-              NUM_TRCKS = number of central tracks in cluster road
C-              IER = 0 OK
C-                   -4 No PELCs found
C-                   -5 Requested PELC not found
C-                  -13 MDST bank not found and path is set to 'MDST'
C-                  -14 Energy correction not done
C-   Controls:
C-
C-
C-      Return total number of PELC BANKS
C-
C-      ENTRY GTPELC_TOTAL(NUM_ELC,IER)
C-          NUM_ELC   [I]  Number of PELC banks
C-          IER       [I]  0 = ok
C-                        -13= no MDST bank and path is set to 'MDST'
C
C-   Created  20-JUN-1991   Andrew J. Milder
C-   Updated  19-DEC-1992   Andrew J. Milder  New MDST format,added
C-                          H-matrix chi-squared. Note this is not a
C-                          change to the argument list as SIG was
C-                          always dimensioned to be SIG(4).
C-   Modified 18-SEP-1993   R. Astur "Make offical and mdst compatible",
C-                          + " change SIG(4) definition to TRUNCATED chisqare
C-   Updated   8-OCT-1993   Marc Paterno    Set GET_UNCORRECTED to FALSE after
C-                                          every call.
C-   Updated  15-OCT-1993   Marc Paterno   Minor clean-up.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
      INCLUDE 'D0$LINKS:IZHMTE.LINK'
      INTEGER N_AUX_PELC_WORDS          ! Extra PELC/PPHO words for version 3
      PARAMETER( N_AUX_PELC_WORDS = 21 )
      CHARACTER*4 PATH
      REAL    E(4),SIG(4),THETA,ETA,PHI,CONE_NRG(5),DIST,ET
      INTEGER NUM_TRCKS,LHMTE
      INTEGER NUM_ELC,IER,IELC,LPELC,GZPELC,NZBANK
      INTEGER JELC,LMDST,GZMDST,NUM_PELC,NREP,IOFF
C: Entry point
      INTEGER LVCOR, I
      LOGICAL GET_UNCORRECTED
      SAVE GET_UNCORRECTED
      DATA GET_UNCORRECTED /.FALSE./
C----------------------------------------------------------------------
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') THEN
C
C  MICRO DST PATH SET- FIND POINTER TO IELC'TH ELECTRON DATA
C
        IER = 0
        LMDST=GZMDST()
        IF( LMDST.LE.0 ) THEN
          IER = -13
          GOTO 999
        ENDIF
        NUM_PELC=IQ(LMDST+12)
        IF( NUM_PELC .LE. 0 ) THEN
          IER = -4
          GOTO 999
        ENDIF
        IF( IELC.GT.NUM_PELC) THEN
          IER = -5
          GOTO 999
        ENDIF
        NREP=IQ(LMDST+11)
        IOFF=IQ(LMDST+13)+(IELC-1)*NREP-1
        LPELC = IOFF + LMDST
        SIG(4) = Q(LPELC+NREP - N_AUX_PELC_WORDS)
      ELSE
        IER = 0
        JELC = 1
        LPELC = GZPELC()
        IF (LPELC.LE.0) THEN
          IER = -4
          GOTO 999
        ENDIF
        DO WHILE (LPELC .GT. 0)
          IF (IELC .EQ. JELC) THEN
            GOTO 100
          ELSE
            LPELC = LQ(LPELC)
            JELC = JELC + 1
          ENDIF
        ENDDO
        IER = -5
        GOTO 999
  100   CONTINUE
        LHMTE = LQ(LPELC-IZHMTE)
        IF (LHMTE.GT.0) THEN
          SIG(4) = Q(LHMTE+7)
        ELSE
          SIG(4) = -100.
        ENDIF
      ENDIF
C
      E(1) = Q(LPELC+3)
      E(2) = Q(LPELC+4)
      E(3) = Q(LPELC+5)
      E(4) = Q(LPELC+6)
      ET   = Q(LPELC+7)
C
C: Return corrected energies or uncorrected
C
      IF ( PATH .NE. 'MDST' .AND.  
     &     (.not. GET_unCORRECTED) .AND. 
     &     LQ( LPELC -4) .EQ.  0 ) THEN
C      IER = -14           ! Correction was not done
      ELSEIF ( PATH .NE. 'MDST' .AND. GET_UNCORRECTED .AND. LQ( LPELC -
     &  4 ) .GT. 0 ) THEN
        LVCOR = LQ( LPELC - 4 )
        ET = (1. - Q(LVCOR+2+4)/E(4) )*ET
        DO I = 1, 4
          E(I) = E(I) - Q( LVCOR + 2 + I )
        ENDDO
      ENDIF
      SIG(1) = Q(LPELC+11)
      SIG(2) = Q(LPELC+12)
      SIG(3) = Q(LPELC+13)
      THETA = Q(LPELC+8)
      ETA = Q(LPELC+9)
      PHI = Q(LPELC+10)
C
      CONE_NRG(1) = Q(LPELC+14)
      CONE_NRG(2) = Q(LPELC+15)
      CONE_NRG(3) = Q(LPELC+16)
      CONE_NRG(4) = Q(LPELC+17)
      CONE_NRG(5) = Q(LPELC+18)
C
      NUM_TRCKS = IFIX(Q(LPELC+21))
      DIST = Q(LPELC+22)
  999 GET_UNCORRECTED = .FALSE.
      RETURN
C*************************************************
C  Entry point to get total number of PELC banks
C*************************************************
      ENTRY GTPELC_TOTAL(NUM_ELC,IER)
C
      CALL PATHGT(PATH)
      IF ( PATH.EQ.'MDST' ) THEN
        LMDST = GZMDST()
        IF (LMDST .LE. 0) THEN
          NUM_ELC = 0
          IER = -13
        ELSE
          NUM_ELC=IQ(LMDST+12)
          IER=0
        ENDIF
      ELSE
        LPELC = GZPELC(0)
        IF ( LPELC .GT. 0 ) THEN
          NUM_ELC = NZBANK(IXCOM,LPELC)
          IER = 0
        ELSE
          NUM_ELC = 0
          IER = 0
        ENDIF
      ENDIF
      RETURN
C*************************************************
C Entry point to get Uncorrected energy of electron
C************************************************
      ENTRY GTPELC_UNCORRECTED
      GET_UNCORRECTED = .TRUE.
      RETURN
      END
