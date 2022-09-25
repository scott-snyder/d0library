      SUBROUTINE GTPNUT(NUM, ENUT, ET, TH, ETA, PHI, SIG, IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      get contents of PNUT banks
C-
C-   Inputs  :
C-      NUM = particle number
C-   Outputs :
C-      ENUT(4) =  energy 4-vector
C-      ET     = transverse energy
C-      TH     = theta
C-      ETA    = eta (pseudo-rapidity)
C-      PHI    = phi
C-      SIG(3) = (sigEx)**2, (sigEy)**2, sigEt
C-      IER    = Error flag- 0: OK, <0: not OK
C-
C-   ENTRY GTPNUT_SCALAR_ET : get scalar Et
C-
C-   Created   6-FEB-1989   Serban D. Protopopescu
C-   Updated  19-JUN-1991   Andrew J. Milder
C-                        Modified for MicroDST format -- check PATH
C-   Updated  19-DEC-1992   Andrew J. Milder  New MDST format
C-   Modified 18-SEP-1993   R. Astur "Make offical and mdst compatible",
C-   Updated  26-OCT-1993   Marc Paterno  Add entry GTPNUT_SCALAR_ET
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*4 PATH
      REAL    ENUT(4), ET, TH, ETA, PHI, SIG(3), SCALAR_ET
      INTEGER NUM, GZPNUT, LPNUT, I, NUM_PNUT, IER, NZBANK, LMDST
      INTEGER GZMDST, NREP, IOFF, NUM_NUT
C----------------------------------------------------------------------
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') THEN
        IER = 0
        LMDST = GZMDST()
        IF (LMDST .LE. 0) THEN
          IER = -4
          GOTO 999
        ENDIF
        NUM_PNUT = IQ(LMDST+9)
        IF ( NUM.GT.NUM_PNUT) THEN
          IER = -5
          GOTO 999
        ENDIF
        NREP = IQ(LMDST+8)
        IOFF = IQ(LMDST+10)+(NUM-1)*NREP-1
        LPNUT = LMDST + IOFF
      ELSE
        IER = 0
        LPNUT = GZPNUT(NUM)
      ENDIF
      IF (LPNUT.GT.0) THEN
        DO 1 I = 1, 4
          ENUT(I) = Q(LPNUT+2+I)
    1   CONTINUE
        ET = Q(LPNUT+7)
        TH = Q(LPNUT+8)
        ETA = Q(LPNUT+9)
        PHI = Q(LPNUT+10)
        SIG(1) = Q(LPNUT+11)
        SIG(2) = Q(LPNUT+12)
        SIG(3) = Q(LPNUT+13)
      ELSE
        IER = -4
        DO 2 I = 1, 4
          ENUT(I) = 0.
    2   CONTINUE
        ET = 0
        TH = 0
        ETA = 0
        PHI = 0
        SIG(1) = 0
        SIG(2) = 0
        SIG(3) = 0
      ENDIF
  999 RETURN
C
C*************************************************
C  Entry point to get total number of PNUT banks
C*************************************************
      ENTRY GTPNUT_TOTAL(NUM_NUT, IER)
C
      CALL PATHGT(PATH)
      IF ( PATH.EQ.'MDST' ) THEN
        LMDST = GZMDST()
        IF ( LMDST.LE.0 ) THEN
          IER = -4
          NUM_NUT = 0
        ELSE
          NUM_NUT = IQ(LMDST+9)
          IER = 0
        ENDIF
      ELSE
        LPNUT = GZPNUT(0)
        IF ( LPNUT .GT. 0 ) THEN
          NUM_NUT = NZBANK(IXCOM, LPNUT)
          IER = 0
        ELSE
          NUM_NUT = 0
          IER  = -4
        ENDIF
      ENDIF
      RETURN
C#######################################################################
      ENTRY GTPNUT_SCALAR_ET (NUM, SCALAR_ET, IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the scalar Et from the requested PNUT bank.
C-
C-   Inputs  : NUM       [I] the index of the PNUT bank wanted
C-   Outputs : SCALAR_ET [R] the value of the scalar Et
C-             IER       [I] error status: 0 = success
C-                                        -1 = does not work on microDSTs
C-                                        -2 = no PNUT(num) found
C-
C-   Controls: none
C-
C-   Created  26-OCT-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      CALL PATHGT(PATH)
      IF (PATH .EQ. 'MDST') THEN
        IER = -1
        SCALAR_ET = 0.0
        RETURN
      ENDIF

      LPNUT = GZPNUT(NUM)
      IF (LPNUT .LE. 0) THEN
        IER = -2
        SCALAR_ET = 0.0
        RETURN
      ENDIF

      SCALAR_ET = Q(LPNUT+14)
      IER = 0
      RETURN
      END
