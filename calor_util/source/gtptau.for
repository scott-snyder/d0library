      SUBROUTINE GTPTAU(ITAU,E,ET,THETA,ETA,PHI,RMS_WIDTH,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Get the information on the ITAUth tau particle
C-                          found from the PHYS bank.
C-
C-   Inputs  : ITAU = number of tau selected
C-   Outputs :
C-             E(I) = Ith component of the Tau's 4-vector ( E(1)=Px,...,E(4)
C-                      =energy )
C-             ET   = TRANSVERSE ENERGY
C-             THETA
C-             ETA =
C-             PHI =
C-             RMS_WIDTH = width of the tau jets calorimeter hits
C-             IER = 0 OK
C-                   -4 No PTAUs found
C-                   -5 Requested PTAU not found
C-                  -13 MDST bank not found and path is set to 'MDST'
C-
C-  Entry GTPTAU_TOTAL to get total number of tau banks
C-      ENTRY GTPTAU_TOTAL(NUM_TAU,IER)
C-          NUM_TAU   [I]  Number of PTAU banks
C-          IER       [I]  0 = ok
C-                        -13= no MDST bank and path is set to 'MDST'
C
C-
C-   Controls:
C-
C-   Created  20-JUN-1991   Andrew J. Milder
C-   Updated  19-DEC-1992   Andrew J. Milder   New MDST format
C-   Modified 18-SEP-1993   R. Astur "Make offical and mdst compatible",
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPTAU.LINK'
      CHARACTER*4 PATH
      REAL    E(4),SIG(4),THETA,ETA,PHI,RMS_WIDTH,ET
      INTEGER ITAU,NUM_TAU,IER,LPTAU,GZPTAU,NZBANK
      INTEGER JTAU,LMDST,GZMDST,NUM_PTAU,IOFF,NREP
C----------------------------------------------------------------------
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') THEN
        IER = 0
        LMDST = GZMDST()
        IF (LMDST .LE. 0) THEN
          IER = -13
          GOTO 999
        ENDIF
        NUM_PTAU = IQ(LMDST+15)
        IF ( NUM_PTAU .LE. 0 ) THEN
          IER = -4
          GOTO 999
        ENDIF
        IF( ITAU.GT.NUM_PTAU) THEN
          IER = -5
          GOTO 999
        ENDIF
        NREP=IQ(LMDST+14)
        IOFF=IQ(LMDST+16)+(ITAU-1)*NREP-1
        LPTAU = LMDST + IOFF
      ELSE
        IER = 0
        JTAU = 1
        LPTAU = GZPTAU()
        IF (LPTAU.LE.0) THEN
          IER = -4
          GOTO 999
        ENDIF
        DO WHILE (LPTAU .GT. 0)
          IF (ITAU .EQ. JTAU) THEN
            GOTO 100
          ELSE
            LPTAU = LQ(LPTAU)
            JTAU = JTAU + 1
          ENDIF
        ENDDO
        IER = -5
        GOTO 999
  100   CONTINUE
      ENDIF
      E(1) = Q(LPTAU+3)
      E(2) = Q(LPTAU+4)
      E(3) = Q(LPTAU+5)
      E(4) = Q(LPTAU+6)
      ET = Q(LPTAU+7)
      THETA = Q(LPTAU+8)
      ETA = Q(LPTAU+10)
      PHI = Q(LPTAU+9)
      RMS_WIDTH = Q(LPTAU+11)
  999 RETURN
C*************************************************
C  Entry point to get total number of PTAU banks
C*************************************************
      ENTRY GTPTAU_TOTAL(NUM_TAU,IER)
C
      CALL PATHGT(PATH)
      IF ( PATH.EQ.'MDST' ) THEN
        LMDST=GZMDST()
        IF( LMDST.LE.0 ) THEN
          NUM_TAU=0
          IER=-13
        ELSE
          NUM_TAU=IQ(LMDST+15)
          IER=0
        ENDIF
      ELSE
        LPTAU = GZPTAU()
        IF ( LPTAU .GT. 0 ) THEN
          NUM_TAU = NZBANK(IXCOM,LPTAU)
          IER = 0
        ELSE
          NUM_TAU = 0
          IER = 0
        ENDIF
      ENDIF
C
      RETURN
      END
