      SUBROUTINE CATD_TO_CAEP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert CATD to FAKE CAEP (EM LYR=3, HAD LYR=11) 
C-
C-   Inputs  : CATD BANK
C-   Outputs : CAEP BANK
C-   Controls: NONE
C-
C-   Created   3-JUN-1993   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZLINKC.INC'       ! Protected Link area
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZCAEP.LINK'
      INCLUDE 'D0$LINKS:IZCATD.LINK'
C
      INTEGER LCATD,NONZCH,POINT
      INTEGER LCATD1, GZCATD, NEMTWR,NHDTWR,NMUTWR
      INTEGER I,J,IE,IP,IDLETA,NDATA,IEMEND,IHDEND,ICHK
      INTEGER IETA,LINE,HDPNT,EMLIM,IENER,NBEG,NEND
      INTEGER BVN,ISIGN
      INTEGER IPNTEM,IPNTHD,IPNTMU
      INTEGER PACTWR
      INTEGER JBYT,JBIT,NMAX
      REAL    ENERGY,DELETA,EUNIT,DEUNIT,THETA,PHI,ETA
      REAL    ETMNEM,ETMNHD,EMNMUO
C
      LOGICAL OK
      CHARACTER*40 MSG
      INTEGER J1,K,L,ID,IOH,CAEPPT
      INTEGER GZCAHT,GZCAEP
      INTEGER IPHI,ILYR,IHD
      INTEGER NCH,NR,NCHT,NRT,NTOT,NALOC
C
      INTEGER PAKADR
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
C
      DATA EUNIT,DEUNIT /.1, .01/
C----------------------------------------------------------------------
      LCAEP=GZCAEP()
      IF(LCAEP.NE.0) GOTO 999     ! If CAEP bank exists do nothing
      LCATD1 = GZCATD()
      IF(LCATD1.EQ.0) GOTO 999    ! If CATD bank exists continue
      PTZFLG=.FALSE.
      CALL CPTCAZ
C
      NDATA  = IQ(LCATD1-1)
C
C  ***  Print the content of the bank pointed by LCATD1
C
      CALL GTCATD (LCATD1,IPNTEM,IPNTHD,IPNTMU,NEMTWR,NHDTWR,
     &                         NMUTWR,ETMNEM,ETMNHD,EMNMUO)
C
      CALL BKCAEP(NDATA,LCAEP)
      NR=IQ(LCAEP+2)
      POINT=3
      NONZCH=0
C
      NBEG = IPNTEM  + 1
      NEND = IPNTMU  - 1
      DO 100 I = NBEG,NEND
        PACTWR = IQ(LCATD1+I)
        IE  = JBYT(PACTWR, 1, 7)
        IP  = JBYT(PACTWR, 8, 7)
        IF (IE .LE. NETAL) THEN
          IETA = IE - NETAL - 1
        ELSE
          IETA = IE - NETAL
        ENDIF
        IF(I.LE.NEMTWR) THEN
          ILYR=2
          IF(ABS(IETA).EQ.14) ILYR=7
        ELSE IF (I.GT.IPNTHD ) THEN
          ILYR = 11
          IF(ABS(IETA).EQ.37) ILYR=13
        ELSE 
          GOTO 100
        END IF
        IENER  = JBYT(PACTWR,20,13)
        ENERGY = IENER * EUNIT
        IPHI = IP
        BYTES(BYTE4)=IETA
        BYTES(BYTE3)=IPHI
        BYTES(BYTE2)=ILYR
        POINT=POINT+1
        IQ(LCAEP+POINT)=PAKADR         ! packed physics address
        POINT=POINT+1
        Q(LCAEP+POINT)=ENERGY          ! energy
        NONZCH=NONZCH+1
C
C ****  FILL PTCAEP ARRAY
C
        PTCAEP(IETA,IPHI,ILYR) = NONZCH

  100 CONTINUE
      IF(NONZCH .LT. NDATA) THEN
        NCH = NR*(NONZCH-NDATA)
        CALL MZPUSH(IXCOM,LCAEP,0,NCH,'I')
      ENDIF
      IQ(LCAEP+3)=NONZCH                ! no. of channels
  999 RETURN
      END
