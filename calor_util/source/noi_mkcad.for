      SUBROUTINE NOI_MKCAD(NPRE,NOILEV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill CAD banks from CELL_EN array
C-
C-   Inputs  : NPRE = Number of previously existing CAD banks
C-             NOILEV = NOISE LEVEL:
C-                      0=NONE, 1=TRIGGER LEVEL 1,
C-                      2= LEVEL 2 and OFF-LINE
C-   Outputs :
C-   Controls:
C-       Relevant RCP parameters:
C-
C-    CELL_LOWCUT = minimum energy for which cell is kept
C-    CELL_SYMCUT = minimum absolute value of energy for which
C-                  cell is kept
C-
C-
C-   Created   9-AUG-1991   Allen I. Mincer
C-   modified  2-JUN-1993   Ian Adam 
C-    Replace original zero suppression with suppression according to 
C-    noise distributions.
C-   modified  7-SEP-1993   Ian Adam
C-    Update the call to zero_suppress_array, replace by NOI_ZERO_SUPPRESS 
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      REAL CSYMCUT,CLOWCUT
C
C
C     VARIABLES FOR PACKING AND UNPACKING
C
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
C
      INTEGER I,NCH
      INTEGER GZCADT,LCADT,CADT_LINK(2,0:5)
      INTEGER NCH1,NCH2,NONZCH,NCAD,ICH,ICAD,NCRATE,ADDR,PCRATE
      INTEGER HEADER_LEN,ICRATE,SYNCH,CONTROL_WORD,VERSION,STATUS,PULSER
      INTEGER POINT,NR,ND,IWCONT,PAKADR,IWORD,IER2,IETA,IPHI,ILYR,IOR
      INTEGER GZCPDH,IPH
      LOGICAL FIRST_CELL,DO_ADDR_CHECK,CEXIST
      CHARACTER MSG*40
      REAL ENERGY,E2
      REAL CAD_GAIN
C&IF VAXVMS
      BYTE CONTRW(4)
      EQUIVALENCE(IWCONT,CONTRW)
      BYTE BYTES(4)
      EQUIVALENCE (PAKADR,BYTES)
C&ELSE
C     INTEGER IBITS,IOR,IAND,IBSET,IBCLR
C     LOGICAL BTEST
C&ENDIF
C
C     VARIABLES FOR CAD CREATION
C
      INTEGER CRATE, ADC, BLS, ROTOW, DEPTH,ICOND
      INTEGER NPRE,NOILEV
      LOGICAL OK
C----------------------------------------------------------------------
C
C *** BOOK AND FILL CAD PRIME BANKS
      CALL NOI_EVENT_SELECT(1)
C
C-    FOLLOWING BASED ON CODE TAKEN FROM MKCAD
C-
      IF(NOILEV.GT.0)THEN
        CSYMCUT=CELL_SYMCUT
        CLOWCUT=CELL_LOWCUT
      ELSE
        CSYMCUT=1.E-8
        CLOWCUT=-1.E10
      ENDIF

      IF (DO_ZSP) CALL NOI_ZERO_SUPPRESS(N_SIGMA)

      DO 950 ILYR=NLAYMIN,NLAYMAX
        DO 940 IPHI=NPHIMIN,NPHIMAX
          DO 930 IETA=NETAMIN,NETAMAX
            ENERGY=CELL_EN(IETA,IPHI,ILYR)
            E2=ENERGY
C             MAY NEED TO MAKE A DIFFERENT CUT FOR ICD AND
C             MASSLESS GAPS, NOT YET IMPLEMENTED
            IF(E2.GT.CLOWCUT .AND.
     &            ABS(E2).GT.CSYMCUT)THEN
C
C---          CONVERT TO READOUT SYSTEM INDICES
C
              CALL CPHAD (IETA, IPHI, ILYR, CRATE,
     &              ADC, BLS, ROTOW, DEPTH,ICOND)
              IF ( ICOND.NE.0 ) THEN
                CALL ERRMSG('BAD CPHAD','NOISY',
     &                     'BAD CPHAD CONVERSION','W')
                GO TO 930
              ENDIF
C
C---          SAVE DATA IN CAD1 OR CAD2 BANK
C
              CALL NOI_CADFL(CRATE, ADC, BLS, ROTOW,
     &               DEPTH, IETA, ILYR, ENERGY, NPRE,NOILEV)
C
            ENDIF
  930     CONTINUE
  940   CONTINUE
  950 CONTINUE
C--- NOW COMPRESS OUT THE "ZEROES" IN THE CAD1 AND CAD2 BANKS
      CALL CADZS(1, OK)
      CALL CADZS(2, OK)
C
C *** DONE WITH CAD BOOKING
C
  999 CONTINUE
      RETURN
      END
