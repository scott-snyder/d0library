      FUNCTION QCD_UPK_AIDA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills QCD_AIDA common block with hot cell
C-                         info for use with ntuplemaker
C-
C-   Created   06-DEC-1995  Andrew G. Brandt based on Di Loreto/Hsieh routine
C-   Updated   2-JAN-1996   Bob Hirosky  Fix NAID:out of range bug
C-   Updated   8-MAR-1996   Andrew G. Brandt rename CELL_PAR set IVERT=1
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_AIDA.INC/LIST'
      INTEGER   J, IETA, IPHI, ILYR, SCALED
      REAL      ENERGY, THRESH, RATIO
      LOGICAL   OK, QCD_UPK_AIDA
      INTEGER   VERS, NREP, NMAX, NAID, NHCAN
      REAL EVEC(3), ETAPHI(2), COR(3)
      INTEGER IVERT, IER
      DATA IVERT /1/
      QCD_UPK_AIDA = .TRUE.
C
C Get number of hot cells
C
      CALL GTCAID_GLOBAL(VERS,NREP,NMAX,NAID,NHCAN,THRESH,RATIO,OK)
      IF (.NOT.OK) THEN
         CALL ERRMSG('QCD_UPK_AIDA','QCD_UPK_AIDA',
     +        'GTCAID_GLOBAL Failed ','W')
         GOTO 999
      END IF
C
C Loop over hot cells
C
      NAIDA = 0
      DO J=1,NAID
        IF(NAIDA.GE.NAIDMX) THEN
           CALL ERRMSG('QCD_UPK_AIDA','QCD_UPK_AIDA',
     +       'NOT ALL AIDA CELLS ARE WRITTEN--THIS IS HOT EVENT ','W')
           GO TO 999
        END IF
C
C Get AIDA information
C
        CALL GTCAID(J,IETA,IPHI,ILYR,SCALED,ENERGY,OK)
        IF (.NOT.OK) THEN
          CALL ERRMSG('QCD_UPK_AIDA','QCD_UPK_AIDA',
     +        'GTCAID Failed ','W')
          GO TO 999
        ENDIF
C
C Convert to detector coordinates and ET
C
        CALL CELL_PAR_AIDA(IETA, IPHI, ILYR, IVERT, ENERGY, COR,
     +                ETAPHI, EVEC, IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('QCD_UPK_AIDA','QCD_UPK_AIDA',
     +        'CELL_PAR Failed ','W')
          GO TO 999
        END IF
C
C Save hot cells in QCD_AIDA common
C
        NAIDA = NAIDA + 1
        AIDETA(NAIDA) = ETAPHI(1)                   !ETA of cell
        AIDPHI(NAIDA) = ETAPHI(2)                   !PHI of cell
        AIDET(NAIDA)  = SQRT(EVEC(1)**2+EVEC(2)**2) !ET of cell
        AIDLYR(NAIDA)  = ILYR                       !LYR of cell
      ENDDO ! hot cell loop
 999  RETURN
      END
