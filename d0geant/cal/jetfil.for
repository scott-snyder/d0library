      SUBROUTINE JETFIL(PARTN,IE,IP,IL,CELPOS,ECELL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : JETFIL controls the filling of the JETS and JPTS
C-              banks.  The routine is passed the location and energy deposited
C-              in the current cell and the Isajet number of the initiating
C-              parton. The partons were associated with jets in JETINI, and
C-              this information is passed in /SPEVNT/. The JETS bank contains
C-              the measured energy vector (sum of energy observed times
C-              direction unit vector) associated with each jet. It also
C-              contains the scalar energy, or sum of ECELL.  The JPTS banks
C-              contain a list of the indices of cells contributing to each
C-              jet. The indices are replaced by pointers to the CAEP bank in
C-              JETEND.
C-
C-   Inputs  : PARTN     initiating Isajet parton number
C-             IE        eta index of the cell
C-             IP        phi index of the cell
C-             IL        layer index of the cell
C-             CELPOS(3) vector from the interaction vertex to the cell
C-             ECELL     energy deposited in the cell (GeV)
C-   Outputs : Contained in JETS and JPTS banks.
C-   Controls:
C-
C-   Created   7-OCT-1988   Z. Wolf
C-   Updated  20-JAN-1989   Z. Wolf
C-   Updated  24-MAR-1989   Z. Wolf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   I/O
      INTEGER PARTN
      INTEGER IE,IP,IL
      REAL CELPOS(3)
      REAL ECELL
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
C--   ASSOCIATION OF PARTON WITH JET
      INCLUDE 'D0$INC:SPEVNT.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER JETN
      INTEGER LJET0,GZJETS
C----------------------------------------------------------------------
C
C--   CHECK PARTON NUMBER
      IF(PARTN.LT.0.OR.PARTN.GT.200)GO TO 999
C
C--   CHECK THE LAYER NUMBER FOR LIVE CALORIMETRY
      IF(IL.GE.18)GO TO 999
C
C--   CHECK THE LAYER NUMBER TO EXCLUDE ICDS, MASSLESS GAPS
      IF(IL.EQ.8 .OR. IL.EQ.9 .OR. IL.EQ.10)GO TO 999
C
C--   CHECK ECELL
      IF(ECELL.LE.0.)GO TO 999
C
C--   FIND THE JET THE PARTON IS IN
      JETN=PNTOJT(PARTN)
C
C--   LOCATE THE FIRST JETS BANK
      CALL PATHST('GEAN')
      LJET0=GZJETS()
C
C--   FILL THE JETS AND JPTS BANKS
      CALL FLJETS(LJET0,JETN,CELPOS,ECELL)
      CALL FLJPTS(LJET0,JETN,IE,IP,IL,ECELL)
C
  999 RETURN
      END
