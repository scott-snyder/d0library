      SUBROUTINE GTCAID (IHIT, IETA, IPHI, ILYR,
     &                   SCALED_RATIO, ENERGY, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns data from the Ith channel in the CAID bank.
C-
C-   Use entry GTCAID_GLOBAL to get the "header" information from the CAID bank
C-   (that part which does not vary from hit to hit).
C-
C-   Inputs  : IHIT      [I] number of the hit for which the data is to be
C-                           retrieved.
C-   Outputs : IETA      [I] eta index of the removed cell
C-             IPHI      [I] phi index of the removed cell
C-             ILYR      [I] layer index of the removed cell
C-             SCALED_RATIO [I] hot cell ratio expressed on scale of 0-255
C-             ENERGY    [R] energy of the removed cell
C-             OK        [L] .TRUE. if successful, .FALSE. on error (no CAID
C-                            bank or attempt to read off end of CAID bank)
C-
C-   Controls: none
C-
C-   Created   8-APR-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE  'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER   IHIT, IETA, IPHI, ILYR, SCALED_RATIO
      REAL      ENERGY
      LOGICAL   OK
      INTEGER   VERS, NREP, NMAX, NAID, NCAND
      REAL      ET_THRESH, RATIO
C----------------------------------------------------------------------
      INTEGER  NHITS, LCAID, GZCAID, PACKADDR, PTR, NR
      EXTERNAL GZCAID
      BYTE     BB(4)
      EQUIVALENCE (PACKADDR, BB(1))
C----------------------------------------------------------------------
      IETA = 0
      IPHI = 0
      ILYR = 0
      ENERGY = 0.0
      SCALED_RATIO = 0
      OK = .FALSE.

      LCAID = GZCAID()
      IF (LCAID .LE. 0) RETURN          ! return with error

      NHITS = IQ(LCAID + 4)
      IF ( IHIT .GT. NHITS) RETURN     ! return with error

      NR = IQ(LCAID + 2)
      PTR = LCAID + (IHIT-1)*NR

      PACKADDR = IQ(PTR + 8)
      ENERGY   = Q(PTR + 9)

      SCALED_RATIO = BB(BYTE1)
      ILYR = BB(BYTE2)
      IPHI = BB(BYTE3)
      IETA = BB(BYTE4)

      OK = .TRUE.

      RETURN
C#######################################################################
      ENTRY GTCAID_GLOBAL (VERS, NREP, NMAX, NAID, NCAND,
     &                     ET_THRESH, RATIO, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the information from the CAID bank that does
C-   NOT vary from hit to hit.
C-
C-   Inputs  : none
C-   Outputs : VERS           [I] version number of the bank
C-             NREP           [I] repetition number
C-             NMAX           [I] maximum number of candidates allowed
C-             NAID           [I] number of channels suppressed
C-             NCAND          [I] number of candidates
C-             ET_THRESH      [R] minimum ET for candidate
C-             RATIO          [R] theshold ratio used by AIDA
C-             OK             [L] .TRUE. if successful, .FALSE. on error
C-   Controls: none
C-
C-   Created   8-APR-1993   Marc Paterno
C-
C----------------------------------------------------------------------
      LCAID = GZCAID ()
      IF (LCAID .LT. 0) THEN
        OK = .FALSE.
        RETURN
      ELSE
        OK = .TRUE.
      ENDIF

      OK = .TRUE.
      VERS = IQ(LCAID + 1)
      NREP = IQ(LCAID + 2)
      NMAX = IQ(LCAID + 3)
      NAID = IQ(LCAID + 4)
      NCAND = IQ(LCAID + 5)
      ET_THRESH = Q(LCAID + 6)
      RATIO = Q(LCAID + 7)

      RETURN
      END
