      SUBROUTINE GDGCAH(LGCAH,IPOINT,CELLE,IETA,IPHI,LAYER,ITAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return cell energy and upacked Physics cell indices
C-                              for IPOINT'th cell of ITRAK track
C-           **** Valid for D0Geant V2.0 and above ****
C-
C-   Inputs  : LGCAH    = LINK TO GCAH BANK
C-             IPOINT   = Sequence number of cell to return
C-   Outputs : CELLE(1) = Cell energy (REAL) in GeV
C-                  (2) = Hadronic energy ONLY in GeV
C-             IETA     = Physics system Eta index (-37:37)
C-             IPHI     = Physics Phi index (1:64)
C-             LAYER    = Physics Layer index (augmented) (1:27)
C-             ITAG     = Tag bits
C-
C-   Created  16-FEB-1989   Alan M. Jonckheere
C-   Updated   5-MAY-1989   Stan M. Krzywdzinski
C-                          Corrected extracting data for NCELL, IADDR
C-                          and CELLE
C-   Updated   1-OCT-1989   Chip Stewart  - added code for new bank format
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C Input variables
      INTEGER LGCAH,IPOINT
C Output variables
      REAL    CELLE(2)
      INTEGER IETA,IPHI,LAYER,ITAG
C Local variables
      INTEGER NCELL,IADDR,VERSION,NHEADER,NREPEAT,NSKIP
C----------------------------------------------------------------------
C
      IF ( LGCAH.LE.0 ) GOTO 998
C
C ****  determine version of GCAH (2 or above have differnet format from 1)
C
      VERSION = IQ(LGCAH + 1)
      IF(VERSION .NE. 2) THEN
        NHEADER = 19
        NREPEAT = 2
      ELSE
        NHEADER = IQ(LGCAH+2)
        NREPEAT = IQ(LGCAH+3)
      END IF
C
      NCELL = IQ(LGCAH+NHEADER)
      IF ( IPOINT .GT. NCELL ) GOTO 998
C
C ****  Get and unpack data
      IADDR = IQ(LGCAH+NHEADER+NREPEAT*(IPOINT-1)+1)
      IETA  = IBITS(IADDR,24,8)
      IF ( IETA.GE.128 ) THEN
        IETA = IETA - 256
      ENDIF
      IPHI  = IBITS(IADDR,16,8)
      LAYER = IBITS(IADDR,8,8)
      ITAG  = IBITS(IADDR,0,8)
      CELLE(1) = Q(LGCAH+NHEADER+NREPEAT*(IPOINT-1)+2)
      IF ( NREPEAT.GT.2 ) THEN
        CELLE(2) = Q(LGCAH+NHEADER+NREPEAT*(IPOINT-1)+3)
      ELSE
        CELLE(2) = 0.
      ENDIF
      GOTO 999
C
C ****  Error occured
C
  998 IETA  = 0
      IPHI  = 0
      LAYER = 0
      ITAG  = 0
      CELLE(1) = 0.
      CELLE(2) = 0.
  999 RETURN
      END
