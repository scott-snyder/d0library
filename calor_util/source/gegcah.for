      SUBROUTINE GEGCAH(ITRAK,IPOINT,CELLE,IETA,IPHI,LAYER,ITAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return cell energy and upacked Physics cell indices
C-                              for IPOINT'th cell of ITRAK track
C-           **** Valid for D0Geant V2.0 and above ****
C-
C-   Inputs  : ITRAK    = Bank (track) number to access
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
      INTEGER ITRAK,IPOINT
C Output variables
      REAL    CELLE(2)
      INTEGER IETA,IPHI,LAYER,ITAG
C Zebra variable
      INTEGER LGCAH,GZGCAH
C----------------------------------------------------------------------
C
      LGCAH = GZGCAH(ITRAK)
C
C ****  Call lower level routine to actually get the data
      CALL GDGCAH(LGCAH,IPOINT,CELLE,IETA,IPHI,LAYER,ITAG)
c
  999 RETURN
      END
