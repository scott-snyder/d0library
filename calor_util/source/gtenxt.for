      SUBROUTINE GTENXT(ITRAK,IPOINT,ECELL,IZ,IE,IP,IL,EOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return cell energy and upacked cell indices
C-                              for IPOINT'th cell of ITRAK track
C-           **** Valid for D0Geant V0.11 through V1.7 ****
C-
C-   Inputs  : ITRAK  = Bank (track) number to access
C-             IPOINT = Sequence number of cell to return
C-   Outputs : ECELL  = Cell energy (REAL) in GeV
C-             IZ    = Z division index
C-             IE    = ETA division index
C-             IP    = PHI division index
C-             IL    = LAYER        index
C-             EOD   = .TRUE. if End-Of-Data
C-
C-   Created  12-NOV-1986   Alan M. Jonckheere
C-   Updated  13-APR-1989   A.M.Jonckheere
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C INPUT VARIABLES
      INTEGER ITRAK,IPOINT
C OUTPUT VARIABLES
      REAL ECELL
      INTEGER IZ,IE,IP,IL
      LOGICAL EOD
C
C Main ZEBRA store
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C I/O UNITS
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
C
      INTEGER LGCAH
      INTEGER GZGCAH                    ! Integer Function
      INTEGER NDGCAH,IADDR
C
C----------------------------------------------------------------------
C
      EOD = .FALSE.
C
      LGCAH = GZGCAH(ITRAK)
      IF ( LGCAH.LE.0 ) GOTO 998
C
      NDGCAH = IQ(LGCAH-1)
C
      IF ( 2*IPOINT+19.GT.NDGCAH ) GOTO 998
C
      IADDR = IQ(LGCAH + 2*(IPOINT-1) + 20)
      ECELL = FLOAT(IQ(LGCAH + 2*(IPOINT-1) + 21))/1.E6
      IL = MOD(IADDR,32)
      IADDR = IADDR/32
      IP = MOD(IADDR,64)
      IADDR = IADDR/64
      IE = MOD(IADDR,64)
      IZ = IADDR/64
      GOTO 999
C
C ****  Error or EOD occured
C
  998 IZ = 0
      IE = 0
      IP = 0
      IL = 0
      ECELL = 0.
      EOD   = .TRUE.
C
  999 END
