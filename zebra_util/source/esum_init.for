      SUBROUTINE ESUM_INIT( OB_RESOLVE, OB_THRESH, OB_NAME )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the spatial resolutions and
C-                         the character names of all legal objects
C-
C-   Inputs  :
C-   Outputs :  OB_RESOLVE, OB_NAME, OB_THRESH
C-   Controls:
C-
C-   Created  10-DEC-1991   Richard V. Astur
C-   Updated  11-DEC-1991   James T. Linnemann  add ID_ALL
C-   Updated   3-APR-1992   M.V.S. Rao  : changed to ETMISS, ETSUM
C-   Updated  25-FEB-1995   Amber Boehnlein, added intialization for
C-                          ETOTAL, HTSUM, HTVECT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      REAL OB_RESOLVE( ID_ALL : LAST_TYPE )
      REAL OB_THRESH ( ID_ALL : LAST_TYPE )
      CHARACTER*10 OB_NAME( ID_ALL : LAST_TYPE )
C----------------------------------------------------------------------
C: All
      OB_NAME( ID_ALL ) = 'ALL'
C: Vertex
      OB_NAME( ID_VERTEX ) = 'VERTEX'
      OB_RESOLVE( ID_VERTEX ) = RES_VERTEX
      OB_THRESH ( ID_VERTEX ) = THRESH_VERTEX
C: Jets
      OB_NAME( ID_JET ) = 'JET'
      OB_RESOLVE( ID_JET ) = RES_JET
      OB_THRESH( ID_JET ) = THRESH_JET
C: Muons
      OB_NAME( ID_MUON ) = 'MUON'
      OB_RESOLVE( ID_MUON ) = RES_MUON
      OB_THRESH( ID_MUON ) = THRESH_MUON
C: Electrons
      OB_NAME( ID_ELECTRON ) = 'ELECTRON'
      OB_RESOLVE( ID_ELECTRON ) = RES_ELECTRON
      OB_THRESH( ID_ELECTRON ) = THRESH_ELECTRON
C: Taus
      OB_NAME( ID_TAU ) = 'TAU'
      OB_RESOLVE( ID_TAU ) = RES_TAU
      OB_THRESH( ID_TAU ) = THRESH_TAU
C: Photons
      OB_NAME( ID_PHOTON ) = 'PHOTON'
      OB_RESOLVE( ID_PHOTON ) = RES_PHOTON
      OB_THRESH( ID_PHOTON ) = THRESH_PHOTON
C: Missing Et
      OB_NAME( ID_ETMISS ) = 'ETMISS'
      OB_RESOLVE( ID_ETMISS ) = RES_ETMISS
      OB_THRESH( ID_ETMISS ) = THRESH_ETMISS
C: Sum Et
      OB_NAME( ID_ETSUM ) = 'ETSUM'
      OB_RESOLVE( ID_ETSUM ) = RES_ETSUM
      OB_THRESH( ID_ETSUM ) = THRESH_ETSUM
C: Jet type 1
      OB_NAME( ID_JET_1 ) = 'JET_1'
      OB_RESOLVE( ID_JET_1 ) = RES_JET_1
      OB_THRESH( ID_JET_1 ) = THRESH_JET_1
C: Jet type 2
      OB_NAME( ID_JET_2 ) = 'JET_2'
      OB_RESOLVE( ID_JET_2 ) = RES_JET_2
      OB_THRESH( ID_JET_2 ) = THRESH_JET_2
C: Jet type 3
      OB_NAME( ID_JET_3 ) = 'JET_3'
      OB_RESOLVE( ID_JET_3 ) = RES_JET_3
      OB_THRESH( ID_JET_3 ) = THRESH_JET_3
C: Jet type 4
      OB_NAME( ID_JET_4 ) = 'JET_4'
      OB_RESOLVE( ID_JET_4 ) = RES_JET_4
      OB_THRESH( ID_JET_4 ) = THRESH_JET_4
C: Jet type 5
      OB_NAME( ID_JET_5 ) = 'JET_5'
      OB_RESOLVE( ID_JET_5 ) = RES_JET_5
      OB_THRESH( ID_JET_5 ) = THRESH_JET_5
C: Sum E
      OB_NAME( ID_ETOTAL ) = 'ETOTAL'
      OB_RESOLVE( ID_ETOTAL ) = RES_ETOTAL
      OB_THRESH( ID_ETOTAL) = THRESH_ETOTAL
C: HT SCALAR SUM
      OB_NAME( ID_HTSUM ) = 'HTSUM'
      OB_RESOLVE( ID_HTSUM ) = RES_HTSUM
      OB_THRESH( ID_HTSUM) = THRESH_HTSUM
C: HT VECTOR SUM
      OB_NAME( ID_HTVECT ) = 'HTVECT'
      OB_RESOLVE( ID_HTVECT ) = RES_HTVECT
      OB_THRESH( ID_HTVECT) = THRESH_HTVECT
  999 RETURN
      END
