      SUBROUTINE BLDRFT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills bank DRFT hanging from DGEH. Actually the
C-                         data are given in the routine itself.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  17-FEB-1988   Ghita Rahal-Callot
C-   Updated  31-MAR-1992   Qizhong Li-Demarteau  correct the half thickness
C-                                    of the cathode wall from 0.001 to 0.05
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDRFT.LINK'
      INTEGER LDRFT, NFORMA
      REAL RPHIL(8), XWI(7), STAGG(7)
      REAL CORGEN(3)
      DATA CORGEN / 2.21, 5.625, .05 /
C
C ****  Number of wires and offset of Zmax in DGEH
C
      INTEGER NLAY, NSEC(4), NUWIR, IFDGEH
      DATA NLAY, NSEC, NUWIR, IFDGEH / 4, 32, 32, 32, 32, 7 , 14 /
C
C ****  R and PHI for the cell at the center of each layer
C
      DATA RPHIL /  54.003,5.625,59.185,11.250,64.401,
     &  5.625,69.643,11.250/
C
C ****  Nominal x position of each wire
C
      DATA XWI / -1.91, -1.2, -.6, .0, .6, 1.2, 1.91 /
C
C ****  staggering of each wire
C
      DATA STAGG / -.02, .02, -.02, .02, -.02, .02, -.02 /
C----------------------------------------------------------------------
      CALL MZFORM ( 'DRFT', '7I -F', NFORMA)
      CALL MZBOOK ( IXSTP, LDRFT, LDGEH, -IZDRFT, 'DRFT', 0, 0,
     &              18 + NUWIR*2, NFORMA, 0 )
C
C       Design Values for the CDC drift cells
C
C             +1       I   Number of sensitive layers           4
C             +2       I   Number of sensitive sector in layer0 32
C             +3       I   Number of sensitive sector in layer1 32
C             +4       I   Number of sensitive sector in layer2 32
C             +5       I   Number of sensitive sector in layer3 32
C             +6       I   Number of wires per sector           7
C             +7       I   Offset in DGEH of Z max              14
C                          ( half length along the beam line )
C             +8       F   Half cell height along wire plane    2.21        
C             +9       F   Half opening angle( deg )            5.625
C            +10       F   Half thickness of the septum         0.05
C            +11       F   R cell center layer 0                54.003
C            +12       F   Phi cell center cell 0 layer 0        5.625
C            +13       F   R cell center layer 1                59.185
C            +14       F   Phi cell center cell 0 layer 1       11.250
C            +15       F   R cell center layer 2                64.401
C            +16       F   Phi cell center cell 0 layer 2        5.625
C            +17       F   R cell center layer 3                69.643
C            +18       F   Phi cell center cell 0 layer 3       11.250
C            +19       F   Nominal X position of wire 0         -1.91
C            +20       F   .......................... 1         -1.2
C            +21       F   .......................... 2         -0.6
C            +22       F   .......................... 3          0.0
C            +23       F   .......................... 4          0.6
C            +24       F   .......................... 5          1.2
C            +25       F   .......................... 6          1.91
C            +26       F   Nominal Y position of wire 0          -.02
C            +27       F   .......................... 1           .02
C            +28       F   .......................... 2          -.02
C            +29       F   .......................... 3           .02
C            +30       F   .......................... 4          -.02
C            +31       F   .......................... 5           .02
C            +32       F   .......................... 6          -.02
C---------------------------------------------------------------------------
      IC ( LDRFT + 1 ) = NLAY
      CALL UCOPY (NSEC, IC ( LDRFT + 2 ), 4 )
      IC ( LDRFT + 6 ) = NUWIR
      IC ( LDRFT + 7 ) = IFDGEH
      CALL UCOPY ( CORGEN, C (LDRFT+8), 3 )
      CALL UCOPY ( RPHIL, C(LDRFT+11), 8 )
      CALL UCOPY ( XWI, C ( LDRFT+19 ), NUWIR )
      CALL UCOPY ( STAGG, C (LDRFT + 19+ NUWIR), NUWIR)
  999 RETURN
      END
