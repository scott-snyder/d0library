      SUBROUTINE BLFDTA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills bank FDTA hanging from FDRT. The
C-                         data are given in the routine itself.
C-
C-   Inputs  : none
C-   Outputs :
C-
C-   Created  12-MAY-1988   Jeffrey Bantly   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFDRT.LINK'
      INCLUDE 'D0$LINKS:IZFDTA.LINK'
      INTEGER NFORMA, I, LKFDTA
      REAL ZWI(8), ZHF(8), ZWV(8), YWI(6), STAGG(8)
      REAL XFY(3,6),YFY(3,6)
      CHARACTER*4 CHFDY(6),CHFWY(6),VOLMOT(6),SHAPE
C
C ****  Number of cells, wires per cell, parameters per volume,
C       and number of volumes
C
      INTEGER NUMCEL, NUMWIR, NPARVL, NUMVOL
      DATA NUMCEL, NUMWIR, NPARVL, NUMVOL / 6, 8, 9, 6 /
C
C ****  Volume and shape names
C
      DATA VOLMOT,SHAPE / 'FWA0','FWA1','FWA2','FWA3',
     &                    'FWA4','FWA5','BOX '/
C
C ****  Nominal Z position of each wire in a cell
C
      DATA ZWI / 2.762, 1.952, 1.175, 0.398, -0.380, -1.157,
     &          -1.934, -2.711/
C
C ****  Half-thickness in Z of each wire volume
C
      DATA ZHF / .420, .389, .389, .389, .389, .389, .389, .418 /
C
C ****  Z center of each wire volume
C
      DATA ZWV / -1.106,-1.915, -2.693, -3.471, -4.249, -5.027,
     &           -5.805, -6.612 /
C
C ****  Nominal Y position of all wires in a chamber, by cell (0-5)
C
      DATA YWI / -2.466, 2.466, -2.466, 0.0, 0.0, 0.0 / 
C
C ****  Staggering of each wire in Y for theta full cell (3-5)
C
      DATA STAGG / .02, -.02, .02, -.02, .02, -.02, .02, -.02 /
C
C----------------------------------------------------------------------
C
C  Book FDTA Theta chamber sensitive volumes pieces bank
C
      CALL MZFORM ( 'FDTA', '4I 1H 38F / 1H 6F 2H', NFORMA)
      CALL MZBOOK ( IDVSTP, LFDTA, LFDRT, -IZFDTA, 'FDTA', 0, 0,
     &              43 + NUMCEL*NPARVL, NFORMA, 0 )
C
C       Design Values for the FDC drift cells
C
C             +1       I   Number of sensitive cells in Theta A chamber 6
C             +2       I   Number of wires per cell in Theta chamber    8
C             +3       I   Number of parameters for each volume         9
C             +4       I   Number of volumes                            6
C             +5       H   Volume type                               'box '
C
C       for any Theta cell:  ( J = 5 )
C
C             +1       F   Nominal Z position of wire 0          2.869
C             +2       F   .......................... 1          1.952
C             +3       F   .......................... 2          1.175
C             +4       F   .......................... 3          0.398
C             +5       F   .......................... 4         -0.380
C             +6       F   .......................... 5         -1.157
C             +7       F   .......................... 6         -1.934
C             +8       F   .......................... 7         -2.711
C
C       for any Theta cell:
C
C             +9       F   Half-thickness in Z of wire volume 0       .420
C            +10       F   .................................. 1       .389
C            +11       F   .................................. 2       .389
C            +12       F   .................................. 3       .389
C            +13       F   .................................. 4       .389
C            +14       F   .................................. 5       .389
C            +15       F   .................................. 6       .389
C            +16       F   .................................. 7       .418
C
C            +17       F   Z center of wire volume........... 0     -1.106 
C            +18       F   .................................. 1     -1.915 
C            +19       F   .................................. 2     -2.693 
C            +20       F   .................................. 3     -3.471 
C            +21       F   .................................. 4     -4.249 
C            +22       F   .................................. 5     -5.027 
C            +23       F   .................................. 6     -5.805 
C            +24       F   .................................. 7     -6.612 
C
C       for any Theta chamber:
C                                                           
C            +25       F   Nominal Y position of wire in cell 0     -2.466
C            +26       F   .................................. 1      2.466
C            +27       F   .................................. 2     -2.466
C            +28       F   .................................. 3      0.000
C            +29       F   .................................. 4      0.000
C            +30       F   .................................. 5      0.000
C
C       for Theta full cell:
C
C            +31       F   Nominal Y stagger of wire  0           .02
C            +32       F   .......................... 1          -.02
C            +33       F   .......................... 2           .02
C            +34       F   .......................... 3          -.02
C            +35       F   .......................... 4           .02
C            +36       F   .......................... 5          -.02
C            +37       F   .......................... 6           .02
C            +38       F   .......................... 7          -.02
C
C  for each cell volume of box type:  ( J = 5 + 38 + 
C                                   (volume - 1) * parameters per volume )
C
C            J+1       H   Cell volume name                                 
C             +2       F   X half-width of volume
C             +3       F   Y half-width of volume
C             +4       F   Z half-width of volume 
C             +5       F   X position of volume
C             +6       F   Y position of volume
C             +7       F   Z position of volume
C             +8       H   Subdivision volume name
C             +9       H   Volume in which to position this volume
C
C     The structure of geometry for the theta A chamber is:
C        FDC                           : mother volume of FDC  (in FGEH)
C         --->FTH                      : full theta volume
C                                      :          (2 A's,2 B's)(in FWAL)
C            --->FWTA                  : theta chamber A, non-sensitive
C               --->FWA0 to FWA5       : theta chamber A, non-sens. sectors
C                   --->FDY0 to FDY5   : sensitive gas volumes (6)
C                   --->FYZ0 to FYZ5   : sensitive gas cells (6x8)
C
C---------------------------------------------------------------------------
C
      DATA CHFDY/'FDY0','FDY1','FDY2','FDY3','FDY4','FDY5'/
      DATA CHFWY/'FYZ0','FYZ1','FYZ2','FYZ3','FYZ4','FYZ5'/
C
C  'BOX'  y-drift cells (vertical drift cell in theta A chamber)...
      DATA XFY /  16.10, 2.87, 3.172,
     2            22.05, 2.87, 3.172,
     3            28.01, 2.87, 3.172,
     4            38.89, 5.33, 3.172,
     5            41.09, 5.33, 3.172,
     6            27.35, 5.33, 3.172/
C
C  y-drift cells...
      DATA YFY/ 0.00, 13.87, -3.858,
     2          0.00, 19.82, -3.858,
     3          0.00, 25.78, -3.858,
     4          0.00, 34.19, -3.858,
     5          0.00, 45.08, -3.858,
     6          0.00, 55.96, -3.858/
C
C  Fill the arrays with the parameters
C
      IC ( LFDTA + 1 ) = NUMCEL
      IC ( LFDTA + 2 ) = NUMWIR
      IC ( LFDTA + 3 ) = NPARVL
      IC ( LFDTA + 4 ) = NUMVOL
      CALL UCTOH(SHAPE, C(LFDTA+5), 4, 4 )
C
      LKFDTA = LFDTA + 5
      CALL UCOPY ( ZWI, C ( LKFDTA + 1 ), NUMWIR )
      CALL UCOPY ( ZHF, C ( LKFDTA + 9 ), NUMWIR )
      CALL UCOPY ( ZWV, C ( LKFDTA + 17 ), NUMWIR )
      CALL UCOPY ( YWI, C ( LKFDTA + 25 ), NUMCEL )
      CALL UCOPY ( STAGG, C ( LKFDTA + 31 ), NUMWIR )
      LKFDTA = LKFDTA + 38
C
      DO I = 1, 6
      CALL UCTOH(CHFDY(I), C(LKFDTA+1), 4, 4 )
      C  ( LKFDTA + 2 ) = XFY(1,I)
      C  ( LKFDTA + 3 ) = XFY(2,I)
      C  ( LKFDTA + 4 ) = XFY(3,I)
      C  ( LKFDTA + 5 ) = 0.0
      C  ( LKFDTA + 6 ) = 0.0
      C  ( LKFDTA + 7 ) = YFY(3,I)
      CALL UCTOH(CHFWY(I), C(LKFDTA+8), 4, 4 )
      CALL UCTOH(VOLMOT(I), C(LKFDTA+9), 4, 4 )
      LKFDTA = LKFDTA + NPARVL
      ENDDO
C
C------------------------------------------------------------------------
  999 RETURN
      END
