      SUBROUTINE BLFDTB_D0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills bank FDTB hanging from FDRT. Actually the
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
      INCLUDE 'D0$LINKS:IZFDTB.LINK'
      INTEGER NFORMA, I, LKFDTB
      REAL ZWI(8), ZHF(8), ZWV(8), YWI(6), STAGG(8)
      REAL XFX(3,6),YFX(3,6)
      CHARACTER*4 CHFDX(6),CHFWX(6),VOLMOT(6),SHAPE
      DATA VOLMOT,SHAPE / 'FWB0','FWB1','FWB2','FWB3',
     &                    'FWB4','FWB5','BOX '/
C
C ****  Number of cells, wires per cell, parameters per volume,
C        and number of volumes
C
      INTEGER NUMCEL, NUMWIR, NPARVL, NUMVOL
      DATA NUMCEL, NUMWIR, NPARVL, NUMVOL / 6, 8, 9, 6 /
C
C ****  Nominal Z position of each wire
C
      DATA ZWI / 2.869, 1.952, 1.175, 0.398, -0.380, -1.157,
     &          -1.934, -2.711/
C
C ****  Half-thickness in Z of each wire volume
C
      DATA ZHF / .420, .389, .389, .389, .389, .389, .389, .428 /
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
C ****  Staggering of each wire in a theta full cell (3-5)
C
      DATA STAGG / -.02, .02, -.02, .02, -.02, .02, -.02, .02 /
C
C----------------------------------------------------------------------
C
C  Book FDTB Theta B chamber sensitive volumes pieces bank
C
      CALL MZFORM ( 'FDTB', '4I 1H 38F / 1H 6F 2H', NFORMA)
      CALL MZBOOK ( IDVSTP, LFDTB, LFDRT, -IZFDTB, 'FDTB', 0, 0,
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
C            +16       F   .................................. 7       .428
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
C             +9       H   Volume in which to position this volume ('FWTB')
C
C     The structure of geometry for the theta B quadrant is:
C        FDC                           : mother volume of FDC  (in FGEH)
C         --->FTH                      : full theta volume
C                                      :          (2 A's,2 B's)(in FWAL)
C            --->FWTB                  : theta chamber B, non-sensitive
C               --->FWB0 to FWB5       : theta chamber B, non-sens. sectors
C                   --->FDX0 to FDX5   : sensitive gas volumes (6)
C                   --->FXZ0 to FXZ5   : sensitive gas cells (6x8)
C
C---------------------------------------------------------------------------
C
C
      DATA CHFDX/'FDX0','FDX1','FDX2','FDX3','FDX4','FDX5'/
      DATA CHFWX/'FXZ0','FXZ1','FXZ2','FXZ3','FXZ4','FXZ5'/
C
C  'BOX'  x-drift cells (horizontal drift cell in theta B chamber)...
      DATA XFX /  2.87, 10.14, 3.172,
     2            2.87, 16.10, 3.172,
     3            2.87, 22.05, 3.172,
     4            5.33, 28.01, 3.172,
     5            5.33, 38.89, 3.172,
     6            5.33, 27.35, 3.172/
C
C  x-drift cells...
      DATA YFX/ 13.87, 0.00, -3.858,
     2          19.82, 0.00, -3.858,
     3          25.78, 0.00, -3.858,
     4          34.19, 0.00, -3.858,
     5          45.08, 0.00, -3.858,
     6          55.96, 0.00, -3.858/
C
C  Fill the arrays with the parameters
C
      IC ( LFDTB + 1 ) = NUMCEL
      IC ( LFDTB + 2 ) = NUMWIR
      IC ( LFDTB + 3 ) = NPARVL
      IC ( LFDTB + 4 ) = NUMVOL
      CALL UCTOH(SHAPE, C(LFDTB+5), 4, 4 )
C
      LKFDTB = LFDTB + 5
      CALL UCOPY ( ZWI, C ( LKFDTB + 1 ), NUMWIR )
      CALL UCOPY ( ZHF, C ( LKFDTB + 9 ), NUMWIR )
      CALL UCOPY ( ZWV, C ( LKFDTB + 17 ), NUMWIR )
      CALL UCOPY ( YWI, C ( LKFDTB + 25 ), NUMCEL )
      CALL UCOPY ( STAGG, C ( LKFDTB + 31 ), NUMWIR )
      LKFDTB = LKFDTB + 38
C
      DO I = 1, 6
      CALL UCTOH(CHFDX(I), C(LKFDTB+1), 4, 4 )
      C  ( LKFDTB + 2 ) = XFX(1,I)
      C  ( LKFDTB + 3 ) = XFX(2,I)
      C  ( LKFDTB + 4 ) = XFX(3,I)
      C  ( LKFDTB + 5 ) = 0.0
      C  ( LKFDTB + 6 ) = 0.0
      C  ( LKFDTB + 7 ) = YFX(3,I)
      CALL UCTOH(CHFWX(I), C(LKFDTB+8), 4, 4 )
      CALL UCTOH(VOLMOT(I), C(LKFDTB+9), 4, 4 )
      LKFDTB = LKFDTB + NPARVL
      ENDDO
C
C-----------------------------------------------------------------------
  999 RETURN
      END
