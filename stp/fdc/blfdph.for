      SUBROUTINE BLFDPH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills bank FDPH hanging from FDRT. Actually the
C-                         data are given in the routine itself.
C-
C-   Inputs  : none
C-   Outputs :
C-
C-   Created  12-MAY-1988   Jeffrey Bantly   
C-   Updated   3-OCT-1989   Jeffrey Bantly  re-organize geometry for speedup 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFDRT.LINK'
      INCLUDE 'D0$LINKS:IZFDPH.LINK'
      INTEGER NFORMA, I, LKFDPH
      REAL ZWI(16), ZHF(16), ZWV(16), STAGG(16)
      REAL XFPC(3)
      REAL YFPC(3)
      CHARACTER*4 VOLMOT,SHAPE,SECNAM,VOLNAM
      CHARACTER*4 FPCN(0:5),FPPN(0:5),FPZN(0:5)
      DATA VOLMOT,SHAPE,SECNAM,VOLNAM / 'FDPH','TUBS','FPC ','FPCP'/
      DATA FPCN / 'FPC0','FPC1','FPC2','FPC3','FPC4','FPC5'/
      DATA FPPN / 'FPP0','FPP1','FPP2','FPP3','FPP4','FPP5'/
      DATA FPZN / 'FPZ0','FPZ1','FPZ2','FPZ3','FPZ4','FPZ5'/
C
C ****  Number of sectors, wires per sector, parameters per volume,
C       and number of volumes
C
      INTEGER NUMSEC, NUMWIR, NPARVL, NUMVOL 
      DATA NUMSEC, NUMWIR, NPARVL, NUMVOL / 6, 16, 9, 6 /
C
C ****  Nominal Z position of each wire in a sector
C
      DATA ZWI / -6.00, -5.20, -4.40, -3.60, -2.80, -2.00, -1.20, -.40,
     &             .40,  1.20,  2.00,  2.80,  3.60,  4.40,  5.20, 6.00/
C
C ****  Half-thickness in Z of each wire volume
C
      DATA ZHF / .715, .40, .40, .40, .40, .40, .40, .40,
     &            .40, .40, .40, .40, .40, .40, .40, .715/
C
C ****  Z center of each wire volume
C
      DATA ZWV / .715, 1.83, 2.63, 3.43, 4.23, 5.03, 5.83, 6.63,
     &           7.43, 8.23, 9.03, 9.83, 10.63, 11.43, 12.23, 13.345 /
C
C ****  Staggering of each wire about a center radius of a sector
C
      DATA STAGG / -.02, .02, -.02, .02, -.02, .02, -.02, .02,
     &             -.02, .02, -.02, .02, -.02, .02, -.02, .02 /
C
C----------------------------------------------------------------------
C
C  Book FDPH Phi chamber sensitive volumes pieces bank
C
      CALL MZFORM ( 'FDPH', '4I 1H 64F / 1H 6F 2H', NFORMA)
      CALL MZBOOK ( IDVSTP, LFDPH, LFDRT, -IZFDPH, 'FDPH', 0, 0,
     &              5 + NUMWIR*4 + NPARVL*NUMVOL, NFORMA, 0 )
C
C       Design Values for the FDC PHI drift sectors
C
C             +1       I   Number of sensitive sectors in Phi SUB-chamber 6
C             +2       I   Number of wires per sector in Phi chamber    16
C             +3       I   Number of parameters for each volume          9
C             +4       I   Number of volumes (6 sub-vol x 6 sect/subvol) 6
C             +5       H   Volume type                               'tube'
C
C       for Phi cell:
C
C             +6       F   Nominal Z position of wire 0         -6.60
C             +7       F   .......................... 1         -5.72
C             +8       F   .......................... 2         -4.84
C             +9       F   .......................... 3         -3.96
C            +10       F   .......................... 4         -3.08
C            +11       F   .......................... 5         -2.20
C            +12       F   .......................... 6         -1.32
C            +13       F   .......................... 7          -.44
C            +14       F   .......................... 8           .44
C            +15       F   .......................... 9          1.32
C            +16       F   ..........................10          2.20
C            +17       F   ..........................11          3.08
C            +18       F   ..........................12          3.96
C            +19       F   ..........................13          4.84
C            +20       F   ..........................14          5.72
C            +21       F   ..........................15          6.60
C
C            +22       F   Half-thickness of wire volume 0        .715
C            +23       F   ............................. 1        .40
C            +24       F   ............................. 2        .40
C            +25       F   ............................. 3        .40
C            +26       F   ............................. 4        .40
C            +27       F   ............................. 5        .40
C            +28       F   ............................. 6        .40
C            +29       F   ............................. 7        .40
C            +30       F   ............................. 8        .40
C            +31       F   ............................. 9        .40
C            +32       F   .............................10        .40
C            +33       F   .............................11        .40
C            +34       F   .............................12        .40
C            +35       F   .............................13        .40
C            +36       F   .............................14        .40
C            +37       F   .............................15        .715
C
C            +38       F   Z center of wire volume... 0           .715
C            +39       F   .......................... 1          1.83
C            +40       F   .......................... 2          2.63
C            +41       F   .......................... 3          3.43
C            +42       F   .......................... 4          4.23
C            +43       F   .......................... 5          5.03
C            +44       F   .......................... 6          5.83
C            +45       F   .......................... 7          6.63
C            +46       F   .......................... 8          7.43
C            +47       F   .......................... 9          8.23
C            +48       F   ..........................10          9.03
C            +49       F   ..........................11          9.83
C            +50       F   ..........................12         10.63
C            +51       F   ..........................13         11.43  
C            +52       F   ..........................14         12.23
C            +53       F   ..........................15         13.345
C
C            +54       F   Nominal Y position of wire 0          -.02
C            +55       F   .......................... 1           .02
C            +56       F   .......................... 2          -.02
C            +57       F   .......................... 3           .02
C            +58       F   .......................... 4          -.02
C            +59       F   .......................... 5           .02
C            +60       F   .......................... 6          -.02
C            +61       F   .......................... 7           .02
C            +62       F   .......................... 8          -.02
C            +63       F   .......................... 9           .02
C            +64       F   ..........................10          -.02
C            +65       F   ..........................11           .02
C            +66       F   ..........................12          -.02
C            +67       F   ..........................13           .02
C            +68       F   ..........................14          -.02
C            +69       F   ..........................15           .02
C
C  for each cell volume of tube type: ( J = 69 + 
C                               (volume - 1) * parameters per volume )
C
C            J+1       H   Cell volume name                                 
C             +2       F   inner radius of volume
C             +3       F   outer radius of volume
C             +4       F   half-thickness of volume 
C             +5       F   X position of volume
C             +6       F   Y position of volume
C             +7       F   Z position of volume
C             +8       H   Subdivision volume name
C             +9       H   Volume in which to position this volume ('FWPH')
C
C     The structure of geometry for the phi chamber is:
C        FDC                           : mother volume of FDC   (in FGEH)
C         --->FTH                      : theta chambers         (in FWAL)
C         --->FPH                      : phi chamber            (in FWAL)
C            --->FDPH                  : phi chamber
C               --->FPCN               : sensitive gas sub-volumes (6)
C               --->FPCP               : sensitive gas sectors per sub-vol (6)
C               --->FPCZ               : sensitive gas cells (6x6x16)
C
C---------------------------------------------------------------------------
C
C  'TUBS' phi chamber drift cell (undivided)...
      DATA XFPC/  11.00, 61.29, 7.03/
C  phi chamber sensitive cell...
      DATA YFPC/ 0.00, 0.00,  8.10/
C
C  Fill the arrays with the parameters
C
      IC ( LFDPH + 1 ) = NUMSEC
      IC ( LFDPH + 2 ) = NUMWIR
      IC ( LFDPH + 3 ) = NPARVL
      IC ( LFDPH + 4 ) = NUMVOL
C
      CALL UCTOH(SHAPE, C(LFDPH+5), 4, 4 )
C
      LKFDPH = LFDPH + 5
      CALL UCOPY ( ZWI,   C(LKFDPH+1), NUMWIR )
      CALL UCOPY ( ZHF,   C(LKFDPH+17), NUMWIR )
      CALL UCOPY ( ZWV,   C(LKFDPH+33), NUMWIR )
      CALL UCOPY ( STAGG, C(LKFDPH+49), NUMWIR)
      LKFDPH = LKFDPH + 4*NUMWIR
C
      DO 10 I=0,5
      CALL UCTOH(FPPN(I), C(LKFDPH+1), 4, 4 )
      C  ( LKFDPH + 2 ) = XFPC(1)
      C  ( LKFDPH + 3 ) = XFPC(2)
      C  ( LKFDPH + 4 ) = XFPC(3)
      C  ( LKFDPH + 5 ) = YFPC(1)
      C  ( LKFDPH + 6 ) = YFPC(2)
      C  ( LKFDPH + 7 ) = YFPC(3)
      CALL UCTOH(FPZN(I), C(LKFDPH+8), 4, 4 )
      CALL UCTOH(FPCN(I), C(LKFDPH+9), 4, 4 )
      LKFDPH = LKFDPH + NPARVL
   10 CONTINUE
C
C------------------------------------------------------------------------
  999 RETURN
      END
