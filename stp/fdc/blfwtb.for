      SUBROUTINE BLFWTB
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Builds the bank FWTB hanging from FGEH. This banks
C-                         contains the design values for all the passive
C-                         material and is mainly used in D0GEANT. 
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
      INCLUDE 'D0$LINKS:IZFWAL.LINK'
      INCLUDE 'D0$LINKS:IZFWTB.LINK'
C
      INTEGER NUMVOL, NUMSPVOL, NPARVL, I, LKFWTB
      INTEGER NFORMA
      PARAMETER (NUMVOL=88, NUMSPVOL=3, NPARVL = 9 )
C
      REAL XFXX(6),XFXY(6),XFXZ(5)
      REAL XICX(3,7),XFGX(3,11) 
      REAL XFBUX(3,7),XFHGX(3,7),XFAGX(3,7),XFGSX(3,6)
      REAL XFWGX(3),XFSIX(3),XFRIX(3),XFTBX(4,3)
C
      REAL YFXX(6),YFXY(6),YFXZ(5)
      REAL YICX(3,7),YFGX(3,11)
      REAL YFBUX(3,7),YFHGX(3,7),YFAGX(3,7),YFGSX(3,6)
      REAL YFWGX(3,2),YFSIX(3,2),YFRIX(3),YFTBX(3,3) 
c
      CHARACTER*4 CHFRBX(6),CHFHX(6),CHFAX(6),CHFGX(11)
      CHARACTER*4 CHFULX(7),CHFURX(7),CHFHGX(7),CHFAGX(7),CHFCX(6)
      CHARACTER*4 CFGSLX(6),CFGSRX(6) 
      CHARACTER*4 CHFWGX(2),CHFSIX(2),CHFRIX(1),CHFTBX(3) 
C
      CHARACTER*4 VOLMOT, THESHP, ODDSHP, VOLMTR(6), VOLICM(11)
C
      DATA VOLMOT,THESHP,ODDSHP / 'FWTB','BOX ','TRD1'/
      DATA VOLMTR / 'FWB0','FWB1','FWB2','FWB3','FWB4','FWB5'/
      DATA VOLICM / 'FIB0','FIB1','FIB2','FIB3','FIB4','FIB5','FIB6',
     &  'FWTB','FWTB','FWTB','FWTB'/
C----------------------------------------------------------------------
C
C  Book Theta B chamber passive material pieces bank
C
      CALL MZFORM ( 'FWTB', '3I 1H 2I 1H 2I / 1H 6F 1I 1H', NFORMA)
      CALL MZBOOK ( IDVSTP, LFWTB, LFWAL, -IZFWTB, 'FWTB', 0, 0,
     &              9 + NUMVOL*NPARVL, NFORMA, 0 )
C
C     Design values for FDC pieces that are passive material.
C
C    LC     C/IC
C
C             +1       I   Number of volumes                     88
C             +2       I   Number of parameters for each volume   9
C             +3       I   Number of types of volumes             2
C
C  for each type:  ( J = 3 + (type - 1) * 3 )
C
C            J+1       H   Volume type               'box ','trd1'
C             +2       I   Number of type                85, 3 
C             +3       I   Number of special 'diagonal' 
C                          reflection volumes             3, 0 
C
C  for each box volume:   ( J = 3 + Number of types * 3 +
C                            (box volume - 1) * parameters per volume )
C
C            J+1       H   Volume name                                 
C             +2       F   X half-width of volume
C             +3       F   Y half-width of volume
C             +4       F   Z half-width of volume 
C             +5       F   X position of volume
C             +6       F   Y position of volume
C             +7       F   Z position of volume
C             +8       I   Material number ('box' volumes only)
C             +9       H   Volume in which to position this volume ('FWTB')
C
C  for each trd1 volume: ( J = 3 + Number of types * 3 +
C                          (number of box types * parameters per volume) +
C                          (trd1 volume - 1) * parameters per volume )
C
C            J+1       H   Volume name                                 
C             +2       F   X half-width at low z of volume
C             +3       F   X half-width at high z of volume
C              -       -   Y half-width of volume is fixed at 0.40
C             +4       F   Z half-width of volume 
C             +5       F   X position of volume
C             +6       F   Y position of volume
C             +7       F   Z position of volume
C             +8       I   Rotation ('trd1' volumes only)
C             +9       H   Volume in which to position this volume ('FWTB')
C
C     The structure of geometry for the theta B chamber is:
C        FDC                           : mother volume of FDC (in FGEH)
C         --->FTH                      : full theta volume 
C                                      :        (2 A's, 2 B's)(in FWAL)
C            --->FWTB                  : theta chamber B, passive
C               --->FWB0 to FWB5       : theta sectors of B
C                  --->FBX1 to FBX6    : bottom rohacell layer (6)
C                  --->FHX1 to FHX6    : top honeycomb/rohacell layer (6)
C                  --->FCX1 to FCX6    : internal cabling layer (6)
C                  --->FAX1 to FAX6    : aluminium lid (6)
C               --->FIA0 to FIA6       : intercell walls (7)
C                  --->GX1, GX7 to GX11: G10 walls (6)
C                  --->GX2 to GX6      : aluminium walls (5)
C                  --->ULX1 and ULX7   : not there (2)
C                  --->ULX2 to ULX6    : G10 left wall build ups (5)
C                  --->URX1 to URX7    : G10 right wall build ups (5)
C                  --->HGX1 and HGX7   : not there (2)
C                  --->HGX2 to HGX6    : top honeycomb layer (5)
C                  --->AGX1 and AGX7   : not there (2)
C                  --->AGX2 to AGX6    : aluminium lid (5)
C               --->                   : G10 side walls to cells
C               --->SLX1 to SLX6       : G10 left side walls (6)
C               --->SRX1 to SRX6       : G10 right side walls (6)
C               --->                   : G10 extras
C               --->WGX1 to WGX2       : G10 wings off back (2)
C               --->TBX1 to TBX3       : G10 tabs front and back (3)
C               --->SIX1 to SIX2       : G10 square gas intake (2)
C               --->RIX                : G10 rectangular N intake
C
C*************************************************************************
C
C  Names of pieces
C
      DATA CHFHX/'FHX1','FHX2','FHX3','FHX4','FHX5','FHX6'/
      DATA CHFAX/'FAX1','FAX2','FAX3','FAX4','FAX5','FAX6'/
      DATA CHFCX/'FCX1','FCX2','FCX3','FCX4','FCX5','FCX6'/
      DATA CHFRBX/'FBX1','FBX2','FBX3','FBX4','FBX5','FBX6'/
      DATA CHFGX/'GX1 ','GX2 ','GX3 ','GX4 ','GX5 ','GX6 ',
     &   'GX7 ','GX8 ','GX9 ','GX10 ','GX11 '/
      DATA CHFULX/'ULX1','ULX2','ULX3','ULX4','ULX5',
     &  'ULX6','ULX7'/
      DATA CHFURX/'URX1','URX2','URX3','URX4','URX5',
     &  'URX6','URX7'/
      DATA CHFHGX/'HGX1','HGX2','HGX3','HGX4','HGX5',
     &  'HGX6','HGX7'/
      DATA CHFAGX/'AGX1','AGX2','AGX3','AGX4','AGX5',
     &  'AGX6','AGX7'/
      DATA CFGSLX/'SLX1','SLX2','SLX3','SLX4','SLX5','SLX6'/
      DATA CFGSRX/'SRX1','SRX2','SRX3','SRX4','SRX5','SRX6'/
      DATA CHFWGX/'WGX1','WGX2'/
      DATA CHFTBX/'TBX1','TBX2','TBX3'/
      DATA CHFSIX/'SIX1','SIX2'/
      DATA CHFRIX/'RIX '/
C
C  Sizes of pieces
C
C  'BOX' y-drift cell dimensions(in 6 drfit sectors are horiz top honeycomb 
C            layer, aluminium lid, top rohacell layer, bottom rohacell layer,
C            and internal cabling)...
      DATA XFXX / 2.87, 2.87, 2.87, 5.33, 5.33, 5.33/
      DATA XFXY / 10.14, 16.10, 22.05, 28.01, 38.89, 27.35/
      DATA XFXZ / 5.045, 0.355, 0.08, 0.343, 0.034/
C  'BOX'  x-drift cells(vertical intercell G10 walls)...
      DATA XICX/ 0.33, 10.14, 5.05,
     2           0.11, 16.10, 5.05,
     3           0.11, 22.05, 5.05,
     4           0.11, 28.01, 5.05,
     5           0.11, 38.89, 5.05,
     6           0.11, 38.89, 5.05,
     7           0.33, 19.47, 5.05/
C  'BOX'  x-drift cells(vertical intercell G10 walls)...
      DATA XFGX/ 0.33, 10.14, 4.97,
     2           0.11, 16.10, 3.69,
     3           0.11, 22.05, 3.69,
     4           0.11, 28.01, 3.69,
     5           0.11, 38.89, 3.69,
     6           0.11, 38.89, 3.69,
     7           0.33, 19.47, 4.97,
     8           0.11,  3.10, 0.00,
     9           0.11,  3.10, 0.00,
     1           0.33,  3.94, 5.05,
     2           0.33,  3.94, 5.05/
C  'BOX'  x-drift cells(vertical intercell build up walls)
      DATA XFBUX/ 0.33, 0.00, 0.00,
     2            0.11, 2.98, 1.36,
     3            0.11, 2.98, 1.36,
     4            0.11, 2.98, 1.36,
     5            0.11, 5.44, 1.36,
     6            0.11, 5.77, 1.36,
     7            0.33, 0.00, 0.00/
C  'BOX'  x-drift cells(vertical intercell top honeycomb layer)...
      DATA XFHGX/ 0.33, 10.14, 0.00,
     2            0.11, 16.10, 0.18,
     3            0.11, 22.05, 0.18,
     4            0.11, 28.01, 0.18,
     5            0.11, 38.89, 0.18,
     6            0.11, 38.89, 0.18,
     7            0.33, 27.35, 0.00/
C  'BOX'  x-drift cells(vertical intercell aluminium lid)...
      DATA XFAGX/ 0.33, 10.14, 0.08,
     2            0.11, 16.10, 0.08,
     3            0.11, 22.05, 0.08,
     4            0.11, 28.01, 0.08,
     5            0.11, 38.89, 0.08,
     6            0.11, 38.89, 0.08,
     7            0.33, 19.47, 0.08/
C  'BOX'  x-drift cells(vertical G10 side walls)...
      DATA XFGSX/ 3.19, 0.16, 5.05,
     2            2.98, 0.16, 5.05,
     3            2.98, 0.16, 5.05,
     4            5.44, 0.16, 5.05,
     5            5.55, 0.16, 5.05,
     6            5.66, 0.16, 5.05/
C  'BOX'  x-drift cells(vertical G10 wings)...
      DATA XFWGX/ 3.56, 2.86, 0.40/
C  'TRD1' x-drift cells(vertical G10 tabs)...
      DATA XFTBX/ 9.90,  5.56, 0.40, 1.42,
     2           13.97, 13.97, 0.40, 0.98,
     3            0.01, 13.97, 0.40, 0.75/
C  'BOX'  x-drift cells(vertical G10 square gas intakes)...
      DATA XFSIX/ 0.40, 1.91, 1.91/
C  'BOX'  x-drift cells(vertical G10 rectangular N intake)...
      DATA XFRIX/ 0.40, 1.91, 1.59/
C
C   Positions of pieces
C
C  'BOX' y-drift cell positions(in 6 drift sectors are horiz top honeycomb 
C            layer, aluminium lid, top rohacell layer, bottom rohacell layer,
C            and internal cabling)...
C          YFXY is always 0.00
      DATA YFXX/ 13.87, 19.82, 25.78, 34.19, 45.08, 55.96/
      DATA YFXZ/ -5.045, -7.385, -10.01, -0.343, -7.774/
C  x-drift intercell walls...
      DATA YICX/ 10.67,   0.00, -5.05,
     2           16.84,   0.00, -5.05,
     3           22.80,   0.00, -5.05,
     4           28.75,   0.00, -5.05,
     5           39.64,   0.00, -5.05,
     6           50.52,   0.00, -5.05,
     7           61.62,   0.00, -5.05/
C  x-drift intercell walls...
      DATA YFGX/ 0.00,   0.00, -4.97,
     2           0.00,   0.00, -3.69,
     3           0.00,   0.00, -3.69,
     4           0.00,   0.00, -3.69,
     5           0.00,   0.00, -3.69,
     6           0.00,   0.00, -3.69,
     7           0.00,   0.00, -4.97,
     8           10.89,  13.00, -5.05,
     9           10.89, -13.00, -5.05,
     1           61.62,  23.41, -5.05,
     2           61.62, -23.41, -5.05/
C  x-drift intercell wall build ups...
      DATA YFBUX/ 0.00, 10.14, -10.10,
     2            0.00, 13.12,  -8.74,
     3            0.00, 19.08,  -8.74,
     4            0.00, 25.03,  -8.74,
     5            0.00, 33.45,  -8.74,
     6            0.00, 33.12,  -8.74,
     7            0.00, 27.35, -10.10/
C  x-drift intercell top honeycomb layer...
      DATA YFHGX/ 0.00, 0.00, -10.1,
     2            0.00, 0.00, -7.56,
     3            0.00, 0.00, -7.56,
     4            0.00, 0.00, -7.56,
     5            0.00, 0.00, -7.56,
     6            0.00, 0.00, -7.56,
     7            0.00, 0.00, -10.1/
C  x-drift intercell aluminium lid...
      DATA YFAGX/ 0.00, 0.00, -10.01,
     2            0.00, 0.00, -10.01,
     3            0.00, 0.00, -10.01,
     4            0.00, 0.00, -10.01,
     5            0.00, 0.00, -10.01,
     6            0.00, 0.00, -10.01,
     7            0.00, 0.00, -10.01/
C  x-drift cell side walls...
      DATA YFGSX/ 13.54, 10.30, -5.05,
     2            19.71, 16.26, -5.05,
     3            25.67, 22.21, -5.05,
     4            34.08, 28.17, -5.05,
     5            45.08, 39.05, -5.05,
     6            56.29, 27.51, -5.05/
C     (Note: For YFGSX on other end of cells use -1*YFGSX(2,I))
C  x-drift cell wings...
      DATA YFWGX/ 54.19,  30.53, -0.40,
     2            54.19, -30.53, -0.40/
C  x-drift cell tabs...
      DATA YFTBX/  8.92, 0.00, -0.40,
     2            62.93, 0.00, -0.40,
     3            64.66, 0.00, -0.40/
C  x-drift cell square gas intakes...
      DATA YFSIX/  9.94, -5.39, -3.86,
     2            62.35, -7.62, -3.86/
C  x-drift cell rectangular N intake...
      DATA YFRIX/ 9.94, 4.77, -8.34/
C
C  Fill the arrays with the parameters
C
      IC ( LFWTB + 1 ) = NUMVOL
      IC ( LFWTB + 2 ) = NPARVL
      IC ( LFWTB + 3 ) = 2
C
      LKFWTB = LFWTB + 3
      CALL UCTOH(THESHP, C(LKFWTB+1), 4, 4 )
      IC ( LKFWTB + 2 ) = 85
      IC ( LKFWTB + 3 ) = 3
C
      LKFWTB = LKFWTB + 3
      CALL UCTOH(ODDSHP, C(LKFWTB+1), 4, 4 )
      IC ( LKFWTB + 2 ) = 3
      IC ( LKFWTB + 3 ) = 0
C
      LKFWTB = LKFWTB + 3
      DO I = 1, 6
      CALL UCTOH(VOLMTR(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFXX(I)
      C  ( LKFWTB + 3 ) = XFXY(I)
      C  ( LKFWTB + 4 ) = XFXZ(1)
      C  ( LKFWTB + 5 ) = YFXX(I)
      C  ( LKFWTB + 6 ) = 0.0
      C  ( LKFWTB + 7 ) = YFXZ(1)
      IC ( LKFWTB + 8 ) = 91
      CALL UCTOH(VOLMOT, C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 1, 7
      CALL UCTOH(VOLICM(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XICX(1,I)
      C  ( LKFWTB + 3 ) = XICX(2,I)
      C  ( LKFWTB + 4 ) = XICX(3,I)
      C  ( LKFWTB + 5 ) = YICX(1,I)
      C  ( LKFWTB + 6 ) = YICX(2,I)
      C  ( LKFWTB + 7 ) = YICX(3,I)
      IC ( LKFWTB + 8 ) = 91
      CALL UCTOH(VOLMOT, C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 1, 6
      CALL UCTOH(CHFHX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFXX(I)
      C  ( LKFWTB + 3 ) = XFXY(I)
      C  ( LKFWTB + 4 ) = XFXZ(2)
      C  ( LKFWTB + 5 ) = 0.0 
      C  ( LKFWTB + 6 ) = 0.0
      C  ( LKFWTB + 7 ) = YFXZ(2)
      IC ( LKFWTB + 8 ) = 94
      CALL UCTOH(VOLMTR(I), C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 1, 6
      CALL UCTOH(CHFAX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFXX(I)
      C  ( LKFWTB + 3 ) = XFXY(I)
      C  ( LKFWTB + 4 ) = XFXZ(3)
      C  ( LKFWTB + 5 ) = 0.0 
      C  ( LKFWTB + 6 ) = 0.0
      C  ( LKFWTB + 7 ) = YFXZ(3)
      IC ( LKFWTB + 8 ) = 93
      CALL UCTOH(VOLMTR(I), C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 1, 6
      CALL UCTOH(CHFRBX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFXX(I)
      C  ( LKFWTB + 3 ) = XFXY(I)
      C  ( LKFWTB + 4 ) = XFXZ(4)
      C  ( LKFWTB + 5 ) = 0.0 
      C  ( LKFWTB + 6 ) = 0.0
      C  ( LKFWTB + 7 ) = YFXZ(4)
      IC ( LKFWTB + 8 ) = 95
      CALL UCTOH(VOLMTR(I), C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 1, 6
      CALL UCTOH(CHFCX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFXX(I)
      C  ( LKFWTB + 3 ) = XFXY(I)
      C  ( LKFWTB + 4 ) = XFXZ(5)
      C  ( LKFWTB + 5 ) = 0.0
      C  ( LKFWTB + 6 ) = 0.0
      C  ( LKFWTB + 7 ) = YFXZ(5)
      IC ( LKFWTB + 8 ) = 98
      CALL UCTOH(VOLMTR(I), C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 1, 11
      CALL UCTOH(CHFGX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFGX(1,I)
      C  ( LKFWTB + 3 ) = XFGX(2,I)
      C  ( LKFWTB + 4 ) = XFGX(3,I)
      C  ( LKFWTB + 5 ) = YFGX(1,I)
      C  ( LKFWTB + 6 ) = YFGX(2,I)
      C  ( LKFWTB + 7 ) = YFGX(3,I)
      IC ( LKFWTB + 8 ) = 96
      CALL UCTOH(VOLICM(I), C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 2, 6
      CALL UCTOH(CHFULX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFBUX(1,I)
      C  ( LKFWTB + 3 ) = XFBUX(2,I)
      C  ( LKFWTB + 4 ) = XFBUX(3,I)
      C  ( LKFWTB + 5 ) = YFBUX(1,I)
      C  ( LKFWTB + 6 ) = YFBUX(2,I)
      C  ( LKFWTB + 7 ) = YFBUX(3,I)
      IC ( LKFWTB + 8 ) = 96
      CALL UCTOH(VOLICM(I), C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 2, 6
      CALL UCTOH(CHFURX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFBUX(1,I)
      C  ( LKFWTB + 3 ) = XFBUX(2,I)
      C  ( LKFWTB + 4 ) = XFBUX(3,I)
      C  ( LKFWTB + 5 ) = YFBUX(1,I)
      C  ( LKFWTB + 6 ) = -1*YFBUX(2,I)
      C  ( LKFWTB + 7 ) = YFBUX(3,I)
      IC ( LKFWTB + 8 ) = 96
      CALL UCTOH(VOLICM(I), C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 2, 6
      CALL UCTOH(CHFHGX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFHGX(1,I)
      C  ( LKFWTB + 3 ) = XFHGX(2,I)
      C  ( LKFWTB + 4 ) = XFHGX(3,I)
      C  ( LKFWTB + 5 ) = YFHGX(1,I)
      C  ( LKFWTB + 6 ) = YFHGX(2,I)
      C  ( LKFWTB + 7 ) = YFHGX(3,I)
      IC ( LKFWTB + 8 ) = 94
      CALL UCTOH(VOLICM(I), C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 2, 6
      CALL UCTOH(CHFAGX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFAGX(1,I)
      C  ( LKFWTB + 3 ) = XFAGX(2,I)
      C  ( LKFWTB + 4 ) = XFAGX(3,I)
      C  ( LKFWTB + 5 ) = YFAGX(1,I)
      C  ( LKFWTB + 6 ) = YFAGX(2,I)
      C  ( LKFWTB + 7 ) = YFAGX(3,I)
      IC ( LKFWTB + 8 ) = 93
      CALL UCTOH(VOLICM(I), C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 1, 6
      CALL UCTOH(CFGSLX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFGSX(1,I)
      C  ( LKFWTB + 3 ) = XFGSX(2,I)
      C  ( LKFWTB + 4 ) = XFGSX(3,I)
      C  ( LKFWTB + 5 ) = YFGSX(1,I)
      C  ( LKFWTB + 6 ) = -1*YFGSX(2,I)
      C  ( LKFWTB + 7 ) = YFGSX(3,I)
      IC ( LKFWTB + 8 ) = 96
      CALL UCTOH(VOLMOT, C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 1, 6
      CALL UCTOH(CFGSRX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFGSX(1,I)
      C  ( LKFWTB + 3 ) = XFGSX(2,I)
      C  ( LKFWTB + 4 ) = XFGSX(3,I)
      C  ( LKFWTB + 5 ) = YFGSX(1,I)
      C  ( LKFWTB + 6 ) = YFGSX(2,I)
      C  ( LKFWTB + 7 ) = YFGSX(3,I)
      IC ( LKFWTB + 8 ) = 96
      CALL UCTOH(VOLMOT, C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 1, 2
      CALL UCTOH(CHFWGX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFWGX(1)
      C  ( LKFWTB + 3 ) = XFWGX(2)
      C  ( LKFWTB + 4 ) = XFWGX(3)
      C  ( LKFWTB + 5 ) = YFWGX(1,I)
      C  ( LKFWTB + 6 ) = YFWGX(2,I)
      C  ( LKFWTB + 7 ) = YFWGX(3,I)
      IC ( LKFWTB + 8 ) = 96
      CALL UCTOH(VOLMOT, C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      DO I = 1, 2
      CALL UCTOH(CHFSIX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFSIX(1)
      C  ( LKFWTB + 3 ) = XFSIX(2)
      C  ( LKFWTB + 4 ) = XFSIX(3)
      C  ( LKFWTB + 5 ) = YFSIX(1,I)
      C  ( LKFWTB + 6 ) = YFSIX(2,I)
      C  ( LKFWTB + 7 ) = YFSIX(3,I)
      IC ( LKFWTB + 8 ) = 96
      CALL UCTOH(VOLMOT, C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
      CALL UCTOH(CHFRIX(1), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFRIX(1)
      C  ( LKFWTB + 3 ) = XFRIX(2)
      C  ( LKFWTB + 4 ) = XFRIX(3)
      C  ( LKFWTB + 5 ) = YFRIX(1)
      C  ( LKFWTB + 6 ) = YFRIX(2)
      C  ( LKFWTB + 7 ) = YFRIX(3)
      IC ( LKFWTB + 8 ) = 96
      CALL UCTOH(VOLMOT, C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
C
      DO I = 1, 3
      CALL UCTOH(CHFTBX(I), C(LKFWTB+1), 4, 4 )
      C  ( LKFWTB + 2 ) = XFTBX(1,I)
      C  ( LKFWTB + 3 ) = XFTBX(2,I)
      C  ( LKFWTB + 4 ) = XFTBX(4,I)
      C  ( LKFWTB + 5 ) = YFTBX(1,I)
      C  ( LKFWTB + 6 ) = YFTBX(2,I)
      C  ( LKFWTB + 7 ) = YFTBX(3,I)
      IC ( LKFWTB + 8 ) = 9005
      CALL UCTOH(VOLMOT, C(LKFWTB+9), 4, 4 )
      LKFWTB = LKFWTB + NPARVL
      ENDDO
C
C----------------------------------------------------------------------
  999 RETURN
      END
