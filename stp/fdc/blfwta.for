      SUBROUTINE BLFWTA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Builds the bank FWTA hanging from FGEH. This banks
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
      INCLUDE 'D0$LINKS:IZFWTA.LINK'
C
      INTEGER NUMVOL, NUMSPVOL, NPARVL, I, LKFWTA
      INTEGER NFORMA
      PARAMETER (NUMVOL=88, NUMSPVOL=3, NPARVL = 9 )
C
      REAL XFYX(6),XFYY(6),XFYZ(5)
      REAL XICY(3,7),XFGY(3,11) 
      REAL XFBUY(3,7),XFHGY(3,7),XFAGY(3,7),XFGSY(3,6)
      REAL XFWGY(3),XFSIY(3),XFRIY(3),XFTBY(4,3)
C
      REAL YFYX(6),YFYY(6),YFYZ(5)
      REAL YICY(3,7),YFGY(3,11)
      REAL YFBUY(3,7),YFHGY(3,7),YFAGY(3,7),YFGSY(3,6)
      REAL YFWGY(3,2),YFSIY(3,2),YFRIY(3),YFTBY(3,3) 
C
      CHARACTER*4 CHFRBY(6),CHFHY(6),CHFAY(6),CHFGY(11)
      CHARACTER*4 CHFULY(7),CHFURY(7),CHFHGY(7),CHFAGY(7),CHFCY(6)
      CHARACTER*4 CFGSLY(6),CFGSRY(6) 
      CHARACTER*4 CHFWGY(2),CHFSIY(2),CHFRIY(1),CHFTBY(3) 
C
      CHARACTER*4 VOLMOT, THESHP, ODDSHP, VOLMTR(6), VOLICM(11)
C
      DATA VOLMOT,THESHP,ODDSHP / 'FWTA','BOX ','TRD1'/
      DATA VOLMTR / 'FWA0','FWA1','FWA2','FWA3','FWA4','FWA5'/
      DATA VOLICM / 'FIA0','FIA1','FIA2','FIA3','FIA4','FIA5','FIA6',
     &  'FWTA','FWTA','FWTA','FWTA'/
C----------------------------------------------------------------------
C
C   Book the Theta A chamber passive material pieces bank FWTA
C
      CALL MZFORM ( 'FWTA', '3I 1H 2I 1H 2I / 1H 6F 1I 1H', NFORMA)
      CALL MZBOOK ( IDVSTP, LFWTA, LFWAL, -IZFWTA, 'FWTA', 0, 0,
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
C            J+1       H   Volume type                    'box ','trd1'
C             +2       I   Number of type                     85, 3 
C             +3       I   Number of special 'diagonal' 
C                          reflection volumes                  3, 0 
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
C             +9       H   Volume in which to position this volume ('FWTA')
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
C             +9       H   Volume in which to position this volume ('FWTA')
C
C     The structure of geometry for the theta A chamber is:
C        FDC                           : mother volume of FDC (in FGEH)
C         --->FTH                      : full theta volume
C                                      :         (2 A's,2 B's)(in FWAL)
C            --->FWTA                  : theta quadrant A, passive
C               --->FWA0 to FWA5       : theta sectors of A
C                  --->FBY1 to FBY6    : bottom rohacell layer (6)
C                  --->FHY1 to FHY6    : top honeycomb/rohacell layer (6)
C                  --->FCY1 to FCY6    : internal cabling layer (6)
C                  --->FAY1 to FAY6    : aluminium lid (6)
C               --->FIA0 TO FIA6       : intercell walls
C                  --->GY1, GY7 to GY11: G10 walls (6)
C                  --->GY2 to GY6      : aluminium walls (5)
C                  --->ULY1 and ULY7   : not there (2)
C                  --->ULY2 to ULY6    : G10 left wall build ups (5)
C                  --->URY1 to URY7    : G10 right wall build ups (5)
C                  --->HGY1 and HGY7   : not there (2)
C                  --->HGY2 to HGY6    : top honeycomb layer (5)
C                  --->AGY1 and AGY7   : not there (2)
C                  --->AGY2 to AGY6    : aluminium lid (5)
C               --->                   : G10 side walls to cells
C               --->SLY1 to SLY6       : G10 left side walls (6)
C               --->SRY1 to SRY6       : G10 right side walls (6)
C               --->                   : G10 extras
C               --->WGY1 to WGY2       : G10 wings off back (2)
C               --->TBY1 to TBY3       : G10 tabs front and back (3)
C               --->SIY1 to SIY2       : G10 square gas intake (2)
C               --->RIY                : G10 rectangular N intake
C
C*************************************************************************
C
C  Names of pieces
C
      DATA CHFHY/'FHY1','FHY2','FHY3','FHY4','FHY5','FHY6'/
      DATA CHFAY/'FAY1','FAY2','FAY3','FAY4','FAY5','FAY6'/
      DATA CHFCY/'FCY1','FCY2','FCY3','FCY4','FCY5','FCY6'/
      DATA CHFRBY/'FBY1','FBY2','FBY3','FBY4','FBY5','FBY6'/
      DATA CHFGY/'GY1 ','GY2 ','GY3 ','GY4 ','GY5 ','GY6 ',
     &   'GY7 ','GY8 ','GY9 ','GY10 ','GY11 '/
      DATA CHFULY/'ULY1','ULY2','ULY3','ULY4','ULY5',
     &   'ULY6','ULY7'/
      DATA CHFURY/'URY1','URY2','URY3','URY4','URY5',
     &   'URY6','URY7'/
      DATA CHFHGY/'HGY1','HGY2','HGY3','HGY4','HGY5',
     &   'HGY6','HGY7'/
      DATA CHFAGY/'AGY1','AGY2','AGY3','AGY4','AGY5',
     &   'AGY6','AGY7'/
      DATA CFGSLY/'SLY1','SLY2','SLY3','SLY4','SLY5','SLY6'/
      DATA CFGSRY/'SRY1','SRY2','SRY3','SRY4','SRY5','SRY6'/
      DATA CHFWGY/'WGY1','WGY2'/
      DATA CHFTBY/'TBY1','TBY2','TBY3'/
      DATA CHFSIY/'SIY1','SIY2'/
      DATA CHFRIY/'RIY '/
C
C  Sizes of pieces
C
C  'BOX' y-drift cell dimensions(in 6 drift sectors are horiz top honeycomb 
C            layer, aluminium lid, top rohacell layer, bottom rohacell layer,
C            and internal cabling)...
      DATA XFYX / 16.10, 22.05, 28.01, 38.89, 41.09, 27.35/
      DATA XFYY / 2.87, 2.87, 2.87, 5.33, 5.33, 5.33/
      DATA XFYZ / 5.045, 0.355, 0.08, 0.343, 0.034/
C  'BOX'  y-drift cells(horizontal intercell G10 walls)...
      DATA XICY/ 9.90, 0.33, 5.05,
     2          22.05, 0.11, 5.05,
     3          28.01, 0.11, 5.05,
     4          38.89, 0.11, 5.05,
     5          41.09, 0.11, 5.05,
     6          41.09, 0.11, 5.05,
     7          19.47, 0.33, 5.05/
C  'BOX'  y-drift cells(horizontal intercell G10 walls)...
      DATA XFGY/ 9.90, 0.33, 4.97,
     2          22.05, 0.11, 3.69,
     3          28.01, 0.11, 3.69,
     4          38.89, 0.11, 3.69,
     5          41.09, 0.11, 3.69,
     6          41.09, 0.11, 3.69,
     7          19.47, 0.33, 4.97,
     8           3.10, 0.11, 5.05,
     9           3.10, 0.11, 5.05,
     1           3.94, 0.33, 5.05,
     2           3.94, 0.33, 5.05/
C  'BOX'  y-drift cells(horizontal intercell build up walls)
      DATA XFBUY/ 0.00, 0.33, 0.00,
     2            2.98, 0.11, 1.36,
     3            2.98, 0.11, 1.36,
     4            5.44, 0.11, 1.36,
     5            1.10, 0.11, 1.36,
     6            6.87, 0.11, 1.36,
     7            0.00, 0.33, 0.00/
C  'BOX'  y-drift cells(horizontal intercell top honeycomb layer)...
      DATA XFHGY/ 16.10, 0.33, 0.00,
     2            22.05, 0.11, 0.18,
     3            28.01, 0.11, 0.18,
     4            38.89, 0.11, 0.18,
     5            41.09, 0.11, 0.18,
     6            41.09, 0.11, 0.18,
     7            27.35, 0.33, 0.00/
C  'BOX'  y-drift cells(horizontal intercell aluminium lid)...
      DATA XFAGY/  9.90, 0.33, 0.08,
     2            22.05, 0.11, 0.08,
     3            28.01, 0.11, 0.08,
     4            38.89, 0.11, 0.08,
     5            41.09, 0.11, 0.08,
     6            41.09, 0.11, 0.08,
     7            19.47, 0.33, 0.08/
C  'BOX'  y-drift cells(horizontal G10 side walls)...
      DATA XFGSY/ 0.16, 2.98, 5.05,
     2            0.16, 2.98, 5.05,
     3            0.16, 2.98, 5.05,
     4            0.16, 5.44, 5.05,
     5            0.16, 5.55, 5.05,
     6            0.16, 5.66, 5.05/
C  'BOX'  y-drift cells(horizontal G10 wings)...
      DATA XFWGY/ 2.86, 3.56, 0.40/
C  'TRD1' y-drift cells(horizontal G10 tabs)...
      DATA XFTBY/ 9.90,  5.56, 0.40, 1.42,
     2           13.97, 13.97, 0.40, 0.98,
     3            0.01, 13.97, 0.40, 0.75/
C  'BOX'  y-drift cells(horizontal G10 square gas intakes)...
      DATA XFSIY/ 1.91, 0.40, 1.91/
C  'BOX'  y-drift cells(horizontal G10 rectangular N intake)...
      DATA XFRIY/ 1.91, 0.40, 1.59/
C
C  Positions of pieces
C
C  'BOX' y-drift cell positions(in 6 drift sectors are horiz top honeycomb 
C            layer, aluminium lid, top rohacell layer, bottom rohacell layer,
C            and internal cabling)...
C          YFYX is always 0.00
      DATA YFYY/ 13.87, 19.82, 25.78, 34.19, 45.08, 55.96/
      DATA YFYZ/ -5.045, -7.385, -10.01, -0.343, -7.774/
C  y-drift intercell walls...
      DATA YICY/  0.00, 10.67, -5.05,
     2            0.00, 16.84, -5.05,
     3            0.00, 22.80, -5.05,
     4            0.00, 28.75, -5.05,
     5            0.00, 39.64, -5.05,
     6            0.00, 50.52, -5.05,
     7            0.00, 61.62, -5.05/
C  y-drift intercell walls...
      DATA YFGY/  0.00, 0.00, -4.97,
     2            0.00, 0.00, -3.69,
     3            0.00, 0.00, -3.69,
     4            0.00, 0.00, -3.69,
     5            0.00, 0.00, -3.69,
     6            0.00, 0.00, -3.69,
     7            0.00, 0.00, -4.97,
     8           13.00, 10.89, -5.05,
     9          -13.00, 10.89, -5.05,
     1           23.41, 61.62, -5.05,
     2          -23.41, 61.62, -5.05/
C  y-drift intercell wall build ups...
      DATA YFBUY/ 16.10, 0.00, -10.10,
     2            19.08, 0.00,  -8.74,
     3            25.03, 0.00,  -8.74,
     4            33.45, 0.00,  -8.74,
     5            39.99, 0.00,  -8.74,
     6            34.22, 0.00,  -8.74,
     7            27.35, 0.00, -10.10/
C  y-drift intercell top honeycomb layer...
      DATA YFHGY/ 0.00, 0.00, -10.1,
     2            0.00, 0.00, -7.56,
     3            0.00, 0.00, -7.56,
     4            0.00, 0.00, -7.56,
     5            0.00, 0.00, -7.56,
     6            0.00, 0.00, -7.56,
     7            0.00, 0.00, -10.1/
C  y-drift intercell aluminium lid...
      DATA YFAGY/ 0.00, 0.00, -10.01,
     2            0.00, 0.00, -10.01,
     3            0.00, 0.00, -10.01,
     4            0.00, 0.00, -10.01,
     5            0.00, 0.00, -10.01,
     6            0.00, 0.00, -10.01,
     7            0.00, 0.00, -10.01/
C  y-drift cell side walls...
      DATA YFGSY/ 16.26, 13.76, -5.05,
     2            22.21, 19.71, -5.05,
     3            28.17, 25.67, -5.05,
     4            39.05, 34.08, -5.05,
     5            41.25, 45.08, -5.05,
     6            27.51, 56.29, -5.05/
C     (Note: For YFGSY on other end of cells use -1*YFGSY(1,I))
C  y-drift cell wings...
      DATA YFWGY/  30.53, 54.19, -0.40,
     2            -30.53, 54.19, -0.40/
C  y-drift cell tabs...
      DATA YFTBY/ 0.00,  8.92, -0.40,
     2            0.00, 62.93, -0.40,
     3            0.00, 64.66, -0.40/
C  y-drift cell square gas intakes...
      DATA YFSIY/  5.39,  9.94, -3.86,
     2             7.62, 62.35, -3.86/
C  y-drift cell rectangular N intake...
      DATA YFRIY/ -4.47, 9.94, -8.34/
C
C  Fill the arrays with the parameters
C
      IC ( LFWTA + 1 ) = NUMVOL
      IC ( LFWTA + 2 ) = NPARVL
      IC ( LFWTA + 3 ) = 2
C
      LKFWTA = LFWTA + 3
      CALL UCTOH(THESHP, C(LKFWTA+1), 4, 4 )
      IC ( LKFWTA + 2 ) = 85
      IC ( LKFWTA + 3 ) = 3
C
      LKFWTA = LKFWTA + 3
      CALL UCTOH(ODDSHP, C(LKFWTA+1), 4, 4 )
      IC ( LKFWTA + 2 ) = 3
      IC ( LKFWTA + 3 ) = 0
C
      LKFWTA = LKFWTA + 3
      DO I = 1, 6
      CALL UCTOH(VOLMTR(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFYX(I)
      C  ( LKFWTA + 3 ) = XFYY(I)
      C  ( LKFWTA + 4 ) = XFYZ(1)
      C  ( LKFWTA + 5 ) = 0.0
      C  ( LKFWTA + 6 ) = YFYY(I)
      C  ( LKFWTA + 7 ) = YFYZ(1)
      IC ( LKFWTA + 8 ) = 91
      CALL UCTOH(VOLMOT, C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 1, 7
      CALL UCTOH(VOLICM(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XICY(1,I)
      C  ( LKFWTA + 3 ) = XICY(2,I)
      C  ( LKFWTA + 4 ) = XICY(3,I)
      C  ( LKFWTA + 5 ) = YICY(1,I)
      C  ( LKFWTA + 6 ) = YICY(2,I)
      C  ( LKFWTA + 7 ) = YICY(3,I)
      IC ( LKFWTA + 8 ) = 91
      CALL UCTOH(VOLMOT, C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 1, 6
      CALL UCTOH(CHFHY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFYX(I)
      C  ( LKFWTA + 3 ) = XFYY(I)
      C  ( LKFWTA + 4 ) = XFYZ(2)
      C  ( LKFWTA + 5 ) = 0.0
      C  ( LKFWTA + 6 ) = 0.0
      C  ( LKFWTA + 7 ) = YFYZ(2)
      IC ( LKFWTA + 8 ) = 94
      CALL UCTOH(VOLMTR(I), C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 1, 6
      CALL UCTOH(CHFAY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFYX(I)
      C  ( LKFWTA + 3 ) = XFYY(I)
      C  ( LKFWTA + 4 ) = XFYZ(3)
      C  ( LKFWTA + 5 ) = 0.0
      C  ( LKFWTA + 6 ) = 0.0 
      C  ( LKFWTA + 7 ) = YFYZ(3)
      IC ( LKFWTA + 8 ) = 93
      CALL UCTOH(VOLMTR(I), C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 1, 6
      CALL UCTOH(CHFRBY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFYX(I)
      C  ( LKFWTA + 3 ) = XFYY(I)
      C  ( LKFWTA + 4 ) = XFYZ(4)
      C  ( LKFWTA + 5 ) = 0.0
      C  ( LKFWTA + 6 ) = 0.0 
      C  ( LKFWTA + 7 ) = YFYZ(4)
      IC ( LKFWTA + 8 ) = 95
      CALL UCTOH(VOLMTR(I), C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 1, 6
      CALL UCTOH(CHFCY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFYX(I)
      C  ( LKFWTA + 3 ) = XFYY(I)
      C  ( LKFWTA + 4 ) = XFYZ(5)
      C  ( LKFWTA + 5 ) = 0.0
      C  ( LKFWTA + 6 ) = 0.0 
      C  ( LKFWTA + 7 ) = YFYZ(5)
      IC ( LKFWTA + 8 ) = 98
      CALL UCTOH(VOLMTR(I), C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 1, 11
      CALL UCTOH(CHFGY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFGY(1,I)
      C  ( LKFWTA + 3 ) = XFGY(2,I)
      C  ( LKFWTA + 4 ) = XFGY(3,I)
      C  ( LKFWTA + 5 ) = YFGY(1,I)
      C  ( LKFWTA + 6 ) = YFGY(2,I)
      C  ( LKFWTA + 7 ) = YFGY(3,I)
      IC ( LKFWTA + 8 ) = 96
      CALL UCTOH(VOLICM(I), C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 2, 6
      CALL UCTOH(CHFULY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFBUY(1,I)
      C  ( LKFWTA + 3 ) = XFBUY(2,I)
      C  ( LKFWTA + 4 ) = XFBUY(3,I)
      C  ( LKFWTA + 5 ) = YFBUY(1,I)
      C  ( LKFWTA + 6 ) = YFBUY(2,I)
      C  ( LKFWTA + 7 ) = YFBUY(3,I)
      IC ( LKFWTA + 8 ) = 96
      CALL UCTOH(VOLICM(I), C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 2, 6
      CALL UCTOH(CHFURY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFBUY(1,I)
      C  ( LKFWTA + 3 ) = XFBUY(2,I)
      C  ( LKFWTA + 4 ) = XFBUY(3,I)
      C  ( LKFWTA + 5 ) = -1*YFBUY(1,I)
      C  ( LKFWTA + 6 ) = YFBUY(2,I)
      C  ( LKFWTA + 7 ) = YFBUY(3,I)
      IC ( LKFWTA + 8 ) = 96
      CALL UCTOH(VOLICM(I), C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 2, 6
      CALL UCTOH(CHFHGY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFHGY(1,I)
      C  ( LKFWTA + 3 ) = XFHGY(2,I)
      C  ( LKFWTA + 4 ) = XFHGY(3,I)
      C  ( LKFWTA + 5 ) = YFHGY(1,I)
      C  ( LKFWTA + 6 ) = YFHGY(2,I)
      C  ( LKFWTA + 7 ) = YFHGY(3,I)
      IC ( LKFWTA + 8 ) = 94
      CALL UCTOH(VOLICM(I), C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 2, 6
      CALL UCTOH(CHFAGY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFAGY(1,I)
      C  ( LKFWTA + 3 ) = XFAGY(2,I)
      C  ( LKFWTA + 4 ) = XFAGY(3,I)
      C  ( LKFWTA + 5 ) = YFAGY(1,I)
      C  ( LKFWTA + 6 ) = YFAGY(2,I)
      C  ( LKFWTA + 7 ) = YFAGY(3,I)
      IC ( LKFWTA + 8 ) = 93
      CALL UCTOH(VOLICM(I), C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 1, 6
      CALL UCTOH(CFGSLY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFGSY(1,I)
      C  ( LKFWTA + 3 ) = XFGSY(2,I)
      C  ( LKFWTA + 4 ) = XFGSY(3,I)
      C  ( LKFWTA + 5 ) = -1*YFGSY(1,I)
      C  ( LKFWTA + 6 ) = YFGSY(2,I)
      C  ( LKFWTA + 7 ) = YFGSY(3,I)
      IC ( LKFWTA + 8 ) = 96
      CALL UCTOH(VOLMOT, C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 1, 6
      CALL UCTOH(CFGSRY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFGSY(1,I)
      C  ( LKFWTA + 3 ) = XFGSY(2,I)
      C  ( LKFWTA + 4 ) = XFGSY(3,I)
      C  ( LKFWTA + 5 ) = YFGSY(1,I)
      C  ( LKFWTA + 6 ) = YFGSY(2,I)
      C  ( LKFWTA + 7 ) = YFGSY(3,I)
      IC ( LKFWTA + 8 ) = 96
      CALL UCTOH(VOLMOT, C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 1, 2
      CALL UCTOH(CHFWGY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFWGY(1)
      C  ( LKFWTA + 3 ) = XFWGY(2)
      C  ( LKFWTA + 4 ) = XFWGY(3)
      C  ( LKFWTA + 5 ) = YFWGY(1,I)
      C  ( LKFWTA + 6 ) = YFWGY(2,I)
      C  ( LKFWTA + 7 ) = YFWGY(3,I)
      IC ( LKFWTA + 8 ) = 96
      CALL UCTOH(VOLMOT, C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      DO I = 1, 2
      CALL UCTOH(CHFSIY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFSIY(1)
      C  ( LKFWTA + 3 ) = XFSIY(2)
      C  ( LKFWTA + 4 ) = XFSIY(3)
      C  ( LKFWTA + 5 ) = YFSIY(1,I)
      C  ( LKFWTA + 6 ) = YFSIY(2,I)
      C  ( LKFWTA + 7 ) = YFSIY(3,I)
      IC ( LKFWTA + 8 ) = 96
      CALL UCTOH(VOLMOT, C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
      CALL UCTOH(CHFRIY(1), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFRIY(1)
      C  ( LKFWTA + 3 ) = XFRIY(2)
      C  ( LKFWTA + 4 ) = XFRIY(3)
      C  ( LKFWTA + 5 ) = YFRIY(1)
      C  ( LKFWTA + 6 ) = YFRIY(2)
      C  ( LKFWTA + 7 ) = YFRIY(3)
      IC ( LKFWTA + 8 ) = 96
      CALL UCTOH(VOLMOT, C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
C
      DO I = 1, 3
      CALL UCTOH(CHFTBY(I), C(LKFWTA+1), 4, 4 )
      C  ( LKFWTA + 2 ) = XFTBY(1,I)
      C  ( LKFWTA + 3 ) = XFTBY(2,I)
      C  ( LKFWTA + 4 ) = XFTBY(4,I)
      C  ( LKFWTA + 5 ) = YFTBY(1,I)
      C  ( LKFWTA + 6 ) = YFTBY(2,I)
      C  ( LKFWTA + 7 ) = YFTBY(3,I)
      IC ( LKFWTA + 8 ) = 9006
      CALL UCTOH(VOLMOT, C(LKFWTA+9), 4, 4 )
      LKFWTA = LKFWTA + NPARVL
      ENDDO
C
C-----------------------------------------------------------------------
  999 RETURN
      END
