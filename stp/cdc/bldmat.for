      SUBROUTINE BLDMAT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill banks DMAT hanging from DGEH
C-                         Actually, the data are directly given in this
C-                         routine
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  16-FEB-1988   Ghita Rahal-Callot
C-   Updated  25-MAR-1992   Qizhong Li-Demarteau  added material 86 and 87 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDMAT.LINK'
      INTEGER NMAT, NWMAT, LDMAT, NFORM
C
C ****  Material bank
C
C             +1       I   Number of material                      8
C             +2       I   Number of words per material            11
C  for each material
C
C             +1       I   Material number  ( 80 to 85 )
C             +2 to +6 H   Material name ( up to 20 characters )
C             +7       F   Atomic weight <A>
C             +8       F   Atomic number <Z>
C             +9       F   Density in g/cm3
C            +10       F   Radiation length (cm)
C            +11       F   Absorption length (cm)
C
C     80  'AIR$                '  14.61  7.3     1.205E-3  30423. 67500.
C     81  'ROHA+CARBON$        '  14.61  7.3     0.288     148.   296.
C     82  'ROHA+DELAY$         '  14.61  7.3     0.323     110.   271.
C     83  'G10$                '  14.61  7.3     1.7       19.4   53.1
C     84  'GAS-ARGON$          '  39.95  18.     1.78E-3   12120. 72400. 
C     85  'ALMINUM$            '  26.98  13.     2.7       8.9    37.2
C     86  'PVC+Cu$             '  62.50  32.     1.3       8.38   114.8
C     87  'PVC+G10+Cu$         '  35.09  19.     2.1       3.53   30.62
C
      CALL MZFORM ( 'DMAT', '2I / 1I 5H 5F', NFORM)
      NMAT = 8
      NWMAT = 11
      CALL MZBOOK ( IXSTP, LDMAT, LDGEH, -IZDMAT, 'DMAT', 0, 0,
     &              NMAT*NWMAT+2, NFORM, 0 )
      IC ( LDMAT + 1 ) = NMAT
      IC ( LDMAT + 2 ) = NWMAT
C
C ****  Fill for each material
C
      LDMAT = LDMAT + 2
      IC ( LDMAT + 1 ) = 80
      CALL UCTOH('AIR$                ', C(LDMAT+2), 4, 20 )
      C  ( LDMAT + 7 ) = 14.61
      C  ( LDMAT + 8 ) =  7.3
      C  ( LDMAT + 9 ) = 1.205E-03
      C  ( LDMAT + 10) = 30423.
      C  ( LDMAT + 11) = 67500
C
      LDMAT = LDMAT + 11
      IC ( LDMAT + 1 ) = 81
      CALL UCTOH('ROHA+CARBONE$       ', C(LDMAT+2), 4, 20 )
      C  ( LDMAT + 7 ) = 14.61
      C  ( LDMAT + 8 ) =  7.3
      C  ( LDMAT + 9 ) = .288
      C  ( LDMAT + 10) = 148.
      C  ( LDMAT + 11) = 296.
C
      LDMAT = LDMAT + 11
      IC ( LDMAT + 1 ) = 82
      CALL UCTOH('ROHA+DELAY$         ', C(LDMAT+2), 4, 20 )
      C  ( LDMAT + 7 ) = 14.61
      C  ( LDMAT + 8 ) =  7.3
      C  ( LDMAT + 9 ) = .323
      C  ( LDMAT + 10) = 110.
      C  ( LDMAT + 11) = 271.
C
      LDMAT = LDMAT + 11
      IC ( LDMAT + 1 ) = 83
      CALL UCTOH('G10$                ', C(LDMAT+2), 4, 20 )
      C  ( LDMAT + 7 ) = 14.61
      C  ( LDMAT + 8 ) =  7.3   
      C  ( LDMAT + 9 ) = 1.7
      C  ( LDMAT + 10) = 19.4
      C  ( LDMAT + 11) = 53.1
C
      LDMAT = LDMAT + 11
      IC ( LDMAT + 1 ) = 84
      CALL UCTOH('GAS-ARGON$          ', C(LDMAT+2), 4, 20 )
      C  ( LDMAT + 7 ) = 39.95
      C  ( LDMAT + 8 ) =  18.
      C  ( LDMAT + 9 ) = 1.78E-3
      C  ( LDMAT + 10) = 12120.
      C  ( LDMAT + 11) = 72400.
C
      LDMAT = LDMAT + 11
      IC ( LDMAT + 1 ) = 85
      CALL UCTOH('ALUMINIUM$          ', C(LDMAT+2), 4, 20 )
      C  ( LDMAT + 7 ) = 26.98
      C  ( LDMAT + 8 ) =  13.
      C  ( LDMAT + 9 ) = 2.7
      C  ( LDMAT + 10) = 8.9
      C  ( LDMAT + 11) = 37.2
C
      LDMAT = LDMAT + 11
      IC ( LDMAT + 1 ) = 86
      CALL UCTOH('PVC+Cu$             ', C(LDMAT+2), 4, 20 )
      C  ( LDMAT + 7 ) = 62.5
      C  ( LDMAT + 8 ) = 32.
      C  ( LDMAT + 9 ) = 1.3
      C  ( LDMAT + 10) = 8.38
      C  ( LDMAT + 11) = 114.8
C
      LDMAT = LDMAT + 11
      IC ( LDMAT + 1 ) = 87
      CALL UCTOH('PVC+G10+Cu$         ', C(LDMAT+2), 4, 20 )
      C  ( LDMAT + 7 ) = 35.09
      C  ( LDMAT + 8 ) = 19.
      C  ( LDMAT + 9 ) = 2.1
      C  ( LDMAT + 10) = 3.53
      C  ( LDMAT + 11) = 30.62
C
  999 RETURN
      END
