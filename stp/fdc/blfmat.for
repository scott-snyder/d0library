      SUBROUTINE BLFMAT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create and fill banks FMAT hanging from FGEH.
C-                         The data are given directly in this
C-                         routine
C-   Inputs  :
C-   Outputs :
C-
C-   Created  16-FEB-1988   Ghita Rahal-Callot as BLDMAT.FOR
C-   Altered  12-MAY-1988   Jeffrey Bantly  for FDC detector 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFMAT.LINK'
      INTEGER NMAT, NWMAT, NFORM, LKFMAT
C-----------------------------------------------------------------------
C
C ****  Material bank
C
C             +1       I   Number of material                       8
C             +2       I   Number of words per material            11
C
C  for each material
C
C             +1       I   Material number  ( 91 to 97 )
C             +2 to +6 H   Material name ( up to 20 characters )
C             +7       F   Atomic weight <A>
C             +8       F   Atomic number <Z>
C             +9       F   Density in g/cm3
C            +10       F   Radiation length (cm)
C            +11       F   Absorption length (cm)
C
C     91  'VACUUM$             '  14.61  7.3     1.205e-3  30423. 67500.
C     92  'AIR$                '  14.61  7.3     1.205e-3  30423. 67500.
C     93  'ALUMINUM$           '  26.98  13.     2.7       8.9    37.2
C     94  'HONEYCOMB$          '  14.61  7.3     0.05      800.   1720.
C     95  'ROHACELL$           '  14.61  7.3     0.03      1666.7 2300.
C     96  'G10$                '  14.61  7.3     1.7       19.4   53.1
C     97  'FDC_GAS$            '  39.95  18.     1.78E-3   12120. 72400.
C     98  'INTERNAL_CABLING    '  16.00   8.     2.0       15.0   55.0
C
C***********************************************************************
C
C   Book FDC materials bank FMAT
C
      CALL MZFORM ( 'FMAT', '2I / 1I 5H 5F', NFORM)
      NMAT = 8
      NWMAT = 11
      CALL MZBOOK ( IDVSTP, LFMAT, LFGEH, -IZFMAT, 'FMAT', 0, 0,
     &              NMAT*NWMAT+2, NFORM, 0 )
      IC ( LFMAT + 1 ) = NMAT
      IC ( LFMAT + 2 ) = NWMAT
C
C ****  Fill for each material
C
      LKFMAT = LFMAT + 2
      IC ( LKFMAT + 1 ) = 91
      CALL UCTOH('VACUUM$             ', C(LKFMAT+2), 4, 20 )
      C  ( LKFMAT + 7 ) = 14.61
      C  ( LKFMAT + 8 ) =  7.3
      C  ( LKFMAT + 9 ) = 0.05
      C  ( LKFMAT + 10) = 800.
      C  ( LKFMAT + 11) = 1720.
C
      LKFMAT = LKFMAT + 11
      IC ( LKFMAT + 1 ) = 92
      CALL UCTOH('AIR$                ', C(LKFMAT+2), 4, 20 )
      C  ( LKFMAT + 7 ) = 14.61
      C  ( LKFMAT + 8 ) =  7.3
      C  ( LKFMAT + 9 ) = 1.205e-3
      C  ( LKFMAT + 10) = 30423.
      C  ( LKFMAT + 11) = 67500.
C
      LKFMAT = LKFMAT + 11
      IC ( LKFMAT + 1 ) = 93
      CALL UCTOH('ALUMINIUM$          ', C(LKFMAT+2), 4, 20 )
      C  ( LKFMAT + 7 ) = 26.98
      C  ( LKFMAT + 8 ) =  13.
      C  ( LKFMAT + 9 ) = 2.7
      C  ( LKFMAT + 10) = 8.9
      C  ( LKFMAT + 11) = 37.2
C
      LKFMAT = LKFMAT + 11
      IC ( LKFMAT + 1 ) = 94
      CALL UCTOH('HONEYCOMB$          ', C(LKFMAT+2), 4, 20 )
      C  ( LKFMAT + 7 ) = 14.61
      C  ( LKFMAT + 8 ) =  7.3
      C  ( LKFMAT + 9 ) = 0.05
      C  ( LKFMAT + 10) = 800.
      C  ( LKFMAT + 11) = 1720.
C
      LKFMAT = LKFMAT + 11
      IC ( LKFMAT + 1 ) = 95
      CALL UCTOH('ROHACELL$           ', C(LKFMAT+2), 4, 20 )
      C  ( LKFMAT + 7 ) = 14.61
      C  ( LKFMAT + 8 ) =  7.3
      C  ( LKFMAT + 9 ) = 0.03
      C  ( LKFMAT + 10) = 1666.7
      C  ( LKFMAT + 11) = 2300.
C
      LKFMAT = LKFMAT + 11
      IC ( LKFMAT + 1 ) = 96
      CALL UCTOH('G10$                ', C(LKFMAT+2), 4, 20 )
      C  ( LKFMAT + 7 ) = 14.61
      C  ( LKFMAT + 8 ) =  7.3
      C  ( LKFMAT + 9 ) =  1.70
      C  ( LKFMAT + 10) = 19.4
      C  ( LKFMAT + 11) = 53.1
C
      LKFMAT = LKFMAT + 11
      IC ( LKFMAT + 1 ) = 97
      CALL UCTOH('FDC_GAS$            ', C(LKFMAT+2), 4, 20 )
      C  ( LKFMAT + 7 ) = 39.95
      C  ( LKFMAT + 8 ) = 18.0   
      C  ( LKFMAT + 9 ) = 0.00178
      C  ( LKFMAT + 10) = 12120
      C  ( LKFMAT + 11) = 72400
C
      LKFMAT = LKFMAT + 11
      IC ( LKFMAT + 1 ) = 98
      CALL UCTOH('INTERNAL_CABLING$   ', C(LKFMAT+2), 4, 20 )
      C  ( LKFMAT + 7 ) = 16.0
      C  ( LKFMAT + 8 ) =  8.0 
      C  ( LKFMAT + 9 ) =  2.0
      C  ( LKFMAT + 10) = 15.0
      C  ( LKFMAT + 11) = 55.0
C
C---------------------------------------------------------------------------
  999 RETURN
      END
