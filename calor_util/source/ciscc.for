      LOGICAL FUNCTION CISCC(LADC,LBLS,LROTOW,LDEPTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To test if a ADC/BLS channel is connected
C-                         to a CC calorimeter pad in the cryostat
C-                         (expected usage: CC commisioning 1991)
C-
C-   Inputs  : LADC   adc board
C-             LBLS   bls board
C-             LROTOW read out tower on bls board
C-             LDEPTH depth in bls read out tower
C-   Outputs :
C-   Controls:
C-
C-   Created   8-MAY-1991   James Kourlas  NYUHEP::KOURLAS  from CADPR
C-
C-   See D0 Note 774 for documentation. A copy is in
C-                                   D0$DOCS:CALORIMETER_ADDRESSING.MEM
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
C      INCLUDE 'D0$PARAMS:CAL_PULSE_LIST.PARAMS'
C                         passed variables:
      INTEGER LADC, LBLS, LROTOW, LDEPTH
C                         local variables:
      INTEGER LOGBLS    ! logical bls number 0-95
      INTEGER IMOD      ! LOGBLS mod 6 for CC  or  LOGBLS mod 12 for EC
C
C
C    depth in EC preamp card
C    look up by depth, read_out_tower, CC bls board mod 6
C    if >= 0 then it is the depth in the EC eta 7-11 board
C    else it comes from CC preamp board
C
      INTEGER ECBRD(0:11,0:3,3:5)       ! ECBRD(LDEPTH,LROTOW,MOD(LOGBLS,6))
      DATA ECBRD /
     &  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-2,-2,        ! board 3   eta 6 phi 0
     &  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-2,-2,        ! board 3   eta 6 phi 1
     &  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 1,-2,        ! board 3   eta 7 phi 0
     &  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 3,-2,        ! board 3   eta 7 phi 1
     &  -1,-1,-1,-1,-1,-1,-1,-1,-1, 1, 2,-2,        ! board 4   eta 8 phi 0
     &  -1,-1,-1,-1,-1,-1,-1,-1,-1, 4, 5,-2,        ! board 4   eta 8 phi 1
     &  -1,-1,-1,-1,-1,-1,-1,-1, 5, 6,-2,-2,        ! board 4   eta 9 phi 0
     &  -1,-1,-1,-1,-1,-1,-1,-1, 8, 9,-2,-2,        ! board 4   eta 9 phi 1
     &  -1,-1,-1,-1,-1,-1,-1, 0, 2, 3, 4,-2,        ! board 5   eta 10
     &  -1,-1,-1,-1,-1,-1,-1, 5, 7, 8, 9,-2,        ! board 5   eta 10
     &  -1,-1,-1,-1, 1, 2, 3, 4, 5,-2,-2,-2,        ! board 5   eta 11
     &  -1,-1,-1,-1, 7, 8, 9,10,11,-2,-2,-2/        ! board 5   eta 11
C
C
C----------------------------------------------------------------------
C
      CISCC = .TRUE.                    ! innocent until proven otherwise
C
      LOGBLS = 8 * LADC + LBLS
      IMOD = MOD( LOGBLS, 6 )
      IF( IMOD .GE. 3 ) THEN            ! from merge boards, check table
        IF( ECBRD(LDEPTH, LROTOW, IMOD ) .NE. -1 ) CISCC = .FALSE.
      ELSEIF( LDEPTH .EQ. 11 ) THEN
        CISCC = .FALSE.			! never used
      ENDIF
C
  999 RETURN
      END
