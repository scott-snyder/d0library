      SUBROUTINE CLAYCC
C-------------------------------------------------------------------------
C
C     CREATING THE MECHANICAL LAYER BANK FOR THE CENTRAL CALORIMETER.
C     THE MECHANICAL LAYER BANK WHICH IS ATTACHED TO THE ACTIVE
C     MODULE BANKS.  THIS BANK CONTAINS THE PHYSICAL PAD SEGMENTAION.
C     ZEBRA BANKS CREATED:    CLAY, CSHA
C
C     AUTHOR:    S KAHN       7 JAN 1987
C     REVISION:  ADD PAD DIMENSIONS FROM BLUEPRINTS   S KAHN - 7 JUL 1987
C           PAD INFORMATION FROM BLUEPRINTS 3740.212-ME-228939 (17 SEP 86)
C           AND 3740.212-ME-228931 (15 OCT 86)
C         FILL DATA FROM SRCP BANKS - S KAHN - 5 FEB 1989
C
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$INC:CSHA.DEF'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$PARAMS:CMAT.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZCMAT.LINK'
      INCLUDE 'D0$LINKS:IZCLGA.LINK'
      INCLUDE 'D0$LINKS:IZCLAY.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCSHA.LINK'
      INCLUDE 'D0$LINKS:IZCLNK.LINK'
      INTEGER MCLAY(5), MCSHA(5), LMDSHP, LPREV, LCLAY
      COMMON / LCLAY / LCLAY(2), LMDSHP, LPREV
      INTEGER LZFIND
      INTEGER NPL,NPLCEM,NPLCFH,NPLCCH,NPLEEM
      INTEGER LUNIT
      REAL    DPL,DPLCEM,DPLCFH,DPLCCH,DPLEEM
      REAL    RPL0, RPL1, RPL2, TPL1, TPL2, DTRP
C
      CHARACTER*4 CHAR4,CHAS4
      EQUIVALENCE (CHAR4,MCLAY(1))
      EQUIVALENCE (CHAS4,MCSHA(1))
      DATA MCLAY / 0, 4, 0, 38, 9 /
      DATA MCSHA / 0, 0, 0,  6, 9 /
      DATA CHAR4 / 'CLAY'/
      DATA CHAS4 / 'CSHA'/
      DATA LUNIT / 0 /
C
C     CEN CAL EM LAYERS
C
      CALL MZLINT(IXSTP,'/LCLAY/',LCLAY,LMDSHP,LPREV)
C
      MCSHA(5) = IOCSHA
      NPLCEM = 21
      LQCREG = LZFIND(IDVSTP,LC(LCGEH-IZCREG),ICCAL,IGREGN)     ! GET CREG
      LQCLGA = LZFIND(IDVSTP,LC(LQCREG-IZCLGA),ICCEMA,IGIDEN)    ! GET CLGA
      CALL MZFORM('CLAY','6I9F2I-F',IOCLAY)
C ... FIRST LAYER
      MCLAY(5) = IOCLAY
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLGA,-IZCLAY,MCLAY,0)
      IC(LQCLAY+ILIDEN) = ICCEML             ! layer ID
      IC(LQCLAY+ILETPH) = 1                  ! eta-phi zones on plate
      IC(LQCLAY+ILNPLA) = 2                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 4                  ! perpendicular coordinate
      C(LQCLAY+ILDETA) = 0.1                 ! eta increment
      C(LQCLAY+ILDPHI) = TWOPI/64            ! phi increment
      C(LQCLAY+ILPHI0) = C(LQCLAY+ILPHIC)-0.25*IC(LQCLAY+ILNPHI)*
     +      C(LQCLAY+ILDPHI)
      C(LQCLAY+ILETA0) = 0.05                ! mid-point location of 1st eta
      CALL FLRSRC(LQCLAY,'CCEM_FLOOR1_VOLUME','CCEM_01_VOLUME',
     +      'CC_EM_MOTHER_VOLUME')           ! position information
      CALL CCPAD(LQCLAY,'CCEM_FLOOR1_CELLS') ! pad segmentation data
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1) 
C                                            ! get corresponding CMAT
      MCSHA(4) = 6
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'CCEM_FLOOR1_VOLUME')
      DPL =2* C(LQCSHA+IGPAR4)
      C(LQCLAY+ILDELR) = DPL                  ! layer width in R
      C(LQCLAY+ILNABL) = DPL/C(LQCMAT+IGABSL) ! numb abs lengths
      C(LQCLAY+ILNRDL) = DPL/C(LQCMAT+IGRADL) ! numb rad lengths
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link CSHA
      LC(LQCLAY-IZLMAT) = LQCMAT             ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA             ! link CLGA
      LQCLNK =LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY( LUNIT, LQCLAY)
C ... SECOND LAYER
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICCEML + ICLINC     ! Layer ID
      CALL FLRSRC(LQCLAY,'CCEM_FLOOR2_VOLUME','CCEM_01_VOLUME',
     +      'CC_EM_MOTHER_VOLUME')           ! position information
      CALL CCPAD(LQCLAY,'CCEM_FLOOR2_CELLS') ! pad segmentation data
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1) 
C                                            ! get corresponding CMAT
      MCSHA(4) = 6
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'CCEM_FLOOR2_VOLUME')
      DPL =2* C(LQCSHA+IGPAR4)
      C(LQCLAY+ILDELR) = DPL                  ! layer width in R
      C(LQCLAY+ILNABL) = DPL/C(LQCMAT+IGABSL) ! numb abs lengths
      C(LQCLAY+ILNRDL) = DPL/C(LQCMAT+IGRADL) ! numb rad lengths
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link CSHA
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK              ! link CLNK
      IF(LUNIT .NE. 0) CALL PRCLAY( LUNIT, LQCLAY)
C ... THIRD LAYER
      MCLAY(4) = 51
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICCEML + 2*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 7                   ! number of plates
      C(LQCLAY+ILDETA) = 0.05
      C(LQCLAY+ILDPHI) = TWOPI/128
      C(LQCLAY+ILETA0) = 0.025                ! 1st pad mid-pt position
      CALL FLRSRC(LQCLAY,'CCEM_FLOOR3_VOLUME','CCEM_01_VOLUME',
     +      'CC_EM_MOTHER_VOLUME')           ! position information
      CALL CCPAD(LQCLAY,'CCEM_FLOOR3_CELLS') ! pad segmentation data
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1) 
C                                            ! get corresponding CMAT
      MCSHA(4) = 6
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'CCEM_FLOOR3_VOLUME')
      DPL =2* C(LQCSHA+IGPAR4)
      C(LQCLAY+ILDELR) = DPL                  ! layer width in R
      C(LQCLAY+ILNABL) = DPL/C(LQCMAT+IGABSL) ! numb abs lengths
      C(LQCLAY+ILNRDL) = DPL/C(LQCMAT+IGRADL) ! numb rad lengths
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link CSHA
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY( LUNIT, LQCLAY)
C ... FOURTH LAYER
      MCLAY(4) = 38
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in 1st bank
      IC(LQCLAY+ILIDEN) = ICCEML + 3*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 10                  ! number of plates
      CALL FLRSRC(LQCLAY,'CCEM_FLOOR4_VOLUME','CCEM_01_VOLUME',
     +      'CC_EM_MOTHER_VOLUME')           ! position information
      CALL CCPAD(LQCLAY,'CCEM_FLOOR4_CELLS') ! pad segmentation data
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1) 
C                                            ! get corresponding CMAT
      MCSHA(4) = 6
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'CCEM_FLOOR4_VOLUME')
      DPL =2* C(LQCSHA+IGPAR4)
      C(LQCLAY+ILDELR) = DPL                  ! layer width in R
      C(LQCLAY+ILNABL) = DPL/C(LQCMAT+IGABSL) ! numb abs lengths
      C(LQCLAY+ILNRDL) = DPL/C(LQCMAT+IGRADL) ! numb rad lengths
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link CSHA
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY( LUNIT, LQCLAY)
      CALL LNKLAY(LQCLGA)                     ! link to other EM modules
C
C     CEN CAL FINE HADRONIC LAYERS
C
      NPLCFH = 50                              ! number of FH plates total
      LQCLGA = LZFIND(IDVSTP,LC(LQCREG-IZCLGA),ICCFHA,IGIDEN) ! get CLGA
C ... FIRST FINE HADRONIC LAYER
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLGA,-IZCLAY,MCLAY,0)
      IC(LQCLAY+ILIDEN) = ICCFHL             ! layer ID
      IC(LQCLAY+ILETPH) = 1                  ! eta-phi zones on plate
      IC(LQCLAY+ILNPLA) = 21                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 4                  ! perpendicular coordinate
      C(LQCLAY+ILDETA) = 0.1                 ! eta increment
      C(LQCLAY+ILDPHI) = TWOPI/32            ! phi increment
      C(LQCLAY+ILPHI0) = C(LQCLAY+ILPHIC)-0.25*IC(LQCLAY+ILNPHI)*
     +      C(LQCLAY+ILDPHI)
      C(LQCLAY+ILETA0) = 0.05                ! mid-pt of eta for 1st pad
      CALL FLRSRC(LQCLAY,'CCFH_FLOOR5_VOLUME','CCFH_01_VOLUME',
     +      'CC_FH_MOTHER_VOLUME')           ! position information
      CALL CCPAD(LQCLAY,'CCFH_FLOOR5_CELLS') ! pad segmentation data
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1) 
C                                            ! get corresponding CMAT
      MCSHA(4) = 6
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'CCFH_FLOOR5_VOLUME')
      DPL =2* C(LQCSHA+IGPAR4)
      C(LQCLAY+ILDELR) = DPL                  ! layer width in R
      C(LQCLAY+ILNABL) = DPL/C(LQCMAT+IGABSL) ! numb abs lengths
      C(LQCLAY+ILNRDL) = DPL/C(LQCMAT+IGRADL) ! numb rad lengths
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link CSHA
      LC(LQCLAY-IZLMAT) = LQCMAT             ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA             ! link CLGA
      LQCLNK =LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY( LUNIT, LQCLAY)
C ... SECOND LAYER
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICCFHL + ICLINC     ! Layer ID
      IC(LQCLAY+ILNPLA) = 16                  ! number of plates
      CALL FLRSRC(LQCLAY,'CCFH_FLOOR6_VOLUME','CCFH_01_VOLUME',
     +      'CC_FH_MOTHER_VOLUME')           ! position information
      CALL CCPAD(LQCLAY,'CCFH_FLOOR6_CELLS') ! pad segmentation data
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1) 
C                                            ! get corresponding CMAT
      MCSHA(4) = 6
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'CCFH_FLOOR6_VOLUME')
      DPL =2* C(LQCSHA+IGPAR4)
      C(LQCLAY+ILDELR) = DPL                  ! layer width in R
      C(LQCLAY+ILNABL) = DPL/C(LQCMAT+IGABSL) ! numb abs lengths
      C(LQCLAY+ILNRDL) = DPL/C(LQCMAT+IGRADL) ! numb rad lengths
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link CSHA
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY( LUNIT, LQCLAY)
C  ... THIRD LAYER
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICCFHL + 2*ICLINC   ! Layer ID
      IC(LQCLAY+ILNPLA) = 13                  ! number of plates
      CALL FLRSRC(LQCLAY,'CCFH_FLOOR7_VOLUME','CCFH_01_VOLUME',
     +      'CC_FH_MOTHER_VOLUME')           ! position information
      CALL CCPAD(LQCLAY,'CCFH_FLOOR7_CELLS') ! pad segmentation data
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1) 
C                                            ! get corresponding CMAT
      MCSHA(4) = 6
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'CCFH_FLOOR7_VOLUME')
      DPL =2* C(LQCSHA+IGPAR4)
      C(LQCLAY+ILDELR) = DPL                  ! layer width in R
      C(LQCLAY+ILNABL) = DPL/C(LQCMAT+IGABSL) ! numb abs lengths
      C(LQCLAY+ILNRDL) = DPL/C(LQCMAT+IGRADL) ! numb rad lengths
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link CSHA
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY( LUNIT, LQCLAY)
      CALL LNKLAY(LQCLGA)                     ! link to other CC/FH modules
C
C     CEN CAL COARSE HADRONIC LAYER
C
      NPLCCH = 10                              ! number of CH plates total
      LQCLGA = LZFIND(IDVSTP,LC(LQCREG-IZCLGA),ICCCHA,IGIDEN) ! get CLGA
C ... FIRST COARSE HADRONIC LAYER
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLGA,-IZCLAY,MCLAY,0)
      IC(LQCLAY+ILIDEN) = ICCCHL             ! layer ID
      IC(LQCLAY+ILETPH) = 1                  ! eta-phi zones on plate
      IC(LQCLAY+ILNPLA) = 10                 ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 4                  ! perpendicular coordinate
      C(LQCLAY+ILDETA) = 0.1                 ! eta increment
      C(LQCLAY+ILDPHI) = TWOPI/32            ! phi increment
      C(LQCLAY+ILPHI0) = C(LQCLAY+ILPHIC)-0.25*IC(LQCLAY+ILNPHI)*
     +      C(LQCLAY+ILDPHI)
      C(LQCLAY+ILETA0) = 0.05                ! eta0
      CALL FLRSRC(LQCLAY,'CCCH_FLOOR8_VOLUME','CCCH_01_VOLUME',
     +      'CC_CH_MOTHER_VOLUME')           ! position information
      CALL CCPAD(LQCLAY,'CCCH_FLOOR8_CELLS') ! pad segmentation data
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1) 
C                                            ! get corresponding CMAT
      MCSHA(4) = 7
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'CCCH_FLOOR8_VOLUME')
      DPL = 2. * C(LQCSHA+IGPAR5)            ! layer width
      C(LQCLAY+ILDELR) = DPL
      C(LQCLAY+ILNABL) = DPL/C(LQCMAT+IGABSL) ! numb abs lengths
      C(LQCLAY+ILNRDL) = DPL/C(LQCMAT+IGRADL) ! numb rad lengths
      LC(LQCLAY-IZLMAT) = LQCMAT             ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA             ! link CLGA
      LC(LQCLAY-IZLSHA) = LQCSHA             ! shape bank is same as module
      LQCLNK =LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY( LUNIT, LQCLAY)
      CALL LNKLAY(LQCLGA)                    ! link to other CC/CH modules
C
      LCLAY(1) = 0
      RETURN
      END

