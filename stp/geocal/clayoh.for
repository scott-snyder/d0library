      SUBROUTINE CLAYOH
C-------------------------------------------------------------------------
C
C     CREATING THE MECHANICAL LAYER BANK FOR THE OUTER HADRONIC
C     REGION OF THE END CALORIMETER.  THE MECHANICAL LAYER BANK 
C     IS ATTACHED TO THE ACTIVE MODULE BANKS.  THIS BANK CONTAINS
C     THE PHYSICAL PAD SEGMENTAION.  THE NUMBERS USED IN THIS 
C     SUBROUTINE COME FROM AL ITO'S CONSTANTS FOR HIS 'ZR' PROGRAM
C
C     ZEBRA BANKS CREATED:    CLAY, CSHA
C
C     AUTHOR:    S KAHN       7 JUNE 1988
C
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$INC:CSHA.DEF'
      INCLUDE 'D0$PARAMS:CMAT.PARAMS'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZCLAY.LINK'
      INCLUDE 'D0$LINKS:IZCLGA.LINK'
      INCLUDE 'D0$LINKS:IZCSHA.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCMAT.LINK'
      INCLUDE 'D0$LINKS:IZCLNK.LINK'
      INTEGER MCLAY(5), MCSHA(5), LMDSHP, LPREV, LSHP1, LSHP2, LCLAY
      COMMON / LCLAY / LCLAY(2), LMDSHP, LSHP1, LSHP2, LPREV
      INTEGER LZFIND
      INTEGER NPL,NPLOCH 
      REAL    RPL1, RPL2, TPL1, TPL2, DTRP, INCH
      REAL    PAX1(3), PAX2(3)
      INTEGER LPRINT
C
      CHARACTER*4 CHAR4,CHAS4
      EQUIVALENCE (CHAR4,MCLAY(1))
      EQUIVALENCE (CHAS4,MCSHA(1))
      DATA MCLAY / 0, 4, 0, 35, 9 /
      DATA MCSHA / 0, 0, 0, 13, 9 /
      DATA CHAR4 / 'CLAY'/
      DATA CHAS4 / 'CSHA'/
      DATA INCH /2.54/                   ! converts inches to cm
      DATA LPRINT / 0 /
C
C     END CAL OH/CH LAYERS
C
      CALL MZLINT(IXSTP,'/LCLAY/',LCLAY,LMDSHP,LPREV)
C
      MCSHA(5) = IOCSHA
      NPLOCH = 60
      LQCREG = LZFIND(IDVSTP,LC(LCGEH-1),ISECAL,IGREGN)     ! GET CREG
      LQCLGA = LZFIND(IDVSTP,LC(LQCREG-IZCLGA),ICOCHA,IGIDEN)    ! GET CLGA
      CALL MZFORM('CLAY','6I9F2I4F-F',IOCLAY)
C
C ... FIRST LAYER - FIRST GROUP IN FIRST READ OUT
C
      MCLAY(5) = IOCLAY
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLGA,-IZCLAY,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(4,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(15,C(LQCLAY),JBRO,NBRO)      ! R.O. number
      IC(LQCLAY+ILIDEN) = ICOCHL             ! layer ID
      IC(LQCLAY+ILNPLA) = 2                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 9                  ! perpendicular coordinate
      C(LQCLAY+ILDELZ) = 4.1084 * INCH        ! Z extent of layer
C                                        !  at 30 degrees to Z)
      CALL FLRSRC(LQCLAY,'EC_OCH+O+01','EC_OCH+1','NONE')
      CALL ECPAD(LQCLAY,'OH_DIVISIONS+Z','O+01')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LQCLNK =LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT             ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA             ! link CLGA
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_OCH+O+01')
      LSHP1 = LQCSHA
C
C ... SECOND LAYER - SECOND GROUP IN FIRST READOUT
C
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(4,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(15,C(LQCLAY),JBRO,NBRO)      ! R.O. number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICOCHL + ICLINC     ! Layer ID
      IC(LQCLAY+ILNPLA) = 2                   ! number of plates
      C(LQCLAY+ILDELZ) = 4.1084 * INCH        ! Z extent of layer
      CALL FLRSRC(LQCLAY,'EC_OCH+O+02','EC_OCH+1','NONE')
      CALL ECPAD(LQCLAY,'OH_DIVISIONS+Z','O+02')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
C                                        ! outer corner radius
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! lnik CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK -- SAME AS ABOVE
      LC(LQCLAY-IZLSHA) = LSHP1               ! link from CLAY bank
C
C ... THIRD LAYER - THIRD GROUP IN FIRST READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(3,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(4,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(15,C(LQCLAY),JBRO,NBRO)      ! R.O. number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICOCHL + 2*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 2                   ! number of plates
      C(LQCLAY+ILDELZ) = 4.1084 * INCH        ! Z extent of layer
      CALL FLRSRC(LQCLAY,'EC_OCH+O+03','EC_OCH+1','NONE')
      CALL ECPAD(LQCLAY,'OH_DIVISIONS+Z','O+03')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK -- USE SAME SHAPE BANK
      LC(LQCLAY-IZLSHA) = LSHP1               ! link from CLAY bank
C
C ... FOURTH LAYER - FOURTH GROUP OF FIRST READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(4,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(4,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(15,C(LQCLAY),JBRO,NBRO)      ! R.O. number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in 1st bank
      IC(LQCLAY+ILIDEN) = ICOCHL + 3*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 2                   ! number of plates
      C(LQCLAY+ILDELZ) = 4.1728 * INCH        ! Z extent of layer
      CALL FLRSRC(LQCLAY,'EC_OCH+O+04','EC_OCH+1','NONE')
      CALL ECPAD(LQCLAY,'OH_DIVISIONS+Z','O+04')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK -- ADD EXTRA LENGTH FOR EXTRA R.O. BOARD
      CALL MZLIFT(IDVSTP, LQCSHA, LCGEH, -IZCSHA, MCSHA, 0)
      CALL SHPSRC(LQCSHA,'EC_OCH+O+04')
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      LSHP2 = LQCSHA                         ! save for other layers
C
C ... FIFTH LAYER - FIRST GROUP IN SECOND READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(4,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(16,C(LQCLAY),JBRO,NBRO)      ! R.O. number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICOCHL + 4*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 2                   ! number of plates
      C(LQCLAY+ILDELZ) = 4.1084 * INCH        ! Z extent of layer
      CALL FLRSRC(LQCLAY,'EC_OCH+O+05','EC_OCH+1','NONE')
      CALL ECPAD(LQCLAY,'OH_DIVISIONS+Z','O+05')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK -- SAME SHAPE AS ABOVE
      LC(LQCLAY-IZLSHA) = LSHP1               ! link from CLAY bank
C
C ... SIXTH LAYER - SECOND GROUP OF SECOND READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(4,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(16,C(LQCLAY),JBRO,NBRO)      ! R.O. number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in 1st bank
      IC(LQCLAY+ILIDEN) = ICOCHL + 5*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 2                   ! number of plates
      C(LQCLAY+ILDELZ) = 4.1084 * INCH        ! Z extent of layer
      CALL FLRSRC(LQCLAY,'EC_OCH+O+06','EC_OCH+1','NONE')
      CALL ECPAD(LQCLAY,'OH_DIVISIONS+Z','O+06')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK -- SAME SHAPE AS ABOVE
      LC(LQCLAY-IZLSHA) = LSHP1               ! link from CLAY bank
C
C ... SEVENTH LAYER - THIRD GROUP IN SECOND READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(3,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(4,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(16,C(LQCLAY),JBRO,NBRO)      ! R.O. number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICOCHL + 6*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 2                   ! number of plates
      C(LQCLAY+ILDELZ) = 4.1084 * INCH        ! Z extent of layer
      CALL FLRSRC(LQCLAY,'EC_OCH+O+07','EC_OCH+1','NONE')
      CALL ECPAD(LQCLAY,'OH_DIVISIONS+Z','O+07')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK -- SAME SHAPE AS ABOVE
      LC(LQCLAY-IZLSHA) = LSHP1               ! link from CLAY bank
C
C ... EIGHTH LAYER - FOURTH GROUP OF SECOND READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(4,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(4,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(16,C(LQCLAY),JBRO,NBRO)      ! R.O. number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in 1st bank
      IC(LQCLAY+ILIDEN) = ICOCHL + 7*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 2                   ! number of plates
      C(LQCLAY+ILDELZ) = 4.1728 * INCH        ! Z extent of layer
      CALL FLRSRC(LQCLAY,'EC_OCH+O+08','EC_OCH+1','NONE')
      CALL ECPAD(LQCLAY,'OH_DIVISIONS+Z','O+08')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK-- SAME SHAPE AS ABOVE
      LC(LQCLAY-IZLSHA) = LSHP2               ! link from CLAY bank
C
C ... NINETH COARSE HADRONIC LAYER - FIRST GROUP OF THIRD DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(4,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(17,C(LQCLAY),JBRO,NBRO)      ! R.O. number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in 1st bank
      IC(LQCLAY+ILIDEN) = ICOCHL + 8*ICLINC  ! layer ID
      IC(LQCLAY+ILNPLA) = 2
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 9                  ! perpendicular coordinate
      C(LQCLAY+ILDELZ) = 4.1084 * INCH       ! Z extent of layer
      CALL FLRSRC(LQCLAY,'EC_OCH+O+09','EC_OCH+1','NONE')
      CALL ECPAD(LQCLAY,'OH_DIVISIONS+Z','O+09')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LC(LQCLAY-IZLMAT) = LQCMAT             ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA             ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK -- SAME SHAPE AS ABOVE
      LC(LQCLAY-IZLSHA) = LSHP1              ! link from CLAY bank
C
C ... TENTH LAYER - SECOND GROUP OF THIRD READOUT DEPTH
C
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(4,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(17,C(LQCLAY),JBRO,NBRO)      ! R.O. number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICOCHL + 9*ICLINC   ! Layer ID
      IC(LQCLAY+ILNPLA) = 2                   ! number of plates
      C(LQCLAY+ILDELZ) = 4.1084 * INCH        ! Z extent of layer
      CALL FLRSRC(LQCLAY,'EC_OCH+O+10','EC_OCH+1','NONE')
      CALL ECPAD(LQCLAY,'OH_DIVISIONS+Z','O+10')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK -- SAME SHAPE AS ABOVE
      LC(LQCLAY-IZLSHA) = LSHP1               ! link from CLAY bank
C
C  ... ELEVENTH LAYER - THIRD GROUP OF THIRD READOUT DEPTH
C
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(3,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(4,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(17,C(LQCLAY),JBRO,NBRO)      ! R.O. number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICOCHL + 10*ICLINC  ! Layer ID
      IC(LQCLAY+ILNPLA) = 2                   ! number of plates
      C(LQCLAY+ILDELZ) = 4.1084 * INCH        ! Z extent of layer
      CALL FLRSRC(LQCLAY,'EC_OCH+O+11','EC_OCH+1','NONE')
      CALL ECPAD(LQCLAY,'OH_DIVISIONS+Z','O+11')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK-- SAME SHAPE AS ABOVE
      LC(LQCLAY-IZLSHA) = LSHP1               ! link from CLAY bank
C
C  ... TWELVETH LAYER - FOURTH GROUP OF THIRD READOUT DEPTH
C
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(4,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(4,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(17,C(LQCLAY),JBRO,NBRO)      ! R.O. number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICOCHL + 11*ICLINC  ! Layer ID
      IC(LQCLAY+ILNPLA) = 2                   ! number of plates
      C(LQCLAY+ILDELZ) = 4.1728 * INCH        ! Z extent of layer
      CALL FLRSRC(LQCLAY,'EC_OCH+O+12','EC_OCH+1','NONE')
      CALL ECPAD(LQCLAY,'OH_DIVISIONS+Z','O+12')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
C                                        ! outer radius
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK-- SAME SHAPE AS ABOVE
      LC(LQCLAY-IZLSHA) = LSHP2              ! link from CLAY bank
      CALL LNKLAY(LQCLGA)                    ! put in links to secondary
C                                            ! modules
C
      LCLAY(1) = 0
      RETURN
      END
