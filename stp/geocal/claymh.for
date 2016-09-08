      SUBROUTINE CLAYMH
C-------------------------------------------------------------------------
C
C     CREATING THE MECHANICAL LAYER BANK FOR THE MIDDLE HADRONIC
C     REGION OF THE END CALORIMETER.  THE MECHANICAL LAYER BANK
C     IS ATTACHED TO THE ACTIVE MODULE BANKS.  THIS BANK CONTAINS
C     THE PHYSICAL PAD SEGMENTAION.  THE NUMBERS USED IN THIS
C     SUBROUTINE COME FROM AL ITO'S CONSTANTS FOR HIS 'ZR' PROGRAM
C
C     ZEBRA BANKS CREATED:    CLAY, CSHA
C
C     AUTHOR:    S KAHN       27 APR 1988
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
      INCLUDE 'D0$LINKS:IZCMAT.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCSHA.LINK'
      INCLUDE 'D0$LINKS:IZCLGA.LINK'
      INCLUDE 'D0$LINKS:IZCLNK.LINK'
      INTEGER MCLAY(5), MCSHA(5), LMDSHP, LPREV, LCLAY
      COMMON / LCLAY / LCLAY(2), LMDSHP, LPREV
      INTEGER LZFIND
      INTEGER NPL,NPLMFH,NPLMCH, LPRINT
      REAL    DPL,DPLCEM,DPLCFH,DPLCCH,DPLEEM
      REAL    RPL1, RPL2, TPL1, TPL2, DTRP, INCH
C
      CHARACTER*4 CHAR4,CHAS4
      EQUIVALENCE (CHAR4,MCLAY(1))
      EQUIVALENCE (CHAS4,MCSHA(1))
      DATA MCLAY / 4HCLAY, 4, 0, 35, 9 /
      DATA MCSHA / 4HCSHA, 0, 0,  7, 9 /
      DATA CHAR4 / 'CLAY'/
      DATA CHAS4 / 'CSHA'/
      DATA INCH /2.54/                   ! converts inches to cm
      DATA LPRINT / 0 /
C
C     END CAL MH/FH LAYERS
C
      CALL MZLINT(IXSTP,'/LCLAY/',LCLAY,LMDSHP,LPREV)
C
      MCSHA(5) = IOCSHA
      LQCREG = LZFIND(IDVSTP,LC(LCGEH-IZCREG),ISECAL,IGREGN)     ! GET CREG
      LQCLGA = LZFIND(IDVSTP,LC(LQCREG-IZCLGA),ICMFHA,IGIDEN)    ! GET CLGA
      CALL MZFORM('CLAY','6I9F2I4F-F',IOCLAY)
C
C ... FIRST LAYER - FIRST GROUP IN FIRST READ OUT
C
      MCLAY(5) = IOCLAY
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLGA,-IZCLAY,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(11,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      IC(LQCLAY+ILIDEN) = ICMFHL             ! layer ID
      IC(LQCLAY+ILNPLA) = 7                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 3                  ! perpendicular coordinate
      CALL FLRSRC(LQCLAY,'EC_MFA+01A+','EC_MFA+1','NONE')
      CALL ECPAD_MH(LQCLAY,'MFH_DIVISIONS+Z','01A+')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LQCLNK =LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT             ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA             ! link CLGA
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_MFA+01A+')
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C
C ... SECOND LAYER - SECOND GROUP IN FIRST READOUT
C
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(11,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),ILZPD0) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICMFHL + ICLINC     ! Layer ID
      IC(LQCLAY+ILNPLA) = 8                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_MFA+02A+','EC_MFA+1','NONE')
      CALL ECPAD_MH(LQCLAY,'MFH_DIVISIONS+Z','02A+')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! lnik CLGA
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_MFA+02A+')
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C
C ... THIRD LAYER - FIRST GROUP IN SECOND READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(12,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),ILZPD0) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICMFHL + 2*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 7                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_MFA+03A+','EC_MFA+1','NONE')
      CALL ECPAD_MH(LQCLAY,'MFH_DIVISIONS+Z','03A+')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_MFA+03A+')
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C
C ... FOURTH LAYER - SECOND GROUP OF SECOND READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(12,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),ILZPD0) ! copy in 1st bank
      IC(LQCLAY+ILIDEN) = ICMFHL + 3*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 8                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_MFA+04A+','EC_MFA+1','NONE')
      CALL ECPAD_MH(LQCLAY,'MFH_DIVISIONS+Z','04A+')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_MFA+04A+')
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C
C ... FIFTH LAYER - FIRST GROUP IN THIRD READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(13,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),ILZPD0) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICMFHL + 4*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 7                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_MFA+05A+','EC_MFA+1','NONE')
      CALL ECPAD_MH(LQCLAY,'MFH_DIVISIONS+Z','05A+')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_MFA+05A+')
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C
C ... SIXTH LAYER - SECOND GROUP OF THIRD READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(13,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),ILZPD0) ! copy in 1st bank
      IC(LQCLAY+ILIDEN) = ICMFHL + 5*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 8                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_MFA+06A+','EC_MFA+1','NONE')
      CALL ECPAD_MH(LQCLAY,'MFH_DIVISIONS+Z','06A+')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_MFA+06A+')
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C
C ... SEVENTH LAYER - FIRST GROUP IN FOURTH READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(14,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),ILZPD0) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICMFHL + 6*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 7                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_MFA+07A+','EC_MFA+1','NONE')
      CALL ECPAD_MH(LQCLAY,'MFH_DIVISIONS+Z','07A+')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_MFA+07A+')
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C
C ... EIGHTH LAYER - SECOND GROUP OF FOURTH READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(14,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),ILZPD0) ! copy in 1st bank
      IC(LQCLAY+ILIDEN) = ICMFHL + 7*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 8                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_MFA+08A+','EC_MFA+1','NONE')
      CALL ECPAD_MH(LQCLAY,'MFH_DIVISIONS+Z','08A+')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_MFA+08A+')
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
      CALL LNKLAY(LQCLGA)                    ! place links in secondary modules
C
C     END CAL MIDDLE COARSE HADRONIC LAYERS
C
      LQCLGA = LZFIND(IDVSTP,LC(LQCREG-IZCLGA),ICMCHA,IGIDEN) ! get CLGA
C
C ... FIRST COARSE HADRONIC LAYER - FIRST GROUP OF FIFTH DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLGA,-IZCLAY,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(3,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(15,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      IC(LQCLAY+ILIDEN) = ICMCHL             ! layer ID
      IC(LQCLAY+ILNPLA) = 4                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 3                  ! perpendicular coordinate
      CALL FLRSRC(LQCLAY,'EC_MCA+01C+','EC_MCA+1','NONE')
      CALL ECPAD_MH(LQCLAY,'MCH_DIVISIONS+Z','01C+')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LQCLNK =LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT             ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA             ! link CLGA
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_MCA+01C+')
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C
C ... SECOND LAYER - SECOND GROUP OF FIFTH READOUT DEPTH
C
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(3,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(15,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),ILZPD0) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICMCHL + ICLINC     ! Layer ID
      IC(LQCLAY+ILNPLA) = 5                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_MCA+02C+','EC_MCA+1','NONE')
      CALL ECPAD_MH(LQCLAY,'MCH_DIVISIONS+Z','02C+')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_MCA+02C+')
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C
C  ... THIRD LAYER - THIRD GROUP OF FIFTH READOUT DEPTH
C
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(3,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(3,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(15,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),ILZPD0) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICMCHL + 2*ICLINC   ! Layer ID
      IC(LQCLAY+ILNPLA) = 5                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_MCA+03C+','EC_MCA+1','NONE')
      CALL ECPAD_MH(LQCLAY,'MCH_DIVISIONS+Z','03C+')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_MCA+03C+')
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
      CALL LNKLAY(LQCLGA)                    ! put in links to secondary
C                                            ! modules
C
      LCLAY(1) = 0
      RETURN
      END
