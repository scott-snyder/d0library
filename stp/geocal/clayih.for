      SUBROUTINE CLAYIH
C-------------------------------------------------------------------------
C
C     CREATING THE MECHANICAL LAYER BANK FOR THE INNER HADRONIC
C     REGION OF THE END CALORIMETER.  THE MECHANICAL LAYER BANK 
C     IS ATTACHED TO THE ACTIVE MODULE BANKS.  THIS BANK CONTAINS
C     THE PHYSICAL PAD SEGMENTAION.  THE NUMBERS USED IN THIS 
C     SUBROUTINE COME FROM AL ITO'S CONSTANTS FOR HIS 'ZR' PROGRAM
C
C     ZEBRA BANKS CREATED:    CLAY, CSHA
C
C     AUTHOR:    S KAHN       29 APR 1988
C     REVISIONS:  GET INFORMATION FROM SRCP BANKS -- S KAHN 24 FEB 89
C
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$PARAMS:CMAT.PARAMS'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$INC:CSHA.DEF'
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
      INTEGER LPRINT
      INTEGER NPL,NPLMFH,NPLMCH
      REAL    INCH
C
      CHARACTER*4 CHAR4,CHAS4
      EQUIVALENCE (CHAR4,MCLAY(1))
      EQUIVALENCE (CHAS4,MCSHA(1))
      DATA MCLAY / 0, 4, 0, 50, 9 /
      DATA MCSHA / 0, 0, 0,  7, 9 /
      DATA CHAR4 / 'CLAY'/
      DATA CHAS4 / 'CSHA'/
      DATA INCH /2.54/                   ! converts inches to cm
      DATA LPRINT / 0 /
C
C     END CAL IH/FH LAYERS
C
      CALL MZLINT(IXSTP,'/LCLAY/',LCLAY,LMDSHP,LPREV)
C
      MCSHA(5) = IOCSHA
      LQCREG = LZFIND(IDVSTP,LC(LCGEH-IZCREG),ISECAL,IGREGN)     ! GET CREG
      LQCLGA = LZFIND(IDVSTP,LC(LQCREG-IZCLGA),ICIFHA,IGIDEN)    ! GET CLGA
C
C ... FIRST LAYER - FIRST GROUP IN FIRST READ OUT
C
      CALL MZFORM('CLAY','6I9F2I4F2I4F-F',IOCLAY)
      MCLAY(5) = IOCLAY
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLGA,-IZCLAY,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(11,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      IC(LQCLAY+ILIDEN) = ICIFHL             ! layer ID
      IC(LQCLAY+ILNPLA) = 8                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 3                  ! perpendicular coordinate
      CALL FLRSRC(LQCLAY,'EC_EIF+F+01','IFH_MOTHER_VOLUME+Z','NONE')
      CALL ECPAD(LQCLAY,'IFH_DIVISIONS+Z','F+01')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LQCLNK=LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT             ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA             ! link CLGA
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_EIF+F+01')
C
C ... SECOND LAYER - SECOND GROUP IN FIRST READOUT
C
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(11,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICIFHL + ICLINC     ! Layer ID
      IC(LQCLAY+ILNPLA) = 9                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_EIF+F+02','IFH_MOTHER_VOLUME+Z','NONE')
      CALL ECPAD(LQCLAY,'IFH_DIVISIONS+Z','F+02')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! lnik CLGA
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_EIF+F+02')
C
C ... THIRD LAYER - FIRST GROUP IN SECOND READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(12,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICIFHL + 2*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 8                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_EIF+F+03','IFH_MOTHER_VOLUME+Z','NONE')
      CALL ECPAD(LQCLAY,'IFH_DIVISIONS+Z','F+03')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA
      CALL SHPSRC(LQCSHA,'EC_EIF+F+03')
C
C ... FOURTH LAYER - SECOND GROUP OF SECOND READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(12,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in 1st bank
      IC(LQCLAY+ILIDEN) = ICIFHL + 3*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 8                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_EIF+F+04','IFH_MOTHER_VOLUME+Z','NONE')
      CALL ECPAD(LQCLAY,'IFH_DIVISIONS+Z','F+04')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_EIF+F+04')
C
C ... FIFTH LAYER - FIRST GROUP IN THIRD READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(13,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICIFHL + 4*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 8                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_EIF+F+05','IFH_MOTHER_VOLUME+Z','NONE')
      CALL ECPAD(LQCLAY,'IFH_DIVISIONS+Z','F+05')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_EIF+F+05')
C
C ... SIXTH LAYER - SECOND GROUP OF THIRD READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(13,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in 1st bank
      IC(LQCLAY+ILIDEN) = ICIFHL + 5*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 8                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_EIF+F+06','IFH_MOTHER_VOLUME+Z','NONE')
      CALL ECPAD(LQCLAY,'IFH_DIVISIONS+Z','F+06')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_EIF+F+06')
C
C ... SEVENTH LAYER - FIRST GROUP IN FOURTH READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(14,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICIFHL + 6*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 7                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_EIF+F+07','IFH_MOTHER_VOLUME+Z','NONE')
      CALL ECPAD(LQCLAY,'IFH_DIVISIONS+Z','F+07')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_EIF+F+07')
C
C ... EIGHTH LAYER - SECOND GROUP OF FOURTH READOUT DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(14,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in 1st bank
      IC(LQCLAY+ILIDEN) = ICIFHL + 7*ICLINC   ! layer ID
      IC(LQCLAY+ILNPLA) = 8                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_EIF+F+08','IFH_MOTHER_VOLUME+Z','NONE')
      CALL ECPAD(LQCLAY,'IFH_DIVISIONS+Z','F+08')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_EIF+F+08')
      CALL LNKLAY(LQCLGA)                    ! link to layers secondary modules
C
C     END CAL INNER COARSE HADRONIC LAYERS
C
      NPLMCH = 14                              ! number of CH plates total
      LQCLGA = LZFIND(IDVSTP,LC(LQCREG-IZCLGA),ICICHA,IGIDEN) ! get CLGA
C
C ... FIRST COARSE HADRONIC LAYER - FIRST GROUP OF FIFTH DEPTH
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLGA,-IZCLAY,MCLAY,0)
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(3,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(15,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      IC(LQCLAY+ILIDEN) = ICICHL             ! layer ID
      IC(LQCLAY+ILNPLA) = 4                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 3                  ! perpendicular coordinate
      CALL FLRSRC(LQCLAY,'EC_EIC+C+01','ICH_MOTHER_VOLUME+Z','NONE')
      CALL ECPAD(LQCLAY,'ICH_DIVISIONS+Z','C+01')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LQCLNK=LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT
      LC(LQCLAY-IZLLGA) = LQCLGA             ! link CLGA
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_EIC+C+01')
C
C ... SECOND LAYER - SECOND GROUP OF FIFTH READOUT DEPTH
C
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(3,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(15,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICICHL + ICLINC     ! Layer ID
      IC(LQCLAY+ILNPLA) = 4                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_EIC+C+02','ICH_MOTHER_VOLUME+Z','NONE')
      CALL ECPAD(LQCLAY,'ICH_DIVISIONS+Z','C+02')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_EIC+C+02')
C
C  ... THIRD LAYER - THIRD GROUP OF FIFTH READOUT DEPTH
C
      LPREV = LQCLAY                        ! Save previous bank
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)
      CALL SBYT(3,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(3,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(15,C(LQCLAY),JBRO,NBRO)      ! R. O. layer number
      CALL UCOPY(IC(LPREV+1),IC(LQCLAY+1),MCLAY(4)) ! copy in prev bank
      IC(LQCLAY+ILIDEN) = ICICHL + 2*ICLINC   ! Layer ID
      IC(LQCLAY+ILNPLA) = 5                   ! number of plates
      CALL FLRSRC(LQCLAY,'EC_EIC+C+03','ICH_MOTHER_VOLUME+Z','NONE')
      CALL ECPAD(LQCLAY,'ICH_DIVISIONS+Z','C+03')
      C(LQCLAY+ILNABL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGABSL)    ! number of
C                                        ! absorption lengths
      C(LQCLAY+ILNRDL) = C(LQCLAY+ILDELZ)/C(LQCMAT+IGRADL)    ! number of
C                                        ! radiation lengths
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),IGIDNT)
      LC(LQCLAY-IZLLNK) = LQCLNK             ! link CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT              ! link CMAT
      LC(LQCLAY-IZLLGA) = LQCLGA              ! link CLGA
      IF(LPRINT .NE. 0) CALL PRCLAY(LPRINT, LQCLAY)
C ... SHAPE BANK
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      LC(LQCLAY-IZLSHA) = LQCSHA             ! link from CLAY bank
      CALL SHPSRC(LQCSHA,'EC_EIC+C+03')
      CALL LNKLAY(LQCLGA)                    ! link layers to secondary modules
C
      LCLAY(1) = 0
      RETURN
      END

