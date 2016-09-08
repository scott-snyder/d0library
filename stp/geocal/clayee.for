      SUBROUTINE CLAYEE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CREATES MECHANICAL LAYER BANKS FOR THE END
C-                         ELECTROMAGNETIC CALORIMETER.  THE NUMBERS
C-                         USED CORRESPOND TO THE EC-EM FINAL DESIGN
C-                         TECHNICAL PARAMETERS. (D0 NOTE #757-R.MADARAS)
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-   Zebra Banks Lifted:   CLAY, CSHA
C-
C-   Created  22-AUG-1988   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:CALGEO.INC'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$INC:CSHA.DEF'
      INCLUDE 'D0$PARAMS:CMAT.PARAMS'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZCMAT.LINK'
      INCLUDE 'D0$LINKS:IZCSHA.LINK'
      INCLUDE 'D0$LINKS:IZCLGA.LINK'
      INCLUDE 'D0$LINKS:IZCLAY.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCLNK.LINK'
C
      COMMON /LCLAY/ LCLAY, LPREV
      INTEGER MCLAY(5), MCSHA(5), LPREV, LCLAY(3), JQCLAY
      INTEGER LZFIND, LUNIT
      REAL INCH
      DATA INCH /2.54/
      DATA LUNIT / 0 /
C
      CHARACTER*4 CHAR4,CHAS4
      EQUIVALENCE (CHAR4,MCLAY(1))
      EQUIVALENCE (CHAS4,MCSHA(1))
      DATA MCLAY / 4HCLAY, 4, 0, 50, 9 /
      DATA MCSHA / 4HCSHA, 0, 0,  7, 9 /
      DATA CHAR4 / 'CLAY'/
      DATA CHAS4 / 'CSHA'/
C
      CALL MZLINT(IDVSTP,'/LCLAY/',LCLAY,LCLAY(3),LPREV)
C
      LQCREG = LZFIND(IDVSTP,LC(LCGEH-IZCREG),ISEMCL,IGREGN)  ! get CREG
      CALL MZFORM('CLAY','6I9F2I4F2I4F-F',IOCLAY)
      MCLAY(5) = IOCLAY
C ... FIRST READ OUT LAYER
      LQCLGA = LZFIND(IDVSTP,LC(LQCREG-IZCLGA),ICEC1A,IGIDEN)  ! get CLGA
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLGA,-IZCLAY,MCLAY,0)   ! lift bank
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(1,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(1,C(LQCLAY),JBRO,NBRO)       ! R. O. layer number
      IC(LQCLAY+ILIDEN) = ICEC1L             ! layer ID
      IC(LQCLAY+ILNPLA) = 2                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 3                  ! perpindicular dirn
      CALL FLRSRC(LQCLAY,'EC_EEM+EM+1','EM_MOTHER_VOLUME+Z','NONE')
      C(LQCLAY+ILNRDL) = 0.3                 ! number of rad lengths
      C(LQCLAY+ILNABL) = 0.05                ! number of int lengths
      CALL ECPAD(LQCLAY,'EM1_DIVISIONS+Z','EM+1')
C
C     EC1L SHAPE INFORMATION
C
      MCSHA(5) = IOCSHA
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'EC_EEM+EM+1')      ! fill shape related
C
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1)
      LC(LQCLAY-IZLSHA) = LQCSHA
      LC(LQCLAY-IZLMAT) = LQCMAT
      LC(LQCLAY-IZLLGA) = LQCLGA
      LQCLNK=LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY(LUNIT, LQCLAY)
C
C ... SECOND READ OUT LAYER
C
      LQCLGA = LZFIND(IDVSTP,LC(LQCREG-IZCLGA),ICEC2A,IGIDEN)  ! get CLGA
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLGA,-IZCLAY,MCLAY,0)   ! lift bank
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(1,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(2,C(LQCLAY),JBRO,NBRO)       ! R. O. layer number
      IC(LQCLAY+ILIDEN) = ICEC2L             ! layer ID
      IC(LQCLAY+ILNPLA) = 2                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 3                  ! perpindicular dirn
      CALL FLRSRC(LQCLAY,'EC_EEM+EM+2','EM_MOTHER_VOLUME+Z','NONE')
      C(LQCLAY+ILNRDL) = 2.6                 ! number of rad lengths
      C(LQCLAY+ILNABL) = 0.1                 ! number of int lengths
      CALL ECPAD(LQCLAY,'EM2_DIVISIONS+Z','EM+2')
C
C     EC2L SHAPE INFORMATION
C
      MCSHA(5) = IOCSHA
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'EC_EEM+EM+2')      ! fill in shape info
C
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1)
      LC(LQCLAY-IZLSHA) = LQCSHA
      LC(LQCLAY-IZLMAT) = LQCMAT
      LC(LQCLAY-IZLLGA) = LQCLGA
C      LQCLNK=LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY(LUNIT, LQCLAY)
C
C ... THIRD READ OUT LAYER -- SECTION A
C
      MCLAY(4) = 70
      CALL MZFORM('CLAY','6I9F2I4F2I4F2I4F-F',IOCLAY)
      MCLAY(5) = IOCLAY
      LQCLGA = LZFIND(IDVSTP,LC(LQCREG-IZCLGA),ICEC3A,IGIDEN)  ! get CLGA
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLGA,-IZCLAY,MCLAY,0)   ! lift bank
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(3,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(3,C(LQCLAY),JBRO,NBRO)       ! R. O. layer number
      IC(LQCLAY+ILIDEN) = ICEC3L             ! layer ID
      IC(LQCLAY+ILNPLA) = 2                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 3                  ! perpindicular dirn
      CALL FLRSRC(LQCLAY,'EC_EEM+E+3A','EM_MOTHER_VOLUME+Z','NONE')
      C(LQCLAY+ILNRDL) = 2.6                 ! number of rad lengths
      C(LQCLAY+ILNABL) = 0.10                ! number of int lengths
      CALL ECPAD(LQCLAY,'EM3_DIVISIONS+Z','E+3A')
C     
C     EC3aL SHAPE INFORMATION
C
      MCSHA(5) = IOCSHA
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'EC_EEM+E+3A')      ! fill in shape info
C
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1)
      LC(LQCLAY-IZLSHA) = LQCSHA
      LC(LQCLAY-IZLMAT) = LQCMAT
      LC(LQCLAY-IZLLGA) = LQCLGA
C      LQCLNK=LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY(LUNIT, LQCLAY)
C
C ... THIRD READ OUT LAYER -- SECTION B
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)   ! lift bank
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(3,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(3,C(LQCLAY),JBRO,NBRO)       ! R. O. layer number
      LQCMAT = LC(LQCLGA-IXCMAT)             ! get corresponding CMAT
      IC(LQCLAY+ILIDEN) = ICEC3L+ICLINC      ! layer ID
      IC(LQCLAY+ILNPLA) = 2                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 3                  ! perpindicular dirn
      CALL FLRSRC(LQCLAY,'EC_EEM+E+3B','EM_MOTHER_VOLUME+Z','NONE')
      C(LQCLAY+ILNRDL) = 2.6                 ! number of rad lengths
      C(LQCLAY+ILNABL) = 0.10                ! number of int lengths
      CALL ECPAD(LQCLAY,'EM3_DIVISIONS+Z','E+3B')
C
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'EC_EEM+E+3B')      ! fill in shape info
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1)
      LC(LQCLAY-IZLSHA) = LQCSHA
      LC(LQCLAY-IZLMAT) = LQCMAT
      LC(LQCLAY-IZLLGA) = LQCLGA
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY(LUNIT, LQCLAY)
C
C ... THIRD READ OUT LAYER -- SECTION C
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)   ! lift bank
      CALL SBYT(3,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(3,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(3,C(LQCLAY),JBRO,NBRO)       ! R. O. layer number
      IC(LQCLAY+ILIDEN) = ICEC3L+2*ICLINC    ! layer ID
      IC(LQCLAY+ILNPLA) = 2                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 3                  ! perpindicular dirn
      CALL FLRSRC(LQCLAY,'EC_EEM+E+3C','EM_MOTHER_VOLUME+Z','NONE')
      C(LQCLAY+ILNRDL) = 2.6                 ! number of rad lengths
      C(LQCLAY+ILNABL) = 0.10                ! number of int lengths
      CALL ECPAD(LQCLAY,'EM3_DIVISIONS+Z','E+3C')
C
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'EC_EEM+E+3C')      ! fill in shape info
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1)
      LC(LQCLAY-IZLSHA) = LQCSHA
      LC(LQCLAY-IZLMAT) = LQCMAT
      LC(LQCLAY-IZLLGA) = LQCLGA
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY(LUNIT, LQCLAY)
C
C ... FOURTH READ OUT LAYER -- SECTION A
C
      MCLAY(4) = 51
      CALL MZFORM('CLAY','6I9F2I4F2I4F-F',IOCLAY)
      MCLAY(5) = IOCLAY
      LQCLGA = LZFIND(IDVSTP,LC(LQCREG-IZCLGA),ICEC4A,IGIDEN)  ! get CLGA
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLGA,-IZCLAY,MCLAY,0)   ! lift bank
      CALL SBYT(1,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(4,C(LQCLAY),JBRO,NBRO)       ! R. O. layer number
      IC(LQCLAY+ILIDEN) = ICEC4L             ! layer ID
      IC(LQCLAY+ILNPLA) = 2                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 3                  ! perpindicular dirn
      CALL FLRSRC(LQCLAY,'EC_EEM+E+4A','EM_MOTHER_VOLUME+Z','NONE')
      C(LQCLAY+ILNRDL) = 4.1                 ! number of rad lengths
      C(LQCLAY+ILNABL) = 0.28                ! number of int lengths
      CALL ECPAD(LQCLAY,'EM4_DIVISIONS+Z','E+4A')
C
C     EC4aL SHAPE INFORMATION
C
      MCSHA(5) = IOCSHA
      CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'EC_EEM+E+4A')      ! fill in shape info
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1)
      LC(LQCLAY-IZLSHA) = LQCSHA
      LC(LQCLAY-IZLMAT) = LQCMAT
      LC(LQCLAY-IZLLGA) = LQCLGA
C      LQCLNK=LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY(LUNIT, LQCLAY)
C
C ... FOURTH READ OUT LAYER - SECTION B
C
      CALL MZLIFT(IDVSTP,LQCLAY,LQCLAY,0,MCLAY,0)   ! lift bank
      CALL SBYT(2,C(LQCLAY),JBGRUP,NBGRUP)   ! group number
      CALL SBYT(2,C(LQCLAY),JBNGRP,NBNGRP)   ! total number of groups
      CALL SBYT(4,C(LQCLAY),JBRO,NBRO)       ! R. O. layer number
      IC(LQCLAY+ILIDEN) = ICEC4L+ICLINC      ! layer ID
      IC(LQCLAY+ILNPLA) = 2                  ! number of plate cells
      IC(LQCLAY+ILCOOR) = 345                ! coordinate code
      IC(LQCLAY+ILPERP) = 3                  ! perpindicular dirn
      CALL FLRSRC(LQCLAY,'EC_EEM+E+4B','EM_MOTHER_VOLUME+Z','NONE')
      CALL ECPAD(LQCLAY,'EM4_DIVISIONS+Z','E+4B')
C
C     EC4BL SHAPE INFORMATION
C
      MCSHA(5) = IOCSHA
      CALL MZLIFT(IXSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
      CALL SHPSRC(LQCSHA,'EC_EEM+E+3B')      ! fill in shape info
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1)
C
      LC(LQCLAY-IZLSHA) = LQCSHA
      LC(LQCLAY-IZLMAT) = LQCMAT
      LC(LQCLAY-IZLLGA) = LQCLGA
      LC(LQCLAY-IZLLNK) = LQCLNK
      IF(LUNIT .NE. 0) CALL PRCLAY(LUNIT, LQCLAY)
C----------------------------------------------------------------------
  999 RETURN
      END
