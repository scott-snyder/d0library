      SUBROUTINE CLGACC
C---------------------------------------------------------------
C
C     CREATES ACTIVE MODULE BANKS CLGA
C     ZEBRA BANKS CREATED:   CLGA, CSHA
C     ZEBRA BANKS ALTERED:   CLGI
C
C     AUTHOR:    S KAHN      18 JUNE 1987
C     REVISIONS:   CHANGED SEGMENTATION OF EM REGION TO 32 - S KAHN 
C                                                        7 JULY 87
C                  DZ AND DY ARE INTECHANGED FOR TRD1 - S KAHN 13 JULY 87
C                  CHANGED TO RETRIEVE NUMBERS FROM SRCP - S KAHN 3 FEB 89
C---------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$INC:CSHA.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZCMAT.LINK'
      INCLUDE 'D0$LINKS:IZCLGA.LINK'
      INCLUDE 'D0$LINKS:IZCLGI.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCSHA.LINK'
      INCLUDE 'D0$LINKS:IZCLNK.LINK'
      INTEGER MCLGA(5), MCSHA(5), LORIG, LPCON, LTUBE, NSEG, LCLGA(2)
      REAL    DPHI, CPHI, SPHI, CRACK, DEL
      REAL    PAX1(3), PAX2(3)
      INTEGER LZFIND, IOK, J, MCLNK(5)
      CHARACTER*14    MODULE_VOLUME_NAME
      COMMON / LCLGA / LCLGA, LPCON, LTUBE, LORIG
      CHARACTER*4 CHAR4, CHAS4, CHAL4
      INTEGER ICHARR
      CHARACTER*4 CHARR
      EQUIVALENCE (ICHARR, CHARR), (CHAR4,MCLGA(1)), (CHAS4,MCSHA(1))
      EQUIVALENCE (CHAL4,MCLNK(1))
      DATA MCLGA / 4HCLGA, 6, 1, 18, 9/
      DATA MCSHA / 4HCSHA, 0, 0,  6, 9/
      DATA MCLNK / 4HCLNK, 32, 0, 4, 2/
      DATA CHAR4 / 'CLGA' /
      DATA CHAS4 / 'CSHA' /
      DATA CHAL4 / 'CLNK' /
C
C
C   CENTRAL CALORIMETER MODULES
C
      CALL MZLINT(IXSTP,'/LCLGA/',LCLGA,LTUBE,LORIG)     ! temp link space
      LQCREG = LZFIND(IDVSTP,LC(LCGEH-IZCREG),ICCAL,IGREGN) ! get CREG
C
      MCLGA(5) = IOCLGA                         ! put in form address
C 
C    CEN CAL EM REGION
C
      LQCLGI = LZFIND(IDVSTP,LC(LQCREG-IZCLGI),ICCEMI,IGIDEN) ! GET CLGI
      CALL EZGET('CCEM_NUMBER_MODULES',NSEG,IOK)
      LTUBE  = LC(LQCLGI-IZCSHA)                ! addr to inactive cyl shape
      DPHI = TWOPI/NSEG                         ! 1/32 of TWOPI
      SPHI = SIN(DPHI/2)
      CPHI = COS(DPHI/2)
      CRACK = C(LQCLGI+IGCRAK)                  ! crack width
      CALL MZLIFT(IDVSTP,LQCLNK,LQCREG,-IZCLNK,MCLNK,0)
                                                ! create CLNK to hold links to
                                                ! CLGA
      IC(LQCLNK + IGIDEN) = ICCEMA              ! same ID as lead CLGA bank
      IC(LQCLNK + IGNSEG) = NSEG
      IC(LQCLNK + IGJPHI) = 2
      IC(LQCLNK + IGN1) = 1
C
      DO 100 J = 1, NSEG                        ! loop over modules
      IF( J .EQ. 1) THEN
        MCLGA(3) = 1
      ELSE
        MCLGA(3) = 0
      END IF
      CALL MZLIFT(IDVSTP,LQCLGA,LQCREG,-IZCLGA,MCLGA,0)
      IC(LQCLGA+IGIDEN) = ICCEMA + ICMINC*(J-1) ! CENCAL EM ACTIVE REGION
      IC(LQCLGA+IGNLAY) = 4
      CHARR = 'CEMA'
      IC(LQCLGA+IGNAM ) = ICHARR 
      IC(LQCLGA+IGCOOR) = 345
      IC(LQCLGA+IGPERP) = 4
      C(LQCLGA +IGDPHI) = DPHI
      C(LQCLGA +IGSERL) = FLOAT(MOD(40-J,32))          ! Kroon location         
                                                       ! number
      MODULE_VOLUME_NAME = 'CCEM_00_VOLUME'
      WRITE(MODULE_VOLUME_NAME(6:7),'(I2.2)') J
      CALL CLGSRC(LQCLGA,MODULE_VOLUME_NAME,'CC_EM_MOTHER_VOLUME')
      LC(LQCLNK - J) = LQCLGA                    ! store link for dispatching
C
C
C     PUT IN SHAPE INFORMATION
C
      IF ( J.EQ.1) THEN                         ! shape informatation on
                                        ! first module
        MCSHA(5) = IOCSHA
        CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
        CALL SHPSRC(LQCSHA,'CCEM_01_VOLUME')
      END IF
C
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLGA+IGMATE),1) ! get CMAT
      LC(LQCLGA-IXCMAT) = LQCMAT   ! ref link to CMAT
      LC(LQCLGA-IXCLGI) = LQCLGI   ! ref link to CLGI
      LC(LQCLGA-IXCSHA) = LQCSHA   ! ref link to CSHA
      LC(LQCLGI-IXCLGA) = LQCLGA   ! ref link to CLGA from CLGI
  100 CONTINUE
C
C     CEN CAL FH ACTIVE MODULES
C
      LQCLGI = LZFIND(IDVSTP,LC(LQCREG-IZCLGI),ICCFHI,IGIDEN) ! get CLGI
      CALL EZGET('CCFH_NUMBER_MODULES',NSEG,IOK)
      MCLNK(2) = NSEG
      CALL MZLIFT(IDVSTP,LQCLNK,LQCREG,-IZCLNK,MCLNK,0)
                                                ! create CLNK to hold links to
                                                ! CLGA
      IC(LQCLNK + IGIDEN) = ICCFHA              ! same ID as lead CLGA bank
      IC(LQCLNK + IGNSEG) = NSEG
      IC(LQCLNK + IGJPHI) = 4
      IC(LQCLNK + IGN1) = 6
      DO 200 J=1,NSEG
      IF( J .EQ. 1) THEN
        MCLGA(3) = 1
      ELSE
        MCLGA(3) = 0
      END IF
      CALL MZLIFT(IDVSTP,LQCLGA,LQCLGA,0,MCLGA,0)
C
      LTUBE  = LC(LQCLGI-IZCSHA)                ! addr to inactive cyl shape
      DPHI = TWOPI/NSEG                         ! 1/16 of TWOPI
      SPHI = SIN(DPHI/2)
      CPHI = COS(DPHI/2)
      IC(LQCLGA+IGIDEN) = ICCFHA + ICMINC*(J-1) ! module ident code 
      IC(LQCLGA+IGNLAY) = 3
      CHARR = 'CFHA'
      IC(LQCLGA+IGNAM ) = ICHARR 
      IC(LQCLGA+IGCOOR) = 345
      IC(LQCLGA+IGPERP) = 4
      C(LQCLGA +IGDPHI) = DPHI
      C(LQCLGA +IGSERL) = FLOAT(MOD(42-2*J,32))        ! Kroon location         
                                                       ! number
      MODULE_VOLUME_NAME = 'CCFH_00_VOLUME'
      WRITE(MODULE_VOLUME_NAME(6:7),'(I2.2)') J  ! J is GEANT module label
      CALL CLGSRC(LQCLGA,MODULE_VOLUME_NAME,'CC_FH_MOTHER_VOLUME')
      LC(LQCLNK-J) = LQCLGA
C
C     FH TRD1 SHAPE INFO
C
      IF( J.EQ.1 ) THEN                         ! shape information on
                                        ! first module
        CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
        CALL SHPSRC(LQCSHA,'CCFH_01_VOLUME')
      END IF
C
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLGA+IGMATE),1) ! get CMAT
      LC(LQCLGA-IXCMAT) = LQCMAT   ! ref link to CMAT
      LC(LQCLGA-IXCLGI) = LQCLGI   ! ref link to CLGI
      LC(LQCLGA-IXCSHA) = LQCSHA   ! ref link to CSHA
      LC(LQCLGI-IXCLGA) = LQCLGA   ! ref link to CLGA from CLGI
  200 CONTINUE
C
C     CEN CAL CH ACTIVE CYLINDER
C
      LQCLGI = LZFIND(IDVSTP,LC(LQCREG-IZCLGI),ICCCHI,IGIDEN) ! get CLGI
      CALL EZGET('CCCH_NUMBER_MODULES',NSEG,IOK)
      MCLNK(2) = NSEG
      CALL MZLIFT(IDVSTP,LQCLNK,LQCREG,-IZCLNK,MCLNK,0)
                                                ! create CLNK to hold links to
                                                ! CLGA
      IC(LQCLNK + IGIDEN) = ICCCHA              ! same ID as lead CLGA bank
      IC(LQCLNK + IGNSEG) = NSEG
      IC(LQCLNK + IGJPHI) = 4
      IC(LQCLNK + IGN1) = 4

      DO 300 J = 1, NSEG           ! loop on modules -- J is GEANT
                                   ! module number
      IF( J .EQ. 1) THEN
        MCLGA(3) = 1
      ELSE
        MCLGA(3) = 0
      END IF
      CALL MZLIFT(IDVSTP,LQCLGA,LQCLGA,0,MCLGA,0)
C
      IC(LQCLGA+IGIDEN) = ICCCHA + ICMINC*(J-1)  ! ident code
      IC(LQCLGA+IGNLAY) = 1
      CHARR = 'CCHA'
      IC(LQCLGA+IGNAM ) = ICHARR 
      IC(LQCLGA+IGCOOR) = 345
      IC(LQCLGA+IGPERP) = 4
      C(LQCLGA +IGDPHI) = DPHI
      C(LQCLGA +IGSERL) = FLOAT(MOD(41-2*J,32))     ! Kroon location number
      MODULE_VOLUME_NAME = 'CCCH_00_VOLUME'
      WRITE(MODULE_VOLUME_NAME(6:7),'(I2.2)') J
      CALL CLGSRC(LQCLGA,MODULE_VOLUME_NAME,'CC_CH_MOTHER_VOLUME')
      LC(LQCLNK-J) = LQCLGA
C
C     CH TRD1 SHAPE INFO
C
      IF ( J.EQ.1) THEN
        MCSHA(4) = 7
        DEL = CRACK/(2.0*CPHI)
        CALL MZLIFT(IDVSTP,LQCSHA,LCGEH,-IZCSHA,MCSHA,0)
        CALL SHPSRC(LQCSHA,'CCCH_01_VOLUME')
      END IF
C
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLGA+IGMATE),1) ! get CMAT
      LC(LQCLGA-IXCMAT) = LQCMAT   ! ref link to CMAT
      LC(LQCLGA-IXCLGI) = LQCLGI   ! ref link to CLGI
      LC(LQCLGA-IXCSHA) = LQCSHA   ! ref link to CSHA
      LC(LQCLGI-IXCLGA) = LQCLGA   ! ref link to CLGA from CLGI
  300 CONTINUE
C
      LQCLGA = LC(LQCREG-IZCLGA)   ! pointer to first CLGA
      CALL ZSORTI(IXSTP, LQCLGA, IGIDEN)  ! sort on increasing IDENT CODE 
      LCLGA(1) = 0                  ! deactiveate temp link area
      RETURN
      END
