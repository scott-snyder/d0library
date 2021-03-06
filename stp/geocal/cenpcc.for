      SUBROUTINE CENPCC
C---------------------------------------------------------------
C
C     CREATES END PLATE BANKS FOR CENTRAL CALORIMETER.  THE BANKS
C     ARE IN THE FORMAT OF CLGI BANKS AND ARE HOOKED ONTO THE LINEAR
C     CHAIN.
C
C     ZEBRA BANKS CREATED:   CLGI, CSHA
C     ZEBRA BANKS ALTERED:   CLGI
C
C     AUTHOR:    S KAHN      15 JULY 1987
C---------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:CALGEO.INC'
      INCLUDE 'D0$INC:CLGA.DEF'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$INC:CSHA.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER MCENP(5), MCSHA(5), LORIG, LPCON, LTUBE, NSEG, LCLGA(2)
      REAL    DPHI, CPHI, SPHI, CRACK, DEL
      REAL    PAX1(3), PAX2(3)
      INTEGER LZFIND
      COMMON / LCLGA / LCLGA, LPCON, LTUBE, LORIG
      CHARACTER*4 CHAR4,CHAS4
      INTEGER ICHARR
      CHARACTER*4 CHARR
      EQUIVALENCE (ICHARR,CHARR)
      EQUIVALENCE (CHAR4,MCENP(1))
      EQUIVALENCE (CHAS4,MCSHA(1))
      DATA MCENP / 4HCENP, 5, 0, 18, 9/
      DATA MCSHA / 4HCSHA, 0, 0,  6, 9/
      DATA CHAR4 / 'CENP'/
      DATA CHAS4 / 'CSHA'/

      DATA NSEG  / 16 /
C
C   CENTRAL CALORIMETER MODULES
C
      CALL MZLINT(IXCST,'/LCLGA/',LCLGA,LTUBE,LORIG)     ! temp link space
      LQCREG = LZFIND(IXCDV,LQ(LQCGEH-1),ICCAL,IGREGN) ! get CREG
C
      MCENP(5) = IOCLGA                         ! put in form address
C 
C    CEN CAL EM END PLATE
C
      LQCLGI = LZFIND(IXCDV,LQ(LQCREG-IXCLGI),ICCEMI,IGIDEN) ! GET CLGI
      LTUBE = LQ(LQCLGI-IZCSHA)                 ! CLGI shape
      DPHI = PI/NSEG                            ! 1/32 of TWOPI
      CPHI = COS(DPHI/2)
C
      CALL MZLIFT(IXCDV,LQCENP,LQCREG,-IXCLGI,MCENP,0)
      CALL SBIT1(Q(LQCENP),IBZRFL)
      IQ(LQCENP+IGIDEN) = ICCEME ! CENCAL EM ACTIVE REGION
      IQ(LQCENP+IGNLAY) = 0
      IQ(LQCENP+IGMATE) = IMIRON
      CHARR='TRD1'
      IQ(LQCENP+IGSHAP) = ICHARR
      CHARR='EPEM'
      IQ(LQCENP+IGNAM ) = ICHARR
      IQ(LQCENP+IGCOOR) = 345
      IQ(LQCENP+IGPERP) = 4
      Q(LQCENP +IGRCEN) = 0.5*(Q(LTUBE+IGPAR2)*CPHI+Q(LTUBE+IGPAR1))
      Q(LQCENP +IGPHIC) = 0.5*DPHI
      Q(LQCENP +IGZCEN) = 131.204
      Q(LQCENP +IGDPHI) = DPHI
C
C ... FIRST CHANGE COORD SYSTEM (FOR GEANT TRD2)
C     X' => Y, Y' => Z, Z' => X
      PAX1(1) = ATAN(SQRT(2.))
      PAX1(2) = PI/4.
      PAX1(3) = TWOPI/3.
C ... ROTATE MODULE POSITION
      PAX2(1) = 0.
      PAX2(2) = 0.
      PAX2(3) = 0.5*DPHI
C ... COMBINE ROTATIONS AND STORE
      CALL ROTMLT(PAX2,PAX1,Q(LQCENP+IGTHTE))
C
C     PUT IN SHAPE INFORMATION
C
      CALL MZFORM('CSHA','1H1I-F',IOCSHA)
C
      MCSHA(5) = IOCSHA
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR='TRD1'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 4
      Q(LQCSHA+ IGPAR1) = 8.24     ! EM end plate inner side
      Q(LQCSHA+ IGPAR2) = 9.93     ! EM end plate outer side
      Q(LQCSHA +IGPAR3) = 0.94     ! EM end plate half length of trapezoid
      Q(LQCSHA +IGPAR4) = 10.45    ! EM end plate half width
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMIRON,1) ! get CMAT
      LQ(LQCENP-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCENP-IZCLGI) = LQCLGI   ! ref link to CLGI
      LQ(LQCENP-IZCSHA) = LQCSHA   ! ref link to CSHA
      CALL SPNMOD(LQCENP,2*NSEG)   ! make other modules by rotation
      CALL LNKENP(LQCENP,ICCEMA)   ! link endplates to CLGA banks
C
C     CEN CAL FH END PLATES
C
      CALL MZLIFT(IXCDV,LQCENP,LQCENP,0,MCENP,0)
C
      CALL SBIT1(Q(LQCENP),IBZRFL)
      LQCLGI = LZFIND(IXCDV,LQ(LQCREG-IXCLGI),ICCFHI,IGIDEN) ! get CLGI
      LTUBE = LQ(LQCLGI-IZCSHA)                 ! CLGI shape
      DPHI = TWOPI/NSEG                         ! 1/16 of TWOPI
      CPHI = COS(DPHI/2)
      IQ(LQCENP+IGIDEN) = ICCFHE
      IQ(LQCENP+IGNLAY) = 0
      IQ(LQCENP+IGMATE) = IMIRON
      CHARR='TRD1'
      IQ(LQCENP+IGSHAP) = ICHARR
      CHARR='EPFH'
      IQ(LQCENP+IGNAM ) = ICHARR
      IQ(LQCENP+IGCOOR) = 345
      IQ(LQCENP+IGPERP) = 4
      Q(LQCENP +IGRCEN) = 0.5*(Q(LTUBE+IGPAR2)*CPHI+Q(LTUBE+IGPAR1))
      Q(LQCENP +IGZCEN) = 131.21
      Q(LQCENP +IGPHIC) = -0.25*DPHI
      Q(LQCENP +IGDPHI) = DPHI
C
C ... FIRST CHANGE COORD SYSTEM (FOR GEANT TRD2)
C     X' => Y, Y' => Z, Z' => X
      PAX1(1) = ATAN(SQRT(2.))
      PAX1(2) = PI/4.
      PAX1(3) = TWOPI/3.
C ... ROTATE MODULE POSITION
      PAX2(1) = 0.
      PAX2(2) = 0.
      PAX2(3) = -0.25*DPHI
C ... COMBINE ROTATIONS AND STORE
      CALL ROTMLT(PAX2,PAX1,Q(LQCENP+IGTHTE))
C
C     FH TRD1 SHAPE INFO
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
      CHARR='TRD1'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 4
      Q(LQCSHA+ IGPAR1) = 20.97    ! FH end plate inner side
      Q(LQCSHA+ IGPAR2) = 32.57    ! FH end plate outer side
      Q(LQCSHA +IGPAR3) = 1.58     ! FH end plate half length of trapezoid
      Q(LQCSHA +IGPAR4) = 30.79    ! FH end plate half width
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMCCFH,1)      ! get CMAT
      LQ(LQCENP-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCENP-IZCLGI) = LQCLGI   ! ref link to CLGI
      LQ(LQCENP-IZCSHA) = LQCSHA   ! ref link to CSHA
      CALL SPNMOD(LQCENP,NSEG)     ! make other modules by rotation
      CALL LNKENP(LQCENP,ICCFHA)   ! link CENP to CLGA
C
      LCLGA(1) = 0                  ! deactiveate temp link area
      RETURN
      END
