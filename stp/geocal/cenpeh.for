      SUBROUTINE CENPEH
C---------------------------------------------------------------
C
C     CREATES END PLATE BANKS FOR END HADRONIC CALORIMETER.  THE BANKS
C     ARE IN THE FORMAT OF CLGI BANKS AND ARE HOOKED ONTO THE LINEAR
C     CHAIN.
C
C     ZEBRA BANKS CREATED:   CENP, CSHA
C     ZEBRA BANKS ALTERED:   CLGI
C
C     AUTHOR:    S KAHN      15 OCT 1988
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
      REAL    DPHI, CPHI, SPHI, CRACK, DEL, PLWDTH
      REAL    PAX1(3), PAX2(3), INCH
      INTEGER LZFIND
      COMMON / LCLGA / LCLGA, LPCON, LTUBE, LORIG
      DATA MCENP / 4HCENP, 5, 0, 18, 9/
      DATA MCSHA / 4HCSHA, 0, 0,  7, 9/
      DATA NSEG  / 16 /
      DATA INCH  /2.54/                         ! conversion to cm
C
C   END PLATES FOR END CALORIMETER 
C
      CALL MZLINT(IXCST,'/LCLGA/',LCLGA,LTUBE,LORIG)     ! temp link space
      LQCREG = LZFIND(IXCDV,LQ(LQCGEH-1),ISECAL,IGREGN) ! get CREG
C
      MCENP(5) = IOCLGA                         ! put in form address
C 
C    END CAL MFH END PLATE
C
      LQCLGI = LZFIND(IXCDV,LQ(LQCREG-IXCLGI),ICMFHI,IGIDEN) ! GET CLGI
      LQCLGA = LQ(LQCLGI-IZCLGA)                ! get representative module
      LTUBE = LQ(LQCLGA-IZCSHA)                 ! CLGA shape
      DPHI = PI/NSEG                            ! 1/32 of TWOPI
      PLWDTH = 2.54                             ! plate width = 1 inch
C
      CALL MZLIFT(IXCDV,LQCENP,LQCREG,-IXCLGI,MCENP,0)
      CALL SBIT1(Q(LQCENP),IBZRFL)
      IQ(LQCENP+IGIDEN) = ICMFHE ! ENDCAL MFH ENDPLATE 
      IQ(LQCENP+IGNLAY) = 0
      IQ(LQCENP+IGMATE) = IMIRON
      IQ(LQCENP+IGSHAP) = 4HTRD1
      IQ(LQCENP+IGNAM ) = 4HMFHE
      IQ(LQCENP+IGCOOR) = 345
      IQ(LQCENP+IGPERP) = 3
      Q(LQCENP +IGRCEN) = Q(LQCLGA + IGRCEN)
      Q(LQCENP +IGPHIC) = Q(LQCLGA + IGPHIC)
C ... SHAPE OF MFH/CLGA IS TRD1
      Q(LQCENP +IGZCEN) = Q(LQCLGA+IGZCEN)-Q(LTUBE+IGPAR3)-PLWDTH/2.
      Q(LQCENP +IGDPHI) = DPHI
      Q(LQCENP +IGTHTE) = ATAN(SQRT(2.))
      Q(LQCENP +IGPHIE) = PI/4.
      Q(LQCENP +IGOMGE) = TWOPI/3.
      Q(LQCENP +IGDPHI) = PLWDTH            ! plate width = 1 inch
C
C     PUT IN SHAPE INFORMATION
C
      CALL MZFORM('CSHA','1H1I-F',IOCSHA)
C
      MCSHA(5) = IOCSHA
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      IQ(LQCSHA+IGDESC) = 4HTRD1
      IQ(LQCSHA+IGNPAR) = 4
      Q(LQCSHA+ IGPAR1) = Q(LTUBE+IGPAR1)   ! MCH end plate inner width 
      Q(LQCSHA+ IGPAR2) = Q(LTUBE+IGPAR2)   ! MCH end plate outer width 
      Q(LQCSHA +IGPAR3) = 0.5*PLWDTH        ! MCH end plate half length 
      Q(LQCSHA +IGPAR4) = Q(LTUBE+IGPAR4)   ! MCH end plate begin height
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMIRON,1) ! get CMAT
      LQ(LQCENP-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCENP-IZCLGI) = LQCLGI   ! ref link to CLGI
      LQ(LQCENP-IZCSHA) = LQCSHA   ! ref link to CSHA
      CALL SPNMOD(LQCENP,NSEG)     ! make other modules by rotation
      CALL LNKENP(LQCENP,ICMFHA)   ! link endplates to CLGA banks
C
C     END CAL MCH END PLATES
C
      LORIG = LQCENP
      CALL MZLIFT(IXCDV,LQCENP,LQCENP,0,MCENP,0)
C
      CALL SBIT1(Q(LQCENP),IBZRFL)
      LQCLGI = LZFIND(IXCDV,LQ(LQCREG-IXCLGI),ICMCHI,IGIDEN) ! get CLGI
      LQCLGA = LQ(LQCLGI - IZCLGA)
      LTUBE = LQ(LQCLGA-IZCSHA)
      CALL UCOPY(Q(LORIG+1),Q(LQCENP+1),MCENP(4))
      IQ(LQCENP+IGIDEN) = ICMCHE            ! end cal MCH endplate
      IQ(LQCENP+IGNLAY) = 0
      IQ(LQCENP+IGMATE) = IMIRON
      IQ(LQCENP+IGSHAP) = 4HTRD1
      IQ(LQCENP+IGNAM ) = 4HMCHE
      IQ(LQCENP+IGCOOR) = 345
      IQ(LQCENP+IGPERP) = 3
      Q(LQCENP +IGZCEN) = Q(LQCLGA+IGZCEN)+Q(LTUBE+IGPAR3)+PLWDTH/2.
      Q(LQCENP +IGDPHI) = DPHI
C
      LQ(LQCENP-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCENP-IZCLGI) = LQCLGI   ! ref link to CLGI
      LQ(LQCENP-IZCSHA) = LQCSHA   ! ref link to CSHA
      CALL SPNMOD(LQCENP,NSEG)     ! make other modules by rotation
      CALL LNKENP(LQCENP,ICMCHA)   ! link CENP to CLGA
C 
C    END CAL IFH END PLATE
C
      LQCLGI = LZFIND(IXCDV,LQ(LQCREG-IXCLGI),ICIFHI,IGIDEN) ! GET CLGI
      LQCLGA = LQ(LQCLGI-IZCLGA)                ! get representative module
      LTUBE = LQ(LQCLGA-IZCSHA)                 ! CLGI shape
      DPHI = PI/NSEG                            ! 1/32 of TWOPI
      PLWDTH = 3.175                            ! plate width = 1.25 inch
C
      CALL MZLIFT(IXCDV,LQCENP,LQCREG,-IXCLGI,MCENP,0)
      CALL SBIT1(Q(LQCENP),IBZRFL)
      IQ(LQCENP+IGIDEN) = ICIFHE ! ENDCAL IFH ENDPLATE 
      IQ(LQCENP+IGNLAY) = 0
      IQ(LQCENP+IGMATE) = IMIRON
      IQ(LQCENP+IGSHAP) = 4HTUBS
      IQ(LQCENP+IGNAM ) = 4HIFHE
      IQ(LQCENP+IGCOOR) = 345
      IQ(LQCENP+IGPERP) = 3
      Q(LQCENP +IGRCEN) = 0.
      Q(LQCENP +IGPHIC) = Q(LQCLGA + IGPHIC) 
      Q(LQCENP +IGZCEN) = Q(LQCLGA+IGZCEN)-Q(LTUBE+IGPAR3)-PLWDTH/2.
      Q(LQCENP +IGDPHI) = DPHI
      Q(LQCENP +IGTHTE) = 0.
      Q(LQCENP +IGPHIE) = 0.
      Q(LQCENP +IGOMGE) = 0.
      Q(LQCENP +IGDPHI) = PLWDTH            ! plate width = 1 inch
C
C     PUT IN SHAPE INFORMATION
C
      CALL MZFORM('CSHA','1H1I-F',IOCSHA)
C
      MCSHA(5) = IOCSHA
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      IQ(LQCSHA+IGDESC) = 4HTUBS
      IQ(LQCSHA+IGNPAR) = 5
      Q(LQCSHA+ IGPAR1) = Q(LTUBE+IGPAR1)   ! MCH end plate inner radius
      Q(LQCSHA+ IGPAR2) = Q(LTUBE+IGPAR2)   ! MCH end plate outer radius
      Q(LQCSHA +IGPAR3) = 0.5*PLWDTH        ! MCH end plate half width
      Q(LQCSHA +IGPAR4) = Q(LTUBE+IGPAR4)   ! MCH end plate begin angle 
      Q(LQCSHA +IGPAR5) = Q(LTUBE+IGPAR5)   ! MCH end plate end angle
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMIRON,1) ! get CMAT
      LQ(LQCENP-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCENP-IZCLGI) = LQCLGI   ! ref link to CLGI
      LQ(LQCENP-IZCSHA) = LQCSHA   ! ref link to CSHA
      CALL SPNMOD(LQCENP,NSEG)     ! make other modules by rotation
      CALL LNKENP(LQCENP,ICIFHA)   ! link endplates to CLGA banks
C
C     END CAL ICH END PLATES
C
      LORIG = LQCENP
      CALL MZLIFT(IXCDV,LQCENP,LQCENP,0,MCENP,0)
C
      CALL SBIT1(Q(LQCENP),IBZRFL)
      LQCLGI = LZFIND(IXCDV,LQ(LQCREG-IXCLGI),ICICHI,IGIDEN) ! get CLGI
      LQCLGA = LQ(LQCLGI - IZCLGA)
      LTUBE = LQ(LQCLGA-IZCSHA)
      CALL UCOPY(Q(LORIG+1),Q(LQCENP+1),MCENP(4))
      IQ(LQCENP+IGIDEN) = ICICHE            ! end cal MCH endplate
      IQ(LQCENP+IGNLAY) = 0
      IQ(LQCENP+IGMATE) = IMIRON
      IQ(LQCENP+IGSHAP) = 4HTUBS
      IQ(LQCENP+IGNAM ) = 4HICHE
      IQ(LQCENP+IGCOOR) = 345
      IQ(LQCENP+IGPERP) = 3
      Q(LQCENP +IGZCEN) = Q(LQCLGA+IGZCEN)+Q(LTUBE+IGPAR3)+PLWDTH/2.
      Q(LQCENP +IGDPHI) = DPHI
C
      LQ(LQCENP-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCENP-IZCLGI) = LQCLGI   ! ref link to CLGI
      LQ(LQCENP-IZCSHA) = LQCSHA   ! ref link to CSHA
      CALL SPNMOD(LQCENP,NSEG)     ! make other modules by rotation
      CALL LNKENP(LQCENP,ICICHA)   ! link CENP to CLGA
C
C     END CAL OCH END PLATES
C
      CALL MZLIFT(IXCDV,LQCENP,LQCENP,0,MCENP,0)
C
      CALL SBIT1(Q(LQCENP),IBZRFL)
      LQCLGI = LZFIND(IXCDV,LQ(LQCREG-IXCLGI),ICOCHI,IGIDEN) ! get CLGI
      LQCLGA = LQ(LQCLGI-IZCLGA)                ! link to 1st CLGA bank
      DPHI = Q(LQCLGA+IGDPHI)                   ! 1/16 of TWOPI
      IQ(LQCENP+IGIDEN) = ICOCHE                ! ID
      IQ(LQCENP+IGNLAY) = 0
      IQ(LQCENP+IGMATE) = IMIRON                ! material code - Fe
      IQ(LQCENP+IGSHAP) = 4HTRAP                ! shape
      IQ(LQCENP+IGNAM ) = 4HOCHE                ! name
      IQ(LQCENP+IGCOOR) = 345                   ! coordinate system
      IQ(LQCENP+IGPERP) = 9                     ! perpendicular coordinate
      Q(LQCENP +IGRCEN) = 74.8905 * INCH        ! r coordinate at mid point
      Q(LQCENP +IGZCEN) = 69.2988 * INCH        ! z position at mid point
      Q(LQCENP +IGPHIC) = Q(LQCLGA +IGPHIC)     ! phi position
      Q(LQCENP +IGDPHI) = DPHI
C
C ... FIRST CHANGE COORD SYSTEM (FOR GEANT TRAP)
C     X' => Y, Y' => Z, Z' => X
      PAX1(1) = 0.
      PAX1(2) = 0.
      PAX1(3) = -HALFPI
C ... ROTATE MODULE POSITION
      PAX2(1) = HALFPI
      PAX2(2) = Q(LQCENP+IGPHIC)-HALFPI
      PAX2(3) = -PI/6.
C ... COMBINE ROTATIONS AND STORE
      CALL ROTMLT(PAX2,PAX1,Q(LQCENP+IGTHTE))
C
C     OCH TRAP SHAPE INFO
C
      MCSHA(4) = 13
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
      IQ(LQCSHA+IGDESC) = 4HTRAP
      IQ(LQCSHA+IGNPAR) = 11
      Q(LQCSHA+ IGPAR1) = 0.55     ! OCH end plate half width 
      Q(LQCSHA+ IGPAR2) = 30.      ! OCH end plate dirn to other z face 
      Q(LQCSHA +IGPAR3) = 270.     ! OCH end plate phi dirn to other z
C                                  ! face 
      Q(LQCSHA +IGPAR4) = 14.3178 * INCH  ! OCH H1 half height 
      Q(LQCSHA +IGPAR5) = 12.4288 * INCH  ! OCH BL1 base length
      Q(LQCSHA +IGPAR6) = 17.3625 * INCH  ! OCH TL1 top lenght
      Q(LQCSHA +IGPAR7) = 0.       ! OCH alpha1
      Q(LQCSHA +IGPAR8) = Q(LQCSHA + IGPAR4)   ! H2
      Q(LQCSHA +IGPAR9) = Q(LQCSHA + IGPAR5)   ! BL2
      Q(LQCSHA +IGPA10) = Q(LQCSHA + IGPAR6)   ! TL2
      Q(LQCSHA +IGPA11) = Q(LQCSHA + IGPAR7)   ! Alpha2
C
      LQ(LQCENP-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCENP-IZCLGI) = LQCLGI   ! ref link to CLGI
      LQ(LQCENP-IZCSHA) = LQCSHA   ! ref link to CSHA
      CALL SPNMOD(LQCENP,NSEG)     ! make other modules by rotation
      CALL LNKENP(LQCENP,ICOCHA)   ! link CENP to CLGA
C
      LCLGA(1) = 0                  ! deactiveate temp link area
      RETURN
      END
