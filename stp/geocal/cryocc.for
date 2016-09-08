      SUBROUTINE CRYOCC
C---------------------------------------------------------------
C
C     CREATES INACTIVE CYLINDERS FOR THE CENTRAL CALORIMETER CRYOSTAT
C     BANKS LIFTED:   CRYO, CSHA
C
C     AUTHOR:    S KAHN     19 JAN 1987
C     REVISION:  CORRECT "ROT1" BANK AND ADD "ROT2" BANK
C                NAME CHANGES TO BE CONSISTENT TO GEANT DETECTOR NAMES
C---------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:CALGEO.INC'
      INCLUDE 'D0$INC:CLGA.DEF'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$INC:CSHA.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER MCRYO(5), MCSHA(5), LORIG
      INTEGER LZFIND, LPREV
      CHARACTER*4 CHAR4,CHAS4
      INTEGER ICHARR
      CHARACTER*4 CHARR
      EQUIVALENCE (ICHARR,CHARR)
      EQUIVALENCE (CHAR4,MCRYO(1))
      EQUIVALENCE (CHAS4,MCSHA(1))
      DATA MCRYO / 4HCRYO, 4, 0, 16, 9/
      DATA MCSHA / 4HCSHA, 0, 0,  5, 9/
      DATA CHAR4 / 'CRYO'/
      DATA CHAS4 / 'CSHA'/
C
C   CENTRAL CALORIMETER CRYOSTAT CYLINDERS
C
      LQCREG = LZFIND(IXCDV,LQ(LQCGEH-1),ICCAL,IGREGN) ! get CREG
C
C
      MCRYO(5) = IOCLGA                         ! put in form address
C 
C    CEN CAL EM REGION
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      IQ(LQCRYO+IGIDEN) = ICSROU ! CENCAL CRYO REGION
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'TUBE'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'UCT1'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 4
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 0.
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      Q(LQCRYO +IGDPHI) = TWOPI
C
C     PUT IT SHAPE INFORMATION
C
      CALL MZFORM('CSHA','1H1I-F',IOCSHA)
C
      MCSHA(5) = IOCSHA
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'TUBE'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 3
      Q(LQCSHA+ IGPAR1) = 76.18    ! inner radius
      Q(LQCSHA+ IGPAR2) = 76.82    ! outer radius
      Q(LQCSHA+ IGPAR3) = 133.35   ! half length of cylinder
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMIRON,1) ! get CMAT
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C     INNER CRYO INNER WALL
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCRYO,0,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))
C
      IQ(LQCRYO+IGIDEN) = ICSRIN
      IQ(LQCRYO+IGMATE) = IMIRON
      CHARR= 'TUBE'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'UCT2'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 4
      Q(LQCRYO +IGDPHI) = TWOPI
C
C     INNER CRYO INNER WALL TUBE SHAPE INFO
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
      CHARR= 'TUBE'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 3
      Q(LQCSHA+ IGPAR1) = 82.71    !  inner radius
      Q(LQCSHA+ IGPAR2) = 83.62    !  outer radius
      Q(LQCSHA+ IGPAR3) = 133.35   !  half length of cyl
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C     INNER CRYO OUTER WALL
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCRYO,0,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))
C
      IQ(LQCRYO+IGIDEN) = ICLRIN
      IQ(LQCRYO+IGMATE) = IMIRON
      CHARR= 'TUBE'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'UCT3'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 4
      Q(LQCRYO +IGDPHI) = TWOPI
C
C     INNER CRYO OUTER WALL TUBE SHAPE INFO
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
      CHARR= 'TUBE'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 3
      Q(LQCSHA+ IGPAR1) = 244.16   ! inner radius
      Q(LQCSHA+ IGPAR2) = 245.81   ! outer radius
      Q(LQCSHA+ IGPAR3) = 97.31    ! half length of cyl
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C
C     OUTER CRYO OUTER WALL
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCRYO,0,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))
C
      IQ(LQCRYO+IGIDEN) = ICLROU
      IQ(LQCRYO+IGMATE) = IMIRON
      CHARR= 'TUBE'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'UCT4'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 4
      Q(LQCRYO +IGDPHI) = TWOPI
C
C     OUTER CRYO OUTER WALL TUBE SHAPE INFO
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
      CHARR= 'TUBE'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 3
      Q(LQCSHA+ IGPAR1) = 257.49   ! inner radius
      Q(LQCSHA+ IGPAR2) = 259.08   ! outer radius
      Q(LQCSHA+ IGPAR3) = 97.31    ! half length of cyl
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C
C     OUTER TOP SEGMENT 1
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))
      IQ(LQCRYO+IGIDEN) = ICROT1 ! outer bottom segment
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR= 'CPC2'
      IQ(LQCRYO+IGNAM)  = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 4
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 97.312
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
C
      MCSHA(4) = 50
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 48
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 15           ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 257.49       ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 259.08       ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 1.27         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 256.99       ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 258.58       ! rmax  region 2
      Q(LQCSHA+ IGPA10) = 3.81         ! z     region 3
      Q(LQCSHA+ IGPA11) = 256.36       ! rmin  region 3
      Q(LQCSHA+ IGPA12) = 258.26       ! rmax  region 3
      Q(LQCSHA+ IGPA13) = 5.84         ! z     region 4
      Q(LQCSHA+ IGPA14) = 256.04       ! rmin  region 4
      Q(LQCSHA+ IGPA15) = 257.63       ! rmax  region 4
      Q(LQCSHA+ IGPA16) = 8.26         ! z     region 5
      Q(LQCSHA+ IGPA17) = 255.09       ! rmin  region 5
      Q(LQCSHA+ IGPA18) = 256.99       ! rmax  region 5
      Q(LQCSHA+ IGPA19) = 10.48        ! z     region 6
      Q(LQCSHA+ IGPA20) = 254.14       ! rmin  region 6
      Q(LQCSHA+ IGPA21) = 256.04       ! rmax  region 6
      Q(LQCSHA+ IGPA22) = 12.7         ! z     region 7
      Q(LQCSHA+ IGPA23) = 253.18       ! rmin  region 7
      Q(LQCSHA+ IGPA24) = 255.09       ! rmax region  7
      Q(LQCSHA+ IGPA25) = 14.86        ! z     region 8
      Q(LQCSHA+ IGPA26) = 252.39       ! rmin  region 8
      Q(LQCSHA+ IGPA27) = 253.5        ! rmax  region 8
      Q(LQCSHA+ IGPA28) = 16.83        ! z     region 9
      Q(LQCSHA+ IGPA29) = 250.33       ! rmin  region 9
      Q(LQCSHA+ IGPA30) = 252.55       ! rmax  region 9
      Q(LQCSHA+ IGPA31) = 19.05        ! z     region 10
      Q(LQCSHA+ IGPA32) = 248.42        ! rmin  region 10
      Q(LQCSHA+ IGPA33) = 250.18       ! rmax  region 10
      Q(LQCSHA+ IGPA34) = 21.90        ! z     region 11
      Q(LQCSHA+ IGPA35) = 245.6        ! rmin  region 11
      Q(LQCSHA+ IGPA36) = 247.79       ! rmax  region 11
      Q(LQCSHA+ IGPA37) = 24.45        ! z     region 12
      Q(LQCSHA+ IGPA38) = 242.07       ! rmin  region 12
      Q(LQCSHA+ IGPA39) = 244.93       ! rmax  region 12
      Q(LQCSHA+ IGPA40) = 26.67        ! z     region 13
      Q(LQCSHA+ IGPA41) = 237.63       ! rmin  region 13
      Q(LQCSHA+ IGPA42) = 240.8        ! rmax  region 13
      Q(LQCSHA+ IGPA43) = 33.34        ! z     region 14
      Q(LQCSHA+ IGPA44) = 224.29        ! rmin  region 14
      Q(LQCSHA+ IGPA45) = 228.1        ! rmax  region 14
      Q(LQCSHA+ IGPA46) = 39.37        ! z     region 15
      Q(LQCSHA+ IGPA47) = 209.69       ! rmin  region 15
      Q(LQCSHA+ IGPA48) = 214.13       ! rmax  region 15
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C     OUTER TOP SEGMENT 2
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))
      IQ(LQCRYO+IGIDEN) = ICROT2 ! outer bottom segment
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR= 'CPC3'
      IQ(LQCRYO+IGNAM)  = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 4
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 136.682
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
C
      MCSHA(4) = 26
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 24
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) =  7           ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 209.69       ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 214.13       ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 5.40         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 192.53       ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 198.26       ! rmax  region 2
      Q(LQCSHA+ IGPA10) = 8.63         ! z     region 3
      Q(LQCSHA+ IGPA11) = 181.73       ! rmin  region 3
      Q(LQCSHA+ IGPA12) = 188.4        ! rmax  region 3
      Q(LQCSHA+ IGPA13) = 12.38        ! z     region 4
      Q(LQCSHA+ IGPA14) = 160.14       ! rmin  region 4
      Q(LQCSHA+ IGPA15) = 171.57       ! rmax  region 4
      Q(LQCSHA+ IGPA16) = 13.97        ! z     region 5
      Q(LQCSHA+ IGPA17) = 132.2        ! rmin  region 5
      Q(LQCSHA+ IGPA18) = 158.87       ! rmax  region 5
      Q(LQCSHA+ IGPA19) = 14.59        ! z     region 6
      Q(LQCSHA+ IGPA20) = 132.2        ! rmin  region 6
      Q(LQCSHA+ IGPA21) = 150.93       ! rmax  region 6
      Q(LQCSHA+ IGPA22) = 15.88        ! z     region 7
      Q(LQCSHA+ IGPA23) = 132.2        ! rmin  region 7
      Q(LQCSHA+ IGPA24) = 132.2        ! rmax region  7
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C     OUTER BOTTOM SEGMENT
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))
      IQ(LQCRYO+IGIDEN) = ICROB  ! outer bottom segment
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'CPC1'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 4
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 133.35
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
C
      MCSHA(4) = 38
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 36
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 11           ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 76.          ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 77.59        ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 3.81         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 76.32        ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 77.91        ! rmax  region 2
      Q(LQCSHA+ IGPA10) = 6.35         ! z     region 3
      Q(LQCSHA+ IGPA11) = 77.43        ! rmin  region 3
      Q(LQCSHA+ IGPA12) = 79.33        ! rmax  region 3
      Q(LQCSHA+ IGPA13) = 8.89         ! z     region 4
      Q(LQCSHA+ IGPA14) = 79.16        ! rmin  region 4
      Q(LQCSHA+ IGPA15) = 81.24        ! rmax  region 4
      Q(LQCSHA+ IGPA16) = 11.75        ! z     region 5
      Q(LQCSHA+ IGPA17) = 81.40        ! rmin  region 5
      Q(LQCSHA+ IGPA18) = 84.26        ! rmax  region 5
      Q(LQCSHA+ IGPA19) = 13.96        ! z     region 6
      Q(LQCSHA+ IGPA20) = 85.21        ! rmin  region 6
      Q(LQCSHA+ IGPA21) = 91.86        ! rmax  region 6
      Q(LQCSHA+ IGPA22) = 15.24        ! z     region 7
      Q(LQCSHA+ IGPA23) = 89.65        ! rmin  region 7
      Q(LQCSHA+ IGPA24) = 100.73       ! rmax  region 7
      Q(LQCSHA+ IGPA25) = 16.51        ! z     region 8
      Q(LQCSHA+ IGPA26) = 97.27        ! rmin  region 8
      Q(LQCSHA+ IGPA27) = 109.97       ! rmax  region 8
      Q(LQCSHA+ IGPA28) = 17.78        ! z     region 9
      Q(LQCSHA+ IGPA29) = 104.73       ! rmin  region 9
      Q(LQCSHA+ IGPA30) = 132.20       ! rmax  region 9
      Q(LQCSHA+ IGPA31) = 18.57        ! z     region 10
      Q(LQCSHA+ IGPA32) = 113.78       ! rmin  region 10
      Q(LQCSHA+ IGPA33) = 132.2        ! rmax  region 10
      Q(LQCSHA+ IGPA34) = 19.37        ! z     region 11
      Q(LQCSHA+ IGPA35) = 132.2        ! rmin  region 11
      Q(LQCSHA+ IGPA36) = 132.2        ! rmax  region 11
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C
C
C     INNER BOTTOM SEGMENT
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))
      IQ(LQCRYO+IGIDEN) = ICRIB  ! inner bottom segment
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'CPC4'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 4
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 133.35
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
C
      MCSHA(4) = 35
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 33
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 10           ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 82.07        ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 84.61        ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 2.54         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 82.32        ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 84.86        ! rmax  region 2
      Q(LQCSHA+ IGPA10) = 5.08         ! z     region 3
      Q(LQCSHA+ IGPA11) = 83.34        ! rmin  region 3
      Q(LQCSHA+ IGPA12) = 86.13        ! rmax  region 3
      Q(LQCSHA+ IGPA13) = 7.62         ! z     region 4
      Q(LQCSHA+ IGPA14) = 85.12        ! rmin  region 4
      Q(LQCSHA+ IGPA15) = 88.93        ! rmax  region 4
      Q(LQCSHA+ IGPA16) = 8.89         ! z     region 5
      Q(LQCSHA+ IGPA17) = 86.13        ! rmin  region 5
      Q(LQCSHA+ IGPA18) = 92.48        ! rmax  region 5
      Q(LQCSHA+ IGPA19) = 10.16        ! z     region 6
      Q(LQCSHA+ IGPA20) = 88.42        ! rmin  region 6
      Q(LQCSHA+ IGPA21) = 100.36        ! rmax  region 6
      Q(LQCSHA+ IGPA22) = 11.18        ! z     region 7
      Q(LQCSHA+ IGPA23) = 90.96        ! rmin  region 7
      Q(LQCSHA+ IGPA24) = 107.98       ! rmax  region 7
      Q(LQCSHA+ IGPA25) = 12.45        ! z     region 8
      Q(LQCSHA+ IGPA26) = 99.59        ! rmin  region 8
      Q(LQCSHA+ IGPA27) = 132.2        ! rmax  region 8
      Q(LQCSHA+ IGPA28) = 14.22        ! z     region 9
      Q(LQCSHA+ IGPA29) = 108.74       ! rmin  region 9
      Q(LQCSHA+ IGPA30) = 132.20       ! rmax  region 9
      Q(LQCSHA+ IGPA31) = 14.99        ! z     region 10
      Q(LQCSHA+ IGPA32) = 132.20       ! rmin  region 10
      Q(LQCSHA+ IGPA33) = 132.2        ! rmax  region 10
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMCCRK,1) ! get CMAT
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSCA
C
C
C     INNER TOP SEGMENT 1
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))
      IQ(LQCRYO+IGIDEN) = ICRIT1 ! inner top segment 1
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'CPC5'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 4
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 97.312
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
C
      MCSHA(4) = 50
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 48
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 15.           ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 243.34       ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 245.88       ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 1.27         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 243.34       ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 245.88       ! rmax  region 2
      Q(LQCSHA+ IGPA10) = 3.81         ! z     region 3
      Q(LQCSHA+ IGPA11) = 243.09       ! rmin  region 3
      Q(LQCSHA+ IGPA12) = 245.63       ! rmax  region 3
      Q(LQCSHA+ IGPA13) = 5.84         ! z     region 4
      Q(LQCSHA+ IGPA14) = 242.71       ! rmin  region 4
      Q(LQCSHA+ IGPA15) = 245.37       ! rmax  region 4
      Q(LQCSHA+ IGPA16) = 8.26         ! z     region 5
      Q(LQCSHA+ IGPA17) = 242.2        ! rmin  region 5
      Q(LQCSHA+ IGPA18) = 244.99       ! rmax  region 5
      Q(LQCSHA+ IGPA19) = 10.48        ! z     region 6
      Q(LQCSHA+ IGPA20) = 241.44       ! rmin  region 6
      Q(LQCSHA+ IGPA21) = 244.23       ! rmax  region 6
      Q(LQCSHA+ IGPA22) = 12.7         ! z     region 7
      Q(LQCSHA+ IGPA23) = 239.28       ! rmin  region 7
      Q(LQCSHA+ IGPA24) = 243.34       ! rmax region  7
      Q(LQCSHA+ IGPA25) = 14.86        ! z     region 8
      Q(LQCSHA+ IGPA26) = 239.28       ! rmin  region 8
      Q(LQCSHA+ IGPA27) = 242.32        ! rmax  region 8
      Q(LQCSHA+ IGPA28) = 16.76        ! z     region 9
      Q(LQCSHA+ IGPA29) = 238.26       ! rmin  region 9
      Q(LQCSHA+ IGPA30) = 241.3        ! rmax  region 9
      Q(LQCSHA+ IGPA31) = 19.05        ! z     region 10
      Q(LQCSHA+ IGPA32) = 236.48        ! rmin  region 10
      Q(LQCSHA+ IGPA33) = 239.78       ! rmax  region 10
      Q(LQCSHA+ IGPA34) = 21.84        ! z     region 11
      Q(LQCSHA+ IGPA35) = 233.82       ! rmin  region 11
      Q(LQCSHA+ IGPA36) = 236.23       ! rmax  region 11
      Q(LQCSHA+ IGPA37) = 24.38        ! z     region 12
      Q(LQCSHA+ IGPA38) = 230.64       ! rmin  region 12
      Q(LQCSHA+ IGPA39) = 234.96       ! rmax  region 12
      Q(LQCSHA+ IGPA40) = 26.67        ! z     region 13
      Q(LQCSHA+ IGPA41) = 226.32       ! rmin  region 13
      Q(LQCSHA+ IGPA42) = 231.66       ! rmax  region 13
      Q(LQCSHA+ IGPA43) = 33.34        ! z     region 14
      Q(LQCSHA+ IGPA44) = 210.57        ! rmin  region 14
      Q(LQCSHA+ IGPA45) = 217.43       ! rmax  region 14
      Q(LQCSHA+ IGPA46) = 39.37        ! z     region 15
      Q(LQCSHA+ IGPA47) = 192.54       ! rmin  region 15
      Q(LQCSHA+ IGPA48) = 201.76       ! rmax  region 15
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMCCRK,1) ! get CMAT
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C
C     INNER TOP SEGMENT
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))
      IQ(LQCRYO+IGIDEN) = ICRIT2  ! inner top segment
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'CPC6'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 4
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 136.682
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
C
      MCSHA(4) = 23
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 21
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 6            ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 192.54       ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 201.76        ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 5.46         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 167.76       ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 181.48        ! rmax  region 2
      Q(LQCSHA+ IGPA10) = 7.24         ! z     region 3
      Q(LQCSHA+ IGPA11) = 158.11        ! rmin  region 3
      Q(LQCSHA+ IGPA12) = 174.87        ! rmax  region 3
      Q(LQCSHA+ IGPA13) = 8.76         ! z     region 4
      Q(LQCSHA+ IGPA14) = 132.2        ! rmin  region 4
      Q(LQCSHA+ IGPA15) = 166.74        ! rmax  region 4
      Q(LQCSHA+ IGPA16) = 10.16        ! z     region 5
      Q(LQCSHA+ IGPA17) = 132.2        ! rmin  region 5
      Q(LQCSHA+ IGPA18) = 156.84        ! rmax  region 5
      Q(LQCSHA+ IGPA19) = 11.68        ! z     region 6
      Q(LQCSHA+ IGPA20) = 132.2        ! rmin  region 6
      Q(LQCSHA+ IGPA21) = 132.2         ! rmax  region 6
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMCCRK,1) ! get CMAT
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSCA


      RETURN
      END
