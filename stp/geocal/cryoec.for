      SUBROUTINE CRYOEC
C---------------------------------------------------------------
C
C     CREATES INACTIVE CYLINDERS FOR THE END CALORIMETER CRYOSTAT
C     BANKS LIFTED:   CRYO, CSHA
C
C     AUTHOR:    S KAHN     26 JAN 1987
C     REVISION:  SMALL BEAM PIPE DIMENSIONS  -- S KAHN  22 JUN 1987
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
      DATA MCRYO / 0, 4, 0, 16, 9/
      DATA MCSHA / 0, 0, 0,  5, 9/
      DATA CHAR4 / 'CRYO'/
      DATA CHAS4 / 'CSHA'/
C
C   END CAP CALORIMETER CRYOSTAT SOLIDS OF REVOLUTION
C
      LQCREG = LZFIND(IXCDV,LQ(LQCGEH-1),INECAL,IGREGN) ! get CREG
C
C
      MCRYO(5) = IOCLGA                         ! put in form address
C 
C    END CAL REGION
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      IQ(LQCRYO+IGIDEN) = ICEPC1 ! END CAL CRYO REGION
      CHARR = 'PC01'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 127.
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      Q(LQCRYO +IGDPHI) = TWOPI
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IT SHAPE INFORMATION
C
      CALL MZFORM('CSHA','1H1I-F',IOCSHA)
      MCSHA(4) = 11
      MCSHA(5) = IOCSHA
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 9
      Q(LQCSHA+ IGPAR1) = 0.       ! phi 0
      Q(LQCSHA+ IGPAR2) = 360.     ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 2.       ! numb of z regions
      Q(LQCSHA+ IGPAR4) = 0.       ! z     region 1
      Q(LQCSHA+ IGPAR5) = 251.46   ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 259.08   ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 7.62     ! z     region 2
      Q(LQCSHA+ IGPAR8) = 251.46   ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 259.08   ! rmax  region 2
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMIRON,1) ! get CMAT
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C     SOLID OF REVOLUTION 2
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCRYO,0,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))  ! copy previous
C
      IQ(LQCRYO+IGIDEN) = ICEPC2
      CHARR = 'PC02'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGMATE) = IMIRON
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGZCEN) = 144.323
      Q(LQCRYO +IGDPHI) = TWOPI
      CALL SBIT1(IQ(LQCRYO),IBZRFL)  !  reflect in z
C
C     INNER CRYO INNER WALL TUBE SHAPE INFO
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 9
      Q(LQCSHA+ IGPAR1) = 0.       !  phi 0
      Q(LQCSHA+ IGPAR2) = 360.     !  dphi in degrees 
      Q(LQCSHA+ IGPAR3) = 2.       !  number of Z regions
      Q(LQCSHA+ IGPAR4) = 0.       !  z      region 1
      Q(LQCSHA+ IGPAR5) = 230.51   !  rmin   region 1
      Q(LQCSHA+ IGPAR6) = 245.75   !  rmax   region 1
      Q(LQCSHA+ IGPAR7) = 15.24    !  z      region 2
      Q(LQCSHA+ IGPAR8) = 230.51   !  rmin   region 2
      Q(LQCSHA+ IGPAR9) = 245.75   !  rmax   region 2
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C     SOLID OF REVOLUTION 3
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCRYO,0,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))  ! copy previous
C
      IQ(LQCRYO+IGIDEN) = ICEPC3
      CHARR = 'PC03'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGMATE) = IMIRON
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO+ IGZCEN) = 173.99
      Q(LQCRYO +IGDPHI) = TWOPI
      CALL SBIT1(IQ(LQCRYO),IBZRFL)  !  reflect in z
C
C     PCON SHAPE INFO
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 9
      Q(LQCSHA+ IGPAR1) = 0.       ! phi 0
      Q(LQCSHA+ IGPAR2) = 360.     ! dphi in degree
      Q(LQCSHA+ IGPAR3) = 2.       ! numb of z regions
      Q(LQCSHA+ IGPAR4) = 0.       ! z     region 1
      Q(LQCSHA+ IGPAR5) = 123.19   ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 133.35   ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 3.81     ! z     region 2
      Q(LQCSHA+ IGPAR8) = 123.19   ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 133.35   ! rmax  region 2
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C
C     SOLID OF REVOLUTION 4
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCRYO,0,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))  ! copy previous
C
      IQ(LQCRYO+IGIDEN) = ICEPC4
      CHARR = 'PC04'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGMATE) = IMIRON
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGDPHI) = TWOPI
      Q(LQCRYO +IGZCEN) = 180.59
      CALL SBIT1(IQ(LQCRYO),IBZRFL)  !  reflect in z
C
C     OUTER CRYO OUTER WALL TUBE SHAPE INFO
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
      CHARR= 'TUBE'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 9
      Q(LQCSHA+ IGPAR1) = 0.       ! phi 0
      Q(LQCSHA+ IGPAR2) = 360.     ! dphi
      Q(LQCSHA+ IGPAR3) = 2.       ! numb of regions
      Q(LQCSHA+ IGPAR4) = 0.       ! z    region 1
      Q(LQCSHA+ IGPAR5) = 121.92   ! rmin    region 1
      Q(LQCSHA+ IGPAR6) = 139.7    ! rmax region 1
      Q(LQCSHA+ IGPAR7) = 6.99     ! z    region 2
      Q(LQCSHA+ IGPAR8) = 121.92   ! rmin region 2
      Q(LQCSHA+ IGPAR9) = 139.7    ! rmax region 2
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C
C     SOLID OF REVOLUTION 5
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))  ! copy previous
      IQ(LQCRYO+IGIDEN) = ICEPC5 ! outer bottom segment
      CHARR = 'PC05'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 388.12
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
      CALL MZFORM('CSHA','1H1I-F',IOCSHA)
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 9
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 2            ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 2.95275      ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 63.5         ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 2.54         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 2.95275      ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 63.5         ! rmax  region 2
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C
C     SOLID OF REVOLUTION 6
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))  ! copy previous
      IQ(LQCRYO+IGIDEN) = ICEPC6  
      CHARR = 'PC06'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 383.04
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
      CALL MZFORM('CSHA','1H1I-F',IOCSHA)
C
      MCSHA(4) = 23
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 9
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 2            ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 3.7465       ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 63.5         ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 2.54         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 3.7465       ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 63.5         ! rmax  region 2
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C
C     CURVED SURFACE 7
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))  ! copy previous
      IQ(LQCRYO+IGIDEN) = ICEPC7
      CHARR = 'PC07'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 127.00
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 21
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 6            ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 251.46       ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 251.46       ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 2.54         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 246.38       ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 251.46       ! rmax  region 2
      Q(LQCSHA+ IGPA10) = 15.24        ! z     region 3
      Q(LQCSHA+ IGPA11) = 220.98       ! rmin  region 3
      Q(LQCSHA+ IGPA12) = 226.06       ! rmax  region 3
      Q(LQCSHA+ IGPA13) = 35.56        ! z     region 4
      Q(LQCSHA+ IGPA14) = 171.45       ! rmin  region 4
      Q(LQCSHA+ IGPA15) = 176.53       ! rmax  region 4
      Q(LQCSHA+ IGPA16) = 45.72        ! z     region 5
      Q(LQCSHA+ IGPA17) = 133.35       ! rmin  region 5
      Q(LQCSHA+ IGPA18) = 140.97       ! rmax  region 5
      Q(LQCSHA+ IGPA19) = 49.53        ! z     region 6
      Q(LQCSHA+ IGPA20) = 133.35        ! rmin  region 6
      Q(LQCSHA+ IGPA21) = 133.35        ! rmax  region 6
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMCCRK,1) ! get CMAT
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSCA
C
C
C     CURVED SURFACE 8
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))  ! copy previous
      IQ(LQCRYO+IGIDEN) = ICEPC8 
      CHARR = 'PC08'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 144.32
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
      CALL MZFORM('CSHA','1H1I-F',IOCSHA)
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 21
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 6.            ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 230.51       ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 230.51       ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 5.08         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 219.71       ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 230.51       ! rmax  region 2
      Q(LQCSHA+ IGPA10) = 20.32        ! z     region 3
      Q(LQCSHA+ IGPA11) = 186.69       ! rmin  region 3
      Q(LQCSHA+ IGPA12) = 198.12       ! rmax  region 3
      Q(LQCSHA+ IGPA13) = 30.48        ! z     region 4
      Q(LQCSHA+ IGPA14) = 158.75       ! rmin  region 4
      Q(LQCSHA+ IGPA15) = 172.72       ! rmax  region 4
      Q(LQCSHA+ IGPA16) = 36.83        ! z     region 5
      Q(LQCSHA+ IGPA17) = 139.7        ! rmin  region 5
      Q(LQCSHA+ IGPA18) = 154.94       ! rmax  region 5
      Q(LQCSHA+ IGPA19) = 41.91        ! z     region 6
      Q(LQCSHA+ IGPA20) = 139.7        ! rmin  region 6
      Q(LQCSHA+ IGPA21) = 139.7        ! rmax  region 6
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMCCRK,1) ! get CMAT
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C
C     CURVED SURFACE 9
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))  ! copy previous
      IQ(LQCRYO+IGIDEN) = ICEPC9
      CHARR = 'PC09'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 305.41
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
      Q(LQCSHA+ IGPAR3) = 10.            ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 244.16       ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 245.75        ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 5.08         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 243.52       ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 245.11        ! rmax  region 2
      Q(LQCSHA+ IGPA10) = 15.24         ! z     region 3
      Q(LQCSHA+ IGPA11) = 240.03        ! rmin  region 3
      Q(LQCSHA+ IGPA12) = 241.62        ! rmax  region 3
      Q(LQCSHA+ IGPA13) = 20.32         ! z     region 4
      Q(LQCSHA+ IGPA14) = 234.95       ! rmin  region 4
      Q(LQCSHA+ IGPA15) = 237.49        ! rmax  region 4
      Q(LQCSHA+ IGPA16) = 25.40        ! z     region 5
      Q(LQCSHA+ IGPA17) = 229.24        ! rmin  region 5
      Q(LQCSHA+ IGPA18) = 232.41        ! rmax  region 5
      Q(LQCSHA+ IGPA19) = 45.72        ! z     region 6
      Q(LQCSHA+ IGPA20) = 186.69       ! rmin  region 6
      Q(LQCSHA+ IGPA21) = 191.14        ! rmax  region 6
      Q(LQCSHA+ IGPA22) = 60.96        ! z     region 7
      Q(LQCSHA+ IGPA23) = 143.51       ! rmin  region 7
      Q(LQCSHA+ IGPA24) = 148.59       ! rmax  region 7
      Q(LQCSHA+ IGPA25) = 71.12        ! z     region 8
      Q(LQCSHA+ IGPA26) = 104.14       ! rmin  region 8
      Q(LQCSHA+ IGPA27) = 111.76       ! rmax  region 8
      Q(LQCSHA+ IGPA28) = 78.74        ! z     region 9
      Q(LQCSHA+ IGPA29) = 63.5         ! rmin  region 9
      Q(LQCSHA+ IGPA30) = 73.66        ! rmax  region 9
      Q(LQCSHA+ IGPA31) = 80.65        ! z     region 10
      Q(LQCSHA+ IGPA32) = 63.5         ! rmin  region 10
      Q(LQCSHA+ IGPA33) = 63.5         ! rmax  region 10
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C
C     CURVE SURFACE 10
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))  ! copy previous
      IQ(LQCRYO+IGIDEN) = ICEP10
      CHARR = 'PC10'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 305.41
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
      CALL MZFORM('CSHA','1H1I-F',IOCSHA)
C
      MCSHA(4) = 38
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 36
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 11.5           ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 257.49       ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 259.08       ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 5.08         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 256.86       ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 258.45       ! rmax  region 2
      Q(LQCSHA+ IGPA10) = 15.24         ! z     region 3
      Q(LQCSHA+ IGPA11) = 254.00       ! rmin  region 3
      Q(LQCSHA+ IGPA12) = 255.59       ! rmax  region 3
      Q(LQCSHA+ IGPA13) = 20.32        ! z     region 4
      Q(LQCSHA+ IGPA14) = 250.19       ! rmin  region 4
      Q(LQCSHA+ IGPA15) = 252.73       ! rmax  region 4
      Q(LQCSHA+ IGPA16) = 25.4         ! z     region 5
      Q(LQCSHA+ IGPA17) = 245.11       ! rmin  region 5
      Q(LQCSHA+ IGPA18) = 247.65       ! rmax  region 5
      Q(LQCSHA+ IGPA19) = 45.72        ! z     region 6
      Q(LQCSHA+ IGPA20) = 204.47       ! rmin  region 6
      Q(LQCSHA+ IGPA21) = 208.28       ! rmax  region 6
      Q(LQCSHA+ IGPA22) = 60.96        ! z     region 7
      Q(LQCSHA+ IGPA23) = 163.93       ! rmin  region 7
      Q(LQCSHA+ IGPA24) = 168.91       ! rmax region  7
      Q(LQCSHA+ IGPA25) = 71.12        ! z     region 8
      Q(LQCSHA+ IGPA26) = 129.54       ! rmin  region 8
      Q(LQCSHA+ IGPA27) = 135.26       ! rmax  region 8
      Q(LQCSHA+ IGPA28) = 78.74        ! z     region 9
      Q(LQCSHA+ IGPA29) = 97.79        ! rmin  region 9
      Q(LQCSHA+ IGPA30) = 105.41       ! rmax  region 9
      Q(LQCSHA+ IGPA31) = 83.82        ! z     region 10
      Q(LQCSHA+ IGPA32) = 63.5         ! rmin  region 10
      Q(LQCSHA+ IGPA33) = 74.93        ! rmax  region 10
      Q(LQCSHA+ IGPA34) = 85.73        ! z     region 11
      Q(LQCSHA+ IGPA35) = 63.5         ! rmin  region 11
      Q(LQCSHA+ IGPA36) = 63.5         ! rmax  region 11
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C     SECTION 11
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))  ! copy previous
      IQ(LQCRYO+IGIDEN) = ICEP11 ! inner bottom segment
      CHARR = 'PC11'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 143.32
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 36
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 11           ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 2.95275      ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 2.95275      ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 1.07         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 2.95275      ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 25.4         ! rmax  region 2
      Q(LQCSHA+ IGPA10) = 2.54         ! z     region 3
      Q(LQCSHA+ IGPA11) = 24.13        ! rmin  region 3
      Q(LQCSHA+ IGPA12) = 36.83        ! rmax  region 3
      Q(LQCSHA+ IGPA13) = 5.08         ! z     region 4
      Q(LQCSHA+ IGPA14) = 43.18        ! rmin  region 4
      Q(LQCSHA+ IGPA15) = 49.53        ! rmax  region 4
      Q(LQCSHA+ IGPA16) = 7.62         ! z     region 5
      Q(LQCSHA+ IGPA17) = 57.15        ! rmin  region 5
      Q(LQCSHA+ IGPA18) = 63.5         ! rmax  region 5
      Q(LQCSHA+ IGPA19) = 12.7         ! z     region 6
      Q(LQCSHA+ IGPA20) = 76.2         ! rmin  region 6
      Q(LQCSHA+ IGPA21) = 81.28         ! rmax  region 6
      Q(LQCSHA+ IGPA22) = 17.78        ! z     region 7
      Q(LQCSHA+ IGPA23) = 90.17        ! rmin  region 7
      Q(LQCSHA+ IGPA24) = 93.98        ! rmax  region 7
      Q(LQCSHA+ IGPA25) = 22.86        ! z     region 8
      Q(LQCSHA+ IGPA26) = 101.6        ! rmin  region 8
      Q(LQCSHA+ IGPA27) = 104.78       ! rmax  region 8
      Q(LQCSHA+ IGPA28) = 25.4         ! z     region 9
      Q(LQCSHA+ IGPA29) = 107.95       ! rmin  region 9
      Q(LQCSHA+ IGPA30) = 110.49       ! rmax  region 9
      Q(LQCSHA+ IGPA31) = 30.48        ! z     region 10
      Q(LQCSHA+ IGPA32) = 120.2        ! rmin  region 10
      Q(LQCSHA+ IGPA33) = 123.19       ! rmax  region 10
      Q(LQCSHA+ IGPA34) = 31.55        ! z     region 11
      Q(LQCSHA+ IGPA35) = 123.19       ! rmin  region 11
      Q(LQCSHA+ IGPA36) = 123.19       ! rmax  region 11R
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMCCRK,1) ! get CMAT
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSCA
C
C     SECTION 12
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))  ! copy previous
      IQ(LQCRYO+IGIDEN) = ICEP12 ! segment 12
      CHARR = 'PC12'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'PCON'
      IQ(LQCRYO+IGSHAP) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 148.40
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      CALL SBIT1(IQ(LQCRYO),IBZRFL) ! reflect on Z
C
C     PUT IN CRYO SHAPE INFORMATION
C
      CALL MZFORM('CSHA','1H1I-F',IOCSHA)
C
      MCSHA(4) = 41
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'PCON'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 39
      Q(LQCSHA+ IGPAR1) = 0.           ! phi 0              
      Q(LQCSHA+ IGPAR2) = 360.         ! dphi in degrees
      Q(LQCSHA+ IGPAR3) = 12.          ! number of z subregions
      Q(LQCSHA+ IGPAR4) = 0.           ! z     region 1
      Q(LQCSHA+ IGPAR5) = 3.7465       ! rmin  region 1
      Q(LQCSHA+ IGPAR6) = 3.7465       ! rmax  region 1
      Q(LQCSHA+ IGPAR7) = 1.07         ! z     region 2
      Q(LQCSHA+ IGPAR8) = 3.7465       ! rmin  region 2
      Q(LQCSHA+ IGPAR9) = 25.4         ! rmax  region 2
      Q(LQCSHA+ IGPA10) = 2.54         ! z     region 3
      Q(LQCSHA+ IGPA11) = 26.67        ! rmin  region 3
      Q(LQCSHA+ IGPA12) = 35.56        ! rmax  region 3
      Q(LQCSHA+ IGPA13) = 5.08         ! z     region 4
      Q(LQCSHA+ IGPA14) = 44.45        ! rmin  region 4
      Q(LQCSHA+ IGPA15) = 49.53        ! rmax  region 4
      Q(LQCSHA+ IGPA16) = 7.62         ! z     region 5
      Q(LQCSHA+ IGPA17) = 55.88        ! rmin  region 5
      Q(LQCSHA+ IGPA18) = 60.96        ! rmax  region 5
      Q(LQCSHA+ IGPA19) = 10.16        ! z     region 6
      Q(LQCSHA+ IGPA20) = 66.04        ! rmin  region 6
      Q(LQCSHA+ IGPA21) = 69.85        ! rmax  region 6
      Q(LQCSHA+ IGPA22) = 12.7         ! z     region 7
      Q(LQCSHA+ IGPA23) = 73.66        ! rmin  region 7
      Q(LQCSHA+ IGPA24) = 77.47        ! rmax region  7
      Q(LQCSHA+ IGPA25) = 17.78        ! z     region 8
      Q(LQCSHA+ IGPA26) = 87.63        ! rmin  region 8
      Q(LQCSHA+ IGPA27) = 90.17         ! rmax  region 8
      Q(LQCSHA+ IGPA28) = 22.86        ! z     region 9
      Q(LQCSHA+ IGPA29) = 98.36      ! rmin  region 9
      Q(LQCSHA+ IGPA30) = 101.6        ! rmax  region 9
      Q(LQCSHA+ IGPA31) = 27.94        ! z     region 10
      Q(LQCSHA+ IGPA32) = 109.22        ! rmin  region 10
      Q(LQCSHA+ IGPA33) = 111.76       ! rmax  region 10
      Q(LQCSHA+ IGPA34) = 34.29         ! z     region 11
      Q(LQCSHA+ IGPA35) = 119.38       ! rmin  region 11
      Q(LQCSHA+ IGPA36) = 121.92       ! rmax  region 11
      Q(LQCSHA+ IGPA37) = 35.36        ! z     region 12
      Q(LQCSHA+ IGPA38) = 121.92       ! rmin  region 12
      Q(LQCSHA+ IGPA39) = 121.92       ! rmax  region 12
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMCCRK,1) ! get CMAT
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C     END CAP OUTER CRYO INNER WALL
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      CALL SBIT1(Q(LQCRYO),IBZRFL)
      IQ(LQCRYO+IGIDEN) = ICESRO ! END CAL CRYO REGION
      IQ(LQCRYO+IGMATE) = IMIRON ! material
      CHARR= 'TUBE'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'ECT1'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 268.263
      Q(LQCRYO +IGPHIE) = 0.
      Q(LQCRYO +IGTHTE) = 0.
      Q(LQCRYO +IGOMGE) = 0.
      Q(LQCRYO +IGDPHI) = TWOPI
C
C     PUT IT SHAPE INFORMATION
C
      MCSHA(5) = IOCSHA
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
C
      CHARR= 'TUBE'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 3
      Q(LQCSHA+ IGPAR1) = 2.794    ! inner radius
      Q(LQCSHA+ IGPAR2) = 2.953    ! outer radius
      Q(LQCSHA+ IGPAR3) = 124.95   ! half length of cylinder
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
      CALL SBIT1(Q(LQCRYO),IBZRFL)
C
      IQ(LQCRYO+IGIDEN) = ICESRI
      IQ(LQCRYO+IGMATE) = IMIRON
      CHARR= 'TUBE'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'ECT2'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGZCEN) = 268.263
      Q(LQCRYO +IGDPHI) = TWOPI
C
C     INNER CRYO INNER WALL TUBE SHAPE INFO
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
      CHARR= 'TUBE'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 3
      Q(LQCSHA+ IGPAR1) = 3.588    !  inner radius
      Q(LQCSHA+ IGPAR2) = 3.7465   !  outer radius
      Q(LQCSHA+ IGPAR3) = 124.95   !  half length of cyl
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C     INNER CRYO OUTER WALL
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCRYO,0,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))
      CALL SBIT1(Q(LQCRYO),IBZRFL)
C
      IQ(LQCRYO+IGIDEN) = ICELRI
      IQ(LQCRYO+IGMATE) = IMIRON
      CHARR= 'TUBE'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'ECT4'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGZCEN) = 232.51
      Q(LQCRYO +IGDPHI) = TWOPI
C
C     INNER CRYO OUTER WALL TUBE SHAPE INFO
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
      CHARR= 'TUBE'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 3
      Q(LQCSHA+ IGPAR1) = 244.16   ! inner radius
      Q(LQCSHA+ IGPAR2) = 245.75   ! outer radius
      Q(LQCSHA+ IGPAR3) = 72.901   ! half length of cyl
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
      CALL SBIT1(Q(LQCRYO),IBZRFL)
C
      IQ(LQCRYO+IGIDEN) = ICELRO
      IQ(LQCRYO+IGMATE) = IMIRON
      CHARR= 'TUBE'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'ECT3'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGZCEN) = 220.332
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
      Q(LQCSHA+ IGPAR3) = 85.072   ! half length of cyl
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
      RETURN
      END
