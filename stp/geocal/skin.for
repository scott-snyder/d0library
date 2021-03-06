      SUBROUTINE SKIN
C---------------------------------------------------------------
C
C     CREATES INERT CYLINDERS FOR THE END CALORIMETER INNER SKIN
C     AND EM SUPPORT PIPE.  MODELLED AFTER A. P. WHITE GEANT
C     DESCRIPTION.
C 
C     BANKS LIFTED:   CRYO, CSHA
C
C     AUTHOR:    S KAHN     22 JUNE 1987
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
C   INNER HADRONIC SUPPORT PIPE CYLINDERS
C
      LQCREG = LZFIND(IXCDV,LQ(LQCGEH-1),ICCAL,IGREGN) ! get CREG
C
C
      MCRYO(5) = IOCLGA                         ! put in form address
C 
      CALL MZLIFT(IXCDV,LQCRYO,LQCREG,-IXCRYO,MCRYO,0)
      IQ(LQCRYO+IGIDEN) = ICEISK ! INNER HADRONIC SKIN REGION
      IQ(LQCRYO+IGMATE) = IMSKIN ! material
      CHARR= 'TUBE'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'SKIN'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGXCEN) = 0.
      Q(LQCRYO +IGYCEN) = 0.
      Q(LQCRYO +IGZCEN) = 274.32
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
      Q(LQCSHA+ IGPAR1) = 3.81     ! inner radius
      Q(LQCSHA+ IGPAR2) = 4.1275   ! outer radius
      Q(LQCSHA+ IGPAR3) = 78.74    ! half length of cylinder
C
      LQCMAT = LZFIND(IXCDV,LQ(LQCGEH-3),IMIRON,1) ! get CMAT
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
C     ELECTROMAGNETIC SUPPORT PIPE IN END CAP
C
      CALL MZLIFT(IXCDV,LQCRYO,LQCRYO,0,MCRYO,0)
      LPREV = LQ(LQCREG-IXCRYO)
      CALL UCOPY(Q(LPREV+1),Q(LQCRYO+1),MCRYO(4))
C
      IQ(LQCRYO+IGIDEN) = ICELSK   ! EC-EM support pipe
      IQ(LQCRYO+IGMATE) = IMSKIN
      CHARR= 'TUBE'
      IQ(LQCRYO+IGSHAP) = ICHARR
      CHARR = 'ELSK'
      IQ(LQCRYO+IGNAM ) = ICHARR
      IQ(LQCRYO+IGCOOR) = 123
      IQ(LQCRYO+IGPERP) = 3
      Q(LQCRYO +IGZCEN) = 184.162
      Q(LQCRYO +IGDPHI) = TWOPI
C
C     EC-EM SUPPORT PIPE SHAPE
C
      CALL MZLIFT(IXCDV,LQCSHA,LQCGEH,-IXCSHA,MCSHA,0)
      CHARR= 'TUBE'
      IQ(LQCSHA+IGDESC) = ICHARR
      IQ(LQCSHA+IGNPAR) = 3
      Q(LQCSHA+ IGPAR1) = 5.518    !  inner radius
      Q(LQCSHA+ IGPAR2) = 5.715    !  outer radius
      Q(LQCSHA+ IGPAR3) = 11.418   !  half length of cyl
C
      LQ(LQCRYO-IZCMAT) = LQCMAT   ! ref link to CMAT
      LQ(LQCRYO-IZCSHA) = LQCSHA   ! ref link to CSHA
C
      RETURN
      END
