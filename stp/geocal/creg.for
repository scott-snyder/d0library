      SUBROUTINE CREG
C---------------------------------------------------------------
C
C     THIS SUBROUTINE CONTROLS THE CREATION OF DETECTOR REGIONS
C     BANKS CREG.
C
C     AUTHOR:    S. KAHN           4 NOV 1986
C
C---------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:CGHEAD.DEF'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
C
      INTEGER IGREGN, IGNLAY, IGCOOR, IGXCEN, IGYCEN, IGZCEN,
     +        IGPHIE, IGTHTE, IGOMEG, IGFRAC
      PARAMETER   (IGREGN = 11)    ! REGN INDEX
      PARAMETER   (IGNLAY = 12)    ! no of layers index
      PARAMETER   (IGCOOR = 13)    ! coord sys index
      PARAMETER   (IGXCEN = 14)    ! central x index
      PARAMETER   (IGYCEN = 15)    ! central y index
      PARAMETER   (IGZCEN = 16)    ! central z index
      PARAMETER   (IGPHIE = 17)    ! phi angle index
      PARAMETER   (IGTHTE = 18)    ! theta angle index
      PARAMETER   (IGOMEG = 19)    ! omega angle index
      PARAMETER   (IGFRAC = 20)    ! fraction index
      INTEGER MCREG(5), IOCREG, LORIG
C
      DATA MCREG/'CREG', 4, 4, 20, 9/
C
      CALL MZFORM('CREG','8I1F4I-F',IOCREG)
      MCREG(5) = IOCREG
C
C
C     CENTRAL CALORIMETER REGION
C
      CALL MZLIFT(IDVSTP,LQCREG,LCGEH,-IZCREG,MCREG,0) !lift CREG
C
      CALL UCOPY(IC(LCGEH+1),IC(LQCREG+1),9) ! copy header
C
      IC(LQCREG+IGREGN) = ICCAL      ! central calor id
      IC(LQCREG+IGNLAY) = 8          ! number of cen cal layers
      IC(LQCREG+IGCOOR) = 123        ! cartesian coordinate sys
      C(LQCREG+IGXCEN) = 0.          ! nominal center
      C(LQCREG+IGYCEN) = 0.
      C(LQCREG+IGZCEN) = 0.
      C(LQCREG+IGPHIE) = 0.          ! nominal rotation
      C(LQCREG+IGTHTE) = 0.
      C(LQCREG+IGOMEG) = 0.
C
C     NORTH EM CALORIMETER REGION
C
      CALL MZLIFT(IDVSTP,LQCREG,LQCREG,0,MCREG,0)
C
      LORIG = LC(LQCREG+2)           ! save origin bank temporily
      CALL UCOPY(IC(LORIG+1),IC(LQCREG+1),MCREG(4)) !copy cen cal
C
      IC(LQCREG+IGREGN) = INEMCL     ! north EM cal identifier
      IC(LQCREG+IGNLAY) = 4          ! D0 note 409
      IC(LQCREG+IGCOOR) = 123        ! cartesian coordinates
      C(LQCREG+IGXCEN) = 0.          ! X of center point
      C(LQCREG+IGYCEN) = 0.          ! Y of center point
      C(LQCREG+IGZCEN) = -182.476    ! Z of center point (weighted ave)
      C(LQCREG+IGPHIE) = 0.          ! Euler rotation
      C(LQCREG+IGTHTE) = 0.
      C(LQCREG+IGOMEG) = 0.
C
C     SOUTH EM CALORIMETER REGION
C
      LORIG = LQCREG                 ! save north EM pointer
      CALL MZLIFT(IDVSTP,LQCREG,LCGEH,-IZCREG,MCREG,0)
C
      CALL UCOPY(IC(LORIG+1),IC(LQCREG+1),MCREG(4)) ! Copy N to S
C
      IC(LQCREG+IGREGN) = ISEMCL     ! south EM cal identifier
      C(LQCREG+ IGZCEN) = -C(LORIG+IGZCEN) 
C
C     NORTH  HADRONIC REGION
C
      CALL MZLIFT(IDVSTP,LQCREG,LORIG,0,MCREG,0)
C
      CALL UCOPY(IC(LORIG+1),IC(LQCREG+1),MCREG(4))
C
      IC(LQCREG+IGREGN) = INECAL     ! north hadronic cal identifier
      IC(LQCREG+IGNLAY) = 0          ! dont know
      IC(LQCREG+IGCOOR) = 123        ! cartesian coordinates
      C(LQCREG+IGXCEN) = 0.          ! X of center point
      C(LQCREG+IGYCEN) = 0.          ! Y of center point
      C(LQCREG+IGZCEN) = -274.32     ! Z of center point (weighted ave)
      C(LQCREG+IGPHIE) = 0.          ! Euler rotation
      C(LQCREG+IGTHTE) = 0.
      C(LQCREG+IGOMEG) = 0.
C
C     SOUTH HADRONIC REGION
C
      LORIG = LQCREG                 ! save north inner cal pointer
      CALL MZLIFT(IDVSTP,LQCREG,LCGEH,-IZCREG,MCREG,0)
C
      CALL UCOPY(IC(LORIG+1),IC(LQCREG+1),MCREG(4))
C
      IC(LQCREG+IGREGN) = ISECAL     ! south hadronic cal identifier
      C(LQCREG+IGZCEN) = - C(LORIG+IGZCEN)
C
C     NORTH ICD SCINTILLATOR REGION
C
      CALL MZLIFT(IDVSTP,LQCREG,LQCREG,0,MCREG,0)
C
      LORIG = LC(LQCREG+2)           ! save origin bank temporily
      CALL UCOPY(IC(LORIG+1),IC(LQCREG+1),MCREG(4)) !copy cen cal
C
      IC(LQCREG+IGREGN) = INICD      ! north ICD identifier
      IC(LQCREG+IGNLAY) = 1
      IC(LQCREG+IGCOOR) = 123        ! cartesian coordinates
      C(LQCREG+IGXCEN) = 0.          ! X of center point
      C(LQCREG+IGYCEN) = 0.          ! Y of center point
      C(LQCREG+IGZCEN) = -162.5      ! Z of center point (weighted ave)
      C(LQCREG+IGPHIE) = 0.          ! Euler rotation
      C(LQCREG+IGTHTE) = 0.
      C(LQCREG+IGOMEG) = 0.
C
C     SOUTH EM CALORIMETER REGION
C
      LORIG = LQCREG                 ! save north EM pointer
      CALL MZLIFT(IDVSTP,LQCREG,LCGEH,-IZCREG,MCREG,0)
C
      CALL UCOPY(IC(LORIG+1),IC(LQCREG+1),MCREG(4)) ! Copy N to S
C
      IC(LQCREG+IGREGN) = ISICD      ! south ICD identifier
      C(LQCREG+ IGZCEN) = -C(LORIG+IGZCEN) 
C
      RETURN
      END

