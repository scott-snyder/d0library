      SUBROUTINE CMAT
C-----------------------------------------------------------------------
C
C     CREATE MATERIAL DESCRIPTION BANKS.  DIFFERENT KINDS OF BANKS
C     REPRESENT SOLID MATERIALS AND MIXTURES.  NUMBERS FOR THE PURE
C     MATERIALS COMES FROM THE REVIEW OF PARTICLE PROPERTIES, REV MOD
C     PHYS. 56(1984).
C
C     AUTHOR:    S KAHN        7 NOV 1986
C     REVISION:  ADDED NEW MATERIALS FOR SCINTILLATOR AND ABSORBER
C                THIS MATERIALS ARE MIXTURES OF ATOMS WHICH GEANT
C                CAN USE TO CREATE MIXTURE MEDIA.  S KAHN   31 MAY 88
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CMAT.PARAMS/LIST'
      INCLUDE 'D0$LINKS:IZCMAT.LINK'
      INCLUDE 'D0$INC:SCPARR.INC'
C
C
      COMMON / MATLNK / LMATE(2), LALUM, LIRON, LCU, LURAN, LARGON,
     &  LHYDRO, LCARBN, LG10
      INTEGER           LMATE, LALUM, LIRON, LCU, LURAN, LARGON, LHYDRO,
     &  LCARBN, LG10
      INTEGER MCMAT(5), IOCMAT, MCMAT2(5), I, NMIXT, IM, J, LZFIND
      INTEGER NCOMP
      REAL SUM
C
      CHARACTER*4 CHAR4,CHAS4
      INTEGER ICHARR
      CHARACTER*4 CHARR
      EQUIVALENCE (ICHARR,CHARR)
      EQUIVALENCE (CHAR4,MCMAT(1))
      EQUIVALENCE (CHAS4,MCMAT2(1))
      DATA MCMAT / 0, 0, 0, 11, 9/
      DATA MCMAT2/ 0, 4, 0, 14, 9/
      DATA CHAR4 /'CMAT'/
      DATA CHAS4 /'CMAT'/
C
      CALL MZFORM('CMAT','2I2H-F',IOCMAT)
      CALL MZLINT(IXSTP,'/MATLNK/',LMATE,LALUM,LG10)
C
C     ALUMINUM
C
      MCMAT(5) = IOCMAT
      CALL MZLIFT(IDVSTP,LQCMAT,LCGEH,-IZCMAT,MCMAT,0)
      IC(LQCMAT + IGIDNT) = 9     ! ALUMINUM
      IC(LQCMAT + IGNMAT) = 1
      CHARR= 'ALUM'
      IC(LQCMAT + IGNAME) = ICHARR
      CHARR= 'INUM'
      IC(LQCMAT + IGNAME + 1) = ICHARR
      C(LQCMAT + IGA)     =  26.98
      C(LQCMAT + IGZ)     = 13.0
      C(LQCMAT + IGDENS)  = 2.7
      C(LQCMAT + IGRADL)  = 8.9
      C(LQCMAT + IGABSL)  = 39.4
      C(LQCMAT + IGFMAT)  = 1.0
      LALUM = LQCMAT
C
C     IRON
C
      CALL MZLIFT(IDVSTP,LQCMAT,LQCMAT,0,MCMAT,0)
      IC(LQCMAT + IGIDNT) = 10    ! IRON
      IC(LQCMAT + IGNMAT) = 1
      CHARR= 'IRON'
      IC(LQCMAT + IGNAME) = ICHARR
      CHARR= '    '
      IC(LQCMAT + IGNAME + 1) = ICHARR
      C(LQCMAT + IGA)     =  55.85
      C(LQCMAT + IGZ)     = 26.0
      C(LQCMAT + IGDENS)  = 7.87
      C(LQCMAT + IGRADL)  = 1.76
      C(LQCMAT + IGABSL)  = 16.76
      C(LQCMAT + IGFMAT)  = 1.0
      LIRON = LQCMAT
C
C     COPPER
C
      CALL MZLIFT(IDVSTP,LQCMAT,LQCMAT,0,MCMAT,0)
      IC(LQCMAT + IGIDNT) = 11   ! COPPER
      IC(LQCMAT + IGNMAT) = 1
      CHARR= 'COPP'
      IC(LQCMAT + IGNAME) = ICHARR
      CHARR= 'ER  '
      IC(LQCMAT + IGNAME + 1) = ICHARR
      C(LQCMAT + IGA)     =  63.54
      C(LQCMAT + IGZ)     = 29.0
      C(LQCMAT + IGDENS)  = 8.96
      C(LQCMAT + IGRADL)  = 1.43
      C(LQCMAT + IGABSL)  = 15.06
      C(LQCMAT + IGFMAT)  = 1.0
      LCU = LQCMAT
C
C     URANIUM
C
      CALL MZLIFT(IDVSTP,LQCMAT,LQCMAT,0,MCMAT,0)
      IC(LQCMAT + IGIDNT) = 14   ! URANIUM
      IC(LQCMAT + IGNMAT) = 1
      CHARR= 'URAN'
      IC(LQCMAT + IGNAME) = ICHARR
      CHARR= 'IUM '
      IC(LQCMAT + IGNAME + 1) = ICHARR
      C(LQCMAT + IGA)     =  238.03
      C(LQCMAT + IGZ)     = 92.0
      C(LQCMAT + IGDENS)  = 18.95
      C(LQCMAT + IGRADL)  = 0.32
      C(LQCMAT + IGABSL)  = 10.50
      C(LQCMAT + IGFMAT)  = 1.0
      LURAN = LQCMAT
C
C     LIQUID ARGON
C
      CALL MZLIFT(IDVSTP,LQCMAT,LQCMAT,0,MCMAT,0)
      IC(LQCMAT + IGIDNT) = 17   ! LIQ ARGON
      IC(LQCMAT + IGNMAT) = 1
      CHARR= 'LIQA'
      IC(LQCMAT + IGNAME) = ICHARR
      CHARR= 'RGON'
      IC(LQCMAT + IGNAME + 1) = ICHARR
      C(LQCMAT + IGA)     =  39.95
      C(LQCMAT + IGZ)     = 18.0
      C(LQCMAT + IGDENS)  = 1.40
      C(LQCMAT + IGRADL)  = 14.0
      C(LQCMAT + IGABSL)  = 83.71
      C(LQCMAT + IGFMAT)  = 1.0
      LARGON = LQCMAT
C
C     HYDROGEN  (TO BE USED MAINLY IN COMPOUNDS SUCH AS POLYETHYLENE)
C
      CALL MZLIFT(IDVSTP, LQCMAT, LQCMAT, 0, MCMAT, 0)
      IC(LQCMAT + IGIDNT) = 1    ! HYDROGEN
      IC(LQCMAT + IGNMAT) = 1    ! 1 component material
      CHARR= 'HYDR'    ! name
      IC(LQCMAT + IGNAME) = ICHARR       ! name
      CHARR= 'OGEN'
      IC(LQCMAT + IGNAME + 1) = ICHARR
      C(LQCMAT + IGA) = 1.0079   ! atomic mass
      C(LQCMAT + IGZ) = 1.0      ! atomic number
      C(LQCMAT + IGDENS) = 0.0708     ! nominal density as gas (STP)
      C(LQCMAT + IGRADL) = 890.  ! nominal radiation length
      C(LQCMAT + IGABSL) = 790.  ! nominal absorption length
      LHYDRO = LQCMAT
C
C     CARBON - (USED FOR MAKING COMPOSITE MATERIALS SUCH AS POLYPROPOLENE)
C
      CALL MZLIFT(IDVSTP, LQCMAT, LQCMAT, 0, MCMAT, 0)
      IC(LQCMAT + IGIDNT) = 6    ! CARBON
      IC(LQCMAT + IGNMAT) = 1    ! 1 component material
      CHARR= 'CARB'    ! name
      IC(LQCMAT + IGNAME) = ICHARR            ! name
      CHARR= 'ON  '
      IC(LQCMAT + IGNAME + 1) = ICHARR
      C(LQCMAT + IGA) = 12.011   ! atomic mass
      C(LQCMAT + IGZ) = 6.0      ! atomic number
      C(LQCMAT + IGDENS) = 1.55       ! nominal density as gas (STP)
      C(LQCMAT + IGRADL) = 27.5  ! nominal radiation length
      C(LQCMAT + IGABSL) = 49.9  ! nominal absorption length
      LCARBN = LQCMAT
C
C     G10
C
      CALL MZLIFT(IDVSTP,LQCMAT,LQCMAT,0,MCMAT,0)
      IC(LQCMAT + IGIDNT) = 18   ! G10
      IC(LQCMAT + IGNMAT) = 1
      CHARR= 'G10 '
      IC(LQCMAT + IGNAME) = ICHARR
      CHARR= '    '
      IC(LQCMAT + IGNAME + 1) = ICHARR
      C(LQCMAT + IGA)     =  13.88
      C(LQCMAT + IGZ)     = 7.1
      C(LQCMAT + IGDENS)  = 1.7
      C(LQCMAT + IGRADL)  = 19.4
      C(LQCMAT + IGABSL)  = 53.06
      C(LQCMAT + IGFMAT)  = 1.0
      LG10 = LQCMAT
C
C     TITANIUM
C
      CALL MZLIFT(IDVSTP,LQCMAT,LQCMAT,0,MCMAT,0)
      IC(LQCMAT + IGIDNT) = 19   ! titanium
      IC(LQCMAT + IGNMAT) = 1
      CHARR= 'TITA'
      IC(LQCMAT + IGNAME) = ICHARR
      CHARR= 'NIUM'
      IC(LQCMAT + IGNAME + 1) = ICHARR
      C(LQCMAT + IGA) = 47.9
      C(LQCMAT + IGZ) = 22.
      C(LQCMAT + IGDENS) = 4.5
      C(LQCMAT + IGRADL) = 3.59
      C(LQCMAT + IGABSL) = 0.    ! ? (program calculates it anyway)
      C(LQCMAT + IGFMAT) = 1.
C
C     CEN CAL MIXTURE
C
      CALL SLSRCP('SRCP_UCAL')
      CALL GTSRCP('CC_MIXTURES',IVAL,1)
      NMIXT = IVAL(1)
      IM = 1
C
      DO 100 I = 1,NMIXT
      CALL MZLIFT(IDVSTP,LQCMAT,LQCMAT,0,MCMAT2,0)
      IC(LQCMAT + IGIDNT) = IVAL(IM+4)   ! CEN CAL MIXTURE ID
      IC(LQCMAT + IGNMAT) = IVAL(IM+5)   ! number of materials
      IC(LQCMAT + IGNAME) = IVAL(IM+1)   ! name
      IC(LQCMAT + IGNAME + 1) = IVAL(IM+2)
      CALL UCOPY(RVAL(IM+IVAL(IM+5)+6), C(LQCMAT+IGFMAT), IVAL(IM+5))
C                                        ! copy mixture fractions
      DO 90 J = 1, IVAL(IM+5)
   90 LC(LQCMAT - J) = LZFIND(IDVSTP, LC(LCGEH-IZCMAT), IVAL(IM+J+5),
     &  IGIDNT)
      IM = IM + 2*IVAL(IM+5) + 5
      CALL MIXMAT(LQCMAT)
  100 CONTINUE
C
C     END CAL MIXTURE
C
      CALL SLSRCP('SRCP_ECAL')
      CALL GTSRCP('EC_MIXTURES',IVAL,1)
      NMIXT = IVAL(1)
      IM = 1
C
      DO 200 I = 1,NMIXT
      CALL MZLIFT(IDVSTP,LQCMAT,LQCMAT,0,MCMAT2,0)
      IC(LQCMAT + IGIDNT) = IVAL(IM+4)   ! END CAL MIXTURE ID
      IC(LQCMAT + IGNMAT) = IVAL(IM+5)   ! number of materials
      IC(LQCMAT + IGNAME) = IVAL(IM+1)   ! name
      IC(LQCMAT + IGNAME + 1) = IVAL(IM+2)
      CALL UCOPY(RVAL(IM+IVAL(IM+5)+6), C(LQCMAT+IGFMAT), IVAL(IM+5))
C                                        ! copy mixture fractions
C ... NORMALIZE FRACTIONS
      SUM  = 0.
C
      DO 170 J = 1, IVAL(IM+5)
  170 SUM = SUM + C(LQCMAT + IGFMAT + J-1)
C
      DO 180 J = 1, IVAL(IM+5)
  180 C(LQCMAT + IGFMAT + J-1) = C(LQCMAT + IGFMAT + J-1)/SUM
C
      DO 190 J = 1, IVAL(IM+5)
  190 LC(LQCMAT - J) = LZFIND(IDVSTP, LC(LCGEH-IZCMAT), IVAL(IM+J+5),
     &  IGIDNT)
      IM = IM + 2*IVAL(IM+5) + 5
      CALL MIXMAT(LQCMAT)
  200 CONTINUE
C
C     POLYETHYLENE MIXTURE
C
      CALL MZLIFT(IDVSTP,LQCMAT,LQCMAT,0,MCMAT2,0)
      IC(LQCMAT+IGIDNT) = 54          !  POLYETHELNE MIX FOR ABSORBER
      IC(LQCMAT+IGNMAT) = 2
      CHARR=  'POLY'
      IC(LQCMAT+IGNAME) =  ICHARR
      CHARR= 'ETHL'
      IC(LQCMAT+IGNAME+1) = ICHARR
      C(LQCMAT + IGFMAT) = 0.6667     ! fract H
      C(LQCMAT + IGFMAT+1)=0.3333     ! fract C
      C(LQCMAT + IGDENS) = 0.95       ! density
      C(LQCMAT + IGRADL) = 48.        ! radiation length
      C(LQCMAT + IGABSL) = 78.4       ! absorbtion length
      LC(LQCMAT - 1) = LHYDRO
      LC(LQCMAT - 2) = LCARBN
      CALL SBIT1(IC(LQCMAT),IBMIXT)   ! This material to be considered a
C                                     ! mixture of atoms in above ratios
C
C     SCINTILLATOR MIXTURE
C
      CALL MZLIFT(IDVSTP,LQCMAT,LQCMAT,0,MCMAT2,0)
      CALL GTSRCP('ICD_MIXTURES',IVAL,1)
      NMIXT = IVAL(1)
      IM = 1
C
      DO 300 I = 1,NMIXT
      NCOMP = ABS(IVAL(IM+5))
      CALL MZLIFT(IDVSTP,LQCMAT,LQCMAT,0,MCMAT2,0)
      IC(LQCMAT + IGIDNT) = IVAL(IM+4)   ! END CAL MIXTURE ID
      IC(LQCMAT + IGNMAT) = NCOMP        ! number of materials
      IC(LQCMAT + IGNAME) = IVAL(IM+1)   ! name
      IC(LQCMAT + IGNAME + 1) = IVAL(IM+2)
      CALL UCOPY(RVAL(IM+NCOMP+6), C(LQCMAT+IGFMAT), NCOMP)
C                                        ! copy mixture fractions
C ... NORMALIZE FRACTIONS
      SUM  = 0.
C
      DO 270 J = 1, NCOMP
  270 SUM = SUM + C(LQCMAT + IGFMAT + J-1)
C
      DO 280 J = 1, NCOMP
  280 C(LQCMAT + IGFMAT + J-1) = C(LQCMAT + IGFMAT + J-1)/SUM
C
      DO 290 J = 1, NCOMP
  290 LC(LQCMAT - J) = LZFIND(IDVSTP, LC(LCGEH-IZCMAT), IVAL(IM+J+5),
     &  IGIDNT)
      IM = IM + 2*NCOMP + 5
      CALL MIXMAT(LQCMAT)
  300 CONTINUE
      CALL SBIT1(IC(LQCMAT),IBMIXT)   ! This material to be considered a
C                                     ! mixture of atoms in above ratios
      CALL SBIT1(IC(LQCMAT),IBLAND)   ! Landau fluctuations flag is set
C
      LMATE(1) = 0                    ! return temp link storage
      RETURN
      END
