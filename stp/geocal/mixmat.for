      SUBROUTINE MIXMAT(L)
C-------------------------------------------------------------------
C
C     THIS SUBROUTINE FILLS WORDS FOR A COMPOSITE MATERIAL BANK
C     THE COMPOSITE VALUES ARE OBTAINED IN A MANNER SIMILAR TO 
C     THAT USED IN GEANT.  GEANT ROUTINES GHMIX,GHSIGM,GHSIG ARE
C     CALLED.
C     THE INPUT PARAMETER L IS THE BANK ADDRESS OF THE COMPOSITE
C     MATERIAL BANK TO BE FILLED.
C
C     AUTHOR:    S KAHN          14 NOV 1986
C
C-------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:CMAT.DEF'
      INCLUDE 'D0$INC:CALGEO.INC'
      COMMON/GCUNIT/ LIN,LOUT,NUNITS(6)
      INTEGER LIN,LOUT,NUNITS
      REAL AEFF,ZEFF,RADINV,ZC,ALZ,XINV,AL183,WMAT(4),DENS,ALR2AV
      REAL A(4),AHEFF,ABSEFF
      REAL GXSI,GFCOUL, GHSIGM
      INTEGER NLM,I,L1,L
C
C*amj don't need and illegal on IBM*      DATA LOUT / 6 /  ! output unit
      DATA AL183 /5.20948/, ALR2AV/ 1.39621E-03/
C
      NLM = IQ(L+IGNMAT)    ! number of composite materials
      IF ( NLM .LE. 1) RETURN
      AEFF = 0
      ZEFF = 0.
      RADINV = 0.
      DENS = 0.
C
      DO 40 I = 1,NLM
      L1 = LQ(L-I)          ! bank locations of the pure materials
      IF ( L1.EQ.0 ) THEN
          WRITE(LOUT,35) I
 35       FORMAT(' MATERIAL NUMBER ',I2,2X,'NOT FOUND')
          CALL DZSHOW('INCONSISTENT MATERIAL BANK',IXCDV,L,'B',0,0,0,0)
      END IF
      WMAT(I) = Q(L+IGFMAT+I-1)  ! fract by weight
      AEFF = AEFF + Q(L1+IGA) * WMAT(I)
      ZEFF = ZEFF + Q(L1+IGZ) * WMAT(I)
      DENS = DENS + Q(L1+IGDENS) * WMAT(I)
      ZC = Q(L1+IGZ)
      A(I) = Q(L1+IGA)
      ALZ = ALOG(ZC)/3.
      XINV = ZC*(ZC+GXSI(ZC))*(AL183-ALZ-GFCOUL(ZC))/A(I)
      RADINV = RADINV + XINV*WMAT(I)
  40  CONTINUE
C
      Q(L+IGZ) = ZEFF
      Q(L+IGA) = AEFF
      Q(L+IGDENS) = DENS
      Q(L+IGRADL) = 1.0/(ALR2AV*DENS*RADINV)
      CALL GHMIX(A,WMAT,NLM,AHEFF)
      ABSEFF = 10000.*AHEFF/(6.022*DENS*GHSIGM(5.0,8,AHEFF))
      Q(L+IGABSL) = ABSEFF
      RETURN
      END
