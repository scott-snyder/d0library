      SUBROUTINE GTNINO
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Tracking routine for "GEANTINO" type tracks              *
C.    *       EM shower parameterization from                          *
C.    *       Longo & Sestili NIM 128 (1975)  283                      *
C.    *       Linn NIM XXX (19YY)  ZZZ                                 *
C.    *       Region of validity  0.010 < E < 1.0  GeV                 *
C.    *                                                                *
C.    *    ==>Called by : GTVOL                                        *
C.    *       Author    S.Linn  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCTMED.INC/LIST'
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:GCMATE.INC/LIST'
      INCLUDE 'D0$INC:GCCUTS.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:DCALOG.INC/LIST'
C
      INTEGER NS,I,J,IHIT
      REAL XLEN,AE,BE,DE,EPS,NORM,X,G,F,GAMMA,STEPX,XSTEP
      REAL A1,A2,B1,B2
      SAVE A1,A2,B1,B2,AE,BE,NORM,XLEN,FIRST
      LOGICAL FIRST/.TRUE./
C.
C.    ------------------------------------------------------------------
C
C              Store shower params locally
C
      IF(FIRST)THEN
        IF ( SCAL(10).EQ.3. .OR. SCAL(10).EQ.4. .OR. 
     +         SCAL(10).EQ.7..OR.  SCAL(10).EQ.8. ) THEN
           STOP' This GTNINO used for CALTOWERS only!'
        ENDIF
        A1=DCUSER(1)
        A2=DCUSER(2)
        B1=DCUSER(3)
        B2=DCUSER(4)
        FIRST=.FALSE.
      ENDIF

C                Set up shower params.
C
      IF ( SLENG.EQ.0. ) THEN
        XLEN = 0.
        AE = A1 + A2*ALOG(GETOT)
        IF( AE .LT. 1.)THEN
           AE=1.0
           WRITE(6,*)' Bad shower params in GTNINO',GETOT
        ENDIF
        BE = B1 + B2*ALOG(GETOT)
        NORM = GETOT/GAMMA(AE)
      ENDIF
C
C                Determine step sizes
C
      STEP = BIG
      CALL GTNEXT
      STEP = SNEXT + PREC
      EPS = 0.01*EPSIL
      IF ( STEP.LT.EPS ) STEP = EPS
      SLENG = SLENG + STEP
      IF ( RADL.GT.1000. ) THEN
        DO I = 1,3
          VECT(I) = VECT(I) + STEP * VECT(I+3)
        ENDDO
        GOTO 99
      ENDIF
      NS = NINT(STEP/(X0STEP*RADL))
      IF ( NS.GT.0 ) THEN
        STEPX = STEP/FLOAT(NS)
      ELSE
        STEPX = STEP
        NS = 1
      ENDIF
      XSTEP = BE*STEPX/RADL
C
C                Step and integrate
C
      X = XLEN - XSTEP/2.
      DO J = 1,NS
        X = X + XSTEP
        G = ((AE-2.)*(AE-1.)/(X*X)) - (2.*(AE-1.)/X) + 1.
        F = (X**(AE-1.))*EXP(-X)
        DESTEP = NORM*F*XSTEP*(1.+G*XSTEP*XSTEP/24.)
        IF( DESTEP .LT. GETOT ) THEN
            GETOT = GETOT - DESTEP
        ELSE
            DESTEP = GETOT
            GETOT=0.
        ENDIF
        IF ( GETOT.LT.LOECUT ) THEN
            DESTEP = DESTEP + GETOT
            GETOT = 0.
        ENDIF
        CALL DSCHIT(ITRA,VECT,IHSET,CHARGE,INWVOL,DESTEP)!AJ Towers
        DESTEP = 0.
        IF ( GETOT.EQ.0. ) THEN 
           ISTOP = 1
           GOTO 99
        ENDIF
        DO I = 1,3
           VECT(I) = VECT(I) + STEPX * VECT(I+3)
        ENDDO
      ENDDO

      ISTOP = 0
      XLEN = XLEN + BE*STEP/RADL

   99 INWVOL = 2
      NMEC = 1
      LMEC(NMEC) = 1
      END
