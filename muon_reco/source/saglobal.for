      SUBROUTINE SAGLOBAL(LMUON,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Kalman fit for SAMUS and global fit
C-                         with FDC
C-      For each SAMUS track:
C-      1. Refit using Kalman filtering method SAMUS tracks from
C-         SATN(SATS) banks
C-      2. Find the nearest FDC track
C-      3. Check whether is isolated of other FDC tracks
C-      4. Combine SAMUS Kalman fit with FDC fit using their
C-         error matrices
C-
C-   Inputs  :
C-            LMUON       --    Link to MUON bank
C-   Outputs :
C-            IERR        --    Completion status
C-   Controls:
C-
C----------------------------------------------------------------------
C-
C-   Created   5-MAY-1994   Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZMUKF.LINK'
      INTEGER LMUON,IERR
C----------------------------------------------------------------------
      LOGICAL FIRST
      INTEGER SKFIT,LMUKF,LFDCT,SKHST
C<<
      REAL    FDCDMIN,FDCDMAX
      SAVE    SKFIT,FDCDMIN,FDCDMAX,SKHST
      DATA    FIRST/.TRUE./
      REAL    DIST,DIST2,SAMZ
      REAL    FDCFIT(5),SAMFIT(5),GLFIT(5)
      REAL    FDCINF(5,5),SAMINF(5,5),GLINF(5,5),GLERR(5,5),AP,THETA
      REAL    GLCOV(5,5),SAMERR(5,5)
      INTEGER VERS,NP,I,DIR,IDH,NSHIT,NFHIT,ISSEG
      REAL    MUONVEC(3),FDCVEC(3)
      REAL    GRAD(5)
      REAL    AB,PT,PHI,PX,PY,PZ
      REAL    DCHI2,SAMCHI2,FDCCHI2,BDL
      REAL    STATUS
C<<
      IF(FIRST) THEN
        CALL    EZPICK('SAMUS_UTIL_PARAM')
        CALL    EZGET('SAKFIT',SKFIT,IERR)
        IF(IERR.NE.0) SKFIT = 2
        IF(SKFIT.GT.2) THEN
          CALL EZGET('SAKHST',SKHST,IERR)
          IF(IERR.NE.0) SKHST = 1
          CALL    EZGET('FDCDMIN',FDCDMIN,IERR)
          IF(IERR.NE.0) FDCDMIN = 35.
          CALL    EZGET('FDCDMAX',FDCDMAX,IERR)
          IF(IERR.NE.0) FDCDMAX = 30.
        END IF
        IF( SKHST .GT. 0 ) THEN
          CALL    HCDIR('//PAWC',' ')
          CALL    HMDIR('SGBL','S')
C<<
          CALL    HBOOK1(100,'SAGLOBAL STATUS (N)', 40,
     +              -20., 20., 0.)
          CALL    HBOOK1(200,'SAGLOBAL STATUS (N)', 40,
     +              -20., 20., 0.)
C<<
          CALL    HBOOK1(101,'LOG10 FDC SAMUS DIST (N)',
     +              100, -1., 4., 0.)
          CALL    HBOOK1(201,'LOG10 FDC SAMUS DIST (S)',
     +              100, -1., 4., 0.)
C<<
          CALL    HBOOK1(102,'LOG10 FDC SAMUS DIST2 (N)',
     +              100, -1., 4., 0.)
          CALL    HBOOK1(202,'LOG10 FDC SAMUS DIST2 (S)',
     +              100, -1., 4., 0.)
C<<
          CALL    HBOOK1(111, 'XFDC - XSAMUS (N)',
     +              100, -1., 1., 0.)
          CALL    HBOOK1(211, 'XFDC - XSAMUS (S)',
     +              100, -1., 1., 0.)
C<<
          CALL    HBOOK1(112, 'YFDC - YSAMUS (N)',
     +              100, -1., 1., 0.)
          CALL    HBOOK1(212, 'YFDC - YSAMUS (S)',
     +              100, -1., 1., 0.)
C<<
          CALL    HBOOK1(113, 'AFDC - ASAMUS (N)',
     +              100, -.1, .1, 0.)
          CALL    HBOOK1(213, 'AFDC - ASAMUS (S)',
     +              100, -.1, .1, 0.)
C<<
          CALL    HBOOK1(114, 'BFDC - BSAMUS (N)',
     +              100, -.1, .1, 0.)
          CALL    HBOOK1(214, 'BFDC - BSAMUS (S)',
     +              100, -.1, .1, 0.)
C<<
          CALL    HBOOK1(115, 'DELTA 1/P (N)',
     +              100, -.1, .1, 0.)
          CALL    HBOOK1(215, 'DELTA 1/P (S)',
     +              100, -.1, .1, 0.)
C<<
        END IF
        FIRST = .FALSE.
      END IF
C<<
      STATUS = -10.
      IDH = 0
      LMUKF = 0
C<<
      IF( SKHST.GT.0 ) CALL HCDIR('//PAWC/SGBL',' ')
C<<
      IF(LMUON.LE.0) GOTO 998
      DIR = IQ(LMUON+9) - 12
      IF(DIR.NE.1 .AND. DIR.NE.2) GOTO 998
      IDH = 100*DIR
C<<
      STATUS = -9.
C<<
      LMUKF = LQ(LMUON-IZMUKF)
      IF( LMUKF .LE. 0 ) GOTO 999
      IF( SKFIT.LE.2 ) GOTO 999
C
C Find the best match in FDC tracks
C
      STATUS = 0.
      IQ(LMUON+4)  = 10                               ! Fit status
      LFDCT = 0
      CALL    SAGFDCMTCH(LMUKF,DIR,LFDCT,DIST,DIST2,FDCFIT,FDCINF,
     +                ISSEG,SAMFIT,SAMINF)
C<<
      IF( LFDCT.LE.0 ) GOTO 998
      STATUS = 1
C<<
      IF( SKHST.GT.0 ) THEN
        IF( DIST.GT.0.)     CALL    HF1(IDH+1, ALOG10(DIST), 1.)
        IF( DIST2.GT.0.)    CALL    HF1(IDH+2, ALOG10(DIST2), 1.)
        DO I=1,4
          CALL HF1(IDH+10+I, FDCFIT(I)-SAMFIT(I), 1.)
        END DO
      END IF
C<<
      IF( DIST.GT.FDCDMAX ) GOTO 998      ! FDC track Must be close to SAMUS
      STATUS = 2
      IF( DIST2.GT.0. .AND. DIST2.LT.FDCDMIN ) GOTO 998    ! and isolated
      STATUS = 3
C
C If found update fit
C
      CALL KFMRG2(SAMFIT,SAMINF,FDCFIT,FDCINF,GLFIT,GLINF,DCHI2,IERR)
      IF( IERR.NE.1 ) GOTO 998
      IF( SKHST.GT.0 ) CALL   HF1(IDH+15, GLFIT(5)-SAMFIT(5), 1.)
      STATUS = 4
      IF( GLFIT(5).EQ.0. ) GLFIT(5) = SAMFIT(5)
      IF( GLFIT(5).EQ.0. ) GOTO 998
      STATUS = 5
      CALL    UCOPY(GLINF,GLERR,25)
      CALL    RSINV(5,GLERR,5,IERR)
      IF( IERR.NE.0 ) GOTO 998
C
C Save the Global Fit in MUKF bank
C
      CALL    GTMUKF(LMUKF,0,ISSEG,VERS,NP,NSHIT,SAMZ,
     +                SAMCHI2,BDL,SAMFIT,SAMERR)
      FDCCHI2 = Q(LFDCT+19)
      NFHIT = IQ(LFDCT+2)
      CALL    MUKFFL(LMUKF,ISSEG+1,5,NFHIT+NSHIT,SAMZ,
     +            SAMCHI2+DCHI2+FDCCHI2,BDL,GLFIT,GLERR)
c
c Then update MUON bank
c
C<<
      AP = ABS(1./GLFIT(5))
      IQ(LMUON+2)  = INT(SIGN(14.,GLFIT(5)))          ! Id
      IQ(LMUON+4)  = 11                               ! Fit status
      IQ(LMUON+5)  = 0                                ! Num. of CD trks
      IQ(LMUON+10) = 2                                ! Method
      AB = SQRT(1. + GLFIT(3)**2 + GLFIT(4)**2)
      PZ = AP/AB
      PZ = SIGN(PZ,Q(LMUON+13))
      PX = PZ*GLFIT(3)
      PY = PZ*GLFIT(4)
      Q(LMUON+11) = PX                        ! Px
      Q(LMUON+12) = PY                        ! Py
      Q(LMUON+13) = PZ                                ! Pz
      Q(LMUON+14) = AP                                ! P
      THETA = ACOS(PZ/AP)
      PT = SQRT( AP**2 - PZ**2 )
C<<
      Q(LMUON+15) = AP*SIN(THETA)                     ! Pt
      Q(LMUON+16) = THETA                             ! Theta
      Q(LMUON+17) = - ALOG(TAN(THETA/2))              ! Eta
      PHI = ATAN2(PY,PX)
      IF(PHI.LT.0.)    PHI = PHI + 2*PI
      IF(PHI.GT.2.*PI) PHI = PHI - 2*PI
      Q(LMUON+18) = PHI                               ! Phi
C
      Q(LMUON+31) = (SAMCHI2+DCHI2+FDCCHI2)/(NSHIT+NFHIT-5)
      Q(LMUON+37) = GLFIT(1)                          ! X
      Q(LMUON+38) = GLFIT(2)                          ! Y
      Q(LMUON+39) = SAMZ                              ! Z
C
      MUONVEC(3) = 1./SQRT(1. + SAMFIT(3)**2 + SAMFIT(4)**2)
      MUONVEC(3) = SIGN(MUONVEC(3),PZ)
      MUONVEC(1) = SAMFIT(3)*MUONVEC(3)
      MUONVEC(2) = SAMFIT(4)*MUONVEC(3)
C
      FDCVEC(3) = 1./SQRT(1. + FDCFIT(3)**2 + FDCFIT(4)**2)
      FDCVEC(3) = SIGN(FDCVEC(3),PZ)
      FDCVEC(1) = FDCFIT(3)*FDCVEC(3)
      FDCVEC(2) = FDCFIT(4)*FDCVEC(3)
C
      Q(LMUON+40) = ACOS( FDCVEC(1)*MUONVEC(1) +
     +                      FDCVEC(2)*MUONVEC(2) +
     +                      FDCVEC(3)*MUONVEC(3) ) * 180./PI  ! Angle
C<<
      CALL UCOPY(GLINF,GLCOV,25)
      CALL RSINV(5,GLCOV,5,IERR)
      IF( IERR.EQ.0 ) THEN
C
C Compute Sigma**2 Px
C
        GRAD(1) = 0.
        GRAD(2) = 0.
        GRAD(3) = (1.+GLFIT(4)**2)/GLFIT(5)/(AB**3)
        GRAD(4)     = -GLFIT(3)*GLFIT(4)/GLFIT(5)/(AB**3)
        GRAD(5) = -GLFIT(3)/(GLFIT(5)**2)/AB
        CALL MXMLTR(GRAD,GLCOV,Q(LMUON+26),1,5)
C
C Compute Sigma**2 Py
C
        GRAD(4) = (1.+GLFIT(3)**2)/GLFIT(5)/(AB**3)
        GRAD(5) = -GLFIT(4)/(GLFIT(5)**2)/AB
        CALL MXMLTR(GRAD,GLCOV,Q(LMUON+27),1,5)
C
C Compute Sigma**2 Pz
C
        GRAD(3)     = -GLFIT(3)/GLFIT(5)/(AB**3)
        GRAD(4) = -GLFIT(4)/GLFIT(5)/(AB**3)
        GRAD(5) = -1./(GLFIT(5)**2)/AB
        CALL MXMLTR(GRAD,GLCOV,Q(LMUON+28),1,5)
C
C Sigma**2 P
C
        Q(LMUON+29) = GLCOV(5,5)/(GLFIT(5)**4)
C
C Sigma**2 Pt
C
        IF( PT.NE.0 ) THEN
          GRAD(3) = 2.*GLFIT(3)/( GLFIT(5) * AB**2 )**2
          GRAD(4) = 2.*GLFIT(4)/( GLFIT(5) * AB**2 )**2
          GRAD(5) = -2*AP*PT**2
          CALL MXMLTR(GRAD,GLCOV,Q(LMUON+30),1,5)
          Q(LMUON+30) = Q(LMUON+30)/(PT+PT)**2
        END IF
      END IF
      IQ(LMUON+4) = 11
      STATUS = 10.
      GOTO 999
  998 CONTINUE
      IQ(LMUON+4) = 2
  999 CONTINUE
      IF( LMUKF.GT.0 ) CALL   MUKFPUSH(LMUKF)
      IERR = 0
      RETURN
      END
