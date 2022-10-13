      SUBROUTINE SAKMFIT_INIT(READ_ZEBRA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :    READ_ZEBRA  --  Flag: 0 - don't read STP etc.
C-   Outputs :
C-   Controls:    Initializes MUKA_CONST common block
C-
C-   Created  25-MAR-1994   Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$inc:SAKFIT.INC'
      INTEGER READ_ZEBRA
      INTEGER IERR
C----------------------------------------------------------------------
      IF( MUKA_CONST_INIT.NE.0 ) RETURN
C-
      CALL EZPICK ('SAMUS_UTIL_PARAM')
      SAMUS_SIGMA = 0.12
      CALL EZGET ('SAMSIG', SAMUS_SIGMA, IERR)
      VTX_SIGMA = 0.2     ! Default
      CALL EZGET ('SXVER', VTX_SIGMA, IERR)
      VTY_SIGMA = 0.2     ! Default
      CALL EZGET ('SYVER', VTY_SIGMA, IERR)
      VTZ_SIGMA = 1.0     ! Default
      CALL EZGET ('SZVER', VTZ_SIGMA, IERR)
C<<
      PBIAS = 0.
      CALL EZGET ('PBIAS', PBIAS, IERR)
C<<
      USELOS = .TRUE.
      CALL EZGET_l ('USELOS', USELOS, IERR)
C<<
      USE_PREFIT_BC = .TRUE.
      CALL EZGET_l ('USEPREFIT', USE_PREFIT_BC, IERR)
C<<
      USE_VERTEX = .TRUE.
      CALL EZGET_l ('USEVERTEX', USE_VERTEX, IERR)
C<<
      RUN_SAKFIT = 2
      CALL EZGET_i ('SAKFIT', RUN_SAKFIT, IERR)
C<<
c       WRITE(4,1) SAMUS_SIGMA,VTX_SIGMA,VTY_SIGMA,VTZ_SIGMA,
c     +               PBIAS,USELOS,USE_PREFIT_BC,USE_VERTEX,
c     +               RUN_SAKFIT
c 1     FORMAT(X,'MU-KA-RECO parameters'/
c     +        X,'----------------------'/
c     +        X,'SAMUS_SIGMA        : ',F10.5/
c     +        X,'VTX_SIGMA          : ',F10.5/
c     +        X,'VTY_SIGMA          : ',F10.5/
c     +        X,'VTZ_SIGMA          : ',F10.5/
c     +        X,'PBIAS              : ',F10.5/
c     +        X,'USELOS             : ',L/
c     +        X,'USE_PREFIT_BC      : ',L/
c     +        X,'USE_VERTEX         : ',L/
c     +        X,'RUN_SAKFIT         : ',I)
C<<
      CALL EZRSET
C-
      CALL SAKEC_INIT(1)                  ! Init ECEM & ECIH
      CALL SAKMAG_INIT(READ_ZEBRA)        ! Init SAMUS magnet
C-
      MUKA_CONST_INIT = 1
  999 RETURN
      END
