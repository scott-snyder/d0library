      SUBROUTINE SET_ELECTRON_PARS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Use SHWG=1 parametrization if electron below
C-   PT cut set by SCAL
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  19-MAR-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:DCALOG.INC'
      REAL    PPT
C----------------------------------------------------------------------
      IF(IPART.GT.0.AND.IPART.LE.3)THEN
C GAMA ELECTRON OR POSITRON
        IF ( ISTAK.EQ.0 ) THEN
C
C ****  istak = 0 implies primary track
C
          PPT = SQRT(PVERT(1)**2 + PVERT(2)**2)     ! PT OF PARTICLE
          IF ( PPT.LT.SCAL(7) ) THEN        ! USE DEFAULT SHWG VALUES
            LOECUT = LOECUT_DEF
            HIECUT = HIECUT_DEF
            X0STEP = X0STEP_DEF
            IF ( PCAL.GT.1 ) THEN
              WRITE(LOUT,1)ITRA,IPART,PVERT,PPT,SCAL(7),
     &      LOECUT,HIECUT, X0STEP
    1         FORMAT(
     &          ' SET_ELECTRON_PARS: Setting Default SHWG parameters ',
     &      ' Track = ',I5,' Partid = ',I5,/,
     &      ' Momentum 4 vector = ',4F15.5,/,
     &      ' Transverse momentum, Scal (7) ',2F15.5,/,
     &      ' LOECUT,HIECUT,X0STEP = ',3F15.5)
            ENDIF
          ELSE                              ! FORCE SHWG = 0 VALUES
            LOECUT = 0.
            HIECUT = 0.
            X0STEP = 0.
            IF ( PCAL.GT.1 ) THEN
              WRITE(LOUT,2)ITRA,IPART,PVERT,PPT,SCAL(7),
     &      LOECUT,HIECUT, X0STEP
    2         FORMAT(' SET_ELECTRON_PARS: Setting SHWG=0 parameters ',
     &      ' Track = ',I5,' Partid = ',I5,/,
     &      ' Momentum 4 vector = ',4F15.5,/,
     &      ' Transverse momentum, Scal (7) ',2F15.5,/,
     &      ' LOECUT,HIECUT,X0STEP = ',3F15.5)
            ENDIF
          ENDIF
        ENDIF
      ELSE
C
C ****  NOT EM IF HERE
C
        IF ( ISTAK.EQ.0 ) THEN
C
C ****  primary track
C
          LOECUT = LOECUT_DEF
          HIECUT = HIECUT_DEF
          X0STEP = X0STEP_DEF
          IF ( PCAL.GT.1 ) THEN
            WRITE(LOUT,3)ITRA,IPART,PVERT,PPT,SCAL(7),
     &      LOECUT,HIECUT, X0STEP
    3       FORMAT(' SET_ELECTRON_PARS:',
     &        ' NOT EM. Default SHWG parameters ',
     &      ' Track = ',I5,' Partid = ',I5,/,
     &      ' Momentum 4 vector = ',4F15.5,/,
     &      ' Transverse momentum, Scal (7) ',2F15.5,/,
     &      ' LOECUT,HIECUT,X0STEP = ',3F15.5)
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
