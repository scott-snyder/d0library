      SUBROUTINE MUHIST_SCINT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills scintillator time histograms for scint.
C-                         T0 calibration.
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls:
C-
C-   Created  19-DEC-1994  R. Markeloff
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INCLUDE 'D0$INC:ZEBCOM.INC'

      INTEGER NTRAKS, ITRAK
      INTEGER IHIT, JHIT, NCEL, LAT, IADC(8), SCINT_ADDR, IPDT
      INTEGER NPTRAK,NSAM,QUAD,IFW1,IFW2,IFW3,ISPARE
      REAL    XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,XCOSOM,
     &  YCOSOM,ZCOSOM,CHSQBV,CHSNBV,RMOM,RMOMER,ELCAL,ELFE,SPR1,SPR2
      REAL    SCINT_TIME, TXYZ(3), TOF
      REAL    MNETOF
C
C---- Loop over tracks
C
      CALL GTMTRH(NTRAKS)                  ! see how many tracks
      IF (NTRAKS .GT. 0) THEN
        DO ITRAK = 1,NTRAKS
          CALL GTMUOT(ITRAK,NPTRAK,NSAM,QUAD,IFW1,IFW2,IFW3,ISPARE,
     &         XI,YI,ZI,XMAGC,YMAGC,ZMAGC,XCOSIM,YCOSIM,ZCOSIM,
     &         XCOSOM,YCOSOM,ZCOSOM,
     &         CHSQBV,CHSNBV,RMOM,RMOMER,ELCAL,ELFE,SPR1,SPR2)
C
C         Cut on track chi**2
C
          IF (CHSQBV .LE. 0.2) THEN
C
C           Find best scintillator time from MSCT bank
C
            CALL MSCT_HIT(ITRAK,SCINT_TIME,IHIT,SCINT_ADDR,TXYZ)
            IF (SCINT_TIME .NE. -999.) THEN
C
C             Calculate time-of-flight
C
              TOF = MNETOF(ITRAK,TXYZ)
C
C             Fill T0 tuning histogram
C
              IPDT = SCINT_ADDR/256
              CALL HFILL(40000+IPDT,TOF-SCINT_TIME,0.,1.)
C
C             Get raw times from MUHP bank
C
              CALL MUDHIT(IHIT,JHIT,NCEL,LAT,IADC)
C
C             Fill raw time histogram
C
              CALL HFILL(41000+IPDT,FLOAT(IADC(2)),0.,1.)
              CALL HFILL(41000+IPDT,FLOAT(IADC(6)),0.,1.)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C 999 RETURN
      END
