      SUBROUTINE CSF_SCALE(I,IETA,IPHI,ILYR,ENERGY_OLD,ENERGY_NEW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SCALE ENERGY BY CSF CALIBRATION FACTORS
C-
C-   Inputs  : I = cell number-gets links for event on 1st cell
C-             IETA,IPHI,ILYR ETA , PHI LAYER
C-             ENERGY_OLD = OLD ENERGY IN CELL
C-   Outputs : ENEREGY_NEW = NEW CALIBRATED ENERGY
C-   Controls:
C-
C-   Created  15-NOV-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I
      INTEGER IETA,IPHI,ILYR
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:CSFWC.INC'
      REAL    ENERGY_OLD,ENERGY_NEW
      INTEGER GZCSFW
      INTEGER GZCSFC
      INTEGER GZCSFH
      INTEGER LZFIND,ICSFW,ICSFC
      INTEGER IER
      REAL    AWC
      LOGICAL LCHECK
      SAVE LCHECK
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
C
        CALL MZLINK(IXSTP,'/CSFWC/',STRLNK,REFLNK,REFLNK(LNKMX))
C
        CALL CSFBUILD('D0$CALOR_OFF:CSF_FIX.RCP',IER)
C
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET('CHECK_CALIB_RESCALE',LCHECK,IER)
        CALL EZRSET
      ENDIF
C
      IF (I.EQ.1) THEN
        LCSFW = GZCSFW ()
        IF(LCSFW.LE.0) THEN
          CALL ERRMSG('CALORIMETER','CSF_SCALE',
     &      'CSFW BANK NOT PRESENT','W')
          RETURN
        END IF
        LCSFC = GZCSFC ()
        IF(LCSFC.LE.0) THEN
          CALL ERRMSG('CALORIMETER','CSF_SCALE',
     &      'CSFC BANK NOT PRESENT','W')
          RETURN
        END IF
      ENDIF
C
      ICSFW =ILYR+(IABS(IETA)-1)*NLYRL  ! 1 to 629
      IF(LCSFC.GT.0) LCSFC1 = LZFIND(IDVSTP,LCSFC,ILYR,2)
C
C ****  Check crate validity range here
C
      ICSFC = IPHI+(IETA+NETAL)*NPHIL
      IF(LCSFC1.GT.0) THEN
        AWC = C(LCSFW+1+ICSFW) * C(LCSFC1+2+ICSFC) !sampling weights
      ELSE
        AWC = C(LCSFW+1+ICSFW)  !sampling weights
      END IF
C
      ENERGY_NEW = ENERGY_OLD*AWC
      IF ( LCHECK ) THEN
        CALL CHECK_CALIB_RESCALE(IETA,IPHI,ILYR,AWC)
      ENDIF
  999 RETURN
      END
