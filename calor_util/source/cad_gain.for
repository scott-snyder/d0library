      FUNCTION CAD_GAIN(IETA,ILYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the GAIN from energy to adc counts
C-               of channel with crate,adc,bls,rotow,depth. This routine
C-               replaces the CAD_GAIN function for conversion from energy to
C-               ADC counts. The function of CAD_GAIN is different
C-               from the conversion of ADC counts to energy in CAHITS
C-               because:
C-                1. MG/ICD conversion of gap energy into ADC counts is
C-                   to be kepp independent from the conversion of ADC
C-                   counts into total cell energy.
C-                2. Mixture and Plate D0GEANT will generate CAEP banks
C-                   with different energy scales. This routine is meant to
C-                   handle both in a logical way though RCP control.
C-
C-   Inputs  : IETA,ILYR
C-   Outputs : NONE
C-   Controls: CADMAKE_RCP
C-   RETURNS:  CAL ADC GAIN - GEV TO ADC COUNTS
C-
C-   Created  17-MAR-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    CAD_GAIN,CAD_GAIN_PLATES
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER I,J,K,IER
      INTEGER IETA,IPHI,ILYR,CAL_MODULE,TRULEN
      INTEGER LOC,NA,IMOD,JMOD,LMOD
      INTEGER ETALIM(2),LYRLIM(2),IETA0,NETA,ILYR0,NLYR,IW,IW0
      LOGICAL FIRST,EZERR,PLATES,CEXIST
      REAL    A(10),W(NETAL),CADG(NLYRL,NETAL)
      CHARACTER PARAM*32,MODULE*4,MIX*8
      SAVE FIRST,PLATES
      DATA FIRST/.TRUE./,PLATES/.FALSE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
C
C ****   get GAIN constants out of CADMAKE_RCP
C
        CALL EZLOC('CADMAKE_RCP',LOC)
        IF (LOC.EQ.0) THEN
          CALL INRCP('CADMAKE_RCP',IER)
        END IF
        IF (IER.NE.0) THEN
          CALL ERRMSG( ' NO CADMAKE_RCP ','CAD_GAIN',
     &        ' NO CAD BANK ENERGY SCALE ZERO ','W')
          GOTO 999
        END IF
        CALL EZPICK('CADMAKE_RCP')
        PARAM = 'A'//MODULE(1:TRULEN(MODULE))
        IF (PLATES) THEN
          MIX = '_PLATES'
        ELSE
          MIX = '_MIXTURE'
        END IF
        PARAM = 'A'//MIX
        CALL EZGET(PARAM,A,IER)  !  CCEM ECEM CCMG ICD  ECMG
        IF(IER.NE.0) THEN
          CALL ERRMSG( ' NO A PARAMETER ','CAD_GAIN',
     &        ' NO CAD BANK ENERGY SCALE ZERO ','W')
          GOTO 999
        END IF
        CALL EZGET_SIZE(PARAM,NA,IER)  !  NUMBER OF MODULES
        CALL EZRSET
        IF ( NA.LT.10) THEN
          CALL ERRMSG('A_WRONG','CSFBUILD',' NEED 10 VALUES IN A ','W')
          GOTO 999
        END IF
        DO I = 1, NLYRL
          DO J = -NETAL, NETAL
            IF(J.EQ.0) LMOD = 0
            IF(CEXIST(J,1,I)) THEN
              IMOD = CAL_MODULE(J,I,MODULE)
              JMOD = IMOD + 100*I
              IF(JMOD.NE.LMOD) THEN
                LMOD = JMOD
C
C ****  FETCH CSFMAKE.RCP STUFF
C
                CALL CAL_MODULE_LIMITS(IMOD,ETALIM,LYRLIM)
                IETA0=ETALIM(1)
                NETA=ETALIM(2)-ETALIM(1)+1
                ILYR0=LYRLIM(1)
                NLYR=LYRLIM(2)-LYRLIM(1)+1
                CALL EZPICK('CADMAKE_RCP')
                PARAM = 'W_'//MODULE(1:TRULEN(MODULE))//MIX
C
C **** W INDEXED IN ILYR FOR CC,EC, W INDEXED IN IETA FOR MG,ICD
C
                IW0 = ILYR0  ! ILYR0 FOR  CC,EC
                IF ( (IMOD.GE.3).AND.(IMOD.LE.5) ) IW0=ETALIM(1) !IETA MG,ICD
                CALL EZGET(PARAM,W(IW0),IER)
                IF(IER.NE.0) GOTO 999
                CALL EZRSET
              END IF
C  W INDEXED IN ILYR FOR CC,EC, W INDEXED IN IETA FOR MG,ICD
              IW = I
              IF ( (IMOD.GE.3).AND.(IMOD.LE.5) ) IW = ABS(J)
              K =IABS(J)
              CADG(I,K) = A(IMOD)*W(IW)
            END IF
          END DO
        END DO
      ENDIF
      CAD_GAIN = CADG(ILYR,IABS(IETA))
  999 RETURN
C
C#######################################################################
C
      ENTRY CAD_GAIN_PLATES ()
C----------------------------------------------------------------------
C-   Purpose and Methods : SET FLAG TO USE CONVERSION FOR GEANT PLATE
C-                         GEOMETERY
C-   Created  17-MAR-1992   Chip Stewart
C----------------------------------------------------------------------
      PLATES = .TRUE.
      CAD_GAIN_PLATES = 0.0 ! to make FLINT happy
 1999 RETURN
      END
