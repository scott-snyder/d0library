      SUBROUTINE CEMENR_CASH(NDPTH,ENDPTH,PEDPTH,ETOT,ETRANS,EMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Work out the longitudinal and partial
C-                         transverse profile of EM showers
C-                         will work off CASH banks
C-
C-   Inputs  : NDPTH = Number of depths
C-   Outputs : ENDPTH = Energy in each depth
C-             PEDEPTH = percentage Energy in each depth
C-             ETOT    = Total Energy
C-             ETRANS  = Energy in cluster outside of central tower
C-             EMAX    = Highest energy hit in each layer
C-   Controls:
C-
C-   Updated  21-SEP-1992   Rajendran Raja   Does not work out ET
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$LINKS:IZCACH.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CHMATR_NEW.INC'
C
      REAL    ENDPTH(*),PEDPTH(*),EMAX(*),ETOT,ET
      REAL    E, ETRANS ,ENERGY
C
      INTEGER ETA,ETA_CENTRAL
      INTEGER I,NCH,POINTER,IOK
      INTEGER  ETAI,PHII,ILYR
      INTEGER PACKED_WORD
      INTEGER NTIMES,ITIMES
      INTEGER NDPTH,IDEPTH
      DATA ITIMES/0/
      DATA NTIMES/5/                    ! NUMBER OF ERROR MESSAGES.
C----------------------------------------------------------------------
C
      ETA_CENTRAL = ETAC !SETUP IN C_SETUP_ETAC_PHIC
      ETOT = 0.
      ETRANS = 0.
      CALL UZERO(ENDPTH,1,NDPTH)
      CALL UZERO(EMAX,1,NDPTH)
      CALL UZERO(PEDPTH,1,NDPTH)
C
      NCH    = IQ(LCASH+2)
      POINTER=1
      ETOT = 0
c
      DO I = 1,NCH
        POINTER = POINTER+2
        PACKED_WORD = IQ(LCASH+POINTER)
        ENERGY = Q(LCASH+POINTER+1)
        CALL CAEP_INDICES(PACKED_WORD,ETAI,PHII,ILYR)
        ETA =  ETAI
        IDEPTH = 0
        IF(ILYR.LT.LYEM3A)IDEPTH = ILYR
        IF(ILYR.GE.LYEM3A.AND.ILYR.LE.LYEM3D) IDEPTH = 3
        IF(ILYR.GT.LYEM3D.AND.ILYR.LE.MXLYEM) IDEPTH = 4
        IF(ILYR.EQ.MNLYFH)IDEPTH = 5    ! FINE HADRONIC PUNCH THROUGH
C
        IF(IDEPTH.EQ.0)THEN
          ITIMES = ITIMES + 1
          IF(ITIMES.LT.NTIMES)CALL ERRMSG('CALORIMETER','CEMENR',
     &      ' NON EM DEPTH ENCOUNTERED ','W')
          GO TO 20
        ENDIF
C
        ENDPTH(IDEPTH) = ENDPTH(IDEPTH) + ENERGY ! Layer totals
C
        IF(ENERGY.GT.EMAX(IDEPTH))THEN    !  New maximum energy
          EMAX(IDEPTH)=ENERGY
        ENDIF
        ETOT = ETOT + ENERGY                     ! Sum total
        IF(ETA .NE. ETA_CENTRAL) ETRANS = ETRANS + ENERGY ! transverse
   20   CONTINUE
      ENDDO
C
      DO  I = 1, NDPTH
        IF(ETOT.NE.0.0)THEN
          PEDPTH(I) = ENDPTH(I)/ETOT
        ELSE
          PEDPTH(I) = 0.0
        ENDIF
      ENDDO
C
  999 RETURN
      END
