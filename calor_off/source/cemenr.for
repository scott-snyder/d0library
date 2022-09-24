      SUBROUTINE CEMENR(NDPTH,ENDPTH,PEDPTH,ETOT,ET,ETRANS,EMAX,
     &                  ETAMX,PHIMX,USE_CASH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Work out the longitudinal and partial
C-                         transverse profile of EM showers
C-
C-   Inputs  : NDPTH = Number of depths
C-             USE_CASH = use cash banks to compute the varibales.
C-   Outputs : ENDPTH = Energy in each depth
C-             PEDEPTH = percentage Energy in each depth
C-             ETOT    = Total Energy
C-             ET      = Total ET
C-             ETRANS  = Energy in cluster outside of central tower
C-             EMAX    = Highest energy hit in each layer
C-             ETAMX   = eta of highest energy hit in each layer
C-             PHIMX   = phi of highest energy hit in each layer
C-   Controls:
C-
C-   Created  15-MAY-1989   Rajendran Raja
C-   Modified 28-Mar-1990   N.A. Graf 
C-                          Now returns amount of energy in cluster outside
C-                          central tower
C-   Updated  23-OCT-1992   Meenakshi Narain   optionally use CASH banks
C-   Updated  27-OCT-1992   Meenakshi Narain/Natalie Roe 
C-                          add ETAMX ==> eta of maximum energy hit in each
C-                          layer
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL EM
      INTEGER NTIMES,ITIMES
      INTEGER NDPTH,ETAMX(*),PHIMX(*)
      REAL    ENDPTH(*),PEDPTH(*),EMAX(*),ETOT,ET
      REAL    E, ETRANS,ECELL
      INTEGER NRCAEH,INUM,POINTER,ILYR,IDEPTH,I
      INTEGER NRCATE,NCELLS,ETA,ETA_CENTRAL
      INTEGER PHI,PHI_CENTRAL,PACKED_WORD
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$LINKS:IZCACH.LINK'                               
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      LOGICAL USE_CASH
      DATA ITIMES/0/
      DATA NTIMES/5/                    ! NUMBER OF ERROR MESSAGES.
C----------------------------------------------------------------------
      E = Q(LCACL+7)
      ET = Q(LCACL+8)
      ETOT = 0.
      ETRANS = 0.
      CALL UZERO(ENDPTH,1,NDPTH)
      CALL UZERO(EMAX,1,NDPTH)
      CALL UZERO(PEDPTH,1,NDPTH)
      CALL vZERO_i(ETAMX,NDPTH)
      CALL vZERO_i(PHIMX,NDPTH)
C
C ****  GET POINTERS AND ETA OF CENTRAL TOWER
C
      IF (USE_CASH) THEN
        LCASH = LQ(LCACL-2)
        NCELLS = IQ(LCASH+2)
        CALL CMXTWR(ETA_CENTRAL,PHI_CENTRAL)
      ELSE
        NRCAEH = IQ(LCAEH+2)
        NRCATE = IQ(LCATE+2)
        LCACH = LQ(LCACL-IZCACH)          ! belongs to present CACL  
        NCELLS = IQ(LCACH+2)
        POINTER = IQ(LCACH+NCELLS+3)  ! CATE tower # with max E    
        POINTER = NRCATE*(POINTER-1) + LCATE ! Pointer to tower in CATE
        ETA_CENTRAL = IQ(POINTER+12)
      ENDIF
C
      DO 20 INUM = 1,NCELLS
        IF (USE_CASH) THEN
          POINTER = LCASH + 2*(INUM-1)
          PACKED_WORD = IQ(POINTER+3)
          ECELL = Q(POINTER+4)
          CALL CAEP_INDICES(PACKED_WORD,ETA,PHI,ILYR)
        ELSE
          POINTER = (IQ(LCACH+2+INUM)-1)*NRCAEH+LCAEH   ! To CAEH
          ETA =  IQ(POINTER+12)             ! ETA OF CELL
          PHI =  IQ(POINTER+13)             ! PHI OF CELL
          ILYR = IQ(POINTER+14)             ! LAYER NUMBER
          ECELL = Q(POINTER+7)
        ENDIF
        IDEPTH = 0
        IF(ILYR.LT.LYEM3A)IDEPTH = ILYR
        IF(ILYR.GE.LYEM3A.AND.ILYR.LE.LYEM3D) IDEPTH = 3
        IF(ILYR.GT.LYEM3D.AND.ILYR.LE.MXLYEM) IDEPTH = 4
        IF(ILYR.EQ.MNLYFH)IDEPTH = 5    ! FINE HADRONIC PUNCH THROUGH
C
        IF(IDEPTH.EQ.0)THEN
          ITIMES = ITIMES + 1
          IF(ITIMES.LT.NTIMES)CALL ERRMSG('CALORIMETER','CEMENR',
     &    ' NON EM DEPTH ENCOUNTERED ','W')
          GO TO 20
        ENDIF
        ENDPTH(IDEPTH) = ENDPTH(IDEPTH) + ECELL ! Layer totals
        IF(ECELL.GT.EMAX(IDEPTH))THEN    !  New maximum energy
          EMAX(IDEPTH)=ECELL
          ETAMX(IDEPTH)=ETA
          PHIMX(IDEPTH)=PHI
        ENDIF
        ETOT = ETOT + ECELL             ! Sum total
        IF(ETA .NE. ETA_CENTRAL) ETRANS = ETRANS + ECELL    ! transverse
   20 CONTINUE
      IF(ABS(E-ETOT).GT.0.05)THEN
        IF(ITIMES.LT.NTIMES)CALL ERRMSG('CALORIMETER','CEMENR',
     &    ' ENERGY SUM MISMATCH ','W')
      ENDIF
      DO 30 I = 1, NDPTH
        IF(ETOT.NE.0.0)THEN
          PEDPTH(I) = ENDPTH(I)/ETOT
        ELSE
          PEDPTH(I) = 0.0
        ENDIF
   30 CONTINUE
C                                       ! WORK OUT PERCENTAGES HERE
  999 RETURN
      END
