      SUBROUTINE EM_MVTXVAR(LCLUS,LZTRKPV,ZVTXELE,ETZV,THZV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : LCLUS,LZTRKPV : link to PELC/PPHo and associated ZTRK
C-   Outputs : ZVTXELE  [R]  : zvertex determined using CAl cog and trk Cog
C-             ETZV(4) [R]  : Et with respect to 1st 4 vertices
C-             THZV(4) [R]  : theta with respect to 1st 4 vertices
C-   Controls: caphel_rcp
C-
C-   Created  12-AUG-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INCLUDE 'D0$LINKS:IZCASH.LINK'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER NVERT,NV,LCLUS,LZTRKPV
      INTEGER I,NEVOLD,STATUS
      SAVE    NEVOLD
      REAL    ETZV(4),THZV(4),TH,ECLUS,RHO,ZCLUS
      REAL    ZVTX_INFO(3, 4),ZV,ZVERT
      REAL    XBAR3(3),DBAR3(3),ETAPHI(3),DETAPHI(3),WEIGHT_CUT
      REAL    ZVTXELE,XE,YE,ZE,ZTRK,XTRK,YTRK
      REAL    RTRK,RCAL
      REAL    XVERT,DX,YVERT,DY
      INTEGER LZTRK,LZFIT,LDTRK,LFDCT
      INTEGER IER,LRCP,LCACL,LCASH
      LOGICAL first,OK,OKZ
      SAVE first
      DATA first / .true. /

C----------------------------------------------------------------------
      CALL VZERO(ETZV,4)
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZLOC('CAPHEL_RCP',LRCP)
        OK = LRCP .GT. 0
        IF (.NOT. OK) THEN
          CALL INRCP('CAPHEL_RCP',IER)
          IF (IER.EQ.0) CALL EZPICK('CAPHEL_RCP')
          IF (IER.EQ.0) CALL EZERR(IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('EM_MVTXVAR','EM_MVTXVAR',
     &        ' CAPHEL_RCP not found','F')
          ENDIF
          CALL EZRSET
        ENDIF
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('WEIGHT_CUT',WEIGHT_CUT,IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('EM_MVTXVAR','EM_MVTXVAR',
     &        'problem reading CAPHEL RCP parameters','F')
        ENDIF
        CALL EZRSET
      ENDIF

C ****  Get Zvertex
      if (nevold.ne.IQ(LHEAD+9)) then
        NEVOLD = IQ(LHEAD+9)
        CALL VERTEX_INFO(4,NV,ZVTX_INFO,OKZ)
        NVERT = MIN(4,NV)
        ZVERT = ZVTX_INFO(1,1)
        CALL VXY_BEAM1(ZVERT,XVERT,DX,YVERT,DY,STATUS) 
      endif
      IF (.NOT.OKZ) GOTO 999
C **** Get Shower centroid 
      LCACL=LQ(LCLUS-2)
      IF(LCACL.GT.0)THEN
        LCASH=LQ(LCACL-IZCASH)
        IF(LCASH.GT.0)THEN
          CALL CM3POS_PV(LCASH,WEIGHT_CUT,XBAR3,DBAR3,ETAPHI,
     &            DETAPHI,ZVERT)
        ENDIF
      ENDIF
C
C ****  compute electron-vertex  position
C
      LZTRK = LZTRKPV
      ZTRK    = -999.
      ZVTXELE = -999.
      IF(LZTRK.GT.0)THEN
        LZFIT=LQ(LZTRK-1)
        LDTRK=LQ(LZTRK-7)
        LFDCT=LQ(LZTRK-8)
        IF(LZFIT.GT.0)THEN
          IF (LDTRK.GT.0.AND.LFDCT.EQ.0) THEN   ! use DTRK as correction for
                                                !   zcog needed
            XTRK    = Q(LDTRK+7)-XVERT  ! x
            YTRK    = Q(LDTRK+8)-YVERT  ! y
            ZTRK    = 0.987*Q(LDTRK+11) ! z
          ELSE IF (LFDCT.GT.0) THEN   ! use ZFIT
            XTRK    = Q(LZFIT+11)-XVERT  ! x
            YTRK    = Q(LZFIT+12)-YVERT  ! y
            ZTRK    = Q(LZFIT+15) ! z
          ENDIF
        ENDIF
      ENDIF
      IF(ZTRK.EQ.-999)THEN
        CALL ERRMSG('NO ZTRK track','ELE_ZVERTEX',' ','W')
        GOTO 500
      ENDIF
C... determine z intercept with beam
      XE =XBAR3(1)-XVERT
      YE =XBAR3(2)-YVERT
      ZE =XBAR3(3)
      RTRK=SQRT(XTRK**2+YTRK**2)
      RCAL=SQRT(XE**2+YE**2)
      ZVTXELE=ZTRK-RTRK/(RTRK-RCAL)*(ZTRK-ZE)
  500 continue
C
C** compute ET corresponding to other vertices in the event
C
      ECLUS = Q(LCLUS+6)
      ZCLUS = XBAR3(3)
      LCACL = LQ(LCLUS-2)
      RHO = SQRT(XBAR3(1)**2+XBAR3(2)**2)
      DO I = 1, NVERT
        ZV = ZVTX_INFO(1,I)
        TH  =  ATAN2(RHO,ZCLUS-ZV)
        ETZV(I) = ECLUS*SIN(TH)
        THZV(I) = TH
      ENDDO
  999 RETURN
      END
