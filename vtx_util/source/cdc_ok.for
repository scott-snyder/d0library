      LOGICAL FUNCTION CDC_OK(LDTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOOK AT TRACK'S X-Y IMPACT PARAMETER.  THEN COMPUTE
C-   MINIMUM RZ IMPACT PARAMETER CONSIDERING ALL PRIMARY VERTICIES. IF BEST
C-   MATCH SATISFIES CUT, SET DTRK'S REF LINK (-2) TO THE VERT BANK
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-OCT-1993   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LDTRK
c Locals:
      INTEGER ERR,LVERT,STATUS,RUN,LAYER,LVERH,SAVE
      INTEGER VERT_MINTRK
      REAL VTXR7(0:2),VTXHL(0:2),CDC_IMPXY,CDC_IMPSZ
      REAL X0,Y0,ZVTX,DXB,DYB,DZDR,CS,SN,SXY,DIST,MINZ
      REAL VERT_MAXVAL
      LOGICAL FIRST
c Externals:
      INTEGER RUNNO,GZVERH
c Data:
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET_rarr('VTXHL',VTXHL,ERR)
        CALL EZGET_rarr('VTXR7',VTXR7,ERR)
        CALL EZGET('CDC_IMPXY',CDC_IMPXY,ERR)
        CALL EZGET('CDC_IMPSZ',CDC_IMPSZ,ERR)
        CALL EZGET_i('VERT_MINTRK',VERT_MINTRK,ERR)
        CALL EZGET('VERT_MAXVAL',VERT_MAXVAL,ERR)
        CALL EZRSET
      ENDIF
      CDC_OK = .FALSE.
      IF (Q(LDTRK+9) .EQ. 0.) GO TO 999    ! THETA 
C
C **** R-PHI IMPACT PARAMETER
C
      CS = COS(Q(LDTRK+6))
      SN = SIN(Q(LDTRK+6))
      RUN = RUNNO()
      CALL VXY_BEAM(RUN,X0,DXB,Y0,DYB,STATUS)
      DIST = (Y0 - Q(LDTRK+8))*CS - (X0 - Q(LDTRK+7))*SN
      IF (ABS(DIST) .GT. CDC_IMPXY) GO TO 999
C
C ****  R-Z IMPACT PARAMETER: LOOP OVER PRIMARY VERTICIES
C
      LVERH = GZVERH(1)
      IF (LVERH .EQ. 0) GO TO 999
      LVERT = LQ(LVERH-1)
      MINZ = 999999.
      DO WHILE (LVERT .GT. 0)
        IF (IBITS(IQ(LVERT+2),8,8) .LT. VERT_MINTRK) GO TO 10
        ZVTX = Q(LVERT+5)
        IF ( ABS(ZVTX) .GT. VERT_MAXVAL) GO TO 10
        IF ( IBITS(IQ(LVERT+2),24,2)
     &      +IBITS(IQ(LVERT+2),27,1).EQ.0) GO TO 10
        DZDR = (Q(LDTRK+11)-ZVTX)/Q(LDTRK+10)
        DO LAYER = 0,2
          IF ( ABS(ZVTX+DZDR*VTXR7(LAYER)).GT.VTXHL(LAYER)) GO TO 10
        ENDDO
        SXY =  (Y0 - Q(LDTRK+8))*SN + (X0 - Q(LDTRK+7))*CS
        DIST = Q(LDTRK+11) + SXY*COS(Q(LDTRK+9))/SIN(Q(LDTRK+9))-ZVTX
        IF (ABS(DIST) .LT. MINZ) THEN
          MINZ = ABS(DIST)
          SAVE = LVERT
        ENDIF
   10   LVERT = LQ(LVERT)
      ENDDO
      IF (MINZ .GT. CDC_IMPSZ) GO TO 999
C
C ****  DTRK SATISIFIES IMPACT IN SZ PLANE WITH VERTEX POINTED TO BY SAVE
C
      CDC_OK = .TRUE.
      LQ(LDTRK-2) = SAVE
  999 RETURN
      END
