      LOGICAL FUNCTION VTRAKS_EXTERNAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Alternative to full tracking: Use external tracks to
C-   build roads into VTX
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-DEC-1993   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
c Locals:
      LOGICAL FIRST
      INTEGER VERT_MINTRK,ERR,LVERH,LVERT,LDTRK,LFDCT,LVTXT,LVTRH
      INTEGER MAXTRK,NCDC,NFDC,RUN,STATUS,I,NV,SAVED,NZVTX
      PARAMETER (MAXTRK=100)
      INTEGER IDC(MAXTRK),IDF(MAXTRK),IDV(MAXTRK)
      REAL    VERT_MAXVAL,PHIRD,X0,Y0,DXB,DYB,PHIC,R0,Z0,THETA
      REAL    ZVTX
      LOGICAL USE_CDC,USE_FDC
c Externals:
      INTEGER GZVERH,GZDTRK,GZFDCT,RUNNO,GZVTRH
      LOGICAL CDC_OK,FDC_OK
c Data:
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      VTRAKS_EXTERNAL = .FALSE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('VERT_MINTRK',VERT_MINTRK,ERR)
        CALL EZGET('VERT_MAXVAL',VERT_MAXVAL,ERR)
        CALL EZGET('PHIRD',PHIRD,ERR)
        CALL EZGET('USE_CDC',USE_CDC,ERR)
        CALL EZGET('USE_FDC',USE_FDC,ERR)
        CALL EZRSET
      ENDIF
      VTRAKS_EXTERNAL = .TRUE.
      SAVED = 0
C
C ****  Requires a good primary vertex
C
      LVERH = GZVERH(1)
      IF (LVERH .EQ. 0) GO TO 999
      LVERT = LQ(LVERH-1)
      NZVTX = 0
      DO WHILE (LVERT .GT. 0)
        IF (IBITS(IQ(LVERT+2),8,8) .GE. VERT_MINTRK) THEN
          IF ( ABS(Q(LVERT+5)) .LT. VERT_MAXVAL) THEN
            IF (USE_CDC .AND.
     &             ( IBITS(IQ(LVERT+2),24,2)
     &              +IBITS(IQ(LVERT+2),27,1) .GT. 0) ) NZVTX = NZVTX + 1
            IF (USE_FDC .AND.
     &             ( IBITS(IQ(LVERT+2),26,1) .EQ. 0) ) NZVTX = NZVTX + 1
          ENDIF
        ENDIF
        LVERT = LQ(LVERT)
      ENDDO
      IF (NZVTX .EQ. 0) GO TO 999
C
C **** Loop over external CDC tracks ...
C
      NCDC = 0
      IF (USE_CDC) THEN
        LDTRK = GZDTRK(0)
        DO WHILE (LDTRK .GT. 0)
          IF (CDC_OK(LDTRK)) THEN
            NCDC = NCDC + 1
            IDC(NCDC) = IQ(LDTRK-5)
            IF (NCDC .EQ. MAXTRK) GO TO 10
          ENDIF
          LDTRK = LQ(LDTRK)
        ENDDO
   10 ENDIF
C
C ****  Loop over external FDC tracks...
C
      NFDC = 0
      IF (USE_FDC) THEN
        LFDCT = GZFDCT(0)
        DO WHILE (LFDCT .GT. 0)
          IF (FDC_OK(LFDCT)) THEN
            NFDC = NFDC + 1
            IDF(NFDC) = IQ(LFDCT-5)
            IF (NFDC .EQ. MAXTRK) GO TO 20
          ENDIF
          LFDCT = LQ(LFDCT)
        ENDDO
   20 ENDIF
C
C ****  Well, have we found anything?
C
      IF (NCDC + NFDC .EQ. 0) GO TO 999
      RUN = RUNNO()
      CALL VXY_BEAM(RUN,X0,DXB,Y0,DYB,STATUS)
C
C ****  Find VTXTs that DTRKs point to
C
      DO I = 1,NCDC
        LDTRK = GZDTRK( IDC(I) )
        LVERT = LQ(LDTRK-2)
        IF (LVERT .EQ. 0) GO TO 30                ! THIS CDC TRACK USED ALREADY
        LQ(LDTRK-2) = 0
        ZVTX = Q(LVERT+5)
        PHIC = ATAN2( Q(LDTRK+8)-Y0 , Q(LDTRK+7)-X0 )
        IF ( PHIC .LT. 0.) PHIC = PHIC + TWOPI
        THETA = ATAN2( Q(LDTRK+10) , Q(LDTRK+11)-ZVTX )
        CALL VTROAD(ZVTX,PHIC-PHIRD,PHIC+PHIRD,THETA,THETA,NV,IDV)
        LDTRK = GZDTRK( IDC(I) )
        CALL VTX_COMPARE(LDTRK,LVTXT)
        IF (LVTXT .GT. 0) SAVED = SAVED + 1
        CALL SAVE_VTXT(LVTXT,LDTRK)
   30 ENDDO
C
C ****  Find VTXTs that FDCTs point to
C
      DO I = 1,NFDC
        LFDCT = GZFDCT( IDF(I) )
        LVERT = LQ(LFDCT-2)
        IF (LVERT .EQ. 0) GO TO 40                ! THIS FDC TRACK USED ALREADY
        LQ(LFDCT-2) = 0
        ZVTX = Q(LVERT+5)
        PHIC = ATAN2( Q(LFDCT+5)-Y0 , Q(LFDCT+4)-X0 )
        IF ( PHIC .LT. 0.) PHIC = PHIC + TWOPI
        CALL FGETZ0(IQ(LFDCT-5),Z0)
        R0 = SQRT(Q(LFDCT+4)**2+Q(LFDCT+5)**2)
        THETA = ATAN2( R0,Z0-ZVTX )
        CALL VTROAD(ZVTX,PHIC-PHIRD,PHIC+PHIRD,THETA,THETA,NV,IDV)
        LFDCT = GZFDCT( IDF(I) )
        CALL VTX_COMPARE(LFDCT,LVTXT)
        IF (LVTXT .GT. 0) SAVED = SAVED + 1
        CALL SAVE_VTXT(LVTXT,LFDCT)
   40 ENDDO
      LVTRH = GZVTRH()
      IF (LVTRH .GT. 0) IQ(LVTRH+7) = SAVED
  999 RETURN
      END
