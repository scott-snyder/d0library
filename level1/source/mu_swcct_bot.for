      SUBROUTINE MU_SWCCT_BOT(X,Y,U,SWBITS,SSW,SEGFLG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test good roads for TOP SAMUS-WAMUS overlap.
C-
C-   Inputs  : X(0:31),Y(0:31),U(0:31) - Coarse centroids.
C-   Outputs : SWBITS(0:15) - Good roads found.
C-             SSW - .TRUE. if good S+S+W combinations found
C-             SEGFLG(2) - .TRUE. if segment fired
C-   Controls: None
C-
C-   Created     MAY-1992   G. Lima
C-   Updated  10-JAN-1993   K. Bazizi, G. Lima
C-        Complete reevaluation of CCT logic (version 2):
C-      * Requires A*B*C combinations only, with good pointing to the
C-        interaction region.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER YU(0:15),X(0:31),Y(0:31),U(0:31),SWBITS(0:15),I
      LOGICAL GOODTRIG,SSW,SEGFLG(2)
      INTEGER IDIM
      PARAMETER (IDIM=170)
      INTEGER IROAD(IDIM),ILOG,IVERS
      DATA ILOG/6/,IVERS/2/
      INTEGER BIT(0:7)

      INTEGER IER
      CHARACTER*80 STRING
      LOGICAL FIRST
      DATA FIRST/.TRUE./

C.. Get trigger logic number 
      IF (FIRST) THEN
        FIRST=.FALSE.
C<<                                         
        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' EZPICK ERR','MU_SWCCT_BOT',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('SAM_CCT_VERS',IVERS,IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' SAM_CCT_VERS','MU_SWCCT_BOT',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZRSET()
      ENDIF

C-------------------------------------------

      GOODTRIG=.FALSE.
      SEGFLG(1)=.FALSE.
      SEGFLG(2)=.FALSE.

C..  Calculate first stage pal logic

      IF(IVERS.EQ.1) THEN

        YU(0)   = Y(10)*U( 9) + Y(10)*U(10) + Y(10)*U(11)
     &          + Y(11)*U(10) + Y(11)*U(11)
     &          + Y(12)*U(11) + Y(12)*U(12)

        YU(1)   = Y(16)*U(10) + Y(16)*U(11)
     &          + Y( 9)*U( 8) + Y( 9)*U( 9)
     &          + Y(10)*U( 8) + Y(10)*U( 9) + Y(10)*U(10) + Y(10)*U(11)
     &          + Y(11)*U(10) + Y(11)*U(11)

        YU(2)   = Y( 8)*U( 6) + Y( 8)*U( 7) + Y( 8)*U( 8)
     &          + Y( 9)*U( 6) + Y( 9)*U( 7) + Y( 9)*U( 8)
     &          + Y(10)*U( 8) + Y(10)*U( 9)

        YU(3)   = Y( 7)*U( 5) + Y( 7)*U( 6)
     &          + Y( 8)*U( 5) + Y( 8)*U( 6) + Y( 8)*U( 7)
     &          + Y( 9)*U( 6) + Y( 9)*U( 7) + Y( 9)*U( 8)

        YU(4)   = Y( 6)*U( 3) + Y( 6)*U( 4) + Y( 6)*U( 5)
     &          + Y( 7)*U( 4) + Y( 7)*U( 5)
     &          + Y( 8)*U( 5)

        YU(5)   = X(16)*U( 2) + X(16)*U( 3)
     &          + Y( 6)*U( 4)

        YU(6)   = 0

        YU( 7)  = 0

        YU( 8)  = Y(18)*U(24) + Y(18)*U(25) + Y(18)*U(26) + Y(18)*U(27)
     &          + Y(19)*U(26) + Y(19)*U(27) + Y(19)*U(28)

        YU( 9)  = Y(17)*U(23) + Y(17)*U(24)
     &          + Y(18)*U(24) + Y(18)*U(25) + Y(18)*U(26)

        YU(10)  = Y(16)*U(21) + Y(16)*U(22) + Y(16)*U(23)
     &          + Y(17)*U(23) + Y(17)*U(24)

        YU(11)  = Y(16)*U(21) + Y(16)*U(22)

        YU(12)  = X(24)*U(18) + X(24)*U(19)
     &          + Y( 4)*U(19) + Y( 4)*U(20)

        YU(13)  = X(24)*U(18)
     &          + X(25)*U(17) + X(25)*U(18)

        YU(14)  = 0

        YU(15)  = 0

      ELSEIF (IVERS.EQ.2) THEN

        YU(0)   = Y(10)*U( 8) + Y(10)*U( 9) + Y(10)*U(10)
     &          + Y(11)*U( 9) + Y(11)*U(10) + Y(11)*U(11) + Y(11)*U(12)
     &          + Y(12)*U(10) + Y(12)*U(11) + Y(12)*U(12)

        YU(1)   = Y( 9)*U( 7) + Y( 9)*U( 8) + Y( 9)*U( 9)
     &          + Y(10)*U( 8) + Y(10)*U( 9) + Y(10)*U(10)
     &          + Y(11)*U( 9) + Y(11)*U(10) + Y(11)*U(11)

        YU(2)   = Y( 8)*U( 5) + Y( 8)*U( 6) + Y( 8)*U( 7)
     &          + Y( 9)*U( 6) + Y( 9)*U( 7) + Y( 9)*U( 8) + Y( 9)*U( 9)
     &          + Y(10)*U( 7) + Y(10)*U( 8) + Y(10)*U( 9) + Y(10)*U(10)

        YU(3)   = Y( 7)*U( 4) + Y( 7)*U( 5) + Y( 7)*U( 6)
     &          + Y( 8)*U( 4) + Y( 8)*U( 5) + Y( 8)*U( 6) + Y( 8)*U( 7)
     &          + Y( 9)*U( 6) + Y( 9)*U( 7) + Y( 9)*U( 8)

        YU(4)   = Y( 6)*U( 2) + Y( 6)*U( 3) + Y( 6)*U( 4)
     &          + Y( 7)*U( 3) + Y( 7)*U( 4) + Y( 7)*U( 5)
     &          + Y( 8)*U( 4) + Y( 8)*U( 5) + Y( 8)*U( 6)
     &          + X(16)*U( 1) + X(16)*U( 2) + X(16)*U( 3)

        YU(5)   = X(16)*U( 1) + X(16)*U( 2) + X(16)*U( 3)
     &          + X(17)*U( 0) + X(17)*U( 1) + X(17)*U( 2)
     &          + X(18)*U( 0) + X(18)*U( 1)

        YU(6)   = X(16)*U( 2) + X(16)*U( 3)
     &          + X(17)*U( 0) + X(17)*U( 1) + X(17)*U( 2)
     &          + X(18)*U( 0) + X(18)*U( 1)

        YU( 7)  = 0


        YU( 8)  = Y(18)*U(23) + Y(18)*U(24) + Y(18)*U(25)
     &          + Y(19)*U(25) + Y(19)*U(26) + Y(19)*U(27)
     &          + Y(20)*U(26) + Y(20)*U(27) + Y(20)*U(28)

        YU( 9)  = Y(17)*U(22) + Y(17)*U(23) + Y(17)*U(24)
     &          + Y(18)*U(23) + Y(18)*U(24) + Y(18)*U(25) + Y(18)*U(26)
     &          + Y(19)*U(25) + Y(19)*U(26) + Y(19)*U(27)

        YU(10)  = Y(16)*U(20) + Y(16)*U(21) + Y(16)*U(22) + Y(16)*U(23)
     &          + Y(17)*U(22) + Y(17)*U(23) + Y(17)*U(24)
     &          + Y(18)*U(23) + Y(18)*U(24)

        YU(11)  = Y(16)*U(20) + Y(16)*U(21) + Y(16)*U(22)
     &          + Y(17)*U(22) + Y(17)*U(23) + Y(17)*U(24)

        YU(12)  = X(24)*U(17) + X(24)*U(18) + X(24)*U(19)
     &          + X(25)*U(16) + X(25)*U(17) + X(25)*U(18)

        YU(13)  = X(24)*U(17) + X(24)*U(18) + X(24)*U(19)
     &          + X(25)*U(16) + X(25)*U(17) + X(25)*U(18)
     &          + X(26)*U(16) + X(26)*U(17) + X(26)*U(18)

        YU(14)  = X(25)*U(16) + X(25)*U(17) + X(25)*U(18)
     &          + X(26)*U(16)

        YU(15)  = 0

      ENDIF


C.. Renormalize YU(0,1,2,..,15) to 1 for use in the logic below
      DO I=0,15
        IF(YU(I).GT.0) YU(I)=1
      ENDDO

C.. Second stage pal logic
      DO I=0,15
        SWBITS(I)=0
        IF ((X(I)*YU(I)).NE.0) THEN
          SWBITS(I)=1
          GOODTRIG=.TRUE.
        ENDIF
      ENDDO


C- get the theta distribution
      DO I=0,7
        BIT(I)=0
      ENDDO
      BIT(0)=SWBITS(0)+SWBITS(8)
      BIT(1)=SWBITS(1)+SWBITS(9)
      BIT(2)=SWBITS(2)+SWBITS(10)
      BIT(3)=SWBITS(3)+SWBITS(11)
      BIT(4)=SWBITS(4)+SWBITS(12)
      BIT(5)=SWBITS(5)+SWBITS(13)
      BIT(6)=SWBITS(6)+SWBITS(14)
      BIT(7)=SWBITS(7)+SWBITS(15)
      DO I=0,7
        IF(BIT(I).NE.0) CALL HFILL(1040+ILOG,FLOAT(I),0.,1.)
      ENDDO

C      CALL SAMWAM_ROADS(3200,X,Y,U,SWBITS)

C.. Compute different roads independently for monitoring
      DO I=1,IDIM
        IROAD(I)=0
      ENDDO

C.. Calculate the roads independently for monitoring
      IROAD(  2) = X( 0)*Y(10)*U( 8)
      IROAD(  3) = X( 0)*Y(10)*U( 9)
      IROAD(  4) = X( 0)*Y(10)*U(10)
      IROAD(  5) = X( 0)*Y(11)*U( 9)
      IROAD(  6) = X( 0)*Y(11)*U(10)
      IROAD(  7) = X( 0)*Y(11)*U(11)
      IROAD(  8) = X( 0)*Y(11)*U(12)
      IROAD(  9) = X( 0)*Y(12)*U(10)
      IROAD( 10) = X( 0)*Y(12)*U(11)
      IROAD( 11) = X( 0)*Y(12)*U(12)

      IROAD( 15) = X( 8)*Y(18)*U(23)
      IROAD( 16) = X( 8)*Y(18)*U(24)
      IROAD( 17) = X( 8)*Y(18)*U(25)
      IROAD( 18) = X( 8)*Y(19)*U(25)
      IROAD( 19) = X( 8)*Y(19)*U(26)
      IROAD( 20) = X( 8)*Y(19)*U(27)
      IROAD( 21) = X( 8)*Y(20)*U(26)
      IROAD( 22) = X( 8)*Y(20)*U(27)
      IROAD( 23) = X( 8)*Y(20)*U(28)

      IROAD( 27) = X( 1)*Y( 9)*U( 7)
      IROAD( 28) = X( 1)*Y( 9)*U( 8)
      IROAD( 29) = X( 1)*Y( 9)*U( 9)
      IROAD( 30) = X( 1)*Y(10)*U( 8)
      IROAD( 31) = X( 1)*Y(10)*U( 9)
      IROAD( 32) = X( 1)*Y(10)*U(10)
      IROAD( 33) = X( 1)*Y(11)*U( 9)
      IROAD( 34) = X( 1)*Y(11)*U(10)
      IROAD( 35) = X( 1)*Y(11)*U(11)

      IROAD( 39) = X( 9)*Y(17)*U(22)
      IROAD( 40) = X( 9)*Y(17)*U(23)
      IROAD( 41) = X( 9)*Y(17)*U(24)
      IROAD( 42) = X( 9)*Y(18)*U(23)
      IROAD( 43) = X( 9)*Y(18)*U(24)
      IROAD( 44) = X( 9)*Y(18)*U(25)
      IROAD( 45) = X( 9)*Y(18)*U(26)
      IROAD( 46) = X( 9)*Y(19)*U(25)
      IROAD( 47) = X( 9)*Y(19)*U(26)
      IROAD( 48) = X( 9)*Y(19)*U(27)

      IROAD( 52) = X( 2)*Y( 8)*U( 5)
      IROAD( 53) = X( 2)*Y( 8)*U( 6)
      IROAD( 54) = X( 2)*Y( 8)*U( 7)
      IROAD( 55) = X( 2)*Y( 9)*U( 6)
      IROAD( 56) = X( 2)*Y( 9)*U( 7)
      IROAD( 57) = X( 2)*Y( 9)*U( 8)
      IROAD( 58) = X( 2)*Y( 9)*U( 9)
      IROAD( 59) = X( 2)*Y(10)*U( 7)
      IROAD( 60) = X( 2)*Y(10)*U( 8)
      IROAD( 61) = X( 2)*Y(10)*U( 9)
      IROAD( 62) = X( 2)*Y(10)*U(10)

      IROAD( 66) = X(10)*Y(16)*U(20)
      IROAD( 67) = X(10)*Y(16)*U(21)
      IROAD( 68) = X(10)*Y(16)*U(22)
      IROAD( 69) = X(10)*Y(16)*U(23)
      IROAD( 70) = X(10)*Y(17)*U(22)
      IROAD( 71) = X(10)*Y(17)*U(23)
      IROAD( 72) = X(10)*Y(17)*U(24)
      IROAD( 73) = X(10)*Y(18)*U(23)
      IROAD( 74) = X(10)*Y(18)*U(24)

      IROAD( 78) = X( 3)*Y( 7)*U( 4)
      IROAD( 79) = X( 3)*Y( 7)*U( 5)
      IROAD( 80) = X( 3)*Y( 7)*U( 6)
      IROAD( 81) = X( 3)*Y( 8)*U( 4)
      IROAD( 82) = X( 3)*Y( 8)*U( 5)
      IROAD( 83) = X( 3)*Y( 8)*U( 6)
      IROAD( 84) = X( 3)*Y( 8)*U( 7)
      IROAD( 85) = X( 3)*Y( 9)*U( 6)
      IROAD( 86) = X( 3)*Y( 9)*U( 7)
      IROAD( 87) = X( 3)*Y( 9)*U( 8)

      IROAD( 91) = X(11)*Y(16)*U(20)
      IROAD( 92) = X(11)*Y(16)*U(21)
      IROAD( 93) = X(11)*Y(16)*U(22)
      IROAD( 94) = X(11)*Y(17)*U(22)
      IROAD( 95) = X(11)*Y(17)*U(23)
      IROAD( 96) = X(11)*Y(17)*U(24)

      IROAD(100) = X( 4)*Y( 6)*U( 2)
      IROAD(101) = X( 4)*Y( 6)*U( 3)
      IROAD(102) = X( 4)*Y( 6)*U( 4)
      IROAD(103) = X( 4)*Y( 7)*U( 3)
      IROAD(104) = X( 4)*Y( 7)*U( 4)
      IROAD(105) = X( 4)*Y( 7)*U( 5)
      IROAD(106) = X( 4)*Y( 8)*U( 4)
      IROAD(107) = X( 4)*Y( 8)*U( 5)
      IROAD(108) = X( 4)*Y( 8)*U( 6)
      IROAD(109) = X( 4)*X(16)*U( 1)
      IROAD(110) = X( 4)*X(16)*U( 2)
      IROAD(111) = X( 4)*X(16)*U( 3)

      IROAD(115) = X(12)*X(24)*U(17)
      IROAD(116) = X(12)*X(24)*U(18)
      IROAD(117) = X(12)*X(24)*U(19)
      IROAD(118) = X(12)*X(25)*U(16)
      IROAD(119) = X(12)*X(25)*U(17)
      IROAD(120) = X(12)*X(25)*U(18)

      IROAD(124) = X( 5)*X(16)*U( 1)
      IROAD(125) = X( 5)*X(16)*U( 2)
      IROAD(126) = X( 5)*X(16)*U( 3)
      IROAD(127) = X( 5)*X(17)*U( 0)
      IROAD(128) = X( 5)*X(17)*U( 1)
      IROAD(129) = X( 5)*X(17)*U( 2)
      IROAD(130) = X( 5)*X(18)*U( 0)
      IROAD(131) = X( 5)*X(18)*U( 1)

      IROAD(135) = X(13)*X(24)*U(17)
      IROAD(136) = X(13)*X(24)*U(18)
      IROAD(137) = X(13)*X(24)*U(19)
      IROAD(138) = X(13)*X(25)*U(16)
      IROAD(139) = X(13)*X(25)*U(17)
      IROAD(140) = X(13)*X(25)*U(18)
      IROAD(141) = X(13)*X(26)*U(16)
      IROAD(142) = X(13)*X(26)*U(17)
      IROAD(143) = X(13)*X(26)*U(18)

      IROAD(147) = X( 6)*X(16)*U( 2)
      IROAD(148) = X( 6)*X(16)*U( 3)
      IROAD(149) = X( 6)*X(17)*U( 0)
      IROAD(150) = X( 6)*X(17)*U( 1)
      IROAD(151) = X( 6)*X(17)*U( 2)
      IROAD(152) = X( 6)*X(18)*U( 0)
      IROAD(153) = X( 6)*X(18)*U( 1)

      IROAD(157) = X(14)*X(25)*U(16)
      IROAD(158) = X(14)*X(25)*U(17)
      IROAD(159) = X(14)*X(25)*U(18)
      IROAD(160) = X(14)*X(26)*U(16)

C.. Set flags for segments fired
      DO I=1,180
        IF(                 I.LE. 13.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT. 13.AND.I.LE. 25.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ELSEIF(I.GT. 25.AND.I.LE. 37.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT. 37.AND.I.LE. 50.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ELSEIF(I.GT. 50.AND.I.LE. 64.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT. 64.AND.I.LE. 76.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ELSEIF(I.GT. 76.AND.I.LE. 89.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT. 89.AND.I.LE. 98.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ELSEIF(I.GT. 98.AND.I.LE.113.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT.113.AND.I.LE.122.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ELSEIF(I.GT.122.AND.I.LE.133.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT.133.AND.I.LE.145.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ELSEIF(I.GT.145.AND.I.LE.155.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT.155.AND.I.LE.162.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ENDIF
      ENDDO

C.. Look for good S+S+W combinations
      SSW=.FALSE.
      DO I=109,160
        IF(IROAD(I).NE.0) SSW=.TRUE.
      ENDDO

      DO I=1,IDIM
        IF(IROAD(I).NE.0) CALL HFILL(1030+ILOG,FLOAT(I),0.,1.)
      ENDDO

  999 RETURN
      END     
