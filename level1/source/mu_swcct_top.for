      SUBROUTINE MU_SWCCT_TOP(X,Y,U,SWBITS,SSW,SEGFLG)
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
      PARAMETER (IDIM=180)
      INTEGER IROAD(IDIM),ILOG,IVERS
      INTEGER BIT(0:7)
      DATA ILOG/5/,IVERS/2/

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
          CALL ERRMSG(' EZPICK ERR','MU_SWCCT_TOP',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('SAM_CCT_VERS',IVERS,IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' SAM_CCT_VERS','MU_SWCCT_TOP',STRING,'F')
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
     &          + Y(11)*U(10) + Y(11)*U(11) + Y(11)*U(12)
     &          + Y(12)*U(11) + Y(12)*U(12)

        YU(1)   = Y( 9)*U( 8) + Y( 9)*U( 9)
     &          + Y(10)*U( 8) + Y(10)*U( 9) + Y(10)*U(10)
     &          + Y(11)*U( 9) + Y(11)*U(10)

        YU(2)   = Y( 8)*U( 6) + Y( 8)*U( 7) + Y( 8)*U( 8)
     &          + Y( 9)*U( 7) + Y( 9)*U( 8) + Y( 9)*U( 9)
     &          + Y(10)*U( 8) + Y(10)*U( 9)

        YU(3)   = Y( 7)*U( 4) + Y( 7)*U( 5) + Y( 7)*U( 6)
     &          + Y( 8)*U( 5) + Y( 8)*U( 6) + Y( 8)*U( 7)
     &          + Y( 9)*U( 7)

        YU(4)   = Y( 6)*U( 3) + Y( 6)*U( 4)
     &          + Y( 7)*U( 3) + Y( 7)*U( 4) + Y( 7)*U( 5)

        YU(5)   = Y(16)*U( 2) + Y(16)*U( 3)
     &          + Y( 6)*U( 3) + Y( 6)*U( 4)
     &          + Y( 7)*U( 4)

        YU(6)  = 0

        YU( 7)  = 0

        YU( 8)  = Y( 8)*U(25) + Y( 8)*U(26)
     &          + Y( 9)*U(26) + Y( 9)*U(27)

        YU( 9)  = Y( 3)*U(24) + Y( 3)*U(25)
     &          + Y( 4)*U(24) + Y( 4)*U(25)
     &          + Y( 5)*U(24)

        YU(10)  = Y( 4)*U( 1)
     &          + Y( 5)*U( 1) + Y( 5)*U( 2)
     &          + Y( 6)*U(22) + Y( 6)*U(23)

        YU(11)  = Y(24)*U(18) + Y(24)*U(19) + Y(24)*U(20)
     &          + Y( 4)*U(20)
     &          + Y( 5)*U(20)
     &          + Y( 6)*U(20)

        YU(12)  = Y(24)*U(18) + Y(24)*U(19)
     &          + Y( 5)*U(19) + Y( 5)*U(20)

        YU(13)  = Y(24)*U(18)
     &          + Y(25)*U(17) + Y(25)*U(18)
     &          + Y(26)*U(16)

        YU(14)  = 0

        YU(15)  = 0

      ELSEIF(IVERS.EQ.2) THEN
        YU( 0)  = Y(10)*U( 8) + Y(10)*U( 9) + Y(10)*U(10)
     &          + Y(11)*U( 9) + Y(11)*U(10) + Y(11)*U(11) + Y(11)*U(12)
     &          + Y(12)*U(10) + Y(12)*U(11) + Y(12)*U(12) + Y(12)*U(13)

        YU( 1)  = Y( 9)*U( 7) + Y( 9)*U( 8) + Y( 9)*U( 9)
     &          + Y(10)*U( 8) + Y(10)*U( 9) + Y(10)*U(10)
     &          + Y(11)*U( 9) + Y(11)*U(10) + Y(11)*U(11)

        YU( 2)  = Y( 8)*U( 6) + Y( 8)*U( 7)
     &          + Y( 9)*U( 6) + Y( 9)*U( 7) + Y( 9)*U( 8) + Y( 9)*U( 9)
     &          + Y(10)*U( 7) + Y(10)*U( 8) + Y(10)*U( 9) + Y(10)*U(10)

        YU( 3)  = Y( 6)*U( 4)
     &          + Y( 7)*U( 4) + Y( 7)*U( 5) + Y( 7)*U( 6)
     &          + Y( 8)*U( 4) + Y( 8)*U( 5) + Y( 8)*U( 6) + Y( 8)*U( 7)
     &          + Y( 9)*U( 6) + Y( 9)*U( 7) + Y( 9)*U( 8)

        YU( 4)  = Y( 6)*U( 2) + Y( 6)*U( 3) + Y( 6)*U( 4)
     &          + Y( 7)*U( 3) + Y( 7)*U( 4) + Y( 7)*U( 5)
     &          + Y( 8)*U( 4) + Y( 8)*U( 5) + Y( 8)*U( 6)
     &          + Y(16)*U( 1) + Y(16)*U( 2)

        YU( 5)  = Y( 6)*U( 1) + Y( 6)*U( 2) + Y( 6)*U( 3)
     &          + Y( 7)*U( 2) + Y( 7)*U( 3) + Y( 7)*U( 4) + Y( 7)*U( 5)
     &          + Y(16)*U( 1) + Y(16)*U( 2) + Y(16)*U( 3)
     &          + Y(17)*U( 0) + Y(17)*U( 1) + Y(17)*U( 2)
     &          + Y(18)*U( 0) + Y(18)*U( 1)

        YU( 6)  = Y(16)*U( 0) + Y(16)*U( 1) + Y(16)*U( 2)
     &          + Y(17)*U( 0) + Y(17)*U( 1) + Y(17)*U( 2)
     &          + Y(18)*U( 0) + Y(18)*U( 1) + Y(18)*U( 2) + Y(18)*U( 3)

        YU( 7)  = 0


        YU( 8)  = Y( 6)*U(25) + Y( 6)*U(26) + Y( 6)*U(27)
     &          + Y( 7)*U(26) + Y( 7)*U(27)
     &          + Y( 8)*U(26) + Y( 8)*U(27) + Y( 8)*U(28)
     &          + Y( 9)*U(26) + Y( 9)*U(27) + Y( 9)*U(28)

        YU( 9)  = Y( 5)*U(24) + Y( 5)*U(25)
     &          + Y( 6)*U(23) + Y( 6)*U(24)
     &          + Y( 7)*U(25) + Y( 7)*U(26)
     &          + Y( 8)*U(23) + Y( 8)*U(24) + Y( 8)*U(25)

        YU(10)  = Y( 4)*U(21) + Y( 4)*U(22) + Y( 4)*U(23) + Y( 4)*U(24)
     &          + Y( 5)*U(22) + Y( 5)*U(23) + Y( 5)*U(24)
     &          + Y( 6)*U(22)
     &          + Y( 7)*U(22) + Y( 7)*U(23) + Y( 7)*U(24)

        YU(11)  = Y( 4)*U(20) + Y( 4)*U(21) + Y( 4)*U(22)
     &          + Y( 5)*U(20) + Y( 5)*U(21) + Y( 5)*U(22)
     &          + Y( 6)*U(19) + Y( 6)*U(20) + Y( 6)*U(21) + Y( 6)*U(22)

        YU(12)  = Y(24)*U(17) + Y(24)*U(18) + Y(24)*U(19)
     &          + Y(25)*U(16) + Y(25)*U(17) + Y(25)*U(18)
     &          + Y(26)*U(16)

        YU(13)  = Y(24)*U(17) + Y(24)*U(18) + Y(24)*U(19)
     &          + Y(25)*U(16) + Y(25)*U(17) + Y(25)*U(18)
     &          + Y(26)*U(16) + Y(26)*U(17) + Y(26)*U(18)

        YU(14)  = Y(25)*U(16) + Y(25)*U(17)
     &          + Y(26)*U(16) + Y(26)*U(17)

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

C      CALL SAMWAM_ROADS(3100,X,Y,U,SWBITS)

C..  Compute different roads independently for monitoring
      DO I=0,IDIM
        IROAD(I)=0
      ENDDO

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
      IROAD( 12) = X( 0)*Y(12)*U(13)

      IROAD( 16) = X( 8)*Y( 6)*U(25)
      IROAD( 17) = X( 8)*Y( 6)*U(26)
      IROAD( 18) = X( 8)*Y( 6)*U(27)
      IROAD( 19) = X( 8)*Y( 7)*U(26)
      IROAD( 20) = X( 8)*Y( 7)*U(27)
      IROAD( 21) = X( 8)*Y( 8)*U(26)
      IROAD( 22) = X( 8)*Y( 8)*U(27)
      IROAD( 23) = X( 8)*Y( 8)*U(28)
      IROAD( 24) = X( 8)*Y( 9)*U(26)
      IROAD( 25) = X( 8)*Y( 9)*U(27)
      IROAD( 26) = X( 8)*Y( 9)*U(28)

      IROAD( 30) = X( 1)*Y( 9)*U( 7)
      IROAD( 31) = X( 1)*Y( 9)*U( 8)
      IROAD( 32) = X( 1)*Y( 9)*U( 9)
      IROAD( 33) = X( 1)*Y(10)*U( 8)
      IROAD( 34) = X( 1)*Y(10)*U( 9)
      IROAD( 35) = X( 1)*Y(10)*U(10)
      IROAD( 36) = X( 1)*Y(11)*U( 9)
      IROAD( 37) = X( 1)*Y(11)*U(10)
      IROAD( 38) = X( 1)*Y(11)*U(11)

      IROAD( 42) = X( 9)*Y( 5)*U(24)
      IROAD( 43) = X( 9)*Y( 5)*U(25)
      IROAD( 44) = X( 9)*Y( 6)*U(23)
      IROAD( 45) = X( 9)*Y( 6)*U(24)
      IROAD( 46) = X( 9)*Y( 7)*U(25)
      IROAD( 47) = X( 9)*Y( 7)*U(26)
      IROAD( 48) = X( 9)*Y( 8)*U(23)
      IROAD( 49) = X( 9)*Y( 8)*U(24)
      IROAD( 50) = X( 9)*Y( 8)*U(25)

      IROAD( 54) = X( 2)*Y( 8)*U( 6)
      IROAD( 55) = X( 2)*Y( 8)*U( 7)
      IROAD( 56) = X( 2)*Y( 9)*U( 6)
      IROAD( 57) = X( 2)*Y( 9)*U( 7)
      IROAD( 58) = X( 2)*Y( 9)*U( 8)
      IROAD( 59) = X( 2)*Y( 9)*U( 9)
      IROAD( 60) = X( 2)*Y(10)*U( 7)
      IROAD( 61) = X( 2)*Y(10)*U( 8)
      IROAD( 62) = X( 2)*Y(10)*U( 9)
      IROAD( 63) = X( 2)*Y(10)*U(10)

      IROAD( 67) = X(10)*Y( 4)*U(21)
      IROAD( 68) = X(10)*Y( 4)*U(22)
      IROAD( 69) = X(10)*Y( 4)*U(23)
      IROAD( 70) = X(10)*Y( 4)*U(24)
      IROAD( 71) = X(10)*Y( 5)*U(22)
      IROAD( 72) = X(10)*Y( 5)*U(23)
      IROAD( 73) = X(10)*Y( 5)*U(24)
      IROAD( 74) = X(10)*Y( 6)*U(22)
      IROAD( 75) = X(10)*Y( 7)*U(22)
      IROAD( 76) = X(10)*Y( 7)*U(23)
      IROAD( 77) = X(10)*Y( 7)*U(24)

      IROAD( 81) = X( 3)*Y( 6)*U( 4)
      IROAD( 82) = X( 3)*Y( 7)*U( 4)
      IROAD( 83) = X( 3)*Y( 7)*U( 5)
      IROAD( 84) = X( 3)*Y( 7)*U( 6)
      IROAD( 85) = X( 3)*Y( 8)*U( 4)
      IROAD( 86) = X( 3)*Y( 8)*U( 5)
      IROAD( 87) = X( 3)*Y( 8)*U( 6)
      IROAD( 88) = X( 3)*Y( 8)*U( 7)
      IROAD( 89) = X( 3)*Y( 9)*U( 6)
      IROAD( 90) = X( 3)*Y( 9)*U( 7)
      IROAD( 91) = X( 3)*Y( 9)*U( 8)

      IROAD( 95) = X(11)*Y( 4)*U(20)
      IROAD( 96) = X(11)*Y( 4)*U(21)
      IROAD( 97) = X(11)*Y( 4)*U(22)
      IROAD( 98) = X(11)*Y( 5)*U(20)
      IROAD( 99) = X(11)*Y( 5)*U(21)
      IROAD(100) = X(11)*Y( 5)*U(22)
      IROAD(101) = X(11)*Y( 6)*U(19)
      IROAD(102) = X(11)*Y( 6)*U(20)
      IROAD(103) = X(11)*Y( 6)*U(21)
      IROAD(104) = X(11)*Y( 6)*U(22)

      IROAD(109) = X( 4)*Y( 6)*U( 2)
      IROAD(110) = X( 4)*Y( 6)*U( 3)
      IROAD(111) = X( 4)*Y( 6)*U( 4)
      IROAD(112) = X( 4)*Y( 7)*U( 3)
      IROAD(113) = X( 4)*Y( 7)*U( 4)
      IROAD(114) = X( 4)*Y( 7)*U( 5)
      IROAD(115) = X( 4)*Y( 8)*U( 4)
      IROAD(116) = X( 4)*Y( 8)*U( 5)
      IROAD(117) = X( 4)*Y( 8)*U( 6)
      IROAD(118) = X( 4)*Y(16)*U( 1)
      IROAD(119) = X( 4)*Y(16)*U( 2)

      IROAD(123) = X(12)*Y(24)*U(17)
      IROAD(124) = X(12)*Y(24)*U(18)
      IROAD(125) = X(12)*Y(24)*U(19)
      IROAD(126) = X(12)*Y(25)*U(16)
      IROAD(127) = X(12)*Y(25)*U(17)
      IROAD(128) = X(12)*Y(25)*U(18)
      IROAD(129) = X(12)*Y(26)*U(16)

      IROAD(133) = X( 5)*Y( 6)*U( 1)
      IROAD(134) = X( 5)*Y( 6)*U( 2)
      IROAD(135) = X( 5)*Y( 6)*U( 3)
      IROAD(136) = X( 5)*Y( 7)*U( 2)
      IROAD(137) = X( 5)*Y( 7)*U( 3)
      IROAD(138) = X( 5)*Y( 7)*U( 4)
      IROAD(139) = X( 5)*Y( 7)*U( 5)
      IROAD(140) = X( 5)*Y(16)*U( 1)
      IROAD(141) = X( 5)*Y(16)*U( 2)
      IROAD(142) = X( 5)*Y(16)*U( 3)
      IROAD(143) = X( 5)*Y(17)*U( 0)
      IROAD(144) = X( 5)*Y(17)*U( 1)
      IROAD(145) = X( 5)*Y(17)*U( 2)
      IROAD(146) = X( 5)*Y(18)*U( 0)
      IROAD(147) = X( 5)*Y(18)*U( 1)

      IROAD(151) = X(13)*Y(24)*U(17)
      IROAD(152) = X(13)*Y(24)*U(18)
      IROAD(153) = X(13)*Y(24)*U(19)
      IROAD(154) = X(13)*Y(25)*U(16)
      IROAD(155) = X(13)*Y(25)*U(17)
      IROAD(156) = X(13)*Y(25)*U(18)
      IROAD(157) = X(13)*Y(26)*U(16)
      IROAD(158) = X(13)*Y(26)*U(17)
      IROAD(159) = X(13)*Y(26)*U(18)

      IROAD(163) = X( 6)*Y(16)*U( 0)
      IROAD(164) = X( 6)*Y(16)*U( 1)
      IROAD(165) = X( 6)*Y(16)*U( 2)
      IROAD(166) = X( 6)*Y(17)*U( 0)
      IROAD(167) = X( 6)*Y(17)*U( 1)
      IROAD(168) = X( 6)*Y(17)*U( 2)
      IROAD(169) = X( 6)*Y(18)*U( 0)
      IROAD(170) = X( 6)*Y(18)*U( 1)
      IROAD(171) = X( 6)*Y(18)*U( 2)
      IROAD(172) = X( 6)*Y(18)*U( 3)

      IROAD(176) = X(14)*Y(25)*U(16)
      IROAD(177) = X(14)*Y(25)*U(17)
      IROAD(178) = X(14)*Y(26)*U(16)
      IROAD(179) = X(14)*Y(26)*U(17)

C.. Set flags for segments fired
      DO I=1,180
        IF(             I.LE. 14.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT. 14.AND.I.LE. 28.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ELSEIF(I.GT. 28.AND.I.LE. 40.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT. 40.AND.I.LE. 52.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ELSEIF(I.GT. 52.AND.I.LE. 65.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT. 65.AND.I.LE. 79.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ELSEIF(I.GT. 79.AND.I.LE. 93.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT. 93.AND.I.LE.106.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ELSEIF(I.GT.106.AND.I.LE.121.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT.121.AND.I.LE.131.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ELSEIF(I.GT.131.AND.I.LE.149.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT.149.AND.I.LE.161.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ELSEIF(I.GT.161.AND.I.LE.174.AND.IROAD(I).NE.0) THEN
          SEGFLG(1)=.TRUE.
        ELSEIF(I.GT.174.AND.I.LE.180.AND.IROAD(I).NE.0) THEN
          SEGFLG(2)=.TRUE.
        ENDIF
      ENDDO

C.. Look for good S+S+W combinations
      SSW=.FALSE.
      DO I=118,129
        IF(IROAD(I).NE.0) SSW=.TRUE.
      ENDDO
      DO I=140,179
        IF(IROAD(I).NE.0) SSW=.TRUE.
      ENDDO

      DO I=1,IDIM
        IF(IROAD(I).NE.0) CALL HFILL(1030+ILOG,FLOAT(I),0.,1.)
      ENDDO

  999 RETURN
      END
