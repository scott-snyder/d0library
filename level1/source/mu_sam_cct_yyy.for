      SUBROUTINE MU_SAM_CCT_YYY(CC_IN,CC_OUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test Ya*Yb*Yc roads in SAMUS.
C-
C-   Inputs  : CC_IN(0:15,6)  - Centroids (passed in triplet finder stage)
C-   Outputs : CC_OUT(0:15,2) - Centroids for which good roads were found
C-                              (A-layer is used as reference).
C-   Controls: None
C-
C-   Created  17-JUN-1992   K. Bazizi, G. Lima
C-   Updated  12-DEZ-1992   K. Bazizi, G. Lima
C-        Complete reevaluation of CCT logic (version 2):
C-      * Lower the Pt edge and increase efficiency by accepting more roads.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER CC_IN(0:15,6),CC_OUT(0:15,6)
      INTEGER I,II,YA(0:31),YB(0:31),YC(0:31),YOUT(0:31)
      INTEGER IDIM
      PARAMETER (IDIM=140)
      INTEGER IROAD(IDIM),ILOG,IVERS
      DATA ILOG/8/
      DATA IVERS/2/
      LOGICAL LTRIG

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
          CALL ERRMSG(' EZPICK ERR','MU_SAM_CCT_YYY',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('SAM_CCT_VERS',IVERS,IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' SAM_CCT_VERS','MU_SAM_CCT_YYY',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZRSET()
      ENDIF

C-------------------------------------------
      DO I=0,15
        YA(I)    = CC_IN(I,1)
        YA(I+16) = CC_IN(I,2)
        YB(I)    = CC_IN(I,3)
        YB(I+16) = CC_IN(I,4)
        YC(I)    = CC_IN(I,5)
        YC(I+16) = CC_IN(I,6)
      ENDDO


      IF (IVERS.EQ.1) THEN

        YOUT( 7) = YA( 7)*YB( 4)*YC( 0)

        YOUT( 8) = YA( 8)*(
     &                     YB( 5)*YC( 1) +
     &                     YB( 6)*YC( 2)
     &                     )

        YOUT( 9) = YA( 9)*YB( 7)*YC( 4)

        YOUT(10) = YA(10)*YB( 8)*YC( 6)

        YOUT(11) = YA(11)*YB(31)*YC( 9)

        YOUT(12) = YA(12)*( YB(30)*YC(11) + YB(30)*YC(12) )

        YOUT(13) = YA(13)*(
     &                     YB(28)*YC(14) +
     &                     YB(29)*YC(13)
     &                     )

        YOUT(14) = YA(14)*(
     &                     YB(27)*YC(15) +
     &                     YB(28)*YC(14) + YB(28)*YC(15)
     &                     )

        YOUT(15) = YA(15)*( YB(25)*YC(23) + YB(25)*YC(24) )


        YOUT(23) = YA(23)*YB(20)*YC(16)

        YOUT(24) = YA(24)*( YB(21)*YC(17) + YB(22)*YC(18) )

        YOUT(25) = YA(25)*YB(23)*YC(20)

        YOUT(26) = YA(26)*YB(24)*YC(22)

        YOUT(27) = YA(27)*YB(15)*YC(25)

        YOUT(28) = YA(28)*( YB(14)*YC(27) + YB(14)*YC(28) )

        YOUT(29) = YA(29)*( YB(12)*YC(30) + YB(13)*YC(29) )

        YOUT(30) = YA(30)*(
     &                     YB(11)*YC(31) +
     &                     YB(12)*YC(30) + YB(12)*YC(31)
     &                     )

        YOUT(31) = YA(31)*( YB( 9)*YC( 7) + YB( 9)*YC( 8) )

      ELSE IF(IVERS.EQ.2) THEN

        YOUT( 7)  = YA(7)*(
     &            +        YB(3)*  YC(0)
     &            +        YB(4)*( YC(0)+YC(1) )
     &            +        YB(5)*(       YC(1)+YC(2) ) )

        YOUT( 8)  = YA(8)*(
     &            +        YB(4)*( YC(0)+YC(1) )
     &            +        YB(5)*(       YC(1)+YC(2) )
     &            +        YB(6)*(             YC(2) ) )

        YOUT( 9)  = YA(9)*(
     &            +        YB(6)*( YC(3)+YC(4) )
     &            +        YB(7)*(      +YC(4)+YC(5) )
     &            +        YB(8)*(             YC(5) ) )

        YOUT(10)  = YA(10)*(
     &            +        YB(7)*( YC(5) )
     &            +        YB(8)*(       YC(6) )
     &            +        YB(9)*(             YC(7) ) )

        YOUT(11)  = YA(11)*(
     &            +       YB( 9)*( YC(7)+YC(8) )
     &            +       YB(10)*(       YC(8)+YC(9) )
     &            +       YB(31)*(             YC(9)+YC(10) ) )

        YOUT(12)  = YA(12)*(
     &            +       YB(31)*( YC(10) )
     &            +       YB(30)*(        YC(11)+YC(12) )
     &            +       YB(29)*(               YC(12) ) )

        YOUT(13)  = YA(13)*(
     &            +       YB(29)*( YC(12)+YC(13) )
     &            +       YB(28)*(               YC(14) ) )

        YOUT(14)  = YA(14)*(
     &            +       YB(28)*( YC(14)+YC(15) )
     &            +       YB(27)*(        YC(15)+YC(26) )
     &            +       YB(26)*(                      YC(25) ) )

        YOUT(15)  = YA(15)*(
     &            +       YB(26)*( YC(25)+YC(24) )
     &            +       YB(25)*(        YC(24)+YC(23) ) )


        YOUT(23)  = YA(23)*(
     &            +       YB(19)*( YC(16) )
     &            +       YB(20)*( YC(16)+YC(17) )
     &            +       YB(21)*(        YC(17)+YC(18) ) )

        YOUT(24)  = YA(24)*(
     &            +       YB(20)*( YC(16)+YC(17) )
     &            +       YB(21)*(        YC(17)+YC(18) )
     &            +       YB(22)*(               YC(18) ) )

        YOUT(25)  = YA(25)*(
     &            +       YB(22)*( YC(19)+YC(20) )
     &            +       YB(23)*(        YC(20)+YC(21) )
     &            +       YB(24)*(               YC(21) ) )

        YOUT(26)  = YA(26)*(
     &            +       YB(23)*( YC(21) )
     &            +       YB(24)*(        YC(22) )
     &            +       YB(25)*(               YC(23) ) )

        YOUT(27)  = YA(27)*(
     &            +       YB(25)*( YC(23)+YC(24) )
     &            +       YB(26)*(        YC(24)+YC(25) )
     &            +       YB(15)*(               YC(25)+YC(26) ) )

        YOUT(28)  = YA(28)*(
     &            +       YB(15)*( YC(26) )
     &            +       YB(14)*(        YC(27)+YC(28) )
     &            +       YB(13)*(               YC(28) ) )

        YOUT(29)  = YA(29)*(
     &            +       YB(13)*( YC(28)+YC(29) )
     &            +       YB(12)*(               YC(30) ) )

        YOUT(30)  = YA(30)*(
     &            +       YB(12)*( YC(30)+YC(31) )
     &            +       YB(11)*(        YC(31)+YC(10) )
     &            +       YB(10)*(                      YC(9) ) )

        YOUT(31)  = YA(31)*(
     &            +       YB(10)*( YC(9)+YC(8) )
     &            +       YB( 9)*(       YC(8)+YC(7) ) )

      ENDIF

      DO I=0,15
        IF(YOUT(I).NE.0) CALL HFILL(1040+ILOG,FLOAT(I),0.,1.)
        IF(YOUT(I+16).NE.0) CALL HFILL(1040+ILOG,FLOAT(31-I),0.,1.)
      ENDDO

C.. Calculate the roads independently for monitoring
                                                
      DO I=1,IDIM
        IROAD(I)=0
      ENDDO

      IF(IVERS.EQ.1) THEN
        IROAD( 2) = YA( 7)*YB( 4)*YC( 0)

        IROAD( 5) = YA( 8)*YB( 5)*YC( 1)
        IROAD( 6) = YA( 8)*YB( 6)*YC( 2)

        IROAD( 9) = YA( 9)*YB( 7)*YC( 4)

        IROAD(12) = YA(10)*YB( 8)*YC( 6)

        IROAD(15) = YA(11)*YB(31)*YC( 9)

        IROAD(18) = YA(12)*YB(30)*YC(11)
        IROAD(19) = YA(12)*YB(30)*YC(12)

        IROAD(22) = YA(13)*YB(28)*YC(14)
        IROAD(23) = YA(13)*YB(29)*YC(13)

        IROAD(26) = YA(14)*YB(27)*YC(15)
        IROAD(27) = YA(14)*YB(28)*YC(14)
        IROAD(28) = YA(14)*YB(28)*YC(15)

        IROAD(31) = YA(15)*YB(25)*YC(23)
        IROAD(32) = YA(15)*YB(25)*YC(24)


        IROAD(65) = YA(23)*YB(20)*YC(16)

        IROAD(62) = YA(24)*YB(21)*YC(17)
        IROAD(61) = YA(24)*YB(22)*YC(18)

        IROAD(58) = YA(25)*YB(23)*YC(20)

        IROAD(55) = YA(26)*YB(24)*YC(22)

        IROAD(52) = YA(27)*YB(15)*YC(25)

        IROAD(49) = YA(28)*YB(14)*YC(27)
        IROAD(48) = YA(28)*YB(14)*YC(28)

        IROAD(45) = YA(29)*YB(12)*YC(30)
        IROAD(44) = YA(29)*YB(13)*YC(29)

        IROAD(41) = YA(30)*YB(11)*YC(31)
        IROAD(40) = YA(30)*YB(12)*YC(30)
        IROAD(39) = YA(30)*YB(12)*YC(31)

        IROAD(36) = YA(31)*YB( 9)*YC( 7)
        IROAD(35) = YA(31)*YB( 9)*YC( 8)
      ENDIF

      IF(IVERS.EQ.2) THEN
        IROAD( 2) = YA( 7)*YB( 3)*YC( 0)
        IROAD( 3) = YA( 7)*YB( 4)*YC( 0)
        IROAD( 4) = YA( 7)*YB( 4)*YC( 1)
        IROAD( 5) = YA( 7)*YB( 5)*YC( 1)
        IROAD( 6) = YA( 7)*YB( 5)*YC( 2)

        IROAD( 9) = YA( 8)*YB( 4)*YC( 0)
        IROAD(10) = YA( 8)*YB( 4)*YC( 1)
        IROAD(11) = YA( 8)*YB( 5)*YC( 1)
        IROAD(12) = YA( 8)*YB( 5)*YC( 2)
        IROAD(13) = YA( 8)*YB( 6)*YC( 2)

        IROAD(16) = YA( 9)*YB( 6)*YC( 3)
        IROAD(17) = YA( 9)*YB( 6)*YC( 4)
        IROAD(18) = YA( 9)*YB( 7)*YC( 4)
        IROAD(19) = YA( 9)*YB( 7)*YC( 5)
        IROAD(20) = YA( 9)*YB( 8)*YC( 5)

        IROAD(23) = YA(10)*YB( 7)*YC( 5)
        IROAD(24) = YA(10)*YB( 8)*YC( 6)
        IROAD(25) = YA(10)*YB( 9)*YC( 7)

        IROAD(28) = YA(11)*YB( 9)*YC( 7)
        IROAD(29) = YA(11)*YB( 9)*YC( 8)
        IROAD(30) = YA(11)*YB(10)*YC( 8)
        IROAD(31) = YA(11)*YB(10)*YC( 9)
        IROAD(32) = YA(11)*YB(31)*YC( 9)
        IROAD(33) = YA(11)*YB(31)*YC(10)

        IROAD(36) = YA(12)*YB(31)*YC(10)
        IROAD(37) = YA(12)*YB(30)*YC(11)
        IROAD(38) = YA(12)*YB(30)*YC(12)
        IROAD(39) = YA(12)*YB(29)*YC(12)

        IROAD(42) = YA(13)*YB(29)*YC(12)
        IROAD(43) = YA(13)*YB(29)*YC(13)
        IROAD(44) = YA(13)*YB(28)*YC(14)

        IROAD(47) = YA(14)*YB(28)*YC(14)
        IROAD(48) = YA(14)*YB(28)*YC(15)
        IROAD(49) = YA(14)*YB(27)*YC(15)
        IROAD(50) = YA(14)*YB(27)*YC(26)
        IROAD(51) = YA(14)*YB(26)*YC(25)

        IROAD(54) = YA(15)*YB(26)*YC(25)
        IROAD(55) = YA(15)*YB(26)*YC(24)
        IROAD(56) = YA(15)*YB(25)*YC(24)
        IROAD(57) = YA(15)*YB(25)*YC(23)


        IROAD(61) = YA(31)*YB( 9)*YC( 7)
        IROAD(62) = YA(31)*YB( 9)*YC( 8)
        IROAD(63) = YA(31)*YB(10)*YC( 8)
        IROAD(64) = YA(31)*YB(10)*YC( 9)

        IROAD(67) = YA(30)*YB(10)*YC( 9)
        IROAD(68) = YA(30)*YB(11)*YC(10)
        IROAD(69) = YA(30)*YB(11)*YC(31)
        IROAD(70) = YA(30)*YB(12)*YC(31)
        IROAD(71) = YA(30)*YB(12)*YC(30)

        IROAD(74) = YA(29)*YB(12)*YC(30)
        IROAD(75) = YA(29)*YB(13)*YC(29)
        IROAD(76) = YA(29)*YB(13)*YC(28)

        IROAD(79) = YA(28)*YB(13)*YC(28)
        IROAD(80) = YA(28)*YB(14)*YC(28)
        IROAD(81) = YA(28)*YB(14)*YC(27)
        IROAD(82) = YA(28)*YB(15)*YC(26)

        IROAD(85) = YA(27)*YB(15)*YC(26)
        IROAD(86) = YA(27)*YB(15)*YC(25)
        IROAD(87) = YA(27)*YB(26)*YC(25)
        IROAD(88) = YA(27)*YB(26)*YC(24)
        IROAD(89) = YA(27)*YB(25)*YC(24)
        IROAD(90) = YA(27)*YB(25)*YC(23)

        IROAD(93) = YA(26)*YB(25)*YC(23)
        IROAD(94) = YA(26)*YB(24)*YC(22)
        IROAD(95) = YA(26)*YB(23)*YC(21)

        IROAD( 98) = YA(25)*YB(24)*YC(22)
        IROAD( 99) = YA(25)*YB(23)*YC(21)
        IROAD(100) = YA(25)*YB(23)*YC(20)
        IROAD(101) = YA(25)*YB(22)*YC(20)
        IROAD(102) = YA(25)*YB(22)*YC(19)

        IROAD(105) = YA(24)*YB(22)*YC(18)
        IROAD(106) = YA(24)*YB(21)*YC(18)
        IROAD(107) = YA(24)*YB(21)*YC(17)
        IROAD(108) = YA(24)*YB(20)*YC(17)
        IROAD(109) = YA(24)*YB(20)*YC(16)

        IROAD(112) = YA(23)*YB(21)*YC(18)
        IROAD(113) = YA(23)*YB(21)*YC(17)
        IROAD(114) = YA(23)*YB(20)*YC(17)
        IROAD(115) = YA(23)*YB(20)*YC(16)
        IROAD(116) = YA(23)*YB(19)*YC(16)

      ENDIF

      DO I=1,IDIM
        IF(IROAD(I).NE.0) CALL HFILL(1030+ILOG,FLOAT(I),0.,1.)
      ENDDO


      LTRIG=.FALSE.
      DO II=1,2
        DO I=0,15
          CC_OUT(I,II)=YOUT(16*(II-1)+I)
          IF(CC_OUT(I,II).GT.0) THEN
            CC_OUT(I,II)=1
            LTRIG=.TRUE.
          ENDIF
        ENDDO
      ENDDO

C      CALL SAMUS_ROADS(4200,YA,YB,YC,LTRIG)

  999 RETURN
      END
