      SUBROUTINE MU_SAM_CCT_XXX(CC_IN,CC_OUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test Xa*Xb*Xc roads in SAMUS.
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
      INTEGER I,II,XA(0:31),XB(0:31),XC(0:31),XOUT(0:31)
      INTEGER IDIM
      PARAMETER (IDIM=140)
      INTEGER IROAD(IDIM),ILOG,IVERS
      DATA ILOG/7/
      DATA IVERS/2/
      LOGICAL LTRIG
      INTEGER SUMIN,SUMOUT

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
          CALL ERRMSG(' EZPICK ERR','MU_SAM_CCT_XXX',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('SAM_CCT_VERS',IVERS,IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' SAM_CCT_VERS','MU_SAM_CCT_XXX',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZRSET()
      ENDIF

C-------------------------------------------
      DO I=0,15
        XA(I)    = CC_IN(I,1)
        XA(I+16) = CC_IN(I,2)
        XB(I)    = CC_IN(I,3)
        XB(I+16) = CC_IN(I,4)
        XC(I)    = CC_IN(I,5)
        XC(I+16) = CC_IN(I,6)
      ENDDO

      IF (IVERS.EQ.1) THEN

        XOUT( 7)  = XA( 7)*XB( 4)*XC( 0)

        XOUT( 8)  = XA( 8)*( XB( 5)*XC( 1) + XB( 5)*XC( 2) )

        XOUT( 9)  = XA( 9)*(
     &                     XB( 6)*XC( 3) +
     &                     XB( 7)*XC( 4) + XB( 7)*XC( 5)
     &                     )

        XOUT(10) = XA(10)*XB( 8)*XC( 6)

        XOUT(11) = XA(11)*XB(31)*XC( 9)

        XOUT(12) = XA(12)*( XB(30)*XC(11) + XB(30)*XC(12) )

        XOUT(13) = XA(13)*(
     &                     XB(28)*XC(14) +
     &                     XB(29)*XC(13)
     &                     )

        XOUT(14) = XA(14)*(
     &                     XB(27)*XC(15) +
     &                     XB(28)*XC(14) + XB(28)*XC(15)
     &                     )

        XOUT(15) = XA(15)*( XB(25)*XC(23) + XB(25)*XC(24) )


        XOUT(23)  = XA(23)*XB(20)*XC(16)

        XOUT(24)  = XA(24)*( XB(21)*XC(17) + XB(21)*XC(18) )

        XOUT(25)  = XA(25)*(
     &                     XB(22)*XC(19) +
     &                     XB(23)*XC(20) + XB(23)*XC(21)
     &                     )

        XOUT(26) = XA(26)*XB(24)*XC(22)

        XOUT(27) = XA(27)*XB(15)*XC(25)

        XOUT(28) = XA(28)*( XB(14)*XC(27) + XB(14)*XC(28) )

        XOUT(29) = XA(29)*(
     &                     XB(12)*XC(30) +
     &                     XB(13)*XC(29)
     &                     )

        XOUT(30) = XA(30)*(
     &                     XB(11)*XC(31) +
     &                     XB(12)*XC(30) + XB(12)*XC(31)
     &                     )

        XOUT(31) = XA(31)*( XB( 9)*XC( 7) + XB( 9)*XC( 8) )

      ELSE IF(IVERS.EQ.2) THEN

        XOUT( 7)  = XA(7)*(
     &            +        XB(3)*  XC(0)
     &            +        XB(4)*( XC(0)+XC(1) )
     &            +        XB(5)*(       XC(1)+XC(2) ) )

        XOUT( 8)  = XA(8)*(
     &            +        XB(4)*( XC(0)+XC(1) )
     &            +        XB(5)*(       XC(1)+XC(2) )
     &            +        XB(6)*(             XC(2) ) )

        XOUT( 9)  = XA(9)*(
     &            +        XB(6)*( XC(3)+XC(4) )
     &            +        XB(7)*(      +XC(4)+XC(5) )
     &            +        XB(8)*(             XC(5) ) )

        XOUT(10)  = XA(10)*(
     &            +        XB(7)*( XC(5) )
     &            +        XB(8)*(       XC(6) )
     &            +        XB(9)*(             XC(7) ) )

        XOUT(11)  = XA(11)*(
     &            +       XB( 9)*( XC(7)+XC(8) )
     &            +       XB(10)*(       XC(8)+XC(9) )
     &            +       XB(31)*(             XC(9)+XC(10) ) )

        XOUT(12)  = XA(12)*(
     &            +       XB(31)*( XC(10) )
     &            +       XB(30)*(        XC(11)+XC(12) )
     &            +       XB(29)*(               XC(12) ) )

        XOUT(13)  = XA(13)*(
     &            +       XB(29)*( XC(12)+XC(13) )
     &            +       XB(28)*(               XC(14) ) )

        XOUT(14)  = XA(14)*(
     &            +       XB(28)*( XC(14)+XC(15) )
     &            +       XB(27)*(        XC(15)+XC(26) )
     &            +       XB(26)*(                      XC(25) ) )

        XOUT(15)  = XA(15)*(
     &            +       XB(26)*( XC(25)+XC(24) )
     &            +       XB(25)*(        XC(24)+XC(23) ) )


        XOUT(23)  = XA(23)*(
     &            +       XB(19)*( XC(16) )
     &            +       XB(20)*( XC(16)+XC(17) )
     &            +       XB(21)*(        XC(17)+XC(18) ) )

        XOUT(24)  = XA(24)*(
     &            +       XB(20)*( XC(16)+XC(17) )
     &            +       XB(21)*(        XC(17)+XC(18) )
     &            +       XB(22)*(               XC(18) ) )

        XOUT(25)  = XA(25)*(
     &            +       XB(22)*( XC(19)+XC(20) )
     &            +       XB(23)*(        XC(20)+XC(21) )
     &            +       XB(24)*(               XC(21) ) )

        XOUT(26)  = XA(26)*(
     &            +       XB(23)*( XC(21) )
     &            +       XB(24)*(        XC(22) )
     &            +       XB(25)*(               XC(23) ) )

        XOUT(27)  = XA(27)*(
     &            +       XB(25)*( XC(23)+XC(24) )
     &            +       XB(26)*(        XC(24)+XC(25) )
     &            +       XB(15)*(               XC(25)+XC(26) ) )

        XOUT(28)  = XA(28)*(
     &            +       XB(15)*( XC(26) )
     &            +       XB(14)*(        XC(27)+XC(28) )
     &            +       XB(13)*(               XC(28) ) )

        XOUT(29)  = XA(29)*(
     &            +       XB(13)*( XC(28)+XC(29) )
     &            +       XB(12)*(               XC(30) ) )

        XOUT(30)  = XA(30)*(
     &            +       XB(12)*( XC(30)+XC(31) )
     &            +       XB(11)*(        XC(31)+XC(10) )
     &            +       XB(10)*(                      XC(9) ) )

        XOUT(31)  = XA(31)*(
     &            +       XB(10)*( XC(9)+XC(8) )
     &            +       XB( 9)*(       XC(8)+XC(7) ) )


      ENDIF   ! End of version 2

                                                        
      DO I=0,15
        IF(XOUT(I).NE.0) CALL HFILL(1040+ILOG,FLOAT(I),0.,1.)
        IF(XOUT(I+16).NE.0) CALL HFILL(1040+ILOG,FLOAT(31-I),0.,1.)
      ENDDO

C.. Calculate the roads independently for monitoring

      DO I=1,IDIM
        IROAD(I)=0
      ENDDO

      IF(IVERS.EQ.1) THEN
        IROAD( 2) = XA( 7)*XB( 4)*XC( 0)

        IROAD( 5) = XA( 8)*XB( 5)*XC( 1)
        IROAD( 6) = XA( 8)*XB( 5)*XC( 2)

        IROAD( 9) = XA( 9)*XB( 6)*XC( 3)
        IROAD(10) = XA( 9)*XB( 7)*XC( 4)
        IROAD(11) = XA( 9)*XB( 7)*XC( 5)

        IROAD(14) = XA(10)*XB( 8)*XC( 6)

        IROAD(17) = XA(11)*XB(31)*XC( 9)

        IROAD(20) = XA(12)*XB(30)*XC(11)
        IROAD(21) = XA(12)*XB(30)*XC(12)

        IROAD(24) = XA(13)*XB(28)*XC(14)
        IROAD(25) = XA(13)*XB(29)*XC(13)

        IROAD(28) = XA(14)*XB(27)*XC(15)
        IROAD(29) = XA(14)*XB(28)*XC(14)
        IROAD(30) = XA(14)*XB(28)*XC(15)

        IROAD(33) = XA(15)*XB(25)*XC(23)
        IROAD(34) = XA(15)*XB(25)*XC(24)


        IROAD(69) = XA(23)*XB(20)*XC(16)

        IROAD(66) = XA(24)*XB(21)*XC(17)
        IROAD(65) = XA(24)*XB(21)*XC(18)

        IROAD(62) = XA(25)*XB(22)*XC(19)
        IROAD(61) = XA(25)*XB(23)*XC(20)
        IROAD(60) = XA(25)*XB(23)*XC(21)

        IROAD(57) = XA(26)*XB(24)*XC(22)

        IROAD(54) = XA(27)*XB(15)*XC(25)

        IROAD(51) = XA(28)*XB(14)*XC(27)
        IROAD(50) = XA(28)*XB(14)*XC(28)

        IROAD(47) = XA(29)*XB(12)*XC(30)
        IROAD(46) = XA(29)*XB(13)*XC(29)

        IROAD(43) = XA(30)*XB(11)*XC(31)
        IROAD(42) = XA(30)*XB(12)*XC(30)
        IROAD(41) = XA(30)*XB(12)*XC(31)

        IROAD(38) = XA(31)*XB( 9)*XC( 7)
        IROAD(37) = XA(31)*XB( 9)*XC( 8)
      ENDIF

      IF(IVERS.EQ.2) THEN
        IROAD( 2) = XA( 7)*XB( 3)*XC( 0)
        IROAD( 3) = XA( 7)*XB( 4)*XC( 0)
        IROAD( 4) = XA( 7)*XB( 4)*XC( 1)
        IROAD( 5) = XA( 7)*XB( 5)*XC( 1)
        IROAD( 6) = XA( 7)*XB( 5)*XC( 2)

        IROAD( 9) = XA( 8)*XB( 4)*XC( 0)
        IROAD(10) = XA( 8)*XB( 4)*XC( 1)
        IROAD(11) = XA( 8)*XB( 5)*XC( 1)
        IROAD(12) = XA( 8)*XB( 5)*XC( 2)
        IROAD(13) = XA( 8)*XB( 6)*XC( 2)

        IROAD(16) = XA( 9)*XB( 6)*XC( 3)
        IROAD(17) = XA( 9)*XB( 6)*XC( 4)
        IROAD(18) = XA( 9)*XB( 7)*XC( 4)
        IROAD(19) = XA( 9)*XB( 7)*XC( 5)
        IROAD(20) = XA( 9)*XB( 8)*XC( 5)

        IROAD(23) = XA(10)*XB( 7)*XC( 5)
        IROAD(24) = XA(10)*XB( 8)*XC( 6)
        IROAD(25) = XA(10)*XB( 9)*XC( 7)

        IROAD(28) = XA(11)*XB( 9)*XC( 7)
        IROAD(29) = XA(11)*XB( 9)*XC( 8)
        IROAD(30) = XA(11)*XB(10)*XC( 8)
        IROAD(31) = XA(11)*XB(10)*XC( 9)
        IROAD(32) = XA(11)*XB(31)*XC( 9)
        IROAD(33) = XA(11)*XB(31)*XC(10)

        IROAD(36) = XA(12)*XB(31)*XC(10)
        IROAD(37) = XA(12)*XB(30)*XC(11)
        IROAD(38) = XA(12)*XB(30)*XC(12)
        IROAD(39) = XA(12)*XB(29)*XC(12)

        IROAD(42) = XA(13)*XB(29)*XC(12)
        IROAD(43) = XA(13)*XB(29)*XC(13)
        IROAD(44) = XA(13)*XB(28)*XC(14)

        IROAD(47) = XA(14)*XB(28)*XC(14)
        IROAD(48) = XA(14)*XB(28)*XC(15)
        IROAD(49) = XA(14)*XB(27)*XC(15)
        IROAD(50) = XA(14)*XB(27)*XC(26)
        IROAD(51) = XA(14)*XB(26)*XC(25)

        IROAD(54) = XA(15)*XB(26)*XC(25)
        IROAD(55) = XA(15)*XB(26)*XC(24)
        IROAD(56) = XA(15)*XB(25)*XC(24)
        IROAD(57) = XA(15)*XB(25)*XC(23)


        IROAD(61) = XA(31)*XB( 9)*XC( 7)
        IROAD(62) = XA(31)*XB( 9)*XC( 8)
        IROAD(63) = XA(31)*XB(10)*XC( 8)
        IROAD(64) = XA(31)*XB(10)*XC( 9)

        IROAD(67) = XA(30)*XB(10)*XC( 9)
        IROAD(68) = XA(30)*XB(11)*XC(10)
        IROAD(69) = XA(30)*XB(11)*XC(31)
        IROAD(70) = XA(30)*XB(12)*XC(31)
        IROAD(71) = XA(30)*XB(12)*XC(30)

        IROAD(74) = XA(29)*XB(12)*XC(30)
        IROAD(75) = XA(29)*XB(13)*XC(29)
        IROAD(76) = XA(29)*XB(13)*XC(28)

        IROAD(79) = XA(28)*XB(13)*XC(28)
        IROAD(80) = XA(28)*XB(14)*XC(28)
        IROAD(81) = XA(28)*XB(14)*XC(27)
        IROAD(82) = XA(28)*XB(15)*XC(26)

        IROAD(85) = XA(27)*XB(15)*XC(26)
        IROAD(86) = XA(27)*XB(15)*XC(25)
        IROAD(87) = XA(27)*XB(26)*XC(25)
        IROAD(88) = XA(27)*XB(26)*XC(24)
        IROAD(89) = XA(27)*XB(25)*XC(24)
        IROAD(90) = XA(27)*XB(25)*XC(23)

        IROAD(93) = XA(26)*XB(25)*XC(23)
        IROAD(94) = XA(26)*XB(24)*XC(22)
        IROAD(95) = XA(26)*XB(23)*XC(21)

        IROAD( 98) = XA(25)*XB(24)*XC(22)
        IROAD( 99) = XA(25)*XB(23)*XC(21)
        IROAD(100) = XA(25)*XB(23)*XC(20)
        IROAD(101) = XA(25)*XB(22)*XC(20)
        IROAD(102) = XA(25)*XB(22)*XC(19)

        IROAD(105) = XA(24)*XB(22)*XC(18)
        IROAD(106) = XA(24)*XB(21)*XC(18)
        IROAD(107) = XA(24)*XB(21)*XC(17)
        IROAD(108) = XA(24)*XB(20)*XC(17)
        IROAD(109) = XA(24)*XB(20)*XC(16)

        IROAD(112) = XA(23)*XB(21)*XC(18)
        IROAD(113) = XA(23)*XB(21)*XC(17)
        IROAD(114) = XA(23)*XB(20)*XC(17)
        IROAD(115) = XA(23)*XB(20)*XC(16)
        IROAD(116) = XA(23)*XB(19)*XC(16)

      ENDIF
                 

      DO I=1,IDIM
        IF(IROAD(I).NE.0) CALL HFILL(1030+ILOG,FLOAT(I),0.,1.)
      ENDDO

      LTRIG=.FALSE.
      DO II=1,2
        DO I=0,15
          CC_OUT(I,II)=XOUT(16*(II-1)+I)
          IF(CC_OUT(I,II).GT.0) THEN
            CC_OUT(I,II)=1
            LTRIG=.TRUE.
          ENDIF
        ENDDO
      ENDDO

C      CALL SAMUS_ROADS(4100,XA,XB,XC,LTRIG)

  999 RETURN
      END
