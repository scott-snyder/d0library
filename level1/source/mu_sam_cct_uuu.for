      SUBROUTINE MU_SAM_CCT_UUU(CC_IN,CC_OUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test Ua*Uc roads in SAMUS.
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
      INTEGER I,II,UA(0:31),UB(0:31),UC(0:31),UOUT(0:31)
      INTEGER IDIM
      PARAMETER (IDIM=80)
      INTEGER IROAD(IDIM),ILOG,IVERS
      DATA ILOG/9/
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
          CALL ERRMSG(' EZPICK ERR','MU_SAM_CCT_UUU',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('SAM_CCT_VERS',IVERS,IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' SAM_CCT_VERS','MU_SAM_CCT_UUU',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZRSET()
      ENDIF

C-------------------------------------------
      DO I=0,15
        UA(I)    = CC_IN(I,1)
        UA(I+16) = CC_IN(I,2)
        UB(I)    = CC_IN(I,3)
        UB(I+16) = CC_IN(I,4)
        UC(I)    = CC_IN(I,5)
        UC(I+16) = CC_IN(I,6)
      ENDDO


      IF (IVERS.EQ.1) THEN

        UOUT( 8) = UA( 8)*( UC( 2)+UC( 3) )

        UOUT( 9) = UA( 9)*( UC( 3)+UC( 4) )

        UOUT(10) = UA(10)*( UC( 5)+UC( 6) )

        UOUT(11) = UA(11)*( UC( 7)+UC( 8) )

        UOUT(12) = UA(12)*( UC(10)+UC(11) )

        UOUT(13) = UA(13)*( UC(12)+UC(13) )

        UOUT(14) = UA(14)*( UC(14)+UC(15) )

        UOUT(15) = UA(15)*( UC(25)+UC(26) )

        UOUT(24) = UA(24)*( UC(18)+UC(19) )

        UOUT(25) = UA(25)*( UC(19)+UC(20) )

        UOUT(26) = UA(26)*( UC(21)+UC(22) )

        UOUT(27) = UA(27)*( UC(23)+UC(24) )

        UOUT(28) = UA(28)*( UC(26)+UC(27) )

        UOUT(29) = UA(29)*( UC(28)+UC(29) )

        UOUT(30) = UA(30)*( UC(30)+UC(31) )

        UOUT(31) = UA(31)*( UC( 9)+UC(10) )

      ELSE IF (IVERS.EQ.2) THEN

        UOUT( 8) = UA( 8)*( UC( 1) + UC( 2) + UC( 3) )

        UOUT( 9) = UA( 9)*( UC( 3) + UC( 4) + UC( 5) )

        UOUT(10) = UA(10)*( UC( 5) + UC( 6) + UC( 7) )

        UOUT(11) = UA(11)*( UC( 7) + UC( 8) + UC( 9) )

        UOUT(12) = UA(12)*( UC( 9) + UC(10) + UC(11) )

        UOUT(13) = UA(13)*( UC(12) + UC(13) + UC(14) )

        UOUT(14) = UA(14)*( UC(14) + UC(15) + UC(27) )

        UOUT(15) = UA(15)*( UC(24) + UC(25) + UC(26) )

        UOUT(24) = UA(24)*( UC(17) + UC(18) + UC(19) )

        UOUT(25) = UA(25)*( UC(19) + UC(20) + UC(21) )

        UOUT(26) = UA(26)*( UC(21) + UC(22) + UC(23) )

        UOUT(27) = UA(27)*( UC(23) + UC(24) + UC(25) )

        UOUT(28) = UA(28)*( UC(25) + UC(26) + UC(27) )

        UOUT(29) = UA(29)*( UC(28) + UC(29) + UC(30) )

        UOUT(30) = UA(30)*( UC(30) + UC(31) + UC(11) )

        UOUT(31) = UA(31)*( UC( 8) + UC( 9) + UC(10) )

      ENDIF


      DO I=0,15
        IF(UOUT(I).NE.0) CALL HFILL(1040+ILOG,FLOAT(I),0.,1.)
        IF(UOUT(I+16).NE.0) CALL HFILL(1040+ILOG,FLOAT(31-I),0.,1.)
      ENDDO

C..  Calculate the roads independently for monitoring
      DO I=1,IDIM
        IROAD(I)=0
      ENDDO

      IF(IVERS.EQ.1) THEN
        IROAD(  2) = UA( 8)*UC( 2)
        IROAD(  3) = UA( 8)*UC( 3)

        IROAD(  6) = UA( 9)*UC( 3)
        IROAD(  7) = UA( 9)*UC( 4)

        IROAD( 10) = UA(10)*UC( 5)
        IROAD( 11) = UA(10)*UC( 6)

        IROAD( 14) = UA(11)*UC( 7)
        IROAD( 15) = UA(11)*UC( 8)

        IROAD( 18) = UA(12)*UC(10)
        IROAD( 19) = UA(12)*UC(11)

        IROAD( 22) = UA(13)*UC(12)
        IROAD( 23) = UA(13)*UC(13)

        IROAD( 26) = UA(14)*UC(14)
        IROAD( 27) = UA(14)*UC(15)

        IROAD( 30) = UA(15)*UC(25)
        IROAD( 31) = UA(15)*UC(26)

        IROAD( 34) = UA(31)*UC(10)
        IROAD( 35) = UA(31)*UC( 9)

        IROAD( 38) = UA(30)*UC(31)
        IROAD( 39) = UA(30)*UC(30)

        IROAD( 42) = UA(29)*UC(29)
        IROAD( 43) = UA(29)*UC(28)

        IROAD( 46) = UA(28)*UC(27)
        IROAD( 47) = UA(28)*UC(26)

        IROAD( 50) = UA(27)*UC(24)
        IROAD( 51) = UA(27)*UC(23)

        IROAD( 54) = UA(26)*UC(22)
        IROAD( 55) = UA(26)*UC(21)

        IROAD( 58) = UA(25)*UC(20)
        IROAD( 59) = UA(25)*UC(19)

        IROAD( 62) = UA(24)*UC(19)
        IROAD( 63) = UA(24)*UC(18)
      ENDIF

      IF(IVERS.EQ.2) THEN
        IROAD(  2) = UA( 8)*UC( 1)
        IROAD(  3) = UA( 8)*UC( 2)
        IROAD(  4) = UA( 8)*UC( 3)

        IROAD(  7) = UA( 9)*UC( 3)
        IROAD(  8) = UA( 9)*UC( 4)
        IROAD(  9) = UA( 9)*UC( 5)

        IROAD( 12) = UA(10)*UC( 5)
        IROAD( 13) = UA(10)*UC( 6)
        IROAD( 14) = UA(10)*UC( 7)

        IROAD( 17) = UA(11)*UC( 7)
        IROAD( 18) = UA(11)*UC( 8)
        IROAD( 19) = UA(11)*UC( 9)

        IROAD( 22) = UA(12)*UC( 9)
        IROAD( 23) = UA(12)*UC(10)
        IROAD( 24) = UA(12)*UC(11)

        IROAD( 27) = UA(13)*UC(12)
        IROAD( 28) = UA(13)*UC(13)
        IROAD( 29) = UA(13)*UC(14)

        IROAD( 32) = UA(14)*UC(14)
        IROAD( 33) = UA(14)*UC(15)
        IROAD( 34) = UA(14)*UC(27)

        IROAD( 37) = UA(15)*UC(26)
        IROAD( 38) = UA(15)*UC(25)
        IROAD( 39) = UA(15)*UC(24)
                                 

        IROAD( 43) = UA(31)*UC( 8)
        IROAD( 44) = UA(31)*UC( 9)
        IROAD( 45) = UA(31)*UC(10)

        IROAD( 48) = UA(30)*UC(11)
        IROAD( 49) = UA(30)*UC(31)
        IROAD( 50) = UA(30)*UC(30)

        IROAD( 53) = UA(29)*UC(30)
        IROAD( 54) = UA(29)*UC(29)
        IROAD( 55) = UA(29)*UC(28)

        IROAD( 58) = UA(28)*UC(27)
        IROAD( 59) = UA(28)*UC(26)
        IROAD( 60) = UA(28)*UC(25)

        IROAD( 63) = UA(27)*UC(25)
        IROAD( 64) = UA(27)*UC(24)
        IROAD( 65) = UA(27)*UC(23)

        IROAD( 68) = UA(26)*UC(23)
        IROAD( 69) = UA(26)*UC(22)
        IROAD( 70) = UA(26)*UC(21)

        IROAD( 73) = UA(25)*UC(21)
        IROAD( 74) = UA(25)*UC(20)
        IROAD( 75) = UA(25)*UC(19)

        IROAD( 78) = UA(24)*UC(19)
        IROAD( 79) = UA(24)*UC(18)
        IROAD( 80) = UA(24)*UC(17)
      ENDIF


      DO I=1,IDIM
        IF(IROAD(I).NE.0) CALL HFILL(1030+ILOG,FLOAT(I),0.,1.)
      ENDDO

      LTRIG=.FALSE.
      DO II=1,2
        DO I=0,15
          CC_OUT(I,II)=UOUT(16*(II-1)+I)
          IF(CC_OUT(I,II).GT.0) THEN
            CC_OUT(I,II)=1
            LTRIG=.TRUE.
          ENDIF
        ENDDO
      ENDDO

C      CALL SAMUS_ROADS(4300,UA,UB,UC,LTRIG)

  999 RETURN
      END
