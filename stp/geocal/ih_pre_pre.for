      SUBROUTINE IH_PRE_PRE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Really use Al Ito uncivilized numbers
C-                        and knock them to usable form
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  13-NOV-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NZDIV
      DATA NZDIV/11/
      INTEGER LSR(1000,11),I,LEN3,NRAD(11),J,K,NPHI(100,11)
      REAL RSR(1000,11),ETA(100,11),THET,ARG,ZSTART,ENDP1,PHIRAP
      REAL ZEND,ENDP2,CHPLATE
      EQUIVALENCE (LSR,RSR)
C
      CHARACTER*4 ZDVN(11)
      DATA ZDVN/'ZD01','ZD02','ZD03','ZD04','ZD05','ZD06','ZD07',
     &          'ZD08','ZD09','ZD10','ZD11'/
C
      CHARACTER*32 NMSRCP
      CHARACTER*4 CHR
C----------------------------------------------------------------------
      CALL GTSRCP('ARGON_GAP',ARG,1)
      CALL GTSRCP('STARTING_Z',ZSTART,1)
      CALL GTSRCP('ENDING_Z',ZEND,1)
      CALL GTSRCP('FIRST_END_PLATE',ENDP1,1)
      CALL GTSRCP('LAST_END_PLATE',ENDP2,1)
      CALL GTSRCP('PHI_CHANGE_RAPIDITY',PHIRAP,1)
      CALL GTSRCP('CH_ABSORBER',CHPLATE,1)
C
      DO 100 I = 1,NZDIV
        CALL ADDSTR(ZDVN(I),'(1)',NMSRCP,LEN3)
        CALL GTSRCP(NMSRCP,LSR(1,I),1)
        NRAD(I) = 0
        DO 200 J = 1,100
          IF(LSR(J,I).EQ.0)GO TO 201
  200   CONTINUE
  201   NRAD(I)= J-4     !Number of radii
        NPHI(1,I) = 64   !decreasing order for RADII
        DO 300 K = 1,NRAD(I)
          THET = ATAN2(RSR(3+K,I),RSR(3,I))
          ETA(K,I) = -ALOG(SIN(THET/2.0)/COS(THET/2.0))   !PSEUDO
          NPHI(K+1,I) = 64    !Number of Phi divisions per 2PI
          IF((ETA(K,I)+.01).GT.PHIRAP)NPHI(K+1,I) = 32   !FOR LARGE ETA,ROUNDING
C                                        ! ERRORS BEING TAKEN CARE OF.
  300   CONTINUE
  100 CONTINUE
C
      DO 400 I = 1,NZDIV
        IF(I.EQ.1)THEN
          RSR(2,I) = ZSTART+ENDP1   !starting point of module+end plate
        ENDIF
        IF(I.NE.NZDIV)THEN
          RSR(2,I+1) = RSR(2,I+1)+ARG   !Adding argon gap to Z division
C                                       ! after signal board.
          RSR(3,I)   = RSR(2,I+1)
        ELSE  !LAST DIVISION
          RSR(3,I) = ZEND-ENDP2         !END OF MODULE - LAST ENDPLATE
        ENDIF
        IF(I.EQ.8)THEN    !8 IS THE LAST FH. FH STOPS AT STEEL PLATE
          RSR(3,I)=RSR(3,I)-CHPLATE-ARG
        ENDIF
        IF(I.EQ.9)THEN   !9 IS THE CH. CH STARTS AT THE FIRST STEEL PLATE.
          RSR(2,I)= RSR(2,I)-CHPLATE-ARG
        ENDIF
  400 CONTINUE
      DO 500 I =1,NZDIV
        WRITE(20,1)RSR(1,I),RSR(2,I),RSR(3,I),NRAD(I),
     &    (RSR((NRAD(I)+4-K),I),K=1,NRAD(I))
        WRITE(20,2)NRAD(I)+1,(NPHI((NRAD(I)+2-K),I),K=1,NRAD(I)+1)
    1   FORMAT(2X,'''',A4,'''',2F10.3,I5,/,(2X,7F10.3))
    2   FORMAT(2X,I8,/,(2X,7I10))
  500   CONTINUE
C
        DO 600 I = 1,NZDIV  !-VE Z'S
          CALL UHTOC(RSR(1,I),4,CHR,4)
          CHR(2:2)='-'
          CALL UCTOH(CHR,RSR(1,I),4,4)
        WRITE(20,1)RSR(1,I),-RSR(2,I),-RSR(3,I),NRAD(I),
     &    (RSR((NRAD(I)+4-K),I),K=1,NRAD(I))
        WRITE(20,2)NRAD(I)+1,(NPHI((NRAD(I)+2-K),I),K=1,NRAD(I)+1)
  600 CONTINUE
C

  999 RETURN
      END

