      SUBROUTINE B1M_SETUP_JETS(NJTS,ETAJET,PHIJET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select good jets from the event
C-                         return .FALSE. if kist is empty
C-   Inputs  : none
C-   Outputs : vectors ETAJET,PHIJET for the NJTS accepted jets
C-   Controls: none
C-
C-   Created  16-SEP-1995   Arthur Maciel
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'

      LOGICAL FIRST,DECENT_JET
      INTEGER GZJETS,LJETS,MXJ,NJTS,IER
      PARAMETER (MXJ=32)
      REAL TEMPLATE(5,5)
      REAL JETCONE,ETAJET(MXJ),PHIJET(MXJ)
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./
      DATA TEMPLATE/
     &  1.,6.,1.0,0.,0.,      ! CONE R=1.0
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  2.,7.,2.0,8.,2.,      ! NN 2x2
     &  2.,7.,1.0,8.,1. /     ! NN 1x1
C----------------------------------------------------------------------
C-
      IF (FIRST) THEN
        FIRST=.FALSE.
        JETCONE = 0.7
        PRINT *,' '
        PRINT *,'***** B1M_SETUP_JETS  HAS BEEN ACCESSED *****'
        PRINT *,'***** choice for jet conesize: ',JETCONE
        PRINT *,' '
      ENDIF

C-- Init.
      NJTS = 0
      CALL VZERO(ETAJET,MXJ)
      CALL VZERO(PHIJET,MXJ)

C-- define conditions for JETS.
      IF(JETCONE.EQ.1.0) CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
      IF(JETCONE.EQ.0.7) CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
      IF(JETCONE.EQ.0.5) CALL SET_CAPH('CONE_JET',TEMPLATE(1,3),IER)
      IF(JETCONE.EQ.0.3) CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
      IF(JETCONE.EQ.0.1) CALL SET_CAPH('NN_JET',TEMPLATE(1,5),IER)

      IF (IER.NE.0) THEN
        CALL ERRMSG('Jet Unpack Error','B1M_SETUP_JETS',
     &    '...Returning Without Jet Entries...','W')
        GOTO 999
      ENDIF
C-
C- loop over jets
C-
      LJETS=GZJETS()
      DO WHILE (LJETS.GT.0)
C-
C- check for jet quality
C-
        DECENT_JET=.FALSE.

        IF (Q(LJETS+6).GT.10.0)      THEN          ! min. Et
          IF(ABS(Q(LJETS+9)).LT.4.0)  THEN         ! max. Eta
            IF (Q(LJETS+14).GT.0.001)  THEN        ! Em-frac-min
              IF (Q(LJETS+14).LT.0.99)  THEN       ! Em-frac-max
                IF (Q(LJETS+18).LT.0.50) THEN      ! Hd-frac-max
                  IF(Q(LJETS+19).LT.20.0) THEN     ! Hot Ratio
                    DECENT_JET=.TRUE.
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        IF (.NOT. DECENT_JET) GOTO 10              ! go to next jet

        NJTS     =  NJTS+1
        IF (NJTS.GT.MXJ) THEN
          CALL ERRMSG('Interrupt Jet Search','B1M_SETUP_JETS',
     &      '...Too many jets to handle...','W')
          NJTS=NJTS-1
          GOTO 999
        ENDIF

C- Get Eta and Phi entries for this good Jet

        PHIJET(NJTS)=  Q(LJETS+8)
        ETAJET(NJTS)=  Q(LJETS+9)

   10   LJETS=LQ(LJETS)  ! pointer to next jet

      ENDDO                                  ! (LJETS.GT.0)
  999 CALL RESET_CAPH
      RETURN
      END
