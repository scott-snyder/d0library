      LOGICAL FUNCTION GLOB_FIX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   1-OCT-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C----------------------------------------------------------------------
      INTEGER   CORR_LEVEL_PNUT
      SAVE      CORR_LEVEL_PNUT
      INTEGER LGLOB, LVCOR, LGLOB1
      INTEGER GZGLOB, GZDTRH, GZCAEH,GZVCOR
      INTEGER NCD, N, IER
      INTEGER STATUS,NCEMFH, LEVEL, I
      REAL    MR_SUME,MR_SUMET,MR_VECET(6),EGLOB(8), ENERGY, R
      REAL    HOT_SUME, HOT_VSUMET, MRX_VECET(6), CHNEG_VECET(6)
      REAL    ENEG,ETNEG
      REAL    ET_THRNEG(2)
      LOGICAL   EZERROR, OK, BUILD_GLOB
      EXTERNAL  EZERROR
      LOGICAL first
      SAVE first
      DATA first / .true. /
      SAVE ENERGY, LEVEL
      DATA  ENERGY  /0.3/ ! Cell Energy Threshold
      DATA  LEVEL   /1/   ! Do main ring correction if micro-blnaking is on
      CHARACTER*4 BANK
C----------------------------------------------------------------------
C
      GLOB_FIX = .TRUE.

      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP('CAFIX_RCP',STATUS)    ! Read in RCP file
        OK = STATUS .EQ. 0
        IF ( .NOT. OK ) THEN
          CALL ERRMSG
     &      ('NO_CAFIX_RCP','GLOB_FIX','No CAFIX.RCP file','W')
        ENDIF
        CALL EZPICK('CAFIX_RCP')
        IF ( EZERROR(STATUS) ) THEN
          CALL ERRMSG
     &      ('NO_CAFIX_RCP_BANK','CAFIX','No CAFIX_RCP bank','F')
        ELSE
C
C ****  Read parameters from RCP file
C
          CALL EZGET('CORR_LEVEL_PNUT',CORR_LEVEL_PNUT, STATUS )
          IF ( STATUS .EQ. 0 ) CALL EZGET('BUILD_GLOB',BUILD_GLOB,
     &      STATUS)
          CALL EZGET('CORR_LEVEL_MAIN_RING',I,STATUS)
          IF ( STATUS .EQ. 0 ) THEN
            LEVEL = I
          ENDIF
          CALL EZGET('CELL_ENERGY_THRESHOLD',R,STATUS)
          IF ( STATUS .EQ. 0 ) THEN
            ENERGY = R
          ENDIF
          CALL EZGETA('NEGATIVE_CELL_ENERGY_THRESH',0,0,0,N,IER)
          CALL EZGETA('NEGATIVE_CELL_ENERGY_THRESH',1,N,1,
     &      ET_THRNEG,IER)
          IF ( STATUS .NE. 0 ) THEN
            CALL ERRMSG('CAFIX RCP error','GLOB_FIX',
     &        'Parameter read failure','F')
            RETURN
          ENDIF
        ENDIF
        CALL EZRSET
      ENDIF
C
C ****  BOOK GLOB bank or EXIT if it exists already
C
      LGLOB = GZGLOB()    ! GET LINK.
      IF ( LGLOB .LE. 0 ) THEN
        CALL BKGLOB(LGLOB)
        IF ( LGLOB .LE. 0 ) THEN
          CALL ERRMSG('BKGLOB_FAILED','GLOBFL',
     &      'Unable to book GLOB bank','W')
          GOTO 999
        ENDIF
      ELSE 
        CALL BKGLOB(LGLOB) ! book another GLOB bank
        LGLOB1=LQ(LGLOB)   ! get old link
        CALL UCOPY(IQ(LGLOB1+2),IQ(LGLOB+2),2)
        CALL UCOPY(Q(LGLOB1+17),Q(LGLOB+17),1)
        CALL UCOPY(IQ(LGLOB1+18),IQ(LGLOB+18),11)
        CALL MZDROP(IXCOM,LGLOB1,' ') ! drop old bank
      ENDIF
C
C ****  DO VCOR BANKS
C
      LVCOR = GZVCOR()
      DO WHILE ( LVCOR .GT. 0 )
        CALL UHTOC(IQ(LVCOR+2),4,BANK,4)
        IF((BANK.EQ.'MRFX').OR.(BANK.EQ.'MRFN')) THEN
          CALL MZDROP(IXCOM,LVCOR,' ')
        ENDIF
        LVCOR = LQ( LVCOR )
      ENDDO
C
C ****  Get Number of CD tracks
C
      LBANK = GZDTRH()
      IF ( LBANK .GT. 0 ) THEN
        NCD = IQ(LBANK+2)
      ELSE
        NCD =-1
      ENDIF
C
      IQ(LGLOB+1) = 6       ! VERSION
      IQ(LGLOB+3) = NCD     ! NUMBER OF CD TRACKS
      IQ(LGLOB+4) = 0

      LCAEH=GZCAEH()
      IF(LCAEH.LE.0)THEN
        CALL ERRMSG
     &    ('CAEH_NOT_FOUND','GLOBFL','GLOB bank not complete','W')
        GOTO 999    ! If bank does NOT exist do nothing
      END IF
C
C Use CORRMR to fill GLOB words
C
      CALL CORRMR(ENERGY,ET_THRNEG,MR_SUME,MR_SUMET,MR_VECET,EGLOB,
     &  NCEMFH,MRX_VECET,CHNEG_VECET,STATUS)
      IQ(LGLOB+4)  = NCEMFH        ! NCELLS CC E>0.3 EM+FH
C
      CALL UCOPY(EGLOB(1),Q(LGLOB+5),8) ! VARIOUS E AND ET WORDS
      Q(LGLOB+13)  = MR_SUME       ! TOT ENERGY ALONG MR
      Q(LGLOB+14)  = MR_SUMET      ! TOT ET ALONG MR
C
      CALL CALC_HOT_ET(HOT_SUME,HOT_VSUMET)
      Q(LGLOB+15)  = HOT_SUME           ! TOT ENERGY HOT CELLS
      Q(LGLOB+16)  = HOT_VSUMET           ! TOT VEC ET HOT
C
      CALL ENEG_FROM_CAEQ(ENEG,ETNEG)
      Q(LGLOB+29)  = ENEG      ! negative energy
      Q(LGLOB+30)  = ETNEG      ! negative ET 

      Q(LGLOB+31)  = CHNEG_VECET(1)
      Q(LGLOB+32)  = CHNEG_VECET(2)
      Q(LGLOB+33)  = CHNEG_VECET(3)
C
C ****  Build correction vector
C
      IF     ( (LEVEL .EQ. 1) .OR. (LEVEL .EQ. 2) ) THEN
C
C ****  Do only if MICRO-BLANKING is TRUE
C
        IF ( (STATUS .EQ. 1) .OR.
     &       (STATUS .EQ.-2) ) THEN
          IF (LQ(LGLOB-2).EQ.0) THEN
            CALL VCORFL(0,'MRFX',6,MR_VECET,LVCOR)
            LQ(LGLOB-2)  = LVCOR ! Reference link to VCOR
          ENDIF
          CALL VCORFL(0,'MRFN',6,MRX_VECET,LVCOR)
          LQ(LGLOB-3)  = LVCOR ! Reference link to VCOR
        ELSE
          IF (LEVEL .EQ. 2) THEN
            CALL VCORFL(0,'MRFN',6,MRX_VECET,LVCOR)
            LQ(LGLOB-3)  = LVCOR ! Reference link to VCOR
          ENDIF
        ENDIF
C
      ELSEIF ( LEVEL .EQ. 3 ) THEN
C
C ****  Do ALWAYS
C
        IF (LQ(LGLOB-2).EQ.0) THEN
          CALL VCORFL(0,'MRFX',6,MR_VECET,LVCOR)
          LQ(LGLOB-2)  = LVCOR ! Reference link to VCOR
        ENDIF
        CALL VCORFL(0,'MRFN',6,MRX_VECET,LVCOR)
        LQ(LGLOB-3)  = LVCOR ! Reference link to VCOR
      ENDIF
C
  999 RETURN
      END
