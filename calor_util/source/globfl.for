      SUBROUTINE GLOBFL(IFLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank GLOB.
C-
C-   Inputs  : IFLAG  [I]   0 - Initialize, pick constants from CURRENT
C-                              RCP bank:
C-
C-                              CALL EZGET('CORR_LEVEL_MAIN_RING',...)  (1)
C-                              CALL EZGET('CELL_ENERGY_THRESHOLD',..)  (0.3)
C-
C-                          1 - Fill
C-   Outputs : None
C-   Controls: None
C-
C-   Created  15-DEC-1992  Harrison B. Prosper, Ulrich Heintz,
C-                         Andrew Brandt
C-   Updated  14-JAN-1993   Harrison B. Prosper
C-
C-    Add IFLAG
C-   Updated  23-MAR-1994   Meenakshi Narain   fill HOT_SUME, HOT_VSUMET
C-                          and update bank version number to 4
C-   Updated  11-OCT-1994   Serban D. Protopopescu   
C-                          added word 29 and 30, negative energy and ET
C-                          bank version 5
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IFLAG
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C----------------------------------------------------------------------
      INTEGER LGLOB, LVCOR, LGLOB1
      INTEGER GZGLOB, GZDTRH, GZCAEH
      INTEGER NCD, N, IER
      REAL    MR_SUME,MR_SUMET,MR_VECET(6),EGLOB(8), ENERGY, R
      REAL    HOT_SUME, HOT_VSUMET, MRX_VECET(6), CHNEG_VECET(6)
      INTEGER STATUS,NCEMFH, LEVEL, I
      INTEGER IPOL_WAM,IPOL_SAM,IERR,MR_BITS
      INTEGER IWORDS
      INTEGER ANDOR_WORDS(8) !8 long words for all level 1 andor terms
      REAL TIME29,ENEG,ETNEG
      REAL    ET_THRNEG(2)
C----------------------------------------------------------------------
      SAVE ENERGY, LEVEL
      DATA  ENERGY  /0.3/ ! Cell Energy Threshold
      DATA  LEVEL   /1/   ! Do main ring correction if micro-blnaking is on
C----------------------------------------------------------------------
C
C ****  Get constants from current RCP file
C
      IF ( IFLAG .EQ. 0 ) THEN
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
        GOTO 999
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
      ELSE IF (IQ(LGLOB+1).LT.5) THEN
        CALL BKGLOB(LGLOB) ! book another GLOB bank
        LGLOB1=LQ(LGLOB)   ! get old link
        IQ(LGLOB+1)=5      ! upgrade to version 5
        CALL UCOPY(IQ(LGLOB1+2),IQ(LGLOB+2),3)
        CALL UCOPY(Q(LGLOB1+5),Q(LGLOB+5),13)
        CALL UCOPY(IQ(LGLOB1+18),IQ(LGLOB+18),11)
        CALL ENEG_FROM_CAEQ(ENEG,ETNEG)
        Q(LGLOB+29)  = ENEG      ! negative energy
        Q(LGLOB+30)  = ETNEG      ! negative ET 
        CALL MZDROP(IXCOM,LGLOB1,' ') ! drop old bank
        GOTO 999
      ELSE
        GOTO 999    ! GLOB bank exists
      ENDIF
C
C ****  FILL BANK
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
      IQ(LGLOB+2) = 0       ! QUALITY
      IQ(LGLOB+3) = NCD     ! NUMBER OF CD TRACKS
      IQ(LGLOB+4) = 0
C fill in main ring information
      CALL MAIN_RING(TIME29,MR_BITS,IERR)
      IF(IERR.EQ.0) THEN
        Q(LGLOB+17) = TIME29
        IQ(LGLOB+18) = MR_BITS
      ENDIF
C Copy over all L1 and/or terms
      CALL ANDOR_TERM_PACK(ANDOR_WORDS,IERR)
      IF (IERR.EQ.0) THEN
        DO IWORDS = 1, 8
          IQ(LGLOB+20+IWORDS) = ANDOR_WORDS(IWORDS)
        ENDDO
      ELSE
        CALL ERRMSG('AND/OR_TERMS_NOT_COPIED','GLOBFL',
     &    'No and/or information in GLOB','W')
      ENDIF
C fill in magnet polarity information (DBL3 access can trigger garbage
C          collection)
      CALL MAG_POLARITY(IPOL_WAM,IPOL_SAM,IERR)
      LGLOB = GZGLOB()
      IF(.NOT.BTEST(IERR,0)) THEN
        IQ(LGLOB+19) = IPOL_WAM
      ENDIF
      IF(.NOT.BTEST(IERR,1)) THEN
        IQ(LGLOB+20) = IPOL_SAM
      ENDIF
C
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
C
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
  999 RETURN
      END
