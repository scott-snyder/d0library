      LOGICAL FUNCTION MCONST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the muon database parameters
C-   the calling routine sets the RCP bank so usable by either
C    D0USER or EXAMINE. USE_DBL controls what occurs.
C    USE_DBL  = 0   read in SMUO bank from SMUO_FILE
C               1   read in SMUO bank and also read electronic constants
C                   from the database (i.e. geoemtry from file still)
C               2   read in everything from the database
C               3   read elec calib from SMUO and read geom from database
C         10,11,12  as 0,1,2 except write out complet SMUO structure
C                   to a file
C      note that USE_DBL=0 can read in either simply yhe geometry or a
C      complete SMUO structure depending upon which file is pointed to
C      in the SMUO structure.
C      D. Hedin Dec. 1992
C      Updated  Add geometry database part, Atsushi 1-10-92
C      DH leave field map commented out as looks like it isn't needed
C      AT add USE_DBL=3 07-APR-92
C      AT modify for 2 MC STP version selection without DBL3 07-APR-92
C      DH 4/92 CLOSE OUTPUT FILE
C      MODIFIED  11-AUG-1993      S. ABACHI   Automatic stp file finding
C      AT Jan-10-94 STP file type geometry database
C                   Calib database is not called by MC data
C-   Updated  11-APR-1994   K. Wyatt Merritt  Fix IF FIRST problem with
C-                             too many INTMSGs from Monte Carlo
C-   Updated  29-AUG-1995   Andrei Mayorov  LMUD1=0 process as real data
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*(*) GEOFIL
      PARAMETER ( GEOFIL  = 'SMUO_FILE' )
      CHARACTER*(*) GEOFIL2
      PARAMETER ( GEOFIL2  = 'SMUO_FILE_OLD' )
      INTEGER IER,LENGTH,USE_DBL,I_DBL,IUNIT
      INTEGER GZSMUO,IERR
      CHARACTER*80 FILNAM
      LOGICAL OK,FIRST,F2,F3,OK2
      INTEGER OLD_MC
      INTEGER LMUD1,GZMUD1,FLG
      INTEGER FLAG_MUD1
      CHARACTER*80 MSG
      LOGICAL MC_FLAG
      DATA FIRST,F2,F3 /.TRUE.,.TRUE.,.TRUE./
C
      MCONST= .FALSE.
      OK = .TRUE.
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        LMUD1=GZMUD1(0)
        FLG = FLAG_MUD1(0)
        IF( LMUD1 .EQ. 0 ) THEN
          IF(F3) THEN
            F3 = .FALSE.
            CALL INTMSG(' MCONST:LMUD1=0 ')
          ENDIF
          FLG = FLAG_MUD1(1)
          MCONST = .TRUE.
          OLD_MC = 0
        ELSE
          IF(IQ(LMUD1+4) .EQ. 1) THEN
            OLD_MC=1      ! old MONTE CARLO
          ELSE
            OLD_MC = 0
          ENDIF
        ENDIF
        CALL EZGET('USE_DBL',USE_DBL,IER)
        I_DBL=MOD(USE_DBL,10)
        IF(I_DBL.EQ.0.OR.I_DBL.EQ.1.OR.I_DBL.EQ.3) THEN  ! GET SMUO STRUCTURE
          IF ( OLD_MC.EQ.1 ) THEN
            CALL EZGETS (GEOFIL2,1,FILNAM,LENGTH,IER)
            CALL INTMSG('Use old muon geometry file')
          ELSE
            CALL EZGETS (GEOFIL,1,FILNAM,LENGTH,IER)
          ENDIF
          IF(LENGTH .EQ. 0) THEN
            CALL D0_ABORT( 'Invalid STP file name at MURECO_RCP' )
          END IF
          MSG = ' Muon STP file '//FILNAM(1:LENGTH)//' was used.'
          CALL INTMSG(MSG)
CC
          IF(IER.EQ.0) THEN
            CALL MRZCON('ALL',FILNAM(1:LENGTH),0,OK2)
            IF(OK2) THEN
              CALL INTMSG(' MCONST SMUO init OK : FILE')
            ELSE
              CALL INTMSG(' MCONST SMUO init fails: FILE')
              GO TO 999
            ENDIF
          ELSE
            CALL INTMSG(' MCONST SMUO init fails: EZGET')
            GO TO 999
          ENDIF
        END IF
      ENDIF
CA
      IF ( IQ(LHEAD+1).LT.1000) THEN
        MC_FLAG = .FALSE.
      ELSE
        MC_FLAG = .TRUE.
      END IF
C
      OK  = .TRUE.
      OK2 = .TRUE.
      IF(I_DBL .EQ. 1) THEN
        IF ( .NOT.MC_FLAG ) CALL MRDBCN (OK)
      ELSE IF(I_DBL .EQ. 2) THEN
        IF ( .NOT.MC_FLAG ) CALL MRDBCN (OK)
        CALL MRDBGE(OK2) ! Read geometry from database
      ELSE IF ( I_DBL.EQ.3 ) THEN
        CALL MRDBGE(OK2) ! Read geometry from database
      ENDIF
      MCONST = OK.AND.OK2
C
C-   GET FIELD MAP
C
C      CALL EZGET('USE_FIELD_MAP',USE_FIELD_MAP,IER)
C      IF(USE_FIELD_MAP) THEN
C        CALL EZGETS('FIELD_MAP',1,FILNAM,LENGTH,IER)
C        CALL MRZCON('MMAP',FILNAM(1:LENGTH),0,OK2)
C        IF(OK2) THEN
C          CALL INTMSG(' Field Map Read In')
C        ELSE
C          CALL INTMSG(' Field Map Read in Fails')
C        ENDIF
C      ENDIF
CCCC    SEE IF WE WANT TO SAVE THE SMUO STRUCTURE
      IF(F2) THEN
        F2=.FALSE.
        IF(USE_DBL.GE.10) THEN
          CALL GTUNIT(789,IUNIT,IERR)
          IF(IERR.EQ.0) THEN
            CALL D0OPEN(IUNIT,'SMUO_SAVE.OUT','OU',OK)
            CALL FZFILE(IUNIT,0,'O')
            LSMUO=GZSMUO('STPC')
            IF(LSMUO.NE.0) THEN
              CALL FZOUT(IUNIT,IDVSTP,LSMUO,1,' ',1,0,0)
              CALL INTMSG(' SMUO_SAVE.OUT written to disk')
            ENDIF
            CALL FZENDO(IUNIT,'T')
            CLOSE(UNIT=IUNIT)
            CALL RLUNIT(789,IUNIT,IERR)
          ELSE
            CALL INTMSG(' MCONST:GTUNIT error ')
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
