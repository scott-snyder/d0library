      LOGICAL FUNCTION SCONST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the SAMUS database parameters
C-                         (adapted from MCONST)
C-   the calling routine sets the RCP bank so usable by either
C-   D0USER or EXAMINE. USE_DBL controls what occurs.
C-   USE_DBL  = 0   read in SSAM bank from GEOMETRY_FILE
C-              1   read in SSAM bank and also read electronic constants
C-                  from the database (i.e. geoemtry from file still)
C-              2   read in everything from the database
C-        10,11,12  as 0,1,2 except write out complet SSAM structure
C-                  to a file
C-     note that USE_DBL=0 can read in either simply yhe geometry or a
C-     complete SSAM structure depending upon which file is pointed to
C-     in the SSAM structure.
C-   Created 23-DEC-1991   Daria Zieminska
C-   DH 4/92 CLOSE OUTPUT FILE
C-   Updated  29-DEC-1992   Alexander Efimov   Add reading data from
C-                          SAMUS STP file and SAMUS DBL3 data base.
C-   Updated  11-MAR-1993   Alexander Kozelov  Remove bug in STP reading
C-   Repaired 13-MAR-1993   Diehl: IF usedbl=0 need a way to set OK .true.
C-   Repaired 17-MAR-1993   Hedin if use_dbl=0, allow OK=true return
C-   Updated  17-JAN-1994   Yuri Gutnikov - reading from SAMUS STP files
C-                          The possibility of reading from different
C-                          STP files has been added.
C-   Updated  11-MAR-1994   Alexander Efimov - change INTMSG to ERRMSG
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSSAM.LINK'
      INCLUDE 'D0$LINKS:IZMUD1.LINK/LIST'  ! link pointer to MUD1.
      CHARACTER*(*) GEOFIL
      PARAMETER ( GEOFIL  = 'GEOMETRY_FILE' )
      INTEGER MR_MAX
      PARAMETER ( MR_MAX=100 )
      INTEGER LENGTH,USE_DBL,I_DBL,IUNIT,RUN,RUN_OLD,MAX_RUN
      INTEGER LSSAM,GZSSAM,IERR,I,NPAR,J
      INTEGER LMUD1,GZMUD1,NDATA,L
      INTEGER RUNNO
      INTEGER RUN_NO(MR_MAX),COUNT,COUNT_OLD
      CHARACTER*64 FILNAM
      CHARACTER*32 MESSID, CALLER
      CHARACTER*80 MESSAG
      CHARACTER*80 MSG
      LOGICAL OK,FIRST,F2,OK2,DROP,FIRST1
      SAVE RUN,FIRST,FIRST1,F2,COUNT
      DATA FIRST1/.TRUE./
      DATA FIRST,F2 /.TRUE.,.TRUE./
C
      SCONST= .FALSE.
      OK = .TRUE.
      CALLER = 'SCONST'
C
      IF (FIRST) THEN
        RUN_OLD=RUNNO()
        DROP= .FALSE.
        COUNT_OLD=0
      ELSE
        RUN_OLD=RUN
        DROP=.TRUE.
        COUNT_OLD=COUNT
      END IF
      RUN=RUNNO()
      CALL EZPICK('SAMRECO_RCP')
      CALL EZGET('USE_DBL',USE_DBL,IERR)
      IF(IERR.NE.0) THEN
        MESSID = 'SCONST: Cant read USE_DBL'
        WRITE (MESSAG, '(''EZGET: '',I7)') IERR
        CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
        GOTO 999
      ENDIF
      NPAR=0
C
C ****  Read array with border numbers of the runs
C
      CALL EZGET('MAX_RUN',MAX_RUN,IERR)        ! Length of array
      CALL EZGET('NO_RU',RUN_NO,IERR)
      IF (IQ(LHEAD+1).GE.1000) THEN   ! MC data
        LMUD1=GZMUD1()
        NDATA=IQ(LMUD1-1)
        L=LMUD1
        COUNT=IBITS(IQ(L+6),16,8)  ! Read version of STP file
        I_DBL=0
        USE_DBL=0
      ELSE
        I_DBL=MOD(USE_DBL,10)
        IF(I_DBL.EQ.0.OR.I_DBL.EQ.1.OR.I_DBL.EQ.2) THEN
C
C ****  Find number of the STP file
C
          COUNT=MAX_RUN
          DO I=1,MAX_RUN-1
            IF((RUN.GE.RUN_NO(I)).AND.(RUN.LT.RUN_NO(I+1))) THEN
              COUNT=I
              GO TO 110
            END IF
          ENDDO
        END IF
      END IF
  110 IF(COUNT.NE.COUNT_OLD) THEN       ! Necessary to read new STP?
        IF(FIRST) THEN
          DROP=.FALSE.
        ELSE
          DROP=.TRUE.
        END IF
      ELSE
        DROP=.FALSE.
      END IF
      CALL EZGETS ('GEOMETRY_FILE',COUNT,FILNAM,LENGTH,IERR)
      IF (IERR.NE.0) THEN
        MESSID = 'SCONST: GEOM init fails'
        WRITE (MESSAG, '(''File: '',(A))') FILNAM(1:LENGTH)
        CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
        OK = .FALSE.
        GO TO 999
      ENDIF
      IF (FIRST1) THEN
        MESSID = 'SCONST: read geometry'
        WRITE (MESSAG, '(''File: '',(A))') FILNAM(1:LENGTH)
        CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
        CALL SAISTP ( FILNAM(1:LENGTH), IERR)
        IF(IERR.EQ.0) THEN
          MESSID = 'SCONST: geometry init finished OK'
          MESSAG = ' '
          CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
        ELSE
          MESSID = 'SCONST: geometry init fails.'
          WRITE (MESSAG, '(''File: '',(A))') FILNAM(1:LENGTH)
          CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
          OK = .FALSE.
          GO TO 999
        ENDIF
      ENDIF
      FIRST1 = .FALSE.
      IF (DROP) THEN
        MESSID = 'SCONST: read geometry'
        WRITE (MESSAG, '(''File: '',(A))') FILNAM(1:LENGTH)
        CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
        CALL SAISTP (FILNAM(1:LENGTH), IERR)
        IF (IERR .EQ. 0) THEN
          MESSID = 'SCONST: geometry init finished OK'
          MESSAG = ' '
          CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
        ELSE
          MESSID = 'SCONST: geometry init fails.'
          WRITE (MESSAG, '(''File: '',(A))') FILNAM(1:LENGTH)
          CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
          OK = .FALSE.
          GOTO 999
        ENDIF
      ENDIF
      FIRST = .FALSE.
C
C ****  
C
      IF(I_DBL .EQ. 1) THEN
        CALL SAINIT (OK)
C        CALL MRDBCN (OK)
      ENDIF
      IF(I_DBL .EQ. 2) THEN
        CALL SAINIT (OK)
      ENDIF
CCCC    SEE IF WE WANT TO SAVE THE SSAM STRUCTURE
      IF(F2) THEN
        F2=.FALSE.
        IF(USE_DBL.GE.10) THEN
          CALL GTUNIT(789,IUNIT,IERR)
          IF(IERR.EQ.0) THEN
            CALL D0OPEN(IUNIT,'SSAM_SAVE.OUT','OU',OK2)
            CALL FZFILE(IUNIT,0,'O')
            LSSAM=GZSSAM('STPC')
            IF(LSSAM.NE.0) THEN
              CALL FZOUT(IUNIT,IDVSTP,LSSAM,1,' ',1,0,0)
              MESSID = 'SCONST: SSAM_SAVE.OUT written to disk'
              MESSAG = ' '
              CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
            ENDIF
            CALL FZENDO(IUNIT,'T')
            CLOSE(UNIT=IUNIT)
            CALL RLUNIT(789,IUNIT,IERR)
          ELSE
            MESSID = 'SCONST: GTUNIT error'
            MESSAG = ' '
            CALL ERRMSG (MESSID, CALLER, MESSAG, 'W')
          ENDIF
        ENDIF
      ENDIF
  999 SCONST = OK
      CALL EZRSET
      RETURN
      END
