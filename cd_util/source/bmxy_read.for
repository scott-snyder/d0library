      LOGICAL FUNCTION BMXY_Read(Run)
      IMPLICIT NONE
      INTEGER Run
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in the BMXY bank from the database
C-      (XY beam-position information).  This routine is called by XYRCP,
C-      which is called by VERTEX.
C-
C-   Inputs  : Run, the run number for which data are requested
C-
C-   Returns : .TRUE. if read was successful, .FALSE. otherwise
C-
C-   Created  11-APR-1994   Justin R. Bendich
C-   Updated   6-JUL-1995   Liang-ping Chen 
C-      IF((Run .GE. LastBV) .AND. (Run .LE. LastEV)), 
C-      set direction=0 and copy the current BMXY, rather than GOTO 9000
C-      This is a fix for samples which have early
C-      runs (which use VXY_BEAM1 to access the ASCII file) inserted
C-      between two events in the same validity region.
C-   Updated  25-Jan-1996   Herbert Greenlee
C-      Added machine block to use C i/o in UNIX (for ibm compatibility).
C-
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C----------------------------------------------------------------------
C-
C-   COMMON blocks from the bowels of ZEBRA.  This way we don't have to
C-   know whether INZCOM or INZSTP was called first.
C-
C----------------------------------------------------------------------
      INTEGER Fnc(4), ZLQ(100), ZIQ(92)
      COMMON/ZEBQ/ Fnc, ZLQ
      EQUIVALENCE (ZLQ(9), ZIQ(1))
C
      INTEGER Filler(17), Store1Offset
      COMMON/MZCA/ Filler, Store1Offset
C----------------------------------------------------------------------
C-
C-   Some of Lars' code follows.  This should really be in a .INC file,
C-   but it wasn't when i wrote the routine.
C-
C..........................SOOORYYY....................................
      INTEGER MAXDBTOPD
      PARAMETER (MAXDBTOPD=15)
      INTEGER MAXDBFILES
      PARAMETER (MAXDBFILES=100)
      INTEGER NDBTOPD
      CHARACTER*4 DBTOPD(MAXDBTOPD)
      INTEGER NDBFILES(MAXDBTOPD)
      CHARACTER*80 DBFILPOI(MAXDBTOPD)
      INTEGER DBFILOPN(MAXDBTOPD)
      CHARACTER*80 DBFILES(MAXDBFILES,MAXDBTOPD)
      INTEGER DBTIM(2,MAXDBFILES,MAXDBTOPD)
      INTEGER DBRUN(2,MAXDBFILES,MAXDBTOPD)
      COMMON/DBMFILES/
     &  NDBTOPD,DBTOPD,NDBFILES,DBFILOPN,DBTIM,DBRUN,DBFILES,DBFILPOI
C......................................................................
      CHARACTER*30 Path
      CHARACTER*24 XYPath
      CHARACTER*34 Message
      CHARACTER*3 Trimmed
C&IF VAXVMS
C&ELSE
C&      CHARACTER*8 TOPDIR
C&      CHARACTER*255 LOCAL_FILENAME
C&      INTEGER CONTEXT
C&      LOGICAL LIB$FIND_FILE
C&ENDIF
      INTEGER LastBV, LastEV, DBMU_FNO, Number, Lgth, TopID, LUnit,
     &        Err, RecLgth, NKeys, NWords, BasePtr, D3UNIT,
     &        KeyOffset, LastKey, Direction, Ptr
      LOGICAL First, OK, Going, OneLeft, InRange, DBMU_FILE_INIT
      DATA LastBV/999999999/, LastEV/0/, LUnit/-998/, First/.TRUE./,
     &     RecLgth/0/, LastKey/-1/
      DATA Path/'//RSM/RDET/VTX/BMXY           '/, Lgth/19/
   10 FORMAT('Error opening database, run ',I6)
   20 FORMAT('IQUEST = ',I5)
   30 FORMAT('Couldn''t find run ',I6)
C
      BMXY_Read = .TRUE.
      IF(First) THEN
        IF(.NOT.
     & DBMU_FILE_INIT('RSM','DBL3$RSM:RSM_DBFILES.DAT',Number,' '))
     & CALL ERRMSG('DBMU_FILE_INIT', 'BMXY_Read', 'failed', 'W')
        First = .FALSE.
        TopID = DBMU_FNO('RSM', Trimmed)
        LUnit = D3UNIT(' ', 'RSM')
        CALL EZLOC('VERTEX_RCP', Number)
        OK = Number .GT. 0
        IF(.NOT. OK) THEN
          CALL INRCP('VERTEX_RCP', Err)
          OK = Err .EQ. 0
          IF(.NOT. OK)
     &      CALL ERRMSG('INRCP', 'BMXY_Read', 'VERTEX_RCP', 'W')
        ENDIF
        IF(OK) THEN
          CALL EZPICK('VERTEX_RCP')
          CALL EZGETS('XYPATH', 1, XYPath, Number, Err)
          IF(Err .EQ. 0) THEN
            Path(7:) = XYPath
            Lgth = Number + 6
          ELSE
            CALL ERRMSG('VERTEX_RCP', 'BMXY_Read', 'XYPATH', 'W')
          ENDIF
          CALL EZRSET
        ENDIF
      ENDIF
      Number = DBFILOPN(TopID)
      IF(Number .GT. 0) THEN
        Going = (Run .LT. DBTIM(1, Number, TopID)) .OR.
     &          (Run .GT. DBTIM(2, Number, TopID))
      ELSE
        Going = .TRUE.
      ENDIF
      IF(Going) THEN
        Number = 0
        DO 100 WHILE(Going)
          Number = Number + 1
          Going = Number .LE. NDBFILES(TopID)
          IF(Going) Going = (Run .LT. DBTIM(1, Number, TopID)) .OR.
     &                      (Run .GT. DBTIM(2, Number, TopID))
  100   CONTINUE
        IF(Number .GT. NDBFILES(TopID)) THEN
          CALL ERRMSG(' ', 'BMXY_Read', 'No valid database found', 'W')
          BMXY_Read = .FALSE.
          GOTO 9000
        ENDIF
        IF(LUnit .GT. 0) THEN
C&IF VAXVMS
          CALL RZEND('RSM')
          CLOSE(LUnit)
C&ELSE
C&          CALL RZCLOS('RSM', ' ')
C&ENDIF
        ELSE
          LUnit = D3UNIT('N', 'RSM')
          IF(LUnit .LE. 0) THEN
            CALL ERRMSG('D3UNIT', 'BMXY_Read',
     & 'Can''t get unit on which to open database', 'W')
            BMXY_Read = .FALSE.
            GOTO 9000
          ENDIF
        ENDIF
C&IF VAXVMS
        CALL D0RZOPEN(LUnit, DBFILES(Number, TopID), 'ISU', RecLgth, OK)
        IF(.NOT. OK) THEN
C&ELSE
C&        context = 0
C&        ok = lib$find_file(DBFILES(Number, TopID), local_filename,
C&     &    context)
C&        call lib$find_file_end(context)
C&        RecLgth = 1024
C&        if(ok)CALL RZOPEN(LUnit, topdir, local_filename, 'CSP', RecLgth,
C&     &    Err)
C&        IF(.not.ok.or.Err.ne.0) THEN
C&ENDIF
          WRITE(Message,10) Run
C&IF VAXVMS
          CALL ERRMSG('D0RZOPEN', 'BMXY_Read', Message, 'W')
C&ELSE
C&          CALL ERRMSG('RZOPEN', 'BMXY_Read', Message, 'W')
C&ENDIF
          BMXY_Read = .FALSE.
          GOTO 9000
        ENDIF
C&IF VAXVMS
        CALL RZFILE(LUnit, 'RSM', ' ')
C&ELSE
C&        CALL RZFILE(LUnit, 'RSM', 'CS')
C&ENDIF
        IF(IQUEST(1) .NE. 0) THEN
          Write(Message(1:14),20) IQUEST(1)
          CALL ERRMSG('RZFILE', 'BMXY_Read', Message(1:14), 'W')
          BMXY_Read = .FALSE.
          GOTO 9000
        ENDIF
        DBFILOPN(TopID) = Number
      ENDIF
      CALL RZCDIR(Path(1:Lgth), ' ')
      IF(IQUEST(1) .NE. 0) THEN
        Write(Message(1:14),20) IQUEST(1)
        CALL ERRMSG('RZCDIR fails', 'BMXY_Read', Message(1:14), 'W')
        BMXY_Read = .FALSE.
        GOTO 9000
      ENDIF
      NKeys = IQUEST(7)
      NWords = IQUEST(8) + 1
      BasePtr = IQUEST(11)
      KeyOffset = IQUEST(13)
      IF(Run .GT. LastEV) THEN
        Direction = 1
      ELSE
        Direction = -1
      ENDIF
C  next line is in effect 7/95, see comment above.       
      IF((Run .GE. LastBV) .AND. (Run .LE. LastEV)) Direction = 0
      OneLeft = .TRUE.
      Going = .TRUE.
      DO 500 WHILE(Going)
        Number = LastKey + Direction
        InRange = (Number .LT. NKeys) .AND. (Number .GE. 0)
        DO 300 WHILE(Going .AND. InRange)
          Ptr = Store1Offset + BasePtr + KeyOffset + NWords * Number
          Going = (Run .LT. ZIQ(Ptr + 3)) .OR. (Run .GT. ZIQ(Ptr + 4))
          IF(Going) THEN
            Number = Number + Direction
            InRange = (Number .LT. NKeys) .AND. (Number .GE. 0)
          ENDIF
  300   CONTINUE
        Going = OneLeft .AND. Going
        Direction = -Direction
        OneLeft = .FALSE.
  500 CONTINUE
      IF(.NOT. InRange) THEN
        WRITE(Message(1:24),30) Run
        CALL ERRMSG('Not in DB', 'BMXY_Read', Message(1:24), 'W')
        BMXY_Read = .FALSE.
        GOTO 9000
      ENDIF
      LastKey = Number
      LastBV = ZIQ(Ptr + 3)
      LastEV = ZIQ(Ptr + 4)
      IF(LBMXY .GT. 0) THEN
        CALL MZDROP(IXSTP, LBMXY, ' ')
        LBMXY = 0
      ENDIF
      CALL RZIN(IDVSTP, LBMXY, 1, Number + 1, 1000, 'S')
      IF(IQUEST(1) .NE. 0) THEN
        Write(Message(1:14),20) IQUEST(1)
        CALL ERRMSG('RZIN fails', 'BMXY_Read', Message(1:14), 'W')
        BMXY_Read = .FALSE.
        GOTO 9000
      ENDIF
      IF(LBMXY .LE. 0) BMXY_Read = .FALSE.
 9000 END
