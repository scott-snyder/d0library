      SUBROUTINE VTX_HVSLOG(NSEC,SUPPLY,V_TGT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read HV channel to logical assignments from
C-               VTRAKS_RCP.  First part of RCP contains default assignments;
C-               next part has an incremental history, keyed by run number
C-
C-   Inputs  : VTRAKS_RCP
C-   Outputs : NSEC(1:192) -- number of HV sectors connected to supply channel.
C-             SUPPLY(I/O,LAYER,SECTPR) -- Supply INNER/OUTER feed of
C-                  LAYER,SECTOR is connected to
C-   Controls:
C-
C-   Created  10-DEC-1992   Ed Oltman
C-   Updated  12-FEB-1993   Ed Oltman  FIX BUG: INITIALIZE NSEC TO 0 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c I/O:
      INTEGER NSEC(192),SUPPLY(2,0:31,0:2)
      REAL    V_TGT(192)
c Locals:
      CHARACTER*10 INNER,OUTER
      CHARACTER*1  SEVER
      LOGICAL      BYPASS_DBL3_ERROR
      INTEGER LEN,ERR,ICH_I,ICH_O,CHR,INT,NELS,I,RUN,ERROR
      INTEGER  LOGID(2)
      INTEGER ICH_I1,ICH_O1
      REAL    V_TGTI,V_TGTO
      INTEGER LAYER,SECTOR
      EQUIVALENCE (LAYER,LOGID(1)),(SECTOR,LOGID(2))
c Externals:
      INTEGER RUNNO
C----------------------------------------------------------------------
      CALL VZERO(NSEC,192)
      CALL EZPICK('VTRAKS_RCP')
      CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,ERR)
      ERROR = ERR
C
C ****  First, read in standard HV channel-to-sector assignment
C
      CALL EZGETA('HV_CHANNELS',0,0,0,NELS,ERR)
      ERROR = ERROR + ERR
      CHR = 1
      INT = 1
      DO WHILE(INT .LT. NELS)
        CALL EZGETS('HV_CHANNELS',CHR  ,INNER,LEN,ERR)
        ERROR = ERROR + ERR
        INT = INT + (LEN+3)/4
        CALL EZGETS('HV_CHANNELS',CHR+1,OUTER,LEN,ERR)
        ERROR = ERROR + ERR
        INT = INT + (LEN+3)/4
        CALL EZGETA('HV_CHANNELS',INT,INT+1,1,LOGID,ERR)
        ERROR = ERROR + ERR
        READ(INNER(8:10),'(I3)') ICH_I
        READ(OUTER(8:10),'(I3)') ICH_O
        CALL EZGETA('HV_CHANNELS',INT+2,INT+2,1,V_TGT(ICH_I),ERR)
        ERROR = ERROR + ERR
        CALL EZGETA('HV_CHANNELS',INT+3,INT+3,1,V_TGT(ICH_O),ERR)
        ERROR = ERROR + ERR
        SUPPLY(1,SECTOR,LAYER) = ICH_I
        SUPPLY(2,SECTOR,LAYER) = ICH_O
        NSEC(ICH_I) = NSEC(ICH_I) + 1
        NSEC(ICH_O) = NSEC(ICH_O) + 1
        CHR = CHR + 2
        INT = INT + 4
      ENDDO
C
C ****  Then read in incremental history
C
      CALL EZGETA('HV_HISTORY',0,0,0,NELS,ERR)
      ERROR = ERROR + ERR
      IF (NELS .GT. 0) THEN
        INT = 1
        CHR = 1
        DO WHILE(INT .LT. NELS)
          CALL EZGETA('HV_HISTORY',INT,INT,0,RUN,ERR)
          ERROR = ERROR + ERR
          CALL EZGETA('HV_HISTORY',INT+1,INT+2,1,LOGID,ERR)
          ERROR = ERROR + ERR
          CALL EZGETS('HV_HISTORY',CHR  ,INNER,LEN,ERR)
          ERROR = ERROR + ERR
          INT = INT + (LEN+3)/4
          CALL EZGETS('HV_HISTORY',CHR+1,OUTER,LEN,ERR)
          ERROR = ERROR + ERR
          INT = INT + (LEN+3)/4
          CALL EZGETA('HV_HISTORY',INT+3,INT+3,1,V_TGTI,ERR)
          ERROR = ERROR + ERR
          CALL EZGETA('HV_HISTORY',INT+4,INT+4,1,V_TGTO,ERR)
          ERROR = ERROR + ERR
          INT = INT + 5
          CHR = CHR + 2
          IF (RUNNO() .GE. RUN) THEN
            ICH_I1 = SUPPLY(1,SECTOR,LAYER)
            ICH_O1 = SUPPLY(2,SECTOR,LAYER)
            READ(INNER(8:10),'(I3)') ICH_I
            READ(OUTER(8:10),'(I3)') ICH_O
C.. UPDATE INNER WIRES/SUPPLY
            NSEC(ICH_I1) = NSEC(ICH_I1) - 1
            NSEC(ICH_I ) = NSEC(ICH_I ) + 1
            SUPPLY(1,SECTOR,LAYER) = ICH_I
            V_TGT(ICH_I) = V_TGTI
C..THEN OUTER
            NSEC(ICH_O1) = NSEC(ICH_O1) - 1
            NSEC(ICH_O ) = NSEC(ICH_O ) + 1
            SUPPLY(2,SECTOR,LAYER) = ICH_O
            V_TGT(ICH_O) = V_TGTO
          ENDIF
        ENDDO
      ENDIF
      CALL EZRSET
      IF (ERROR .NE. 0) THEN
        SEVER = 'F'
        IF (BYPASS_DBL3_ERROR) SEVER = 'W'
        CALL ERRMSG('VTX HV corr NOT made','VTX_HVSLOG',
     &    'VTRAKS_RCP errors -- cannot apply HV correction',SEVER)
      ENDIF
  999 RETURN
      END
