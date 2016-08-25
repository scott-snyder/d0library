      SUBROUTINE CRHITS(PHIMIN,PHIMAX)
C----------------------------------------------------------------------
C-
C  Main routine for hitfinding in CDC sectors along a road.
C  (modified CDHITS)
C  
C  Input: 
C        PHIMIN = minimum phi of the road
C        PHIMAX = maximum phi of the road
C
C  Daria Zieminska Feb. 1989                             
C-   Updated  30-JUN-1989   Qizhong Li-Demarteau   use SRCP 
C-   Updated  11-DEC-1989   Qizhong Li-Demarteau   rewrite 
C-   Updated  07-NOV-1990   Qizhong Li-Demarteau   check if hitfinding 
C-                                       has been done for this sector
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added a choice for the
C-                                                compressed hits bank
C-   Updated  28-AUG-1991   Qizhong Li-Demarteau  added # of hits on sense
C-                                             wire in CDCH and DLYR banks 
C-   Updated  17-MAY-1995   Qizhong Li-Demarteau  added handling for re-recoing
C-                                             from DHIT bank 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC/LIST'
      INCLUDE 'D0$INC:CDPARA.INC/LIST'
      INCLUDE 'D0$INC:DDEBUG.INC/LIST'
C
      REAL    PHIMIN, PHIMAX
      INTEGER NHITS
      INTEGER NHITLA, NHITCD, LAYER, SECTOR
      INTEGER KPDSEC, ION, LOC, STAT
      INTEGER GZDLYR, GZDSEC, GZDCDA, IBSET
      INTEGER IER, NHITSW, NHITLS, NHITS1
      INTEGER RUN, ID, RUNSAV, IDSAV
      INTEGER CMPRSS, PLDSEC, GZCDCH
      PARAMETER( CMPRSS = 12 )
      INTEGER MAXLAY, MAXSEC, ERR, IPATH
      CHARACTER*4 DPATH
      EQUIVALENCE (IPATH, DPATH)
      LOGICAL ON(0:3,0:31), FIRST, DONE
      LOGICAL EZERROR, BUILD_DHIT, BTEST
      LOGICAL REDOCDC, USEDHIT
C
      SAVE FIRST, ION, RUNSAV, IDSAV
      DATA FIRST/.TRUE./
      DATA    ION/1/
      DATA RUNSAV/-1/, IDSAV/-1/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CRHITS',
     &      'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('MAXLAY',MAXLAY,ERR)
        IF (ERR .NE. 0) MAXLAY = 3
        CALL EZGET('MAXSEC',MAXSEC,ERR)
        IF (ERR .NE. 0) MAXSEC = 31
        CALL EZGET('DPATH',IPATH,ERR)
        CALL EZGET('BUILD_DHIT',BUILD_DHIT,ERR)
        CALL EZGET('REDOCDC',REDOCDC,ERR)
        CALL EZGET('USEDHIT',USEDHIT,ERR)
        CALL EZRSET
      ENDIF
C
      NHITCD = 0
      NHITSW = 0
C
C     Find sectors on the road
C
      CALL FLDSEC(PHIMIN,PHIMAX,ON)
C
C     Initialise the bank structure
C
      CALL PATHST(DPATH)
      CALL BKCDCH
      IF (DPATH .NE. 'RECO') THEN
C
C     define the links from the PATH
C
        DO 321 LAYER = 0, MAXLAY
          LDLYR(LAYER) = GZDLYR(LAYER)
          DO 322 SECTOR = 0, MAXSEC
            IF ((ON(LAYER,SECTOR).EQV..FALSE.)) GO TO 322
            LDSEC(SECTOR, LAYER) = GZDSEC(SECTOR,LAYER)
            LDCDA(SECTOR, LAYER) = GZDCDA(SECTOR,LAYER)
            LOC = LDSEC(SECTOR,LAYER)
            IF (LOC .NE. 0) THEN
              STAT = IQ(LOC)
              IQ(LOC) = IBSET(STAT,ION)
            ENDIF
            IF (DBGFLG .AND. LVLDBG(4).GT.0) THEN
              KPDSEC = LDSEC(SECTOR,LAYER)
              CALL PRDSEC(LUNDBG,KPDSEC,LVLDBG(4),'SINGLE',LVLDBG(4))
            ENDIF
  322     CONTINUE
  321   CONTINUE
      ELSE
C
        CALL EVNTID(RUN,ID)
        IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
          RUNSAV = RUN
          IDSAV = ID
        ELSE
          GOTO 100
        ENDIF
C        
        LCDCH = GZCDCH()
        IF (REDOCDC .AND. LCDCH .GT. 0) THEN
          IF (USEDHIT) THEN
            DO LAYER = 0, 3
              LDLYR(LAYER) = GZDLYR(LAYER)
              IF (LDLYR(LAYER) .GT. 0) 
     &          CALL MZDROP(IXCOM, LDLYR(LAYER), ' ')
              IQ(LCDCH+6+LAYER) = 0
            ENDDO
            IQ(LCDCH+10) = 0
          ELSE
            IF (LCDCH .GT. 0) CALL MZDROP(IXCOM,LCDCH,' ')
          ENDIF
        ENDIF
C
C     do hitfinding
C
 100    DO 20 LAYER = 0, MAXLAY
          NHITLA = 0
          NHITLS = 0
          DO 30 SECTOR = 0, MAXSEC
            IF ((ON(LAYER,SECTOR).EQV..FALSE.)) GO TO 30
            CALL DHTCHK(LAYER,SECTOR,2,DONE)
            IF (DONE) GOTO 31
            CALL DSECHT(LAYER,SECTOR,NHITS,NHITS1)      
C                            ! hitfinding for this sector 
            NHITLA = NHITLA + NHITS
            NHITLS = NHITLS + NHITS1
   31       LOC = LDSEC(SECTOR,LAYER)
            IF (LOC .NE. 0) THEN
              STAT = IQ(LOC)
              IQ(LOC) = IBSET(STAT,ION)
            ENDIF
   30     CONTINUE
          IQ(LDLYR(LAYER)+1) = IQ(LDLYR(LAYER)+1)+NHITLA ! # hits in layer
          IQ(LDLYR(LAYER)+2) = IQ(LDLYR(LAYER)+2)+NHITLS ! # SW hits in layer
          NHITCD = NHITCD + NHITLA
          NHITSW = NHITSW + NHITLS
   20   CONTINUE
        IQ(LCDCH+1) = IQ(LCDCH+1) + NHITCD            ! # hits in CDC
        IQ(LCDCH+10) = IQ(LCDCH+10) + NHITSW          ! # SW hits in CDC
      ENDIF
C
C     fill the compressed bank
C
      DO 22 LAYER = 0, MAXLAY
        DO 32 SECTOR = 0, MAXSEC
          PLDSEC = GZDSEC(SECTOR,LAYER)
          IF (BUILD_DHIT) THEN
            IF (BTEST(IQ(PLDSEC),CMPRSS)) GOTO 32
            CALL DHITFL(LAYER,SECTOR)     
          ENDIF
   32   CONTINUE           
   22 CONTINUE           
C
  999 RETURN
      END
