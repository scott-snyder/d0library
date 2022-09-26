      SUBROUTINE CDHITS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : full hitfinding for whole CDC
C-
C-   Inputs  : none
C-   Outputs : banks DCDA, DSEC
C-
C-   Updated  10-AUG-1987   Olivier Callot
C-   Updated  23-OCT-1987   Olivier Callot   Computes mean Z and correct
C-   Updated  20-JAN-1988   Olivier Callot   Avoid booking empty sectors
C-   Updated   7-JUN-1988   Olivier Callot   Zmean had a wrong sign in cosmics
C-   Updated   5-AUG-1988   Ghita Rahal-Callot : Allows GEANT DSEC bank
C-                                               processing
C-   Updated  15-MAR-1989   Qizhong Li-Demarteau  new structure: routine
C-                                                DSECHT implemented
C-   Updated  22-APR-1989   Qizhong Li-Demarteau  use SRCP
C-   Updated   4-OCT-1989   Qizhong Li-Demarteau  rewrite the part which deals
C-                                                the GEANT hits banks 
C-   Updated  07-NOV-1990   Qizhong Li-Demarteau   check if hitfinding 
C-                                       has been done for this sector
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added a choice for the
C-                                                compressed hits bank
C-   Updated  28-AUG-1991   Qizhong Li-Demarteau  added # of hits on sense
C-                                             wire in CDCH and DLYR banks 
C-   Updated  14-APR-1993   Paul Rubinov   Included call to DC_ENVADJ
C-
C-   Updated   8-MAR-1994   C. Klopfenstein Turn off call to DC_ENVADJ
C-                          when reconstructing from DHIT bank.
C-
C-   Updated  15-MAR-1994   C. Klopfenstein Modify logic for using DHIT
C-                          bank. Always use DHIT if it exists and is
C-                          right version.
C-   Updated  10-APR-1994   Qizhong Li-Demarteau  fixed total number of
C-                                  hits in IQ(LCDCH+1) for reconstructing 
C-                                  from DHIT
C-   Updated  06-FEB-1995   Norman A. Graf set bit in DHIT if delay line
C-                          z fix implemented
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC/LIST'
      INCLUDE 'D0$INC:CDPARA.INC/LIST'
      INCLUDE 'D0$INC:DDEBUG.INC/LIST'
      INCLUDE 'D0$INC:CDCCOS.INC/LIST'
C
      INTEGER NHITS, NBHITS( 0:NBSENS ), PTHITS( 0:NBSENS )
      INTEGER NHITLA, NHITCD, LHIT, RUNNO, LAYER, SECTOR, WIRE
      INTEGER KPDSEC, KPDCDA, NWIR, K
      INTEGER GZDLYR, GZDSEC, GZDCDA
      INTEGER MAXLAY, MAXSEC, ERR, RUNTYP, IPATH
      INTEGER CMPRSS, PLDSEC
      PARAMETER( CMPRSS = 12 )
      INTEGER IER, NHITSW, NHITLS, NHITS1
      CHARACTER*4 DPATH
      EQUIVALENCE (IPATH, DPATH)
      REAL    CORZ 
      LOGICAL EZERROR, BUILD_DHIT
      LOGICAL FIRST, DONE, BTEST, PRESSURE_ADJUST
      integer gzcdd2, gzdhit, ldhit, length_dhit, min_len_dhit
      parameter (min_len_dhit = 3)
      logical reco_from_dhit,fixz
      data reco_from_dhit /.false./
      SAVE FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDHITS',
     &       'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('MAXLAY',MAXLAY,ERR)
        IF (ERR .NE. 0) MAXLAY = 3
        CALL EZGET_i('MAXSEC',MAXSEC,ERR)
        IF (ERR .NE. 0) MAXSEC = 31
        CALL EZGET_i('DPATH',IPATH,ERR)
        CALL EZGET_i('RUNTYP',RUNTYP,ERR)
        CALL EZGET_l('BUILD_DHIT',BUILD_DHIT,ERR)
        CALL EZGET('APPLY_ENV_CORR',PRESSURE_ADJUST,ERR)
        CALL EZGET('FIXZ',FIXZ,IER)
        CALL EZRSET
      ENDIF
C
C check whether we are using DHIT (compressed hits) bank
C
      ldhit = gzdhit()
      length_dhit = 0
      if (ldhit .gt. 0) length_dhit = IQ(ldhit + 3)
      if ((ldhit .gt. 0) .and. 
     &    (length_dhit .ge. min_len_dhit)) reco_from_dhit = .true.
C      if ((gzcdd2() .le. 0) .and. (gzdhit() .gt. 0)) 
C     &  reco_from_dhit = .true.
C
      NHITCD = 0
      NHITSW = 0
      ZMEAN  = 0.
      NBZMES = 0
C
C ****  Initialize gain adjustment - don't do it when using
C       DHIT bank - it's already been corrected.
C
      IF (PRESSURE_ADJUST .and. (.not. reco_from_dhit)) THEN
        CALL DC_ENVADJ(IER)
      ENDIF
C
C ****  Initialise the bank structure
C
      CALL BKCDCH
      IF (DPATH .EQ. 'GEAN') THEN
C
C ****  Redefine the links
C
        DO 321 LAYER = 0, MAXLAY
          LDLYR(LAYER) = GZDLYR(LAYER)
          DO 322 SECTOR = 0, MAXSEC
            LDSEC(SECTOR, LAYER) = GZDSEC(SECTOR,LAYER)
            LDCDA(SECTOR, LAYER) = GZDCDA(SECTOR,LAYER)
            IF ( DBGFLG .AND. LVLDBG(4).GT.0 ) THEN
              KPDSEC = LDSEC(SECTOR,LAYER)
              CALL PRDSEC(LUNDBG,KPDSEC,LVLDBG(4),'SINGLE',LVLDBG(4))
            ENDIF
  322     CONTINUE
  321   CONTINUE
        GO TO 900
      ENDIF
C
      DO 20 LAYER = 0, MAXLAY
        NHITLA = 0
        NHITLS = 0
        DO 30 SECTOR = 0, MAXSEC
          CALL DHTCHK(LAYER,SECTOR,2,DONE)
          IF (DONE) GOTO 30
          CALL DSECHT(LAYER,SECTOR,NHITS,NHITS1)      
C                            ! hitfinding for this sector 
          NHITLA = NHITLA + NHITS
          NHITLS = NHITLS + NHITS1
   30   CONTINUE
        IQ(LDLYR(LAYER)+1) = IQ(LDLYR(LAYER)+1)+NHITLA ! # hits in layer
        IQ(LDLYR(LAYER)+2) = IQ(LDLYR(LAYER)+2)+NHITLS ! # SW hits in layer
        NHITCD = NHITCD + NHITLA
        NHITSW = NHITSW + NHITLS
   20 CONTINUE
      IF (IQ(LCDCH+1) .LE. 0) THEN
        IQ(LCDCH+1) = IQ(LCDCH+1) + NHITCD            ! # hits in CDC
      ENDIF
      IQ(LCDCH+10) = IQ(LCDCH+10) + NHITSW          ! # SW hits in CDC
C
C
C     fill the compressed bank
C
      DO 21 LAYER = 0, MAXLAY
        DO 31 SECTOR = 0, MAXSEC
          PLDSEC = GZDSEC(SECTOR,LAYER)
          IF (BUILD_DHIT) THEN
            IF (BTEST(IQ(PLDSEC),CMPRSS)) GOTO 31
            CALL DHITFL(LAYER,SECTOR)     
          ENDIF
   31   CONTINUE           
   21 CONTINUE   
C
C ****  If z position of hits "corrected" in CDGETZ set bit in DHIT and CDCH
C
        IF (BUILD_DHIT .AND. (.NOT. RECO_FROM_DHIT)) THEN
          LDHIT = GZDHIT()
          IF(LDHIT.GT.0.AND.FIXZ) THEN
            IQ(LDHIT) = IBSET(IQ(LDHIT),0)
            IQ(LCDCH) = IBSET(IQ(LCDCH),0)
          ENDIF
        ENDIF
C
C ****  Correct the position due to trigger time.
C  RUNTYP:        0 -- D0 HALL TEST
C                 1 -- 88 COSMIC IN STONY BROOK
C                 2 -- 89 COSMIC IN STONY BROOK
C
      IF ( RUNTYP .EQ. 1 .AND. NBZMES .GT. 0 ) THEN
        ZMEAN = ZMEAN / NBZMES
        IF( DBGFLG .AND. LVLDBG(4) .GT. 0 )
     &                 WRITE( LUNDBG, 3333 ) NBZMES, ZMEAN
 3333   FORMAT(/10X,I3,' Z coordinates, mean value = ',F10.2)
C
C  ***  The change in position is Z / Light_velocity * drift velocity.
C  ***  Light is 7 ns/m in the PM, i.e. .07 ns/cm. Drift is .004 cm/ns
C
        CORZ = ZMEAN * .07 * .004
        DO 110 LAYER = 0, MAXLAY
          DO 120 SECTOR = 0, MAXSEC
            CALL ZGDSEC( LAYER, SECTOR, NWIR, PTHITS, NBHITS, LHIT )
            IF( LHIT .EQ. 0 ) GOTO 120
            DO 130 WIRE = 0, NWIR-1
              DO 140 K = 1, NBHITS(WIRE)
                Q( PTHITS(WIRE) + 2 ) = Q( PTHITS(WIRE) + 2 ) + CORZ
                Q( PTHITS(WIRE) + 3 ) = Q( PTHITS(WIRE) + 3 ) - CORZ
                PTHITS(WIRE) = PTHITS(WIRE) + LHIT
  140         CONTINUE
  130       CONTINUE
  120     CONTINUE
  110   CONTINUE
      ENDIF
  900 CONTINUE
  999 RETURN
      END
