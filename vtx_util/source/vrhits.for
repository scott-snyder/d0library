      SUBROUTINE VRHITS(PHIMIN,PHIMAX)
C----------------------------------------------------------------------
C
C  Main routine for hitfinding in VTX sectors along a road.
C
C  Input:
C        PHIMIN,PHIMAX      = road parameters
C
C  Daria Zieminska Feb. 1989
C  Peter Grudberg 24-MAY-1989 Add calls for hitfinding with raw data
C-   Updated  20-NOV-1989   Qizhong Li-Demarteau  correct several errors
C-   updated  24-AUG-1991   P. G. add option for compressing hits
C-   Updated  25-OCT-1991   Peter M. Grudberg  hit comp. changes, remove strips 
C-   Updated   8-JUN-1992   Peter M. Grudberg  Handle old VTXH banks in STA
C-                                             files 
C-   Updated  10-APR-1993   Ed Oltman  Clear ION bit if VSEC exists and is not
C-                                     on road 
C-   Updated  15-NOV-1993   Peter Grudberg  Add hitfinding from VCHT 
C-   Updated  15-FEB-1994   Al Clark  Delete all references to VHIT bank.
C-                          Clean up unused variables. 
C-   Updated  20-FEB-1994   Liang-Ping Chen  eliminate THEMIN, THEMAX since     
C-                          they were not used.                                 
C-                          redefine RAW_EXSISTS=.TRUE. if CDD1 or CDH1 exsists
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
C
      REAL PHIMIN,PHIMAX
      INTEGER NLAYER
      PARAMETER (NLAYER=2)
      INTEGER COMPRESS
      PARAMETER ( COMPRESS = 12 )
      INTEGER NSEC(0:NLAYER)
      INTEGER LAYER,SECTOR,ION,STAT,LOC
      INTEGER GZVTXH, GZVLAY, GZVSEC, GZVWDA
      INTEGER DUMMY
      INTEGER IER,ICALL,IPATH
      INTEGER LVCHT, GZVCHT, FULLHIT_BIT
      INTEGER LCDH1, GZCDH1
      LOGICAL ON(0:2,0:31)
      LOGICAL DONE
      LOGICAL FULLVCHT, VCHT_EXISTS, RAW_EXISTS, ISTAT
      CHARACTER*4 PATH,VPATH
      EQUIVALENCE (IPATH,VPATH)
      PARAMETER ( FULLHIT_BIT = 31 )
      DATA ICALL/0/
C-----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('VPATH',IPATH,IER)
        CALL EZGET('ION',ION,IER)
        CALL EZGET('NSEC',NSEC,IER)
        CALL EZRSET
        ICALL=1            
      END IF
      PATH=VPATH
C
C  Find sectors on the road
C
      CALL FLVSEC(PHIMIN,PHIMAX,ON)
C
C  If PATH='GEAN' hit banks already present; define links in /VTXLNK/.
C  If PATH='RECO' find hits and make banks of hits: VSEC and VZLA.
C
      IF (PATH.EQ.'GEAN') THEN
C
C  Use ideal hits from Geant
C
        LVTXH=GZVTXH(DUMMY)
        DO 10 LAYER =  0, NLAYER
          LVLAY(LAYER) = GZVLAY(DUMMY)
          IF ( LVLAY(LAYER) .LE. 0) GO TO 10
          DO 11 SECTOR =  0, NSEC(LAYER)
            IF (.NOT. ON(LAYER,SECTOR)) GO TO 11 ! sector not on a road
            LVSEC(SECTOR,LAYER) = GZVSEC(LAYER,SECTOR)
            LVWDA(SECTOR,LAYER) = GZVWDA(LAYER,SECTOR)
            LOC=LVSEC(SECTOR,LAYER)  ! Flag sectors on roads
            IF ( LOC .GT. 0 ) THEN
              STAT=IQ(LOC)
              IQ(LOC)=IBSET(STAT,ION)
            ENDIF
   11     CONTINUE
   10   CONTINUE
        GO TO 999
      END IF
C
C  Do hitfinding.  If a full VCHT exists, use it.  Otherwise, try to use raw
C  data (if it exists)
C
      FULLVCHT = .FALSE.
      LVCHT = GZVCHT()
      IF ( LVCHT .GT. 0 ) THEN
        FULLVCHT = BTEST(IQ(LVCHT+5),FULLHIT_BIT)
        VCHT_EXISTS = .TRUE.
      ENDIF
C
      LCDD1 = LQ(LHEAD-IZCDD1)
      LCDH1 = GZCDH1()
      RAW_EXISTS = LCDD1 .GT. 0 .OR. LCDH1.GT. 0 
C
      LVTXH = GZVTXH(DUMMY)
      IF (LVTXH .LE. 0) THEN
        CALL BKVTXH
      ENDIF
      DO 100 LAYER=0,NLAYER
        IF (LVLAY(LAYER).LE.0) CALL BKVLAY(LAYER,LVLAY(LAYER))
        DO 200 SECTOR=0,NSEC(LAYER)
          IF (.NOT. ON(LAYER,SECTOR)) THEN
C..Sector not on road..
            LOC = GZVSEC(LAYER,SECTOR)
            IF (LOC .GT. 0) THEN
              STAT = IQ(LOC)
              IQ(LOC) = IBCLR(STAT,ION)
            ENDIF
          ELSE
C
C ****  Hitfinding already done?
C
            CALL VHTCHK(LAYER, SECTOR, 2, DONE)
            IF ( DONE ) GO TO 21
C
C  Hitfinding not done yet for this sector
C  do hitfinding for this sector, book and fill VSEC
C
            IF ( FULLVCHT ) THEN  ! get sector info from VCHT
              CALL VCHT_UNPACK(LAYER,SECTOR,ISTAT)
              IF ( ISTAT .EQ. 0 ) CALL VHTCHK(LAYER,SECTOR,1,DONE) ! set bit
            ELSEIF ( RAW_EXISTS ) THEN ! find hits from raw data
              CALL VSECHT(LAYER,SECTOR)
            ELSEIF ( VCHT_EXISTS ) THEN ! VCHT not full; try anyway (last hope)
              CALL VCHT_UNPACK(LAYER,SECTOR,ISTAT)
              IF ( ISTAT .EQ. 0 ) THEN
                CALL VHTCHK(LAYER,SECTOR,1,DONE) ! set done bit
              ELSE
                CALL ERRMSG('VTX-NO-DATA','VRHITS',
     &            'Hit data not available for requested sector','W')
              ENDIF
            ENDIF
C
   21       CONTINUE
            LOC=LVSEC(SECTOR,LAYER)          ! Flag sectors on roads
            IF ( LOC .NE. 0 ) THEN
              STAT=IQ(LOC)
              IQ(LOC)=IBSET(STAT,ION)
            ENDIF
          ENDIF
  200   CONTINUE
  100 CONTINUE
  999 CONTINUE
      RETURN
      END
