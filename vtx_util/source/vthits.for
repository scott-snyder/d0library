      SUBROUTINE VTHITS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find hits in all sectors of VTX.
C-
C-
C-   Created  FEB-1989   Daria Zieminska
C-   Updated  MAY-1989   Peter M. Grudberg   add calls for hitfinding with
C-                                           raw data
C-   Updated  7-JAN-1990   Peter M. Grudberg   fix MZLINT call
C-   Updated  23-AUG-1991   Peter M. Grudberg   add compressed hits option
C-   Updated  15-OCT-1991   Peter M. Grudberg  remove strips, change hit
C-                                             compression handling
C-   Updated   4-JUN-1992   Peter Grudberg  Handle old VTXH banks in STA files 
C-   Updated  15-NOV-1993   Peter Grudberg  Add hitfinding from VCHT 
C-   Updated  15-FEB-1994   Al Clark  Delete all references to VHIT bank.
C-                          Clean up unused variables.
C-   Updated  20-FEB-1994   Liang-Ping Chen
C-                          redefine RAW_EXSISTS=.TRUE. if CDD1 or CDH1 exsists
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
C
      CHARACTER*4 PATH,VPATH 
      INTEGER NLAYER
      PARAMETER (NLAYER=2)
      INTEGER NSEC(0:NLAYER)
      INTEGER LAYER,SECTOR,ICALL,IER,IPATH
      LOGICAL DONE, ISTAT
      INTEGER DUMMY
      INTEGER LKVSEC, LKVWDA
      INTEGER LVCHT, GZVCHT
      INTEGER LCDH1, GZCDH1
      INTEGER COMPRESS
      PARAMETER ( COMPRESS = 12 )
      INTEGER GZVTXH, GZVLAY, GZVSEC, GZVWDA
      EQUIVALENCE (IPATH,VPATH)
      LOGICAL RAW_EXISTS, VCHT_EXISTS
      DATA ICALL/0/
C-----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('VPATH',IPATH,IER)
        CALL EZGET('NSEC',NSEC,IER)
        CALL EZRSET
        ICALL=1
      END IF
C
      PATH=VPATH 
C
C  If PATH='GEAN' hit banks already present; define links in /VTXLNK/. 
C  If PATH='RECO' find hits and make banks of hits: VSEC and VZLA.
C
      IF ( PATH .EQ. 'GEAN' ) THEN
C
C  Use ideal hits from Geant
C 
        LVTXH = GZVTXH(DUMMY)
        DO 10 LAYER =  0, NLAYER
          LVLAY(LAYER) = GZVLAY(LAYER) 
          IF ( LVLAY(LAYER) .LE. 0) GO TO 10
          DO 11 SECTOR =  0, NSEC(LAYER) 
            LVSEC(SECTOR,LAYER)=GZVSEC(LAYER,SECTOR)
            LVWDA(SECTOR,LAYER)=GZVWDA(LAYER,SECTOR)
   11     CONTINUE
   10   CONTINUE
        GO TO 999
      END IF
C
C  Do hitfinding - use raw data, if it exists.  If no raw data, try to use VCHT
C  compressed hits bank
C
      LCDD1 = LQ(LHEAD-IZCDD1)
      LCDH1 = GZCDH1()
      RAW_EXISTS = LCDD1 .GT. 0 .OR. LCDH1.GT. 0
C
      LVCHT = GZVCHT()
      VCHT_EXISTS = LVCHT .GT. 0
C
      IF ( .NOT. RAW_EXISTS .AND. .NOT. VCHT_EXISTS ) THEN
        CALL ERRMSG('VTX-NO-DATA','VTHITS',
     &    'No VTX data, cannot find hits','W')
        GO TO 999
      ENDIF
C
      LVTXH = GZVTXH(DUMMY)
      IF (LVTXH .LE. 0) THEN
        CALL BKVTXH
      ENDIF
      DO 100 LAYER=0,NLAYER
        IF (LVLAY(LAYER).LE.0) CALL BKVLAY(LAYER,LVLAY(LAYER))
        DO 200 SECTOR=0, NSEC(LAYER)
C
C ****  Hitfinding already done?  Also check for the existence of VWDA, which is
C ****  needed to create VCHT
C
          CALL VHTCHK(LAYER, SECTOR, 2, DONE)
          LKVWDA = GZVWDA(LAYER,SECTOR)
          IF ( DONE .AND. LKVWDA.GT.0 ) GO TO 200
C
C **** do hitfinding for this sector, book and fill VSEC.  Drop old VSEC bank if
C **** necessary
C
          LKVSEC = GZVSEC(LAYER,SECTOR)
          IF ( LKVSEC .GT. 0 ) CALL MZDROP(IXCOM,LKVSEC,' ')
C
          IF ( RAW_EXISTS ) THEN
            CALL VSECHT(LAYER,SECTOR)
          ELSEIF ( VCHT_EXISTS ) THEN
            CALL VCHT_UNPACK(LAYER,SECTOR,ISTAT)
            IF ( .not.ISTAT ) THEN
              CALL VHTCHK(LAYER,SECTOR,1,DONE)  ! Set done bit in VTXH
            ELSE
              CALL ERRMSG('Hit data missing','VTHITS',
     &          'Hit data missing from VCHT bank','W')
            ENDIF
          ENDIF
C
  200   CONTINUE
  100 CONTINUE
  999 RETURN
      END
