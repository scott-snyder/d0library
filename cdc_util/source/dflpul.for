      SUBROUTINE DFLPUL(WITHDL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-         fill histograms: pulse <area>, <width> and <height> vs      
C-                          wire number
C-      
C-   Inputs  : WITHDL (.true. if wire number includs DL)
C-   Outputs : 
C-   Controls: called by DHSFIL
C-
C-   Created  19-AUG-1988   Qizhong Li-Demarteau
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
C
      INTEGER LAYER, SECTOR, WIRE, IHIT, IND, IPTR, NHITS 
      INTEGER KPDSEC, KPDCDA, WIRENO, SWDLNO, MXWIRE
      INTEGER HISID1, HISID2, HISID3
      INTEGER MAXLAY, MAXSEC, ERR
      INTEGER IER
      REAL HEIGHT, WIDTH, AREA
      LOGICAL WITHDL, FIRST
      LOGICAL EZERROR
C
      SAVE FIRST
      DATA   FIRST/.TRUE./
C---------------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DFLPUL',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('MAXLAY',MAXLAY,ERR)
        CALL EZGET('MAXSEC',MAXSEC,ERR)
        CALL EZRSET
      ENDIF
C
      IF( IQ(LCDCH+1) .EQ. 0 ) RETURN           ! No hit in the chamber...
C
      DO 101 LAYER = 0, MAXLAY
        DO 102 SECTOR = 0, MAXSEC
          KPDSEC = LDSEC( SECTOR, LAYER )
          IF (KPDSEC .EQ. 0) GO TO 102
          KPDCDA = LDCDA( SECTOR, LAYER )
          MXWIRE = 6
          IF (WITHDL) MXWIRE = MXFADC
          DO 103 WIRE = 0, MXWIRE
            NHITS = IQ( KPDCDA+4+WIRE )       ! get number of hits
            IF( NHITS .LE. 0 ) GO TO 103
            IND   = KPDCDA + IQ(KPDCDA+2) + 4 + WIRE
            IPTR  = KPDCDA + IQ(IND)
            WIRENO = LAYER*7 + WIRE  ! wire # in 0:27
            IF (WITHDL) SWDLNO = LAYER*11 + WIRE  ! wire # in 0:43
            DO 104 IHIT = 1, NHITS
              AREA = 0.
              WIDTH = 0.
              HEIGHT = 0.
              AREA = Q(IPTR + 3)           ! get peak area from DCDA 
              WIDTH = Q(IPTR + 4)           ! get peak width from DCDA 
              HEIGHT = Q(IPTR + 5)           ! get peak height from DCDA 
              IPTR = IPTR + IQ( KPDCDA+3 )
              IF (WITHDL) THEN
                HISID1 = 2000 + SECTOR
C                CALL HBAVFL(HISID1,FLOAT(SWDLNO),AREA)
                CALL HFILL(HISID1,FLOAT(SWDLNO),AREA,1.)
                HISID2 = 2100 + SECTOR
C                CALL HBAVFL(HISID2,FLOAT(SWDLNO),WIDTH)
                CALL HFILL(HISID2,FLOAT(SWDLNO),WIDTH,1.)
                HISID3 = 2200 + SECTOR
C                CALL HBAVFL(HISID3,FLOAT(SWDLNO),HEIGHT)
                CALL HFILL(HISID3,FLOAT(SWDLNO),HEIGHT,1.)
              ELSE
                HISID1 = 2050 + SECTOR
C                CALL HBAVFL(HISID1,FLOAT(WIRENO),AREA)
                CALL HFILL(HISID1,FLOAT(WIRENO),AREA,1.)
                HISID2 = 2150 + SECTOR
C                CALL HBAVFL(HISID2,FLOAT(WIRENO),WIDTH)
                CALL HFILL(HISID2,FLOAT(WIRENO),WIDTH,1.)
                HISID3 = 2250 + SECTOR
C                CALL HBAVFL(HISID3,FLOAT(WIRENO),HEIGHT)
                CALL HFILL(HISID3,FLOAT(WIRENO),HEIGHT,1.)
              ENDIF
  104       CONTINUE
  103     CONTINUE
  102   CONTINUE
  101 CONTINUE
C
  999 RETURN
      END
