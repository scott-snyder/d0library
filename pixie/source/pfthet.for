      SUBROUTINE PFTHET( HALF,LAYER)
C----------------------------------------------------------------------
C-   Purpose and Methods : Draw one theta display
C-
C-   Inputs  : HALF,LAYER - FDC Half,Layer(inner,outer) being displayed
C-   Outputs : draws theta display
C-
C-   Created  27-JAN-1989   Jeffrey Bantly
C-   Updated   7-FEB-1990   Jeffrey Bantly  general cleanup 
C-   Updated  23-JAN-1991   Jeffrey Bantly  change to PFTKDR call 
C-   Updated  20-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK 
C-   Updated   6-AUG-1991   Robert E. Avery  Add option to only draw
C-                              Quadrant borders, other cleanups.
C-   Updated   7-OCT-1991   Robert E. Avery  Use new FDDELP, also
C-                              move track drawing to pfhalf.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:FDDELP.INC'
      INTEGER HALF, LAYER, QUAD, SECTOR
      INTEGER LFWAL, LFWTA, LFWTB, LFWTX
      INTEGER GZFGEH, GZFWAL, GZFWTA, GZFWTB, IERR, IER
      INTEGER DRASEC
C  FUNCTION:
      LOGICAL EZERROR
      INTEGER FDC_QUADTYPE
C----------------------------------------------------------------------
C
C  Check on presence of STP banks
C
      IF( LSFDC .LE. 0 ) GO TO 999
      IF( LFGEH .LE. 0 ) LFGEH = GZFGEH()
      IF( LFGEH .LE. 5 ) GO TO 999
      LFWTA = GZFWTA()
      LFWTB = GZFWTB()
C
C ****  Pick PIXIE RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFTHET','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('FDC DRAW SECTOR',DRASEC)
      DO  5 QUAD = 0+LAYER*4,3+LAYER*4
        IF(FDC_QUADTYPE(QUAD,HALF) .EQ. 1) THEN
          LFWTX = LFWTA
        ELSE
          LFWTX = LFWTB
        ENDIF
        IF(LFWTX.LE.5) GOTO 5
        CALL PUSETV('FDC HALF',HALF)
        CALL PUSETV('FDC QUAD',QUAD)
        IF( DRASEC .EQ. 0 ) THEN
          CALL PFTQUAD(LFWTX)     
        ELSE
          DO 10 SECTOR = 0, 5
            CALL PUSETV('FDC SECT',SECTOR)
            IF( DRASEC .EQ. 1 ) THEN
              CALL PFTSEC(LFWTX)     ! Drawing each sector
            ELSEIF( DRASEC .EQ. 2 ) THEN
              IF( N_DL_HITS(HALF,QUAD,SECTOR) .GE. 1 ) THEN
                CALL PFTSEC(LFWTX)
              ENDIF
            ELSE
              GOTO 900
            ENDIF
   10     CONTINUE
        ENDIF
    5 CONTINUE
C----------------------------------------------------------------------
  900 CALL EZRSET
  999 RETURN
      END
