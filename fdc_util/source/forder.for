      SUBROUTINE FORDER(EVTDAT,HALF,UNIT,QUAD,SECTOR,WIRE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Spread the zero-suppressed hits back out 
C-                         over the 512 bins and perform the bilinear
C-                         conversion
C-
C-   Inputs  : EVTDAT = the event data from ZDEXPD
C-   Outputs : EVTDAT = spread out event data
C-
C-   Created  28-JUL-1989   Jeffrey Bantly
C-   Updated  20-MAR-1990   Jeffrey Bantly  general cleanup 
C-   Updated  26-APR-1991   Jeffrey Bantly  clean up PARAMS,RCP 
C-   Updated  12-MAY-1992   Susan K. Blessing  Declare PEDS(2) (removed
C-    from FDEVNT.INC).  Change call so that FGTLPD can be called to 
C-    get the pedestals.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDEVNT.INC'
C
      INTEGER EVTDAT(0:LFADC-1),ICALL,IER
      INTEGER I,J,TEMP(0:LFADC-1),LENCLU,LOCCLU,IP,TMPADC
      INTEGER RUNTYPE
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
C
      REAL    BILIPT,BILIRT
      REAL    PEDS(2)
C
      LOGICAL EZERROR
C
      SAVE ICALL
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF( ICALL .EQ. 0 ) THEN
        ICALL = 1
        CALL EZPICK('FTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('FTRAKS','FORDER','FTRAKS_RCP not found.','W')
        ELSE
          CALL EZGET('BILIPT',BILIPT,IER)
          CALL EZGET('BILIRT',BILIRT,IER)
          CALL EZGET_i('RUNTYPE',RUNTYPE,IER)
          CALL EZRSET
        ENDIF
      ENDIF
C
      IP = 0
      CALL VZERO(TEMP(0),LFADC)
      CALL FGTLPD(HALF,UNIT,QUAD,SECTOR,WIRE,PEDS(1),PEDS(2))
C
    5 CONTINUE
      LENCLU = EVTDAT(IP)
C
      IF( LENCLU .NE. 0 ) THEN
        LOCCLU = EVTDAT(IP+1)
        IP = IP + 2
        IF( IQ(LHEAD+1) .GE. 1000) LENCLU = LENCLU - 2
        DO 10 I = 1, LENCLU
        TMPADC = INT(FLOAT(EVTDAT(IP))-PEDS(1))
        IF( RUNTYPE.NE.1 .AND. TMPADC.GT.0 ) 
     &                     CALL ZBICVT(BILIPT,BILIRT,TMPADC)
        IF( (UNIT .LE. 0) .AND. (WIRE .GT. MXWIRT) ) TMPADC = TMPADC*2.
        TEMP(LOCCLU+I) = INT(FLOAT(TMPADC)+PEDS(1))
        IP = IP + 1
   10   CONTINUE
        GOTO 5
      ENDIF
C
      DO 30 I = 0, LFADC-1
        EVTDAT(I) = TEMP(I)
   30 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
