      FUNCTION PWCCHK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return true if event numbers match.
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-AUG-1990   Robert E. Avery
C-   Updated  13-JUL-1992   Susan K. Blessing  Check value of USUNIT
C-    before writing to it.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL PWCCHK
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LTRGR
      INTEGER LCDD2
      INTEGER EVONUM
      INTEGER EVT_HEAD 
      INTEGER EVT_PREV 
      INTEGER EVT_CDD2
      INTEGER EVT_TRGR
      INTEGER USUNIT
      INTEGER LUNIT
      INTEGER IER
      INTEGER ICOUNT
      CHARACTER*50 MESSAGE
      LOGICAL PWCCHK_SAVE
      LOGICAL PRINT
      LOGICAL FIRST
      DATA PRINT /.FALSE./
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK('FDC_RCP')
        CALL EZGET('PWCCHK_PRINT',PRINT,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      EVT_HEAD = EVONUM()      
      IF ( EVT_HEAD .EQ. EVT_PREV ) THEN
        PWCCHK = PWCCHK_SAVE
        GOTO 999
      ENDIF
      EVT_PREV = EVT_HEAD 
C
      PWCCHK = .FALSE.
C
      ICOUNT = ICOUNT+1
      IF ( MOD(ICOUNT,100) .EQ. 1 ) THEN
        LUNIT=USUNIT()
        IF (LUNIT.GT.0) THEN
          WRITE (LUNIT,*) 
     &      'EVENTS ANALAYSED:',ICOUNT,' EVENT NUMBER:',EVT_HEAD
        END IF
      ENDIF
C
      LCDD2 = LQ(LHEAD - 4)
      IF (LCDD2 .EQ. 0) GOTO 999
C
      LTRGR = LQ(LHEAD - 1)
      IF (LTRGR .EQ. 0) GOTO 999
C
      CALL MVBITS ( IQ(LCDD2+2), 16, 16, EVT_CDD2, 0)
      CALL MVBITS ( IQ(LTRGR+2), 16, 16, EVT_TRGR, 0)
C
      IF (  EVT_TRGR .NE. EVT_CDD2  ) THEN
        IF ( PRINT ) THEN
          LUNIT=USUNIT()
          WRITE(MESSAGE,*)' EVT:',EVT_HEAD,
     &                    ' TRGR evt:',EVT_TRGR,
     &                    ' CDD2 evt:',EVT_CDD2,
     &                    ' DIFF:',EVT_TRGR-EVT_CDD2
          WRITE(LUNIT,*) MESSAGE
          CALL INTMSG(MESSAGE)
        ENDIF
        CALL ERRMSG('PWCCHK','PWCCHK',' CDD2 Event mismatch','W')
        GOTO 999
      ENDIF
C
      PWCCHK = .TRUE.
  999 CONTINUE
      PWCCHK_SAVE = PWCCHK 
      RETURN
      END
