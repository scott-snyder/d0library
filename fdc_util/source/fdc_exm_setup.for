      FUNCTION FDC_EXM_SETUP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SETUP FOR FDC EXAMINE
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-FEB-1990   Susan K. Blessing
C-   Updated  19-FEB-1991   Susan K. Blessing   Remove automatic
C-      trigger selection.  Selection must now be made using the
C-      Examine2 menu.
C-   Updated   3-APR-1992   Susan K. Blessing  Use FDC.RCP to control 
C-    existence of optional histogram menu item.
C-   Updated  15-NOV-1993   Susan K. Blessing  Add call to INRCPE. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LRCP,IER
C
      LOGICAL FDC_EXM_SETUP
      LOGICAL FDC_ONLY
C
      DATA FDC_ONLY/.FALSE./
C----------------------------------------------------------------------
C
      FDC_EXM_SETUP = .TRUE.
C
      CALL EZLOC('FDC_RCP',LRCP)                        
      IF (LRCP.LE.0) THEN                                   
        CALL INRCP ('FDC_RCP',IER)  ! Read parameter file into an SRCP bank
        IF (IER .NE. 0) THEN
          CALL ERRMSG('FDC RCP BAD','FDC_EXM_SETUP',
     &    'FDC RCP had a bad read','W')
          GO TO 999
        END IF
        CALL EZLOC('FDC_RCPE',LRCP)                        
        IF (LRCP.LE.0) THEN                                   
          CALL INRCPE('FDC_RCPE',IER)  ! Read parameter file into an SRCP bank
        END IF
      END IF
C
      CALL EZPICK('FDC_RCP')
      CALL EZGET('FDC_ONLY',FDC_ONLY,IER)
      CALL EZRSET
C
      IF (FDC_ONLY) THEN
        CALL FLGBK('FDC_HIST',1)
        CALL MENSET('FDC_HIST')
      END IF
C
  999 RETURN
      END
