      LOGICAL FUNCTION FDC_EXM_PROC_MENU ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inserts  OPTIONAL HISTOGRAMS into PROCESS menu
C-                         for FDC Examine
C-
C-   Returned value  : .TRUE.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  5-NOV-1990   Susan K. Blessing
C-   Updated   7-JAN-1991   Susan K. Blessing  for use with FDC Examine
C-   Updated   3-APR-1992   Susan K. Blessing  Use FDC.RCP to control 
C-    existence of optional histogram menu item.
C-   Updated  15-NOV-1993   Susan K. Blessing  Add call to INRCPE. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER LRCP,IER
C
      LOGICAL FDC_ONLY
C
      CHARACTER*64 MENNAM,MENCOM
C
C----------------------------------------------------------------------
C
      FDC_EXM_PROC_MENU = .TRUE.
C
      CALL EZLOC('FDC_RCP',LRCP)
      IF (LRCP.LE.0) THEN
        CALL INRCP('FDC_RCP',IER)  ! Read parameter file into an SRCP bank
        IF (IER .NE. 0) THEN
          CALL ERRMSG('FDC RCP BAD','FDC_EXM_PROC_MENU',
     &      'FDC RCP had a bad read','W')
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
C
        MENNAM = 'Optional Histograms'
        MENCOM = 'FDC_HIST'
        CALL MENADD('PROCESS',.TRUE.,
     &    MENNAM,
     &    MENCOM,
     &    '      Transfer control to Optional Histogram menu.'//
     &    ' '   )
      END IF
C
  999 RETURN
      END
