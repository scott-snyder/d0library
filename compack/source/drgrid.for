      SUBROUTINE DRGRID(ROW,COLUMN,ROWS,COLUMNS,ROWSPACE,COLSPACE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display a grid of lines, in the main COMPACK
C-   window, starting at position ROW,COLUMN. 
C-   
C-   Call entry point 
C-   DRGRID_SELECT(VDID) to select another window just before a call to
C-   DRGRID.
C-
C-   Inputs  : ROW      [I]     Start Row
C-             COLUMN   [I]     Start Column
C-             ROWS     [I]     Number of rows in grid
C-             COLUMNS  [I]     Number of columns in grid
C-             ROWSPACE [I]     Spacing in rows
C-             COLSPACE [I]     Spacing in columns
C-   Outputs : None
C-
C-   Created   7-JUL-1991   Harrison B. Prosper
C-   Updated  18-JAN-1992   Harrison B. Prosper  
C-    Add entry point DRGRID_SELECT 
C-   Updated   8-MAY-1992   Harrison B. Prosper  
C-      Remove test on TRMFLG 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ROW,  COLUMN
      INTEGER ROWS, COLUMNS
      INTEGER ROWSPACE, COLSPACE
      INTEGER VDID
C----------------------------------------------------------------------
      LOGICAL GETDEV
      INTEGER I,STATUS
      INTEGER START_ROW,START_COL,END_ROW,END_COL,ICOL,IROW
C----------------------------------------------------------------------
C&IF VAXVMS
      INTEGER SMG$DRAW_LINE
C&ENDIF
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C----------------------------------------------------------------------
      INTEGER DISPID
      LOGICAL SELECTED
      SAVE DISPID, SELECTED
C----------------------------------------------------------------------
C&IF VAXVMS
      IF ( GETDEV() ) THEN
        IF ( .NOT. SELECTED ) THEN
          DISPID = MAINID
        ENDIF
C
C ****  Draw vertical lines
C
        START_ROW = ROW
        END_ROW   = ROW + ROWSPACE*ROWS
        ICOL      = COLUMN
        DO I = 1, COLUMNS + 1
          STATUS = SMG$DRAW_LINE(DISPID,
     &                           START_ROW,ICOL,
     &                           END_ROW,ICOL)
          IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
          ICOL = ICOL + COLSPACE
        ENDDO
      ENDIF
C
C ****  Draw horizontal lines
C
      START_COL = COLUMN
      END_COL   = COLUMN + COLSPACE*COLUMNS
      IROW      = ROW
      DO I = 1, ROWS + 1
        STATUS = SMG$DRAW_LINE(DISPID,
     &                         IROW,START_COL,
     &                         IROW,END_COL)
        IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
        IROW = IROW + ROWSPACE
      ENDDO
C&ELSE
C&ENDIF
      SELECTED = .FALSE.
      RETURN
C
      ENTRY DRGRID_SELECT(VDID)
      DISPID = VDID
      SELECTED = .TRUE.
  999 RETURN
      END
