      SUBROUTINE DRRECT(ROW,COLUMN,ROWS,COLUMNS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display a rectangle, in the main COMPACK
C-   window, starting at position ROW,COLUMN.
C-   
C-   Call entry point 
C-   DRRECT_SELECT(VDID) to select another window just before a call to
C-   DRRECT.
C-
C-   Inputs  : ROW      [I]     Start Row
C-             COLUMN   [I]     Start Column
C-             ROWS     [I]     Number of rows in rectangle
C-             COLUMNS  [I]     Number of columns in rectangle
C-   Outputs : None
C-
C-   Created  24-MAR-1992   Harrison B. Prosper
C-   Updated   8-MAY-1992   Harrison B. Prosper  
C-      Remove test on TRMFLG 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ROW,  COLUMN
      INTEGER ROWS, COLUMNS
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
        END_ROW   = ROW + ROWS + 1
        ICOL   = COLUMN
        STATUS = SMG$DRAW_LINE(DISPID,
     &                         START_ROW,ICOL,
     &                         END_ROW,ICOL)
        IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
C
        ICOL   = COLUMN + COLUMNS + 1
        STATUS = SMG$DRAW_LINE(DISPID,
     &                         START_ROW,ICOL,
     &                         END_ROW,ICOL)
        IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
C
C ****  Draw horizontal lines
C
        START_COL = COLUMN
        END_COL   = COLUMN + COLUMNS + 1
        IROW   = ROW
        STATUS = SMG$DRAW_LINE(DISPID,
     &                         IROW,START_COL,
     &                         IROW,END_COL)
        IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
C
        IROW   = ROW + ROWS + 1
        STATUS = SMG$DRAW_LINE(DISPID,
     &                         IROW,START_COL,
     &                         IROW,END_COL)
        IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
      ENDIF
C&ELSE
C&ENDIF
      SELECTED = .FALSE.
      RETURN
C
      ENTRY DRRECT_SELECT(VDID)
      DISPID = VDID
      SELECTED = .TRUE.
  999 RETURN
      END
