      INTEGER FUNCTION LIBPST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Paste display 2 (MINID1) on top of display
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: Uses MINID1 in /SMGCOM/ as ID of virtual display
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated  7-NOV-1990 Scott Snyder: add scrolling viewport support
C-           27-NOV-1990 Scott Snyder: sometimes, calling this routine
C-    will cause a subsequent call to smg$read_string on mainid to fail.
C-    dunno why - seems to be a SMG bug. anyway, calling LIBCOP
C-    seems to fix it.
C-   Updated 23-APR-1991 Scott Snyder
C-    I've now grokked the aforementioned bug: the SMG input routines
C-    can fail if a display is specified explicitly and if there is a
C-    large virdisp with a viewport pasted after that virdisp.
C-    (SMG$$SET_PHYSICAL_CURSOR uses the virdisp geometry instead of the
C-    viewport geometry to check for occlusion of the input line.)  A
C-    better workaround is to just make sure that virdisps with
C-    viewports are pasted before virdisps on which input is done...
C-
C-    So - put MINID1 on the chain before MAINID and remove the call to
C-    LIBCOP...
C-   Updated  1-MAY-1991 Scott Snyder
C-    Make sure that the display is at least SPLLIN-2 rows long before
C-    creating the viewport.
C-   Updated 19-SEP-1991 Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$CURSOR_ROW
      LOGICAL SMG$CREATE_VIEWPORT, SMG$CREATE_VIRTUAL_DISPLAY,
     &        SMG$DELETE_VIEWPORT, SMG$ERASE_DISPLAY,
     &        SMG$MOVE_VIRTUAL_DISPLAY, SMG$PASTE_VIRTUAL_DISPLAY,
     &        SMG$GET_DISPLAY_ATTR, SMG$CHANGE_VIRTUAL_DISPLAY,
     &        SMG$SET_CURSOR_ABS, I
C
      INTEGER SPLSAV, SMG$M_BORDER, LINE
      LOGICAL DISPLAY_CREATED
      SAVE    DISPLAY_CREATED, SMG$M_BORDER, SPLSAV
      DATA DISPLAY_CREATED/.FALSE./
      DATA SPLSAV/0/
      DATA SMG$M_BORDER/1/
C----------------------------------------------------------------------
      I=SMG$ERASE_DISPLAY(MAINID,%VAL(0),%VAL(0),%VAL(0),%VAL(0))
      IF(STAFLG) THEN
        I=SMG$MOVE_VIRTUAL_DISPLAY(MAINID,PASTID,STALIN+SPLLIN,1,
     &                             %VAL(0))
      ELSE
        I=SMG$MOVE_VIRTUAL_DISPLAY(MAINID,PASTID,SPLLIN+1,1,%VAL(0))
      ENDIF
      IF(SPLSAV.NE.SPLLIN) THEN
C
C get rid of the viewport if one exists; create the virdisp if it
C     doesn't yet exist.
C
        IF (DISPLAY_CREATED) THEN
          I=SMG$DELETE_VIEWPORT(MINID1)
          IF (.NOT. I) CALL LIB$SIGNAL(%VAL(I))
c
c make sure that the display has at least SPLLIN-2 lines
c
          I = SMG$GET_DISPLAY_ATTR(MINID1, LINE,
     &                             %VAL(0),%VAL(0),%VAL(0),
     &                             %VAL(0),%VAL(0))
          IF (.NOT. I) CALL LIB$SIGNAL(%VAL(I))
          IF (LINE .LT. SPLLIN-2) THEN
            LINE = SMG$CURSOR_ROW(MINID1)
            I = SMG$CHANGE_VIRTUAL_DISPLAY(MINID1, SPLLIN-2, 
     &          PBCOLS-2,%VAL(0),%VAL(0),%VAL(0))
            IF (.NOT. I) CALL LIB$SIGNAL(%VAL(I))
            I = SMG$SET_CURSOR_ABS(MINID1, LINE,%VAL(0))
            IF (.NOT. I) CALL LIB$SIGNAL(%VAL(I))
          ENDIF
        ELSE
          I=SMG$CREATE_VIRTUAL_DISPLAY(SPLLIN-2,PBCOLS-2,
     &          MINID1,SMG$M_BORDER,%VAL(0),%VAL(0))
          IF (.NOT. I) CALL LIB$SIGNAL(%VAL(I))
          DISPLAY_CREATED = .TRUE.
        ENDIF
C
C create a viewport looking at the window, SPLLIN-2 lines high (not
C  counting borders).
C
        I = SMG$CREATE_VIEWPORT(
     &               MINID1,
     &               MAX(SMG$CURSOR_ROW(MINID1) - (SPLLIN-2) + 1, 1),
     &               1,
     &               SPLLIN-2,
     &               PBCOLS-2)
        IF (.NOT. I) CALL LIB$SIGNAL(%VAL(I))
C
        SPLSAV=SPLLIN
      ENDIF
      IF(STAFLG) THEN
        LIBPST=SMG$PASTE_VIRTUAL_DISPLAY(MINID1,PASTID,STALIN+1,2,
     &                                   MAINID)
        PBROWS=PBSAVE-SPLLIN-STALIN+1    ! Use only bottom part of screen
C                                        ! for menu display from now on
      ELSE
        LIBPST=SMG$PASTE_VIRTUAL_DISPLAY(MINID1,PASTID,2,2, MAINID)
        PBROWS=PBSAVE-SPLLIN
      ENDIF
      RETURN
      END
