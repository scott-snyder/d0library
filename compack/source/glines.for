      SUBROUTINE GLINES(OLDPOS,POS,LABLIN,PARLIN,MAXPAR,LINTOP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Update parameter display for GETDIS
C-
C-   Inputs  : OLDPOS: Position within display to be un-selected
C-             POS:    Position within display to be selected
C-             LABLIN: Array of labels for each parameter
C-             MAXPAR: Maximum number of parameters used
C-             LINTOP: Counter for number of lines scrolled from top
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Modified (corrected ?) : 30-NOV-1989     S. Tisserant (MSU)
C-   Modified 24-JAN-1991 Scott Snyder:
C-     Moved assignment to LINOLD to after the call to GLINES1, since
C-     GLINES1 can change LINTOP.
C-   Modified  5-FEB-1991 Scott Snyder
C-     Add a clear-to-EOL after the LIBPUT call to flush any erroneous
C-     input the user may have entered.
C-   Modified  6-FEB-1991 Scott Snyder
C-     Rewrite scrolling logic. (correctly?)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER OLDPOS,POS,MAXPAR,LINTOP
      CHARACTER*(*) LABLIN(1:MAXPAR),PARLIN(1:MAXPAR)
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LINE,COLUMN,ISTAT,I,POSMAX
      PARAMETER (COLUMN=5)
      INTEGER LIBERL,LIBPUT,LIBCUR,J,MAXITM,TOPCAR,TRULEN
      LINE(I)=2*I+TOPCAR
C----------------------------------------------------------------------
      IF(PBROWS.GT.20) THEN
        MAXITM=(PBROWS/2-4)
        TOPCAR=3
      ELSE
        MAXITM=(PBROWS/2-3)
        TOPCAR=2
      ENDIF
C
C MAXITM = the number of items which can be displayed.
C LINTOP = number of items scrolled off the top of the screen
C        = number of top item in display - 1
C POSMAX = number of last item in display
C
      POSMAX = LINTOP + MAXITM
C
C if the user has moved out of the bounds of the display, reposition it.
C  place the cursor either at the top or bottom of the display, depending
C  on whether the cursor was moved up or down.
C
      IF (POS .LE. LINTOP .OR. POS .GT. POSMAX) THEN
        IF (POS .LT. OLDPOS) THEN
          LINTOP = POS-1
        ELSE
          LINTOP = POS - MAXITM
        ENDIF
        IF (LINTOP + MAXITM .GT. MAXPAR)
     &    LINTOP = MAX(MAXPAR - MAXITM, 0)
        IF (LINTOP .LT. 0 .OR. LINTOP .GE. MAXPAR)
     &    CALL ABOMEN(0, ' GLINES')
        CALL GLINE1(POS, LABLIN, PARLIN, MAXPAR, LINTOP)
      ELSE
        IF(POS.NE.OLDPOS) THEN
          I=LINE(OLDPOS-LINTOP)
          ISTAT=LIBPUT(PARLIN(OLDPOS),I,COLUMN+PBCOLS/2,0)
          ISTAT = LIBERL(I, COLUMN + PBCOLS/2 + TRULEN(PARLIN(OLDPOS)))
        ENDIF
        I=LINE(POS-LINTOP)
        J=PBCOLS/2
        J=J+COLUMN+MIN0(TRULEN(PARLIN(POS)),J-COLUMN)
        ISTAT=LIBCUR(I,J)
      ENDIF
      RETURN
      END
