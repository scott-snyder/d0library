      SUBROUTINE LINES(OLDPOS,POS,MENLIN,MAXPOS,LINTOP,LINOLD,NUMCOL,
     &                 LINSPA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Update menu display when moving from item to
C-                         item
C-
C-   Inputs  : OLDPOS: Old position to be un-selected
C-             POS:    New position to be selected
C-             MENLIN: Array of menu items
C-             MAXPOS: Maximum number of items at this level
C-             LINOLD: Old number of lines scrolled from top
C-             NUMCOL: Number of columns in display
C-             LINSPA: Line spacing in display
C-   Outputs : LINTOP: New number of lines scrolled from top
C-   Controls: None
C-
C-   Updated     26-OCT-1987   Jan S. Hoftun
C-   Documented  22-SEP-1988   Jan S. Hoftun
C-   Modified     6-FEB-1991   Scott Snyder
C-    Corrected offsets in MAXITM calculation. Rewrote scrolling logic.
C-   Updated     30-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER OLDPOS,POS,MAXPOS,LINTOP,LINOLD,NUMCOL,LINSPA
      CHARACTER*(*) MENLIN(1:MAXPOS)
      CHARACTER*132 CTEMP
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LINE,COLUMN,ISTAT,I,POSMAX,K
      INTEGER LIBERL,LIBPUT,LIBCUR,MIN,J,MAXITM,TOPOFF,CURONF
      INTEGER LINLEN, FIRST_COL, LAST_COL
      LINE(I)=LINSPA*((I+NUMCOL-1)/NUMCOL)+TOPOFF
      COLUMN(I)=PBCOLS/3/NUMCOL+MOD(I-1,NUMCOL)*
     *         ((PBCOLS-PBCOLS/3/NUMCOL)/NUMCOL+1)
      LINLEN(I)=(PBCOLS-COLUMN(1))/I-3
C
C first_col(i) = the number of the first item in the line containing item i
C last_col(i)  = the number of the last  item in the line containing item i
C
      FIRST_COL(I) = I - MOD(I-1, NUMCOL)
      LAST_COL(I) = FIRST_COL(I) + NUMCOL-1
C----------------------------------------------------------------------
      IF(PBROWS.GT.20) THEN
        MAXITM=((PBROWS-7)/LINSPA)*NUMCOL
        TOPOFF=3
      ELSE
        MAXITM=((PBROWS-5)/LINSPA)*NUMCOL
        TOPOFF=2
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
C note that `pos - mod(pos-1, numcol)' is the number of the first item
C  in the line containing item POS.
C
      IF (POS .LE. LINTOP .OR. POS .GT. POSMAX) THEN
        IF (POS .LT. OLDPOS) THEN
          LINTOP = FIRST_COL(POS) - 1
        ELSE
          LINTOP = LAST_COL(POS) - MAXITM
        ENDIF
        IF (LINTOP + MAXITM .GT. LAST_COL(MAXPOS))
     &    LINTOP = MAX(LAST_COL(MAXPOS) - MAXITM, 0)
        IF (LINTOP .LT. 0 .OR. LINTOP .GE. MAXPOS)
     &    CALL ABOMEN(0, ' lines')
        LINOLD=LINTOP
        CALL LINES1(POS, MENLIN, MAXPOS, LINTOP, LINOLD, NUMCOL, LINSPA)
      ELSE
        K=MIN(LEN(MENLIN(OLDPOS)),LINLEN(NUMCOL))
C&IF VAXVMS
        ISTAT=LIBPUT('  '//MENLIN(OLDPOS)(1:K),LINE(OLDPOS-LINTOP),
     *      COLUMN(OLDPOS-LINTOP)-2,0)                       !Output NOT highlighted line
C&ELSE
C&        CTEMP = '  '//MENLIN(OLDPOS)(1:K)
C&        ISTAT=LIBPUT(CTEMP(1:K+2),LINE(OLDPOS-LINTOP),
C&     *      COLUMN(OLDPOS-LINTOP)-2,0)                       !Output NOT highlighted line
C&ENDIF
        I=LINE(POS-LINTOP)
        J=COLUMN(POS-LINTOP)
        K=MIN(LEN(MENLIN(POS)),LINLEN(NUMCOL))
C&IF VAXVMS
        ISTAT=LIBPUT('->'//MENLIN(POS)(1:K),I,J-2,1)             !OUTPUT HIGHLITED LINE
C&ELSE
C&        CTEMP = '->'//MENLIN(POS)(1:K)
C&        ISTAT=LIBPUT(CTEMP(1:K+2),I,J-2,1)             !OUTPUT HIGHLITED LINE
C&ENDIF
        ISTAT=LIBCUR(PBROWS,PBCOLS-1)                         !SOME TERMINALS CAN't turn off cursor
C         ISTAT=CURONF(1)                                     !Turn off cursor if possible
      ENDIF
      RETURN
      END
