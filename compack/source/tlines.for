      SUBROUTINE TLINES(OLDPOS,POS,DISLIN,MAXPOS,LINTOP,LINOLD,
     *           NUMSPA,NUMCOL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put cursor (->) in 'correct' place and possibly
C-                         scroll display. Used in TABDIS system.
C-
C-   Inputs  : OLDPOS: Old position within list
C-             POS: Current position in display
C-             DISLIN: Lines to be displayed
C-             MAXPOS: Number of lines to display
C-             LINTOP: Lines scrolled down from top
C-             LINOLD: Old count of scrolled lines
C-             NUMSPA: Spacing in display
C-             NUMCOL: Number of columns in display
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Modified 16-AUG-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER OLDPOS,POS,MAXPOS,LINTOP,LINOLD,NUMSPA,NUMCOL
      CHARACTER*(*) DISLIN(1:MAXPOS)
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LINE,COLUMN,ISTAT,I,POSMAX,K,COL1,TRULEN,MAXLEN
      INTEGER LIBERL,LIBPUT,LIBCUR,MIN,J,MAXITM,TOPOFF,CURONF
      LINE(I)=NUMSPA*((I+NUMCOL-1)/NUMCOL)+TOPOFF+1
      COLUMN(I)=COL1+MOD(I-1,NUMCOL)*(MAXLEN+3)
      MAXLEN=0
      DO I=1,MAXPOS
        J=TRULEN(DISLIN(I))
        IF(J.GT.MAXLEN) THEN
          MAXLEN=J
        ENDIF
      ENDDO
      COL1=MAX(3,(PBCOLS-NUMCOL*(MAXLEN+2))/2)
      IF(PBROWS.GT.20) THEN
        MAXITM=(PBROWS-7)/NUMSPA*NUMCOL
        TOPOFF=3
      ELSE
        MAXITM=(PBROWS-6)/NUMSPA*NUMCOL
        TOPOFF=2
      ENDIF
      IF(MAXPOS.GT.MAXITM) THEN
        IF(POS.GT.OLDPOS.AND.POS.LE.NUMCOL+1) THEN
          POSMAX=1                                            !Reset posmax at the top of the menu
          LINTOP=0                                            !Reset lintop at the top of the menu
        ENDIF
        IF(POS.LE.LINTOP) THEN
          LINTOP=LINTOP-NUMCOL                                !Scroll down
          POSMAX=POSMAX-1
        ENDIF
        IF(POS.GT.MAXITM.AND.POS.GE.(OLDPOS+NUMCOL)
     *   .AND.POSMAX.LT.POS) THEN                            !Scroll lines when POS is increasing
          LINTOP=LINTOP+NUMCOL
          POSMAX=POS
        ENDIF
        IF(LINTOP.NE.LINOLD) THEN                              !Redraw menu when needed
          LINOLD=LINTOP
          CALL TLINE1(POS,DISLIN,MAXPOS,LINTOP,LINOLD,NUMSPA,NUMCOL,
     *               .FALSE.)
        ELSE
          ISTAT=LIBPUT('  ',LINE(OLDPOS-LINTOP),
     *      MAX0(COLUMN(OLDPOS-LINTOP)-2,1),0)                       !Output NOT highlighted line
          I=LINE(POS-LINTOP)
          J=COLUMN(POS-LINTOP)
          ISTAT=LIBPUT('->',I,MAX0(J-2,1),1)                  !Output highlited line
          ISTAT=LIBCUR(PBROWS,PBCOLS-1)                       !Some terminals can't turn off cursor
          ISTAT=CURONF(1)                                     !Turn off cursor if possible
        ENDIF
      ENDIF
      RETURN
      END
