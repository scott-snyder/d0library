      SUBROUTINE TLINE1(POS,DISLIN,MAXPOS,LINTOP,LINOLD,NUMSPA,NUMCOL,
     *                  NEWPAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display a set of items in an ordered way on the
C-                         screen. Used in the TABDIS system.
C-
C-   Inputs  : POS: Current position in display
C-             DISLIN: Lines to be displayed
C-             MAXPOS: Number of lines to display
C-             LINTOP: Lines scrolled down from top
C-             LINOLD: Old count of scrolled lines
C-             NUMSPA: Spacing in display
C-             NUMCOL: Number of columns in display
C-             NEWPAG: Flag to indicate that a whole page should be scrolled
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Modified 16-AUG-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER POS,MAXPOS,LINTOP,LINOLD,NUMSPA,NUMCOL
      CHARACTER*(*) DISLIN(1:MAXPOS)
      LOGICAL NEWPAG
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LINE,COLUMN,ISTAT,I,CURONF,J,K,LIBCAR,TRULEN
      INTEGER LIBERL,LIBPUT,LIBCUR,MAXITM,LOWCAR,TOPCAR,TOPOFF
      INTEGER LINLEN,COLCAR,CU,COL1,MAXLEN
      LINE(I)=NUMSPA*((I+NUMCOL-1)/NUMCOL)+TOPOFF+1
      COLUMN(I)=COL1+MOD(I-1,NUMCOL)*(MAXLEN+3)
      COLCAR(I)=COLUMN(I)+TRULEN(DISLIN(I))/2-1           ! Put CARAT in
C                                                         ! middle of item
      TOPCAR=3
      MAXLEN=0
      DO I=1,MAXPOS
        J=TRULEN(DISLIN(I))
        IF(J.GT.MAXLEN) THEN
          MAXLEN=J
        ENDIF
      ENDDO
      COL1=MAX(3,(PBCOLS-NUMCOL*(MAXLEN+2))/2)
      LINLEN=(2*(PBCOLS-COL1))/NUMCOL
      IF(PBROWS.GT.20) THEN
        MAXITM=(PBROWS-7)/NUMSPA*NUMCOL
        TOPOFF=3
        LOWCAR=PBROWS-3
      ELSE
        MAXITM=(PBROWS-6)/NUMSPA*NUMCOL
        TOPOFF=2
        LOWCAR=PBROWS-2
      ENDIF
      IF(NEWPAG) THEN
        POS=POS+MAXITM
        IF(POS.GT.MAXPOS) THEN                 ! Set POS to beginning of last line
          IF(MOD(MAXPOS,NUMCOL).EQ.0) THEN
            POS=MAXPOS+1-NUMCOL
          ELSE
            POS=MAXPOS-MOD(MAXPOS,NUMCOL)+1
          ENDIF
        ENDIF
        LINTOP=POS-MAXITM+NUMCOL-1
        LINOLD=LINTOP
      ENDIF
      IF(POS.EQ.1.OR.MAXPOS.LT.MAXITM) THEN
        LINTOP=0                            ! Reset LINTOP at the top of the menu
        LINOLD=0                            ! Reset LINOLD at the top of the menu
      ELSEIF(POS.GT.MAXITM.AND.LINTOP.EQ.0) THEN     !Check if switched to smaller screen
        LINTOP=((POS-MAXITM)/NUMCOL)*NUMCOL+NUMCOL
        LINOLD=LINTOP
      ELSEIF(POS.LE.LINTOP) THEN     !Check if item is above current display
        DO WHILE (POS.LE.LINTOP)
          LINTOP=LINTOP-NUMCOL
          LINOLD=LINTOP
        ENDDO
      ENDIF
      ISTAT=LIBERL(TOPCAR,COLCAR(1))
      IF(LINTOP.GT.0) THEN
        DO J=1,NUMCOL
          ISTAT=LIBCAR(TOPCAR,COLCAR(J),0)      ! Put indicator on top
        ENDDO
      ENDIF
      DO I=1,MIN(MAXPOS-LINTOP,MAXITM)
        ISTAT=LIBERL(LINE(I),MAX0(COLUMN(I)-2,1))
        K=I+LINTOP
        J=MIN(LEN(DISLIN(K)),LINLEN)
        CU=COLUMN(I)
        ISTAT=LIBPUT(DISLIN(K)(1:J),LINE(I),CU,0)
      ENDDO
      ISTAT=LIBERL(LOWCAR,MAX0(COLUMN(1)-2,1))
      IF((LINTOP+MAXITM).LT.MAXPOS) THEN
        ISTAT=LIBCAR(LOWCAR,COLCAR(1),0)      ! Put indicator on BOTTOM
        DO J=2,NUMCOL
          IF(MAXPOS-(MAXITM+LINTOP).GT.1) THEN
            ISTAT=LIBCAR(LOWCAR,COLCAR(J),0) ! Put indicator on bottom
          ENDIF
        ENDDO
      ENDIF
      IF(POS.EQ.1) POS=POS+MAXITM-NUMCOL
      IF(MAXPOS.GT.MAXITM) THEN
        ISTAT=LIBPUT('->',LINE(POS-LINTOP),
     *      MAX0(COLUMN(POS-LINTOP)-2,1),1)     !Output cursor ('->')
      ENDIF
      ISTAT=LIBCUR(PBROWS-1,PBCOLS)             !Some terminals can't turn off cursor
      ISTAT=CURONF(1)                           !Turn off cursor if possible
      RETURN
      END
