      SUBROUTINE LINES1(POS,MENLIN,MAXPOS,LINTOP,LINOLD,NUMCOL,LINSPA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display whole new page of menu items
C-
C-   Inputs  : POS:    Current position in display
C-             MENLIN: Array of menu items
C-             MAXPOS: Maximum number of items at this level
C-             NUMCOL: Number of columns in display
C-             LINSPA: Line spacing in display
C-   Outputs : LINTOP: New number of lines scrolled from top
C-             LINOLD: Old number of lines scrolled from top
C-   Controls: None
C-
C-   Documented  22-SEP-1988   Jan S. Hoftun
C-   Modified     6-FEB-1991   Scott Snyder
C-    Corrected TOPCAR and offsets in MAXITM calculation.
C-    Revised POS-out-of-bounds logic; added display batching.
C-   Modified    20-MAR-1991   Scott Snyder
C-    Subtract LINTOP from MAXPOS when deciding how many items
C-    to display.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER POS,MAXPOS,LINTOP,LINOLD,NUMCOL,LINSPA
      CHARACTER*(*) MENLIN(1:MAXPOS)
C&IF VAXVMS
C&ELSE
C&      CHARACTER*132 CTEMP
C&ENDIF
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LINE,COLUMN,ISTAT,I,CURONF,J,K,LIBCAR
      INTEGER LIBERL,LIBPUT,LIBCUR,MAXITM,LOWCAR,TOPCAR,TOPOFF
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
        TOPCAR=3
        TOPOFF=3
        LOWCAR=PBROWS-3
      ELSE
        MAXITM=((PBROWS-5)/LINSPA)*NUMCOL
        TOPCAR=2
        TOPOFF=2
        LOWCAR=PBROWS-2
      ENDIF
      IF(POS.EQ.1.OR.MAXPOS.LT.MAXITM) THEN
        LINTOP=0                            ! Reset LINTOP at the top of the menu
        LINOLD=0                            ! Reset LINOLD at the top of the menu
      ELSE IF (POS .LE. LINTOP .OR. POS .GT. LINTOP + MAXITM) THEN
        LINTOP = FIRST_COL(POS) - 1
      ENDIF
      IF (LINTOP + MAXITM .GT. LAST_COL(MAXPOS))
     &  LINTOP = MAX(LAST_COL(MAXPOS) - MAXITM, 0)
      IF (LINTOP .LT. 0 .OR. LINTOP .GE. MAXPOS)
     &  CALL ABOMEN(0, ' lines1')
      CALL LIBBON
      ISTAT=LIBERL(TOPCAR,COLUMN(1))
      IF(LINTOP.GT.0) THEN
        DO I=1,NUMCOL
          ISTAT=LIBCAR(TOPCAR,COLUMN(I),1)      ! Put indicator on top
        ENDDO
      ENDIF
      DO I=1,MIN(MAXPOS-lintop,MAXITM)
        ISTAT=LIBERL(LINE(I),COLUMN(I))
        K=I+LINTOP
        J=LINLEN(NUMCOL)
        J=MIN(LEN(MENLIN(K)),LINLEN(NUMCOL))
        ISTAT=LIBPUT(MENLIN(K)(1:J),LINE(I),COLUMN(I),0)
      ENDDO
      IF((LINTOP+MAXITM).LT.MAXPOS) THEN
        ISTAT=LIBERL(LOWCAR,COLUMN(1))
        ISTAT=LIBCAR(LOWCAR,COLUMN(1),1)             ! Put indicator on bottom
        IF(NUMCOL.GT.1) THEN
          IF(MAXPOS-(MAXITM+LINTOP).GT.1) THEN
            ISTAT=LIBCAR(LOWCAR,COLUMN(2),1)                ! Put indicator on bottom
          ELSE
            ISTAT=LIBERL(LOWCAR,COLUMN(2))
          ENDIF
        ENDIF
      ELSE
        ISTAT=LIBERL(LOWCAR,COLUMN(1))
      ENDIF
      J=MIN(LEN(MENLIN(POS)),LINLEN(NUMCOL))
C&IF VAXVMS
      ISTAT=LIBPUT('->'//MENLIN(POS)(1:J),LINE(POS-LINTOP),
     *      COLUMN(POS-LINTOP)-2,1)                       !Output highlited line
C&ELSE
C&      CTEMP = '->'//MENLIN(POS)(1:J)
C&      ISTAT=LIBPUT(CTEMP(1:J+2),LINE(POS-LINTOP),
C&     *      COLUMN(POS-LINTOP)-2,1)                       !Output highlited line
C&ENDIF
      ISTAT=LIBCUR(PBROWS-1,PBCOLS)                         !SOME TERMINALS CAN't turn off cursor
      ISTAT=CURONF(1)                                     !Turn off cursor if possible
      CALL LIBBOF
      RETURN
      END
