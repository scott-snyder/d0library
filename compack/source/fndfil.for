      SUBROUTINE FNDFIL(POSI,MAXITM,FILTYP,TOPS,ITEMS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find a present a list of files
C-
C-   Inputs  : MAXITM: Maximum number of files to look for
C-             FILTYP: Name (with wildcards) of files to look for
C-             TOPS:   Title to put on top of display
C-   Outputs : POSI:   Number of the selected file.
C-             ITEMS:  Array of file names.
C-   Controls: None
C-
C-   Modified 22-SEP-1988   Jan S. Hoftun
C-   Modified 27-NOV-1990 Scott Snyder
C-    when the user types KP0 or ENTER to CURSOR, it regenerates the
C-    last menu displayed, wiping out the data we're trying to display!
C-    so replace call to CURSOR with CURSO1 and regenerate the display
C-    ourself should it become necessary.
C-   Modified  4-FEB-1991 Scott Snyder
C-    The cursor gets turned off in LINES1, but this routine never
C-    deigns to reenable it! Turn it back on when we're done.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER POSI,MAXITM
      CHARACTER*(*) TOPS,ITEMS(*),FILTYP
      CHARACTER*132 CTEMP
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      CHARACTER*80 INFILE,TRANUP
      INTEGER ISTAT,ITOP,IOLD,COLS
      INTEGER LIBBIG,LIBERA,LIBCUR,I,K, CURONF
      INTEGER MAXFIL,POSO,LIBSCR,LOCSPA,TRULEN
      LOGICAL REGEN_NEEDED
      CHARACTER*132 BLANK,NEWTOP
      DATA BLANK/' '/
C----------------------------------------------------------------------
      CALL FILLST(FILTYP,MAXITM,MAXFIL,ITEMS)
      IF(MAXFIL.EQ.0) THEN
        IF(FULSCR) THEN
          ISTAT=LIBERA(1,1)
          K=PBCOLS/4-TRULEN(TOPS)/2
          NEWTOP=BLANK(1:MAX0(K,1))//TOPS
          ISTAT=LIBBIG(NEWTOP,1,1,3)
          CALL PFLABL(' ',' ',' ','BACK')
          ISTAT=LIBCUR(3,1)
        ENDIF
C&IF VAXVMS
        CALL INTMSG('0No files satisfying  '//FILTYP(1:TRULEN(FILTYP))//
     &              '  available!'//CHAR(7))
C&ENDIF
C&IF SIUNIX,IBMAIX,ULTRIX,ALFOSF
C&        CTEMP = '0No files satisfying  '//FILTYP(1:TRULEN(FILTYP))//
C&     &          '  available!'//CHAR(7)
C&        CALL INTMSG(CTEMP)
C&ENDIF
        CALL INTMSG(' ')
        PF=4
      ELSE
        ITOP=0
        IOLD=0
        PF=0
        POSI=1
  100   CONTINUE
        IF(PF.EQ.0) THEN
          IF(MAXFIL.LT.5) THEN
            COLS=1
            LOCSPA=2
          ELSEIF(MAXFIL.LT.16) THEN
            COLS=2
            LOCSPA=2
          ELSE
            COLS=2
            LOCSPA=1
          ENDIF
          IF(FULSCR) THEN
            ISTAT=LIBERA(1,1)
            K=PBCOLS/4-TRULEN(TOPS)/2
            NEWTOP=BLANK(1:MAX0(K,1))//TOPS
            ISTAT=LIBBIG(NEWTOP,1,1,3)
            CALL PFLABL('RUN','HELP','MENU','BACK')
            CALL LINES1(POSI,ITEMS,MAXFIL,ITOP,IOLD,COLS,LOCSPA)
  200       CONTINUE
            POSO=POSI
            CALL CURSO1(PF, POSI, MAXFIL, COLS, REGEN_NEEDED)
            IF (REGEN_NEEDED) THEN
              ISTAT=LIBBIG(NEWTOP,1,1,3)
              CALL PFLABL('RUN','HELP','MENU','BACK')
              CALL LINES1(POSI,ITEMS,MAXFIL,ITOP,IOLD,COLS,LOCSPA)
            ENDIF
            IF(POSI.NE.POSO.AND.PF.EQ.0) THEN
              CALL LINES(POSO,POSI,ITEMS,MAXFIL,ITOP,IOLD,COLS,LOCSPA)
            ENDIF
            IF(PF.EQ.0) GOTO 200
            ISTAT = CURONF(0)   ! make sure cursor is on again.
          ELSE
            IF(TRMFLG) THEN
              CALL LINES0(ITEMS,MAXFIL,COLS)
            ENDIF
            CALL CURLIN('{name, #, HELP (#), MENU, BACK}',
     *                     PF,POSI,MAXFIL,INFILE)
            IF(PF.EQ.0.OR.PF.EQ.1.AND.POSI.EQ.0) THEN
              INFILE=TRANUP(INFILE)
              DO I=1,MAXFIL
                IF(INFILE.EQ.ITEMS(I)(1:TRULEN(ITEMS(I)))) THEN
                  PF=1
                  POSI=I
                  GOTO 301
                ENDIF
              ENDDO
C
C     Here only when no match found, set PF=0 to indicate that back
C
              PF=0
              CALL OUTMSG('0Invalid choice: '//INFILE//CHAR(7))
              CALL OUTMSG(' ')
  301         CONTINUE
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
