C&IF VAXVMS
      SUBROUTINE TABDIS(IARR,NUM,NUMSPA,NMBCOL,TYPE,LABEL,FMTIN)
C&ELSE
C&      SUBROUTINE TABDIS(CARR,NUM,NUMSPA,NMBCOL,TYPE,LABEL,FMTIN)
C&ENDIF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display an array of variables in a dynamic
C-                         mode.
C-
C-   Inputs  : IARR:   Array of variables to display
C-             NUM:    Number of variables in array
C-             NUMSPA: Line spacing in display
C-             NMBCOL: Number of columns in display
C-             TYPE:   Type of variable in IARR
C-             LABEL:  Label for top of display
C-             FMTIN:  FORTRAN format descriptor in string.
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated     8-OCT-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUM,NUMSPA,NMBCOL
C&IF VAXVMS
      INTEGER IARR(*)
      CHARACTER*1 TYPE
C&ELSE
C&      CHARACTER*(*) CARR(NUM), TYPE
C&ENDIF
      CHARACTER*(*) LABEL,FMTIN
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      CHARACTER*16 PFSTR(4)
      CHARACTER*80 DISLIN(200),INDUM
      LOGICAL ASTSAV
      INTEGER COMUSE,LOCPOS,LINTOP,LINOLD,LIBSCR,LENINT,POSO,LIBCUR
      INTEGER TRULEN,ISTAT,LIBPUT,I,LIBBIG,NUMUSE
      INTEGER READPF,LIBGET,MAXUSE,IS,ISO,K
      INTEGER IX,CURONF
      REAL X
      EQUIVALENCE (X,IX)
      CHARACTER*12 FMTSTR
      CHARACTER*64 TOPLAB,BLNK
      CHARACTER*80 CHA1
C&IF VAXVMS
C&ELSE
C&      INTEGER XLEN
C&ENDIF
      DATA BLNK/' '/
C----------------------------------------------------------------------
C
C     Save ASTFLG and set it to false for use in this routine
C
      ASTSAV=ASTFLG
      ASTFLG=.FALSE.
C
C     Set up display lines
C
C&IF VAXVMS
      FMTSTR='('//FMTIN//')'
      IF(TYPE.EQ.'I') THEN
        DO I=1,NUM
          WRITE(DISLIN(I),FMT=FMTSTR) IARR(I)
        ENDDO
      ELSEIF(TYPE.EQ.'R') THEN
        DO I=1,NUM
          IX=IARR(I)
          WRITE(DISLIN(I),FMT=FMTSTR) X
        ENDDO
      ELSEIF(TYPE.EQ.'C') THEN
        DO I=1,NUM
          CALL MOVPRT(IARR,I,CHA1)
          WRITE(DISLIN(I),FMT=FMTSTR) CHA1
        ENDDO
      ENDIF
C&ELSE
C&C-
C&C-  The following calls fill character variables FMTSTR, DISLIN and
C&C-  TOPLAB on UNIX.  No references to IARR, TYPE, LABEL or FMTIN are 
C&C-  allowed outside of VAXVMS machine blocks, except in the following 
C&C-  calls.
C&C-
C&      IF(TYPE.EQ.'C')THEN
C&        CALL TABDISC(FMTSTR,DISLIN,TOPLAB,CARR,NUM,TYPE,LABEL,FMTIN)
C&      ELSE
C&        CALL TABDISN(FMTSTR,DISLIN,TOPLAB,CARR,NUM,TYPE,LABEL,FMTIN)
C&      ENDIF
C&ENDIF
C
C     Display first part of display on screen
C
      PF=0
      LOCPOS=1
      LINTOP=0
      LINOLD=0
      IS=1
      ISO=0
      MAXUSE=MIN(NUM,NMBCOL*((PBROWS-3)/NUMSPA))
      DO WHILE (PF.NE.4.AND.IS.LE.NUM)
        CALL TLINE0(DISLIN(IS),MAXUSE,NUMSPA,NMBCOL)
        IF(MAXUSE.LT.NUM.AND.(IS+MAXUSE).LT.NUM) THEN
c          IF(ISO.EQ.IS.AND.PF.NE.4) THEN
c            CALL OUTMSG('0You have seen it ALL, will do a BACK')
c            CALL CURLIN('{<CR> to continue}',PF,LOCPOS,NUM,INDUM)
c            PF=4
c          ELSE
            CALL CURLIN('{<CR> for next part, BACK to end}',
     *         PF,LOCPOS,NUM,INDUM)
            ISO=IS
C           IS=MIN0(IS+MAXUSE,NUM-MAXUSE+1)
            IS=IS+MAXUSE
            IF((IS+MAXUSE).GT.NUM) THEN
              MAXUSE=NUM-IS+1
            ENDIF
c          ENDIF
        ELSE
          PF=4
        ENDIF
      ENDDO
      PF=0
C
C     Restore ASTFLG
C
      ASTFLG=ASTSAV
      RETURN
      END
