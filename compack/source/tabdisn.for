      SUBROUTINE TABDISN(FMTSTR,DISLIN,TOPLAB,IARR,NUM,TYPE,LABEL,
     &  FMTIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display an array of variables in a dynamic
C-                         mode.
C-
C-   Inputs  : IARR:   Array of variables to display
C-             NUM:    Number of variables in array
C-             TYPE:   Type of variable in IARR
C-             FMTIN:  FORTRAN format descriptor in string.
C-   Outputs : DISLIN: Characters to output
C-   Controls: None
C-
C-   Created    8-OCT-1991   Herbert Greenlee
C-      This subroutine performs character variable manipulations for 
C-      TABDIS when that subroutine is called for numeric types.
C-      Note that the output parameters come first so that their lengths 
C-      can be passed correctly.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER NUM
      CHARACTER*(*) DISLIN(*), TOPLAB
      INTEGER IARR(*)
      CHARACTER*1 TYPE
      CHARACTER*(*) LABEL,FMTIN
C
      INTEGER I, K
      INTEGER IX
      REAL X
      EQUIVALENCE (X,IX)
      CHARACTER*(*) FMTSTR
      CHARACTER*64 BLNK
      DATA BLNK/' '/
C----------------------------------------------------------------------
C
C     Set up display lines
C
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
      ENDIF
      K=PBCOLS/4-(LEN(LABEL)+1)/2
      TOPLAB=' '
      WRITE(TOPLAB,101) BLNK(1:K),LABEL
  101 FORMAT(2A)
      RETURN
      END
