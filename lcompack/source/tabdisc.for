      SUBROUTINE TABDISC(FMTSTR,DISLIN,TOPLAB,CARR,NUM,TYPE,LABEL,
     &  FMTIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display an array of variables in a dynamic
C-                         mode.
C-
C-   Inputs  : CARR:   Array of variables to display
C-             NUM:    Number of variables in array
C-             TYPE:   Type of variable in CARR
C-             FMTIN:  FORTRAN format descriptor in string.
C-   Outputs : DISLIN: Characters to output
C-   Controls: None
C-
C-   Created    8-OCT-1991   Herbert Greenlee
C-      This subroutine performs character variable manipulations for 
C-      TABDIS when that subroutine is called for character types.
C-      Note that the output parameters come first so that their lengths 
C-      can be passed correctly.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER NUM
      CHARACTER*(*) DISLIN(*), TOPLAB
      CHARACTER*(*) CARR(*)
      CHARACTER*1 TYPE
      CHARACTER*(*) LABEL,FMTIN
C
      INTEGER I, K
      CHARACTER*(*) FMTSTR
      CHARACTER*64 BLNK
      DATA BLNK/' '/
C----------------------------------------------------------------------
C
C     Set up display lines
C
      FMTSTR='('//FMTIN//')'
      IF(TYPE.EQ.'C') THEN
        DO I=1,NUM
          WRITE(DISLIN(I),FMT=FMTSTR) CARR(I)
        ENDDO
      ENDIF
      K=PBCOLS/4-(LEN(LABEL)+1)/2
      TOPLAB=' '
      WRITE(TOPLAB,101) BLNK(1:K),LABEL
  101 FORMAT(2A)
      RETURN
      END
