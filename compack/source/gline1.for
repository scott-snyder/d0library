      SUBROUTINE GLINE1(POS,LABLIN,PARLIN,MAXPAR,LINTOP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display a set of lines for GETDIS
C-
C-   Inputs  : POS: Current position within set of parameters
C-             LABLIN: Array of labels for the parameters
C-             PARLIN: Array of parameters converted to strings
C-             MAXPAR: Maximum number of parameters to use
C-             LINTOP: Counter for how many lines down from the top the display
C-                     has been scrolled.
C-   Outputs : None
C-
C-   Documented  13-JUN-1988   Jan S. Hoftun
C-   Modified     6-FEB-1991   Scott Snyder
C-    Revise POS-out-of-bounds logic; add display batching.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER POS,MAXPAR,LINTOP
      CHARACTER*(*) LABLIN(1:MAXPAR),PARLIN(1:MAXPAR)
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LINE,COLUMN,ISTAT,I,LIBCAR
      PARAMETER (COLUMN=5)
      INTEGER LIBERL,LIBPUT,LIBCUR,J,MAXITM,LOWCAR,TOPCAR,TRULEN
C----------------------------------------------------------------------
      LINE(I)=2*I+TOPCAR
      IF(PBROWS.GT.20) THEN
        MAXITM=PBROWS/2-4
        TOPCAR=3
        LOWCAR=PBROWS-3
      ELSE
        MAXITM=PBROWS/2-3
        TOPCAR=2
        LOWCAR=PBROWS-3
      ENDIF
      IF (POS .LE. LINTOP .OR. POS .GT. LINTOP + MAXITM) THEN
        LINTOP = POS-1
      ENDIF
      IF (LINTOP + MAXITM .GT. MAXPAR)
     &  LINTOP = MAX(MAXPAR - MAXITM, 0)
      IF (LINTOP .LT. 0 .OR. LINTOP .GE. MAXPAR)
     &  CALL ABOMEN(0, ' gline1')
      CALL LIBBON
      IF(LINTOP.GT.0) THEN
        ISTAT=LIBERL(TOPCAR,COLUMN)
        ISTAT=LIBCAR(TOPCAR,COLUMN,1)      ! Put carat on top
      ELSE
        ISTAT=LIBERL(TOPCAR,COLUMN)
      ENDIF
      DO I=1,MIN0(MAXPAR,MAXITM)
        ISTAT=LIBERL(LINE(I),COLUMN)
        IF(MOD(ISTAT,2).EQ.0) CALL ABOMEN(ISTAT,'LIBERL-->')
        ISTAT=LIBPUT(LABLIN(I+LINTOP),LINE(I),COLUMN,1)
        IF(MOD(ISTAT,2).EQ.0) CALL ABOMEN(ISTAT,'LIBPUT_1-->')
        ISTAT=LIBPUT(PARLIN(I+LINTOP),LINE(I),COLUMN+PBCOLS/2,0)
        IF(MOD(ISTAT,2).EQ.0) CALL ABOMEN(ISTAT,'LIBPUT_2-->')
      ENDDO
      IF((LINTOP+MAXITM).LT.MAXPAR) THEN
        ISTAT=LIBERL(LOWCAR,COLUMN)
        IF(MOD(ISTAT,2).EQ.0) CALL ABOMEN(ISTAT,' ')
        ISTAT=LIBCAR(LOWCAR,COLUMN,1)             ! Put carat on bottom
        IF(MOD(ISTAT,2).EQ.0) CALL ABOMEN(ISTAT,' ')
      ELSE
        ISTAT=LIBERL(LOWCAR,COLUMN)
        IF(MOD(ISTAT,2).EQ.0) CALL ABOMEN(ISTAT,' ')
      ENDIF
      I=PBCOLS/2
      I=I+COLUMN+MIN0(TRULEN(PARLIN(POS)),I-COLUMN)
      ISTAT=LIBCUR(LINE(POS-LINTOP),I)
      IF(MOD(ISTAT,2).EQ.0) CALL ABOMEN(ISTAT,' ')
      CALL LIBBOF
      RETURN
      END
