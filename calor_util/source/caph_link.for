      SUBROUTINE CAPH_LINK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize link area for CAPH (LKCAPH
C                          common block)
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  18-Sep-1990   K. W. Merritt
C-   Updated  13-AUG-1991   Andrew J. Milder  Compatible with MicroDST MDST 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LKCAPH.INC'
C----------------------------------------------------------------------
      CHARACTER*4 PATH
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
C
C ****  Leave if ANLS type
C
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') GOTO 999
C
C ****  Create permanent link area and format block
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        ICAPH = 0
        CALL MZLINK (IXMAIN,'/LKCAPH/',JPROC,JPROC,KCAPH(MXCAPH))
      ENDIF
C
  999 RETURN
      END
