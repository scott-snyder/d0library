      SUBROUTINE JESCAP (ICODE, NINTEG, NREAL, ILIST, RLIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  An access path to hardware features of
C-                          Silicon Graphics processor.  
C-
C-   Inputs  : ICODE    [I]: Escape code number 
C-             NINTEG   [I]: Size of the integer array
C-             NREAL    [I]: Size of real array 
C-             ILIST [I(*)]: Integer array 
C-             RLIST [R(*)]: Real array
C-             
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  13-MAY-1992   Lupe Howell  and  Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESCAPE_CODE_NAMES.DEF'
      INTEGER  ICODE, NINTEG, NREAL, ILIST(*) 
      REAL     RLIST(*)
C----------------------------------------------------------------------
      INTEGER SEGN, IM, IC, IN
      CHARACTER*80 TITLE
C----------------------------------------------------------------------
C
      IF ( ICODE .LT. 0 .OR. ICODE .GT. 32767 ) THEN
        CALL ERRMSG('SGIDI3','JESCAP','ICODE OUTSIDE RANGE','W')
        GOTO 999
C
      ELSEIF ( ICODE .EQ. SETUP_3D ) THEN
C
      ELSEIF ( ICODE .EQ. ROTATE_3D ) THEN
        CALL DHTOC(80,RLIST,TITLE)
        CALL SGI_ROTATE_3D(NINTEG,ILIST,TITLE)
C
      ELSEIF ( ICODE .EQ. CLEAR_3D ) THEN

      ENDIF
C
  999 RETURN
      END
