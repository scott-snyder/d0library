      SUBROUTINE JESCAP (ICODE, NINTEG, NREAL, ILIST, RLIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  An access path to hardware features of
C-                          Evan & Sutherland processor.  Enables
C-                          hardware image transformations using
C-                          Dials and Function keys.
C-
C-   Inputs  : ICODE, NINTEG, NREAL, ILIST, RLIST
C-   Outputs : NINTEG, NREAL, ILIST, RLIST( if ICODE=ROTATE_3D )
C-   Controls: NONE
C-
C-   Created   08-FEB-1989   SHAHRIAR ABACHI
C-   updates   15-JUL-1990   SHAHRIAR ABACHI   New escape codes added (10,11)
C-   updates   10-FEB-1990   SHAHRIAR ABACHI   New escape codes added (13,14)
C-   updates   07-NOV-1990   SHAHRIAR ABACHI   New escape codes added (16)
C-   updates   21-OCT-1991   SHAHRIAR ABACHI   New escape codes added (19,20,21)
C-   Updated  13-MAY-1992   Lupe Howell and Harrison B. Prosper New escape added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESCAPE_CODE_NAMES.DEF'
      INTEGER  ICODE, NINTEG, NREAL, ILIST(*), SEGN
      INTEGER IM, IC, IN
      REAL     RLIST(*)
C
      IF ( ICODE .LT. 0 .OR. ICODE .GT. 32767 ) THEN
        CALL ERROR(' FROM JESCAP:  ICODE OUTSIDE RANGE ')
        GOTO 999
C
      ELSEIF ( ICODE .EQ. SETUP_3D ) THEN
        CALL ECONNT
C
      ELSEIF ( ICODE .EQ. CONNECT_3D ) THEN
        CALL ECONNS (NINTEG, ILIST, NREAL, RLIST)
C
      ELSEIF ( ICODE .EQ. DISCONNECT_3D ) THEN
        CALL EDISCS (NINTEG, ILIST)
C
      ELSEIF ( ICODE .EQ. END_3D ) THEN
        CALL EDISCT
C
      ELSEIF ( ICODE .EQ. RESET_3D ) THEN
        CALL ERESET (NINTEG, ILIST, NREAL, RLIST)
C
      ELSEIF ( ICODE .EQ. REMOVE_SEGMENT ) THEN
        CALL EREMOV (NINTEG, ILIST)
C
      ELSEIF ( ICODE .EQ. INSERT_SEGMENT ) THEN
        CALL EINSRT (NINTEG, ILIST)
C
      ELSEIF ( ICODE .EQ. NOROTATE ) THEN
        CALL ENOROT
C
      ELSEIF ( ICODE .EQ. GET_3D_MATRIX ) THEN
        SEGN = ILIST(1)
        CALL EGETMX(SEGN, RLIST)
C
      ELSEIF ( ICODE .EQ. FLAG_3D_MATRIX ) THEN
        SEGN = ILIST(1)
        CALL EMAT(SEGN)
C
      ELSEIF ( ICODE .EQ. RENDERING_MENU ) THEN
        CALL ERNMEN(NINTEG, ILIST)
C
      ELSEIF ( ICODE .EQ. TRANSFORM_3D_VECTOR ) THEN
        IC = ILIST(1)
        IM = ILIST(2)
        CALL EMULMV(IC, IM, RLIST)
C
      ELSEIF ( ICODE .EQ. STORE_VIEWP_WINDO ) THEN
        IN = ILIST(1)
        CALL ESTOREVW(IN)
C
      ELSEIF ( ICODE .EQ. RECAL_VIEWP_WINDO ) THEN
        IN = ILIST(1)
        CALL ERECALVW(IN)
C
      ELSEIF ( ICODE .EQ. RESET_VIEWP_WINDO ) THEN
        CALL ERESETVW
C
      ELSEIF ( ICODE .EQ. FLUSH_BUFFER ) THEN
        CALL EFLUSH
C
      ELSEIF ( ICODE .EQ. ROTATE_3D ) THEN
        CALL EAS_ROTATE_3D(NINTEG,NREAL,ILIST,RLIST)
C
C.      ELSEIF ( ICODE .EQ. CLEAR_3D ) THEN
C.        CALL EAS_CLEAR_3D
      ENDIF
C
  999 RETURN
      END
