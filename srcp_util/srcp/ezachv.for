      SUBROUTINE EZACHV(CHARR,REMARK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-     Add a parameter, which has space for a 32-CHARACTER STRING,
C-     to an existing SRCP bank. If many parameters are to be added
C-     to an SRCP bank it is more efficient to call EZFILL once per
C-     new parameter and then to call EZEND once at the end. EZACHV
C-     calls EZFILL and EZEND.
C-
C-   Inputs  : CHARR    [C*]    Parameter name
C-             REMARK   [C*]    Short remark
C-   Outputs : None
C-   Controls: None
C-
C-   Created   5-JUN-1989   Tami Kramer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INTEGER NWORD,I
      PARAMETER (NWORD=8)
      REAL ARRAY(NWORD)
      CHARACTER*1 TYPE(NWORD)
      CHARACTER*(*) CHARR
      CHARACTER*(*) REMARK
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST,ARRAY,TYPE
C----------------------------------------------------------------------
C
C  Executable Code:
C  =================
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        DO 10 I = 1, NWORD
          TYPE(I) = 'H'
          ARRAY(I) = 0
   10   CONTINUE
      ENDIF
C
      I = LEN(CHARR)
      CALL EZFILL(CHARR(1:I),REMARK,ARRAY,TYPE,NWORD)
      CALL EZEND
C
  999 RETURN
      END
