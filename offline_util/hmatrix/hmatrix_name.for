      SUBROUTINE HMATRIX_NAME(NAME,LBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Declare a new Hmatrix to Hmatrix package.
C-
C-   Inputs  : NAME     [C*]    Name of Hmatrix
C-             LBANK    [I]     Address of HMTR bank
C-            
C-   Outputs : None
C-   Controls: None
C-
C-   Created  24-JAN-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER LBANK
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:NHMATRIX.INC'
C----------------------------------------------------------------------
      INTEGER I,LL,ID
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        HMTR_TOTAL = 0
      ENDIF
C
C ****  First check if this is a unique name
C
      CALL HMATRIX_FIND(NAME(1:LEN(NAME)),LL,ID)
      IF ( ID .GT. 0 ) THEN
        CALL ERRMSG('HMATRIX','HMATRIX_NAME','Duplicate Hmatrix name',
     &    'W')
        GOTO 999
      ELSE
C
C ****  New Hmatrix name; find next available slot in
C ****  link area.
C
        ID = 0
        DO I =  1,HMTR_MAX
          IF ( HMTR_LINKS(I) .LE. 0 ) THEN
            ID = I
            GOTO 100
          ENDIF
        ENDDO
C
  100   CONTINUE
        IF ( ID .GT. 0 ) THEN
          HMTR_NAME(ID) = NAME(1:LEN(NAME))
          HMTR_LINKS(ID)= LBANK
          HMTR_TOTAL = HMTR_TOTAL + 1
        ELSE
          CALL ERRMSG('HMATRIX','HMATRIX_NAME','Too many Hmatrices','W')
        ENDIF
      ENDIF
  999 RETURN
      END
