      INTEGER FUNCTION LGTYPE(DUM)
C--------------------------------------------------------------------------
C
C     THIS FUNCTION SPECIFIES THE GEOMETRY RUN, TYPE, VERSION DEPENDENT
C     PARAMETERS USED IN CALGEO PROGRAM.
C     THIS PARTICULAR VERSION DESCRIBES THE STANDARD D0 GEOMETRY.
C
C     AUTHOR:   S KAHN      2 JULY 1987
C
C---------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CGHEAD.DEF'
      REAL VERSUN, DUM
C
      LGTYPE = IHSTRD
C
      RETURN
C
      ENTRY VERSUN(DUM)
C
      VERSUN = 1.0
C
      RETURN
      END
