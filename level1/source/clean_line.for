      SUBROUTINE CLEAN_LINE (LINE, LENGTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Cleans a line : keeps only alphanumerics
C-                         characters (A-Z, 0-9), and "(.:,-_)".
C-
C-   Inputs  : LINE : Character string to be cleaned;
C-             LENGTH : String LENGTH.
C-
C-   Outputs : LINE : Cleaned line.
C-
C-   Controls: None.
C-
C-   Created   7-FEB-1990   Sylvain Tisserant (MSU)
C-   Updated  18-OCT-1991   Philippe Laurens, Steven Klocek 
C-                            Removed extra RETURN statements to meet D0
C-                            standards.
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      CHARACTER*(*) LINE
      INTEGER       LENGTH
C
      CHARACTER     LINE_COPY*132, C1*1, C2*1, KEPT*80
      INTEGER       I, J, K, N
C
      DATA KEPT/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ()[]-_,.:*/=$; '/
C
C----------------------------------------------------------------------
C
      CALL UPCASE (LINE,LINE_COPY)
      CALL SWORDS (LINE_COPY,I,J,N)
      LENGTH = 0
      LINE = ' '
      DO J = 1, N
        C1 = LINE_COPY(I:I)
        IF(C1.EQ.'!') GOTO 999
        DO K = 1, 80
          C2 = KEPT(K:K)
          IF(C2.EQ.' ') GOTO 11
          IF(C1.EQ.C2) GOTO 10
        ENDDO
        GOTO 11
   10   LENGTH = LENGTH + 1
        LINE(LENGTH:LENGTH) = C1
   11   I = I + 1
      ENDDO
  999 RETURN
      END
