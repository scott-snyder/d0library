C DEC/CMS REPLACEMENT HISTORY, Element ISASRT.FOR
C *2    22-DEC-1989 12:09:12 CSTEWART "Chip Stewart: Same as SRTFLT but for ISAJET code"
C *1    21-DEC-1989 14:38:53 SERBAN "sort characters"
C DEC/CMS REPLACEMENT HISTORY, Element ISASRT.FOR
      SUBROUTINE ISASRT(X,NCH,IMAP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sorts a floating point array X into ascending order.
C-                         The array IMAP contains ordered list of pointers
C-
C-   Inputs  : X - Floating point array
C-             NCH  - Number of elements in X
C-   Outputs : IMAP - pointer to ordered list in X
C-   Controls: None
C-
C-   Created   3-OCT-1988   Rajendran Raja
C-   Based on the Algorithm of D.L.Shell, High speed sorting
C-   procedure , Communications of the ACM, Vol 2, July 1959, PP 30-32
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL X(*)
      REAL TEMP
      INTEGER IMAP(*),NCH,M,I,J,K,IM,IT
C----------------------------------------------------------------------
      M=NCH
   10 M=M/2       !binary chop
      IF(M.EQ.0)GO TO 999
      K=NCH-M
      J=1
   20 I=J
   30 IM=I+M
      IF(X(I).LE.X(IM))GO TO 40
      TEMP = X(I)
      X(I) = X(IM)
      X(IM) = TEMP
      IT = IMAP(I)
      IMAP(I)=IMAP(IM)
      IMAP(IM)=IT
      I = I-M
      IF(I.GE.1)GO TO 30
   40 J=J+1
      IF(J.GT.K)GO TO 10
      GO TO 20
  999 RETURN
      END
