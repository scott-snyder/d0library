      SUBROUTINE SKF_SORT_HITS(N,DIR,Z,U,SI,CO,SIG,THET,ELOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Simultaneousely sorts arrays Z,U,SI,CO,SIG,THET
C-                          so that Z becomes sorted in ascending or
C-                          descending order, depending on DIR.
C-                          DIR=0 --  Ascending
C-                          DIR=1 --  Descending
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-MAR-1994   Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER DIR,N,DESCORD
      REAL    Z(*),U(*),SI(*),CO(*),SIG(*),THET(*),
     +        ELOS(*)
C<<
      INTEGER MAXN
      PARAMETER (MAXN=200)
      INTEGER INDEX(MAXN)
C<<
      DESCORD = 1
      IF( DIR.EQ.2 ) DESCORD = 0
C<<
      CALL  SORTZV(Z,INDEX,N,1,DESCORD,0)
C<<
      CALL  SKF_REORDER(Z,1,n,INDEX)
      CALL  SKF_REORDER(U,1,n,INDEX)
      CALL  SKF_REORDER(SI,1,n,INDEX)
      CALL  SKF_REORDER(CO,1,n,INDEX)
      CALL  SKF_REORDER(SIG,1,n,INDEX)
      CALL  SKF_REORDER(THET,1,n,INDEX)
      CALL  SKF_REORDER(ELOS,1,N,INDEX)
C<<
  999 RETURN
      END
