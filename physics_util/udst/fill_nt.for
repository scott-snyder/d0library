      SUBROUTINE FILL_NT(X,NDIM,PASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill event in output ntuple
C-
C-   Inputs  : X     - input event vector
C-             NDIM  - dimension of X
C-             PASS  - .true.=filled in ntuple, .false.=rejected
C-                      (enables calling routine to count accepted
C-                       events if a selection is mad in this routine)
C-
C-   Created   2-OCT-1992   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NPRIME,ID,IDX,NDIM,NDIMX,ichk
      REAL X(NDIM),rchk
      CHARACTER*8 NAMES(NDIMX)
      equivalence(rchk,ichk)
      LOGICAL PASS
C
      PASS=.TRUE.
      rchk = x(5)
      CALL HFN(ID,X)
      RETURN
C
      ENTRY BOOK_NT(IDX,NAMES,NDIMX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book output ntuple
C-
C-   Inputs  : IDX   - id number of ntuple
C-             NAMES - array of tags
C-             NDIMX - number of variables in ntuple
C-
C-   Created   2-OCT-1992   Ulrich Heintz
C-
C----------------------------------------------------------------------
      NPRIME=100*NDIMX
      NPRIME=MIN(10000,NPRIME)
      ID=IDX
      CALL HBOOKN(ID,'uDST',NDIMX,'OUTPUT',NPRIME,NAMES)
C----------------------------------------------------------------------
  999 RETURN
      END
