      SUBROUTINE TOP_LEPTONS_REAL_MATRIX_SORT(ARRAY,NVect_Items,
     &  Max_Vects,Sort_Loc)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a floating point 2D Array dimensioned
C-           Array(NVect_Items,Max_Vects), sort the vectors on the 
C-           SORT_LOCth location.  If Sort_Loc>0 increasing order.
C-                                 If Sort_Loc<0 decreasing order.
C-                 EXAMPLE: Array(4,3)=> (PX1,PY1,PZ1,PT1)
C-                                       (PX2,PY2,PZ2,PT2)
C-                                       (PX3,PY3,PZ3,PT3)
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  12-FEB-1993   joey thompson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NVect_Items,Max_Vects,Sort_Loc
      INTEGER WORK_NVECT_Items,WORK_Max_Vects
      REAL     ARRAY(*)
      PARAMETER (WORK_NVect_Items = 100)
      PARAMETER (WORK_Max_Vects  = 200)
C
      INTEGER I,INDEX(WORK_Max_Vects),MODE,NWAY,NSORT
      REAL    WORKLIST(WORK_Max_Vects)
      REAL    WORKARRAY(WORK_NVECT_Items*WORK_Max_Vects)
C----------------------------------------------------------------------
C
C ****  Jump out if inputs are not consistent
C
      IF (Max_Vects.LT.1 .OR. NVect_Items.LT. 1 .OR.
     1    IABS(Sort_Loc).GT.NVect_Items .OR. IABS(Sort_Loc).LT.1)THEN
        CALL ERRMSG('TOP_LEPTONS_REAL_MATRIX_SORT',
     1              'TOP_LEPTONS_REAL_MATRIX_SORT',
     2              'Inputs inconsistent for sort','F')
            
        RETURN
      ENDIF
C
C ****  Copy item to be sorted in to work list; copy array to work array
C
      DO I = 1,Max_Vects
        WORKLIST(I) = ARRAY(((I-1)*NVect_Items)+IABS(Sort_Loc))
      ENDDO
      CALL UCOPY(ARRAY,WORKARRAY,NVect_Items*Max_Vects)
C
C ****  Sort work list
C
      MODE = 1                !Real number
      IF (Sort_Loc.GT.0)THEN
        NWAY = 0              !Ascending sort
      ELSE
        NWAY = 1              !Descending sort
      ENDIF
      NSORT = 0
      CALL SORTZV(WORKLIST,INDEX,Max_Vects,MODE,NWAY,NSORT)
C
C ****  Copy WORKARRAY back into ARRAY in correct order
C
      DO I = 1,Max_Vects
        CALL UCOPY( WORKARRAY(((INDEX(I)-1)*NVect_Items)+1),
     1    ARRAY(((I-1)*NVect_Items)+1),NVect_Items)
      ENDDO
  999 RETURN
      END
