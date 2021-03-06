      SUBROUTINE SORT_DESCEND(ARRAY_SORT,ARRAY_SUB1,ARRAY_SUB2,
     &ARRAY_SUB3,ARRAY_SUB4,ARRAY_SUB5,ARRAY_SUB6,ARRAY_SUB7,
     &ARRAY_SUB8,ARRAY_SUB9,COUNT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SORT ARRAY_SORT IN DESCENDING VALUE
C-   ACCORDING TO THE VALUES IN ARRAY_SORT WHILE SORTING THE ELEMENTS
C-   OF THE ARRAY_SUB1... IN A LIKE MANNER
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-FEB-1992   DOUGLAS M. NORMAN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER COUNT
      REAL ARRAY_SORT(COUNT),ARRAY_SUB1(COUNT),ARRAY_SUB2(COUNT)
      REAL ARRAY_SUB3(COUNT),ARRAY_SUB4(COUNT),ARRAY_SUB5(COUNT)
      REAL ARRAY_SUB6(COUNT),ARRAY_SUB7(COUNT)
      REAL ARRAY_SUB8(COUNT),ARRAY_SUB9(COUNT)
      REAL TEMP,TEMP1,TEMP2,TEMP3,TEMP4,TEMP5,TEMP6,TEMP7
      REAL TEMP8,TEMP9
      INTEGER INDEX,NSORT
      LOGICAL SORTED
C----------------------------------------------------------------------
C******CHECK COUNT*************
       IF (COUNT.LT.2) THEN
         RETURN
        ENDIF
C
C******PERFORM SORT**********************************
C
         NSORT=COUNT-1
         SORTED=.FALSE.
         DO WHILE(.NOT.SORTED)
           SORTED=.TRUE.
C
          DO INDEX=1,NSORT
           IF (ARRAY_SORT(INDEX).LT.ARRAY_SORT(INDEX+1)) THEN
              TEMP=ARRAY_SORT(INDEX)
              TEMP1=ARRAY_SUB1(INDEX)
              TEMP2=ARRAY_SUB2(INDEX)
              TEMP3=ARRAY_SUB3(INDEX)
              TEMP4=ARRAY_SUB4(INDEX)
              TEMP5=ARRAY_SUB5(INDEX)
              TEMP6=ARRAY_SUB6(INDEX)
              TEMP7=ARRAY_SUB7(INDEX)
              TEMP8=ARRAY_SUB8(INDEX)
              TEMP9=ARRAY_SUB9(INDEX)
C
              ARRAY_SORT(INDEX)=ARRAY_SORT(INDEX+1)
              ARRAY_SUB1(INDEX)=ARRAY_SUB1(INDEX+1)
              ARRAY_SUB2(INDEX)=ARRAY_SUB2(INDEX+1)
              ARRAY_SUB3(INDEX)=ARRAY_SUB3(INDEX+1)
              ARRAY_SUB4(INDEX)=ARRAY_SUB4(INDEX+1)
              ARRAY_SUB5(INDEX)=ARRAY_SUB5(INDEX+1)
              ARRAY_SUB6(INDEX)=ARRAY_SUB6(INDEX+1)
              ARRAY_SUB7(INDEX)=ARRAY_SUB7(INDEX+1)
              ARRAY_SUB8(INDEX)=ARRAY_SUB8(INDEX+1)
              ARRAY_SUB9(INDEX)=ARRAY_SUB9(INDEX+1)
C
              ARRAY_SORT(INDEX+1)=TEMP
              ARRAY_SUB1(INDEX+1)=TEMP1
              ARRAY_SUB2(INDEX+1)=TEMP2
              ARRAY_SUB3(INDEX+1)=TEMP3
              ARRAY_SUB4(INDEX+1)=TEMP4
              ARRAY_SUB5(INDEX+1)=TEMP5
              ARRAY_SUB6(INDEX+1)=TEMP6
              ARRAY_SUB7(INDEX+1)=TEMP7
              ARRAY_SUB8(INDEX+1)=TEMP8
              ARRAY_SUB9(INDEX+1)=TEMP9
                SORTED=.FALSE.
           ENDIF
          ENDDO
         ENDDO
  999 RETURN
      END
