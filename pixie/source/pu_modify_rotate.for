      SUBROUTINE PU_MODIFY_ROTATE(IDX,CAM,UPVEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets the new camera point and upvectors 
C-   for rotation.
C-
C-   Inputs  : IDX     [I]: Index of the the screen to be modify
C-             CAM  [R(*)]: Array with the new camera values
C-             UPVEC[R(*)]: Array with the new upvector values
C-  
C-   Outputs : None
C-
C-   Created  30-MAY-1991   Lupe Howell, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IDX
      REAL    CAM(*),UPVEC(*)
C
      INTEGER IER
C----------------------------------------------------------------------
C
C **** Store NEW window values
C
      CALL PU_SET_SCREEN_PARAM(IDX,'CAMX',CAM(1),IER)
      CALL PU_SET_SCREEN_PARAM(IDX,'CAMY',CAM(2),IER)
      CALL PU_SET_SCREEN_PARAM(IDX,'CAMZ',CAM(3),IER)
      CALL PU_SET_SCREEN_PARAM(IDX,'UPVECX',UPVEC(1),IER)
      CALL PU_SET_SCREEN_PARAM(IDX,'UPVECY',UPVEC(2),IER)
      CALL PU_SET_SCREEN_PARAM(IDX,'UPVECZ',UPVEC(3),IER)
      
      IF ( IER .NE. 0 ) THEN
        CALL INTMSG(' PU_MODIFY_ROTATE: Could NOT Set'//
     &       'rotation parameters ')
      ENDIF

  999 RETURN
      END
