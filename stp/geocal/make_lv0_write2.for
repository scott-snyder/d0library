      SUBROUTINE MAKE_LV0_WRITE2(LUN,PARAM,NAME,SHAPE,MED,MOTH,PTYP,
     &      NS,NA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : WRITE TILE DIV FOR GEOLV0
C-
C-   Inputs  : 
C-   Outputs : NONE
C-   Controls: SRCP_RAW_LV0.DAT
C-
C-   Created  27-FEB-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN,I,J,K,IR,M,N,P,MED
      INTEGER NS, NA, X, Y, Z
      CHARACTER*(*) PARAM,NAME,SHAPE,MOTH,PTYP
C----------------------------------------------------------------------
      DO IR = 1 ,1
        CALL SWORDS(PARAM,I,J,K)
        WRITE(PARAM(J:J),'(I1)')IR
        CALL SWORDS(NAME,M,N,P)
        CALL SWORDS(MOTH,X,Y,Z)
        WRITE(LUN,1)PARAM(I:J), NAME(M:N), 
     &      SHAPE,MED,MOTH(X:Y),PTYP,
     &      NS,NA
        WRITE(LUN,2)
    1   FORMAT(1X,'\ARRAY  ',A32,
     &    /1X,'''',A4,'''  ''',A4,'''',5X,I3,5X,'''',A4,''' ''',A4,'''',
     &    /1X,2I10)
    2   FORMAT(/1X,'\END')
      ENDDO
  999 RETURN
      END

