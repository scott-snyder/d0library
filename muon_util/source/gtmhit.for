      SUBROUTINE GTMHIT(ORENT,ITRACK,NHITA,NHITBC,XPLANE,XDRIFT,XWIRE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack MHIT bank
C-
C-   Inputs  : ITRACK - track number 
C-   Outputs : NHITA,NHITBC, - number of hits in each layer                     
C-     XDRIFT - dirft direction global coordinates,                             
C-     XPLANE - plane direction global coordinates ,                            
C-     RAWHIT - array with the raw hit numbers
C-   Controls: 
C-
C-   Created  15-NOV-1991   A.Klatchko
C-   Updated  27-NOV-1991   A.Klatchko  FOR MC 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER ITRACK,NHITA,NHITBC,GZMHIT,LMHIT,I,HI,NHTR,ORENT
      REAL XDRIFT(*),XPLANE(*),XWIRE(*),X(13),Y(13),Z(13)
C----------------------------------------------------------------------
      LMHIT = GZMHIT(ITRACK)
      NHITA = IQ(LMHIT+3)
      NHITBC = IQ(LMHIT+4)
      NHTR = NHITA+NHITBC
C  read varing part
      HI = 5
      DO I =1,NHTR
        X(I) = Q(LMHIT +  HI)  
        HI = HI + 1
        Y(I) = Q(LMHIT +  HI)  
        HI = HI + 1
        Z(I) = Q(LMHIT +  HI)  
        HI = HI + 1
      ENDDO
C
C                             
      IF(ORENT .EQ. 1)THEN    
        CALL UCOPY(X,XPLANE,NHTR)
        CALL UCOPY(Y,XWIRE,NHTR)
        CALL UCOPY(Z,XDRIFT,NHTR)
      ELSEIF(ORENT .EQ. 2)THEN
        CALL UCOPY(Y,XPLANE,NHTR)
        CALL UCOPY(X,XWIRE,NHTR)
        CALL UCOPY(Z,XDRIFT,NHTR)
      ELSEIF(ORENT .EQ. 3)THEN
        CALL UCOPY(Z,XPLANE,NHTR)
        CALL UCOPY(Y,XWIRE,NHTR)
        CALL UCOPY(X,XDRIFT,NHTR)
      ELSEIF(ORENT .EQ. 4)THEN
        CALL UCOPY(Z,XPLANE,NHTR)
        CALL UCOPY(X,XWIRE,NHTR)
        CALL UCOPY(Y,XDRIFT,NHTR)
      ENDIF                   
C                             
  999 RETURN
      END
