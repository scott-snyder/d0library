C======================================================================
      SUBROUTINE PLDBAR(NSIDE,XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,DSET)
C======================================================================
C   
C									
C	DATE : February 23, 1988              			    
C       ======
C
C     Purpose: Will draw a bar in a LEGO plot. 
C
C     Inputs:  NSIDE -Number of sides of the bar to draw (3 or 6)
C              XMIN - Minimum value for the bin in the x dir
C              XMAX - Maximum value for the bin in the x dir
C              YMIN - Minimum value for the bin in the y dir
C              YMAX - Maximum value for the bin in the y dir
C              ZMIN - Minimum value for the bin in the z dir (up)
C              ZMAX - Maximum value for the bin in the z dir
C              DSET - Variable to determine if there is more than one 
C                     array in the data set. If it is the color of the
C                     to of the bin will change.
C                      If DSET = 1 withe top for 1st data set
C                              <> 1 yellow for the 2nd data set
C======================================================================
      IMPLICIT NONE
C====================================================================== 
C    Argument Declaration:
C    =====================
      INTEGER NSIDE
        REAL XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,
     X   XBOX(4),YBOX(4),ZBOX(4)  
        INTEGER DSET
C===========================================================================    
C   CHECKING FOR 0 HIGH BLOCKS 
      IF ((ZMIN.EQ.0) .AND. (ZMAX.EQ.0)) GO TO 100
	ZBOX(1)=ZMIN
	ZBOX(2)=ZMIN
	ZBOX(3)=ZMAX
	ZBOX(4)=ZMAX
C If NSIDE=3, only draw the front and top of the bar
        IF(NSIDE.EQ.3)GO TO 50
        XBOX(1)=XMIN
        XBOX(2)=XMAX
        XBOX(3)=XMAX
        XBOX(4)=XMIN
	CALL VFILL(YBOX,4,YMIN)  
C Draw left back side
	CALL J3PLGN(XBOX,YBOX,ZBOX,4)  
	XBOX(1)=XMIN
	XBOX(2)=XMAX
	XBOX(3)=XMAX
	XBOX(4)=XMIN
	YBOX(1)=YMIN
	YBOX(2)=YMIN
	YBOX(3)=YMAX
	YBOX(4)=YMAX
	CALL VFILL(ZBOX,4,ZMIN)  
C Draw bottom of bar
	CALL J3PLGN(XBOX,YBOX,ZBOX,4)  
	YBOX(1)=YMIN
	YBOX(2)=YMAX
	YBOX(3)=YMAX
	YBOX(4)=YMIN
	ZBOX(1)=ZMIN
	ZBOX(2)=ZMIN
	ZBOX(3)=ZMAX
	ZBOX(4)=ZMAX
	CALL VFILL(XBOX,4,XMAX)  
C Draw right back side
	CALL J3PLGN(XBOX,YBOX,ZBOX,4)  
   50   YBOX(1)=YMIN
        YBOX(2)=YMAX
        YBOX(3)=YMAX
        YBOX(4)=YMIN
	CALL VFILL(XBOX,4,XMIN)   
C Draw right front side
	CALL J3PLGN(XBOX,YBOX,ZBOX,4)  
	XBOX(1)=XMIN
	XBOX(2)=XMAX
	XBOX(3)=XMAX
	XBOX(4)=XMIN
	CALL VFILL(YBOX,4,YMAX)   
C draw left front side
	CALL J3PLGN(XBOX,YBOX,ZBOX,4)  
	CALL VFILL(ZBOX,4,ZMAX)   
        XBOX(1)=XMIN
        XBOX(2)=XMAX
        XBOX(3)=XMAX
        XBOX(4)=XMIN
        YBOX(1)=YMIN
        YBOX(2)=YMIN
        YBOX(3)=YMAX
        YBOX(4)=YMAX
C       BUILDING TOP OF BLOCK WITHE
	CALL J3PLGN(XBOX,YBOX,ZBOX,4)  
        GO TO 100
 100    CONTINUE 
	RETURN 
	END    
