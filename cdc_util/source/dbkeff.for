      SUBROUTINE DBKEFF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Book efficiency histograms for sense wires and delay lines
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: called by DTRDIA
C-             ENTRY points: DTSEFF(HSTEFF) 
C-
C-   Created  18-AUG-1988   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL YES,HSTEFF
C
      YES=.FALSE.
      CALL GETPAR(1,' Book efficiency histograms? Y/N>','L',YES)
      IF(.NOT.YES) GOTO 999
C
      CALL HBOOK1(1000,'number of full tracks$',100,-1.5,98.5,0.)
      CALL HBOOK1(1001,'number of hits(XY) on a full track$',
     &  30,-1.5,28.5,0.)
      CALL HBOOK1(1002,'number of hits(Z) on a full track$',
     &  9,-0.5,8.5,0.)
      CALL HBOOK1(1003,'sense wire efficiency$',30,-1.5,28.5,0.)
      CALL HBOOK1(1004,'RZ used wires $',30,-1.5,28.5,0.)
      CALL HBOOK1(1005,'delay line efficiency $',30,-1.5,28.5,0.)
      CALL HBOOK1(1006,'number of segments (lay 0)$',130,-1.5,128.5,0.)
      CALL HBOOK1(1007,'number of segments (lay 1)$',130,-1.5,128.5,0.)
      CALL HBOOK1(1008,'number of segments (lay 2)$',130,-1.5,128.5,0.)
      CALL HBOOK1(1009,'number of segments (lay 3)$',130,-1.5,128.5,0.)
      CALL HBOOK1(1010,'Theta of Tracks $',64,-0.01,3.15,0.)
      CALL HBOOK1(1011,'Beam Position in Z$',150,-150.0,150.0,0.)
C      CALL HBOOK2(1012,'THETA vs ZBEAM$',
C     &  150,-150.0,150.0,64,-0.01,3.15,0.0)
      CALL HBOOK1(1013,'Phi of Tracks $',64,-0.01,6.29,0.)
      CALL HBOOK1(1014,'Phi of Segment in Layer 0$',64,-0.01,6.29,0.)
      CALL HBOOK1(1015,'Phi of Segment in Layer 1$',64,-0.01,6.29,0.)
      CALL HBOOK1(1016,'Phi of Segment in Layer 2$',64,-0.01,6.29,0.)
      CALL HBOOK1(1017,'Phi of Segment in Layer 3$',64,-0.01,6.29,0.)
C
 999  RETURN
C---------------------------------------------------------------------------
      ENTRY DTSEFF(HSTEFF)
      HSTEFF = YES
      RETURN
      END
