      SUBROUTINE FTRKPAR(LFDCT,SCERR,COOR0,XVEC,ERVEC) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : convert a central track from cylindrical to
C-   carthesian coordinates
C-   
C-   The parameters are 
C    XVEC(1-4)  a,b,c,d 
C-   ERVEC(4,4) error matrix
C-
C-   Inputs  :  LFDCT: VTX or FDC bank address 
C-              PHI0 - rotation angle in the XY  plane 
C-              SCERR: scaling factor for the error
C-   Outputs :  CORR0: central coordinate 
C-              XVEC,ERVEC
C-
C-   Controls: 
C-
C-   Created  31-May-1993   Daria Zieminska
C-                          Andrzej Zieminski
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER IFDCT,LFDCT
      REAL XVEC(*),ERVEC(4,4) 
      REAL COOR0,SCERR,Z0
C
C  FDC track parameters
C
      IFDCT=IQ(LFDCT-5)
      CALL FGETZ0(IFDCT,Z0) 
      XVEC(1)=Q(LFDCT+4)
      XVEC(2)=Q(LFDCT+7)   ! Bf
      XVEC(3)=Q(LFDCT+5)
      XVEC(4)=Q(LFDCT+8)   ! Df
      ERVEC(1,1)=Q(LFDCT+9)*SCERR
      ERVEC(2,2)=Q(LFDCT+16)*SCERR
      ERVEC(3,3)=Q(LFDCT+13)*SCERR
      ERVEC(4,4)=Q(LFDCT+18)*SCERR
      ERVEC(1,2)=Q(LFDCT+11)*SCERR
      ERVEC(3,4)=Q(LFDCT+15)*SCERR
      ERVEC(1,3)=Q(LFDCT+10)*SCERR
      ERVEC(1,4)=Q(LFDCT+12)*SCERR
      ERVEC(2,3)=Q(LFDCT+14)*SCERR
      ERVEC(2,4)=Q(LFDCT+17)*SCERR
      ERVEC(2,1)=ERVEC(1,2)
      ERVEC(3,1)=ERVEC(1,3)
      ERVEC(4,1)=ERVEC(1,4)
      ERVEC(3,2)=ERVEC(2,3)
      ERVEC(4,2)=ERVEC(2,4)
      ERVEC(4,3)=ERVEC(3,4)
C
      COOR0 = Z0
C
  999 RETURN
      END
