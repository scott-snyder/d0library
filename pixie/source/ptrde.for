      SUBROUTINE PTRDE
C====================================================================
C
C  Description:  Displays end view of TRD
C  ============  
C
C  Parameter Descriptions:
C  =========================
C  
C  None
C
C  Author:
C  =======
C  Sharon Hagopian
C
C  Revision History:
C  =================
C  Original Creation - June 6,1986
C  Updated  Jan 10, 1990  Lupe Howell Implementing Color Table
C  
C=====================================================================
C
C  Local Declarations:
C  ====================
C
      IMPLICIT NONE
      CHARACTER*5 CEXP
      REAL YDP3,RANG,STANG
      REAL PI,XCENT,YCENT,ZCENT
      REAL RAD(2)
      REAL ANG(2)
      REAL AA
      REAL RAV
      REAL R(4) ! BOUNDARIES OF TRD LAYERS
      REAL DELANG ! ANGULAR 1/2 WIDTH OF CELL IN DEGREES
      REAL DEGRAD
           REAL RMAT(4,4)  ! DI-3000 rotation matrix
           REAL DUM        ! Dummy argument
C
      INCLUDE 'D0$INC:GRAPHF77.INC'
      CHARACTER*3 ICOLOR, KCOLOR
      INTEGER ISEG
      INTEGER NSEC,ILAY,ICELL 
      REAL ENRG,PTEGET
      REAL XPLAN,XBAS
      INTEGER IENRG
C
      DATA YDP3/14.6305/
      DATA R/17.50,28.05,38.60,49.15/
      DATA PI/3.1415927/
      DATA DELANG/.7031/
      DATA ICOLOR/'RED'/
C
C  Executable Code:
C  =================
C
C
      DEGRAD=PI/180.
      NSEC=1
C
C  ======================================================================
C
      DO 100 ILAY=1,3
      DO 100 ICELL=1,256
      RANG=180.-(FLOAT(ICELL)-1.)*DELANG*2.
      CALL JMODON(.TRUE.)
C SET UP DI-3000 transformation for rotation around z-axis
      CALL JTRANS(RMAT,9,RANG,DUM,DUM,DUM,DUM,DUM,DUM)
      CALL JMODEL(RMAT)
      CALL JWCLIP(.TRUE.)
C Get current viewport values
      CALL PZOPEN(0,ISEG)
      IF(ICELL.EQ.1)THEN
        CALL PXCOLR('FOR')   ! Foreground color
        CALL JCIRCL(0.,0.,0.,R(ILAY),0)
        IF(ILAY.EQ.1)THEN
          CALL JCIRCL(0.,0.,0.,3.7,0)
          CALL JCIRCL(0.,0.,0.,16.2,0)
          CALL JCIRCL(0.,0.,0.,51.8,0)
          CALL JCIRCL(0.,0.,0.,71.9,0)
          CALL JCIRCL(0.,0.,0.,75.,0)
          CALL JCIRCL(0.,0.,0.,95.,0)
        ENDIF
      ENDIF
      CALL PXCOLFILL(ICOLOR)
C
C  Calculate the sector stagger and the rotation angle in radians.
C  ===============================================================
C
C
C  Calculate the center position of the first drawn sector, in this
C  case, sector 0 is drawn first.
C  =================================================================
C
         ANG(1)=-DELANG
         ANG(2)=+DELANG
         RAD(1)=R(ILAY)
         RAD(2)=R(ILAY+1)
         RAV=(RAD(1)+RAD(2))/2.
         AA=RANG*DEGRAD
         XCENT = RAV
         YCENT = 0.
         ZCENT = 0.
C
C  Draw sector 0 in LAYER 0
C  =========================
C
        ENRG=PTEGET(ILAY,ICELL)
        CALL PTECUT(ENRG,KCOLOR)
        CALL PXCOLFILL(KCOLOR)
        CALL PXCOLR(KCOLOR)
        CALL PTCELL(RAD,ANG)
        CALL JJUST(2,2)
        CALL JSIZE(1.5,1.)
        CALL PXCOLR('FOR')    ! Foreground color
        CALL J3MOVE(XCENT,YCENT,ZCENT)
        XPLAN=+1.
        XBAS=-1.
        IF(ABS(RANG).GT.90.)THEN
          XPLAN=-1.
          XBAS=+1.
        ENDIF
        CALL JBASE(XBAS,0.,0.)
        CALL JPLANE(0.,XPLAN,0.)
        IENRG=ENRG
        CALL H3ITOC(IENRG,5,CEXP)
        CALL J3STRG(CEXP)
        CALL JRCLOS
  100 CONTINUE
      CALL PZOPEN(0,ISEG)
      CALL JCOLOR(0)
      CALL JCIRCL(0.,0.,0.,R(4),0)
      CALL JRCLOS
      RETURN
      END
