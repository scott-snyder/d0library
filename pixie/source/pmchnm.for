C======================================================================
      SUBROUTINE PMCHNM(NMOD,XC,YC,ZC,DX,DY,DZ)
C======================================================================
C
C    Purpose and Methods : To put a chamber number by each chamber in
C                          the single view
C 
C    Inputs  : XC,YC,ZC   Position of center of box
C              DX,DY,DZ   Half lengths of the width, heigth, and
C                         depth of the box
C 
C    Created  14-NOV-1989   Carol C. Francis
C
C======================================================================
      IMPLICIT NONE
C======================================================================
C   LOCAL DECLARATIONS
C   ==================
C
      INTEGER IVIEW       !Current view
      INTEGER NMOD
      INTEGER IMOD,IPLN,IWIR,IORENT
      REAL VOFF,WLEN,VECT(3)
      REAL XC,YC,ZC       !Center of box
      REAL DX,DY,DZ       !Half-length of width, heigth, and depth
      CHARACTER*3 CHNM
C
C======================================================================
C   EXECUTABLE CODE 
C   ===============
C
C   Get information from zebra bank about processed hits   
C   ====================================================
C
         CALL PXITOC(NMOD,3,CHNM)
         CALL MUGEOM(NMOD,0,0,VECT,WLEN,VOFF,IORENT)
         IORENT = ABS(IORENT)
C
         IF (IORENT.EQ.1) CALL J3MOVE((XC+(-45.)),(YC+DY+25.),0.)
         IF (IORENT.EQ.2) CALL J3MOVE((XC+DX+20.),YC+0.,0.)
         IF (IORENT.EQ.3) CALL J3MOVE((XC+(-45.)),(YC+DY+25.),0.)
         IF (IORENT.EQ.4) CALL J3MOVE((XC+DX+20.),YC+0.,0.)
         CALL JSIZE(48.,48.)
         CALL J3STRG(CHNM)
C
  999 RETURN
      END
