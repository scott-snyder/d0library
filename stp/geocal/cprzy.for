C DEC/CMS REPLACEMENT HISTORY, Element CPRZY.FOR
C *1     9-NOV-1988 10:03:38 HARRY "Routines to convert raw cryostat geometry to geant format"
C DEC/CMS REPLACEMENT HISTORY, Element CPRZY.FOR
      SUBROUTINE CPRZY(LUN,NAME,SHAPE,VNAME,MEDIUM,A1,A2,THICK,Z,Y,NPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out (Z,Y) coordinates in SRCP format.
C-
C-   Inputs  : LUN         Unit number of output stream
C-             NAME        Section name
C-             SHAPE       GEANT shape
C-             VNAME       GEANT volume name
C-             MEDIUM      GEANT  medium number
C-             A1,A2       Cross-sectional angles at end of section
C-             THICK       Thickness of section
C-             Z(*),Y(*)   Coordinates of points defining section
C-             NPT         Number of points
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Created  1-NOV-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      REAL          Z(*),Y(*)
      CHARACTER*4   VNAME,SHAPE
      REAL          THICK,A1,A2
      INTEGER       I,LUN,NPT,MEDIUM
C----------------------------------------------------------------------
      WRITE(LUN,215) NAME,NPT,'LINE',VNAME,MEDIUM,THICK,A1,A2
  215 FORMAT(' \ARRAY ',A32,/,
     &           1X,I8,7X,'! Number of points'/,
     &           1X,4X,'''',A4,'''',5X,'! GEANT shape'/,
     &           1X,4X,'''',A4,'''',5X,'! GEANT volume name'/,
     &           1X,I8,7X,'! GEANT medium number'/,
     &           1X,F10.3,5X,'! Thickness'/,
     &           1X,F10.3,5X,'! Angle 1'/,
     &           1X,F10.3,5X,'! Angle 2'/,
     &           '! POINT',3X,'Z-COORD',3X,'Y-COORD')
      WRITE(LUN,216) (I,Z(I),Y(I),I=1,NPT)
  216 FORMAT(1X,I6,2F10.3)
      WRITE(LUN,217)
  217 FORMAT(' \END')
C
  999 RETURN
      END
