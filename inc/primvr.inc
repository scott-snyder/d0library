C DEC/CMS REPLACEMENT HISTORY, Element PRIMVR.INC
C *2    24-JUL-1989 00:28:59 ABACHI "UPDATE"
C *1    11-MAY-1989 14:06:00 ABACHI "Inc files for EAS emulator"
C DEC/CMS REPLACEMENT HISTORY, Element PRIMVR.INC
C **** COMMON BLOCK PRIMVR
C
C    This is the level 4 common block used by the primitive routines for
C    data and counters of the graphics primitives
C    The variables consist of:
C        THREED         logical        Flag indicating dimensionality.
C                                         THREED = .TRUE.  --> 3-D
C                                                = .FALSE. --> 2-D
C        CPX, CPY, CPZ  real           Indicates the current position of 
C                                      to perform graphics primitive.
C        NVECTS         integer        Number of vectors defined in the
C                                      VECTS array.
C        VSTAT          logical*1      Status of each vector:
C                                         VSTAT = .TRUE.  draw
C                                         VSTAT = .FALSE. move
C        VECTS          real array     Vectors used to draw lines:
C                                        VECTS(1,N) = X Component
C                                        VECTS(2,N) = Y Component
C                                        VECTS(3,N) = Z Component
C                                        VECTS(4,N) = Intensity
C 
C
      INTEGER MVECTS
      PARAMETER (MVECTS = 10000)
      REAL CPX, CPY, CPZ, VECTS(4,MVECTS)
      INTEGER NVECTS
      LOGICAL*1 VSTAT(MVECTS)
      LOGICAL THREED

      COMMON /PRIMVR/ CPX, CPY, CPZ, NVECTS, VSTAT, VECTS
      COMMON /PRIMVR/ THREED
