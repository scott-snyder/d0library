C **** COMMON BLOCK TEXATT
C
C    This is the level 6 common block used for text primitive attributes.
C    The variables consist of:
C        CPATH          integer        Current character path;
C                                      values of 1..4.
C        DPATH          ingeger        Default character path;
C                                      values of 1..4.
C        CHJUST         integer        Current horizontal justification;
C                                      values of 1..3.
C        CVJUST         integer        Current vertical justification;
C                                      values of 1..3.
C        DHJUST         integer        Default horizontal justification;
C                                      values of 1..3.
C        DVJUST         integer        Default vertical justification;
C                                      values of 1..3.
C        CXSIZE         real           Current character size with 
C                                      respect to the X-window.
C        CYSIZE         real           Current character size with
C                                      respect to the Y-window.
C        DXSIZE         real           Default character size with 
C                                      respect to the X-window.
C        DYSIZE         real           Default character size with
C                                      respect to the Y-window.
C        CGAP           real           Current character gap.
C        DGAP           real           Default character gap.
C        CXBASE         real           Current character base line x-
C                                      component of base vector.
C        CYBASE         real           Current character base line y-
C                                      component of base vector.
C        CZBASE         real           Current character base line z-
C                                      component of base vector.
C        DXBASE         real           Default character base line x-
C                                      component of base vector.
C        DYBASE         real           Default character base line y-
C                                      component of base vector.
C        DZBASE         real           Default character base line z-
C                                      component of base vector.
C        CXPLAN         real           Current character plane x-
C                                      component of plane vector.
C        CYPLAN         real           Current character plane y-
C                                      component of plane vector.
C        CZPLAN         real           Current character plane z-
C                                      component of plane vector.
C        DXPLAN         real           Default character plane x-
C                                      component of plane vector.
C        DYPLAN         real           Default character plane y-
C                                      component of plane vector.
C        DZPLAN         real           Default character plane z-
C                                      component of plane vector.
      INTEGER CPATH, DPATH, CHJUST, DHJUST, CVJUST, DVJUST
      REAL CXSIZE, DXSIZE, CYSIZE, DYSIZE, CGAP, DGAP
      REAL CXBASE, DXBASE, CYBASE, DYBASE, CZBASE, DZBASE
      REAL CXPLAN, DXPLAN, CYPLAN, DYPLAN, CZPLAN, DZPLAN

      COMMON /TEXATT/ CPATH,  DPATH,  CHJUST, DHJUST, CVJUST, DVJUST
      COMMON /TEXATT/ CXSIZE, DXSIZE, CYSIZE, DYSIZE, CGAP,   DGAP
      COMMON /TEXATT/ CXBASE, DXBASE, CYBASE, DYBASE, CZBASE, DZBASE
      COMMON /TEXATT/ CXPLAN, DXPLAN, CYPLAN, DYPLAN, CZPLAN, DZPLAN
