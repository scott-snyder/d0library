      SUBROUTINE MSHITS(IUDET,IMOD,IPLN,IWIR)
C------------------------------------------------------------------
C-    S/R MSHITS decodes module/plane/wire numbers of muon PDT    -
C- from volume names in GEANT common block.    This routine is    -
C- called by S/R STPMU.                                           -
C-                                                                -
C-  Input:                                                        -
C-    IUDET    H  detector name                                   -
C-    NLEVEL   I  level number at which the last medium  /GCVOLU/ -
C-                search stopped in Geant tracking.               -
C-    NAMES(j) H  volume names at j-th level.            /GCVOLU/ -
C-    NUMBER(j)H  user volume number                     /GCVOLU/ -
C-   (for debugging)                                              -
C-    IDEBUG   I  debug control.   If 1, print out.      /GCFLAG/ -
C-    LOUT     I  unit number for printer.               /GCUNIT/ -
C-    DHIT     I  print control in Hit section.          /D0LOG/  -
C-                                                                -
C-  Output:                                                       -
C-    IMOD     I  module number                                   -
C-    IPLN     I  plane number                                    -
C-    IWIR     I  wire number  (=0,1,2,....23)                    -
C-                                                                -
C-  S.Kunori    30-Mar-87                                         -
C------------------------------------------------------------------
      IMPLICIT NONE
C                                 
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'     ! to get IDEBUG.
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'     ! to get LOUT.
      INCLUDE 'D0$INC:GCVOLU.INC/LIST'     ! to get NLEVEL,NAMES()
      INCLUDE 'D0$INC:D0LOG.INC/LIST'      ! to get DHIT.
      INTEGER IUDET,IMOD,IPLN,IWIR       
C  --local variables.
      INTEGER I,I1,I2,I3
      INTEGER ICALL
      CHARACTER*4 ANAME
      CHARACTER*1 AN(11)
      DATA ICALL/0/
      DATA AN/'0','1','2','3','4','5','6','7','8','9','A'/
C        
C  -- convert hollerith volume name to character one...
      CALL UHTOC(NAMES(NLEVEL),4,ANAME,4)
C  -- find out module number...
      IF(ANAME(1:2).EQ.'AC') THEN             
        I1=0
      ELSE IF(ANAME(1:2).EQ.'BC') THEN
        I1=100
      ELSE IF(ANAME(1:2).EQ.'CC') THEN
        I1=200
      ELSE    
        I1=-1000
      ENDIF
      DO 100 I=1,11
        IF(ANAME(3:3).EQ.AN(I)) I2=I-1
        IF(ANAME(4:4).EQ.AN(I)) I3=I-1         
100   CONTINUE
      IMOD=I1+I2*10+I3
C  -- find out plane number and wire number...      
C  -- assuming lowest level is for cell and second lowest for plane
      IPLN=NUMBER(NLEVEL-1)
      IWIR=NUMBER(NLEVEL)-1
C----------debug-------------------------------------------------
CC      IF(IDEBUG.EQ.1) THEN                                       
CC      IF(DHIT.EQ.1) THEN
CC         ICALL=ICALL+1
CC         WRITE(LOUT,60) ICALL,NLEVEL,IMOD,IPLN,IWIR
CC     +       ,(NAMES(I),NUMBER(I),I=1,NLEVEL)
CC60       FORMAT(' == DEBUG IN S/R MSHIT1 ==  ICALL,NLEVEL='
CC     +   ,2I3,3X,'IMOD,IPLN,IWIR=',3I4/(8(3X,A4,2X,I4)))
CC      ENDIF
CC      ENDIF
C----------end debug---------------------------------------------
      RETURN
      END
