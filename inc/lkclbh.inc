C----------------------------------------------------------------------
C- 
C-   Name      LKCLBH.INC
C-   Purpose   Permanent link area for Fast One dimensional Histograming
C-   Created   18-JAN-1990 Jan Guida, Srini Rajagopalan
C-
C----------------------------------------------------------------------
      INTEGER MXID
      PARAMETER (MXID=15000)        
      INTEGER CHEAD
      PARAMETER (CHEAD=8)           
      INTEGER LCHST(MXID)           
      INTEGER HIDMX                 
      INTEGER HIDERR                
C 
      COMMON /LKCLBH/ HIDMX,HIDERR,LCHST
C----------------------------------------------------------------------
