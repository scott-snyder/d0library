C----------------------------------------------------------------------
C- 
C-   Name      L2CLBH.INC
C-   Purpose   Permanent link area for Fast One dimensional Histograming
C-   Created   18-JAN-1990 Jan Guida, Srini Rajagopalan
C-   Copied to level2 version for single L2_VERTEX_CDC histogram
C----------------------------------------------------------------------
      INTEGER MXID
      PARAMETER (MXID=1)            
      INTEGER CHEAD
      PARAMETER (CHEAD=8)           
      INTEGER LCHST(MXID)           
      INTEGER HIDMX                 
      INTEGER HIDERR                
C 
      COMMON /L2CLBH/ HIDMX,HIDERR,LCHST
C----------------------------------------------------------------------
