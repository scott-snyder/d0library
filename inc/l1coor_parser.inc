C----------------------------------------------------------------------
C-
C-   Created  26-JUL-1991   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from LV1_PARSER to
C-                              L1COOR_PARSER. 
C-
C----------------------------------------------------------------------
C
C       Declare the list
C
      INTEGER LIST_TOP
      INTEGER LIST(1:LIST_SIZE,OBJ_INDEX:ITEM_T_INDEX)
C
      INTEGER PARSE_STATUS
      INTEGER PARSE_LINE_NUM
C
      COMMON /L1COOR_PARSER/ LIST_TOP, LIST, PARSE_STATUS, 
     &  PARSE_LINE_NUM
