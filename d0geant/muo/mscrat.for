      SUBROUTINE MSCRAT(NCRATE,IDCRAT,NMODUS,IDMODU)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine returns a crate number,
C-   number of modules and module number for given sequential
C-   crate number, NCRATE.     This routine assumes that it is
C-   called from s/r DIGMUO with NCRATE in ascending order from
C-   1 to infinite.    When this program goes beyound existing
C-   number of crates, this program returns -1 for NMODUS.
C-
C-   The table between crate id and module numbers are taken from
C-   M.Fortner's file d0::user1:[fortner.examine]muo_addr.dat on
C-   4-April-1991 and hard coded in this routine.   This should
C    be obtained from the data base in future.
C-
C-   Inputs  : NCRATE    I    sequential crate number.
C-   Outputs : IDCRAT    I    crate ID number
C-             NMODUS    I    number of modules for this crate.
C-             IDMODU()  I    module numbers
C-
C-   Controls: 
C-
C-   Created   4-APR-1991   Shuichi Kunori
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCRATE,IDCRAT,NMODUS,IDMODU(12)
      INTEGER MXCRAT
      PARAMETER (MXCRAT=18)
      INTEGER IDCRXX(MXCRAT),NMODXX(MXCRAT),IDMOXX(12,MXCRAT)
      INTEGER I
C----------------------------------------------------------------------
CC                         Module  Crate ID   Slot ID
      DATA IDCRXX( 1) /  2 /
      DATA NMODXX( 1) / 11 /
      DATA IDMOXX( 1, 1) /  294 / ! 002       1
      DATA IDMOXX( 2, 1) /  295 / ! 002       2
      DATA IDMOXX( 3, 1) /  304 / ! 002       3
      DATA IDMOXX( 4, 1) /  305 / ! 002       4
      DATA IDMOXX( 5, 1) /  296 / ! 002       5
      DATA IDMOXX( 6, 1) /  297 / ! 002       6
      DATA IDMOXX( 7, 1) /  306 / ! 002       7
      DATA IDMOXX( 8, 1) /  307 / ! 002       8
      DATA IDMOXX( 9, 1) /  283 / ! 002       9
      DATA IDMOXX(10, 1) /  285 / ! 002      10
      DATA IDMOXX(11, 1) /  280 / ! 002      11

      DATA IDCRXX( 2) / 12      / ! 
      DATA NMODXX( 2) /  9      / ! 
      DATA IDMOXX( 1, 2) /  290 / ! 012       1
      DATA IDMOXX( 2, 2) /  291 / ! 012       2
      DATA IDMOXX( 3, 2) /  300 / ! 012       3
      DATA IDMOXX( 4, 2) /  301 / ! 012       4
      DATA IDMOXX( 5, 2) /  292 / ! 012       5
      DATA IDMOXX( 6, 2) /  293 / ! 012       6
      DATA IDMOXX( 7, 2) /  302 / ! 012       7
      DATA IDMOXX( 8, 2) /  303 / ! 012       8
      DATA IDMOXX( 9, 2) /  281 / ! 012       9

      DATA IDCRXX( 3) / 22      / ! 
      DATA NMODXX( 3) /  8      / ! 
      DATA IDMOXX( 1, 3) /  147 / ! 022       1
      DATA IDMOXX( 2, 3) /  140 / ! 022       2
      DATA IDMOXX( 3, 3) /  141 / ! 022       3
      DATA IDMOXX( 4, 3) /  142 / ! 022       4
      DATA IDMOXX( 5, 3) /  143 / ! 022       5
      DATA IDMOXX( 6, 3) /  144 / ! 022       6
      DATA IDMOXX( 7, 3) /  145 / ! 022       7
      DATA IDMOXX( 8, 3) /  146 / ! 022       8

      DATA IDCRXX( 4) / 32      / ! 
      DATA NMODXX( 4) / 10      / ! 
      DATA IDMOXX( 1, 4) /  190 / ! 032       1
      DATA IDMOXX( 2, 4) /  191 / ! 032       2
      DATA IDMOXX( 3, 4) /  192 / ! 032       3
      DATA IDMOXX( 4, 4) /  193 / ! 032       4
      DATA IDMOXX( 5, 4) /  194 / ! 032       5
      DATA IDMOXX( 6, 4) /  195 / ! 032       6
      DATA IDMOXX( 7, 4) /  196 / ! 032       7
      DATA IDMOXX( 8, 4) /  197 / ! 032       8
      DATA IDMOXX( 9, 4) /  180 / ! 032       9
      DATA IDMOXX(10, 4) /  183 / ! 032      10

      DATA IDCRXX( 5) / 42      / ! 
      DATA NMODXX( 5) /  5      / ! 
      DATA IDMOXX( 1, 5) /  091 / ! 042       1
      DATA IDMOXX( 2, 5) /  092 / ! 042       2
      DATA IDMOXX( 3, 5) /  094 / ! 042       3
      DATA IDMOXX( 4, 5) /  095 / ! 042       4
      DATA IDMOXX( 5, 5) /  097 / ! 042       5

      DATA IDCRXX( 6) / 52      / ! 
      DATA NMODXX( 6) / 10      / ! 
      DATA IDMOXX( 1, 6) /  247 / ! 052       1
      DATA IDMOXX( 2, 6) /  240 / ! 052       2
      DATA IDMOXX( 3, 6) /  241 / ! 052       3
      DATA IDMOXX( 4, 6) /  242 / ! 052       4
      DATA IDMOXX( 5, 6) /  243 / ! 052       5
      DATA IDMOXX( 6, 6) /  244 / ! 052       6
      DATA IDMOXX( 7, 6) /  206 / ! 052       7
      DATA IDMOXX( 8, 6) /  216 / ! 052       8
      DATA IDMOXX( 9, 6) /  236 / ! 052       9
      DATA IDMOXX(10, 6) /  246 / ! 052      10

      DATA IDCRXX( 7) / 62      / ! 
      DATA NMODXX( 7) /  9      / ! 
      DATA IDMOXX( 1, 7) /  217 / ! 062       1
      DATA IDMOXX( 2, 7) /  227 / ! 062       2
      DATA IDMOXX( 3, 7) /  237 / ! 062       3
      DATA IDMOXX( 4, 7) /  210 / ! 062       4
      DATA IDMOXX( 5, 7) /  220 / ! 062       5
      DATA IDMOXX( 6, 7) /  230 / ! 062       6
      DATA IDMOXX( 7, 7) /  211 / ! 062       7
      DATA IDMOXX( 8, 7) /  221 / ! 062       8
      DATA IDMOXX( 9, 7) /  231 / ! 062       9

      DATA IDCRXX( 8) / 72      / ! 
      DATA NMODXX( 8) / 11      / ! 
      DATA IDMOXX( 1, 8) /  117 / ! 072       1
      DATA IDMOXX( 2, 8) /  127 / ! 072       2
      DATA IDMOXX( 3, 8) /  137 / ! 072       3
      DATA IDMOXX( 4, 8) /  110 / ! 072       4
      DATA IDMOXX( 5, 8) /  120 / ! 072       5
      DATA IDMOXX( 6, 8) /  130 / ! 072       6
      DATA IDMOXX( 7, 8) /  111 / ! 072       7
      DATA IDMOXX( 8, 8) /  121 / ! 072       8
      DATA IDMOXX( 9, 8) /  131 / ! 072       9
      DATA IDMOXX(10, 8) /  116 / ! 072      10
      DATA IDMOXX(11, 8) /  136 / ! 072      11

      DATA IDCRXX( 9) / 82      / ! 
      DATA NMODXX( 9) /  9      / ! 
      DATA IDMOXX( 1, 9) /  016 / ! 082       1
      DATA IDMOXX( 2, 9) /  026 / ! 082       2
      DATA IDMOXX( 3, 9) /  036 / ! 082       3
      DATA IDMOXX( 4, 9) /  010 / ! 082       4
      DATA IDMOXX( 5, 9) /  020 / ! 082       5
      DATA IDMOXX( 6, 9) /  030 / ! 082       6
      DATA IDMOXX( 7, 9) /  011 / ! 082       7
      DATA IDMOXX( 8, 9) /  021 / ! 082       8
      DATA IDMOXX( 9, 9) /  031 / ! 082       9

      DATA IDCRXX(10) / 92      / ! 
      DATA NMODXX(10) / 11      / ! 
      DATA IDMOXX( 1,10) /  264 / ! 092       1
      DATA IDMOXX( 2,10) /  274 / ! 092       2
      DATA IDMOXX( 3,10) /  265 / ! 092       3
      DATA IDMOXX( 4,10) /  275 / ! 092       4
      DATA IDMOXX( 5,10) /  266 / ! 092       5
      DATA IDMOXX( 6,10) /  276 / ! 092       6
      DATA IDMOXX( 7,10) /  267 / ! 092       7
      DATA IDMOXX( 8,10) /  277 / ! 092       8
      DATA IDMOXX( 9,10) /  253 / ! 092       9
      DATA IDMOXX(10,10) /  255 / ! 092      10
      DATA IDMOXX(11,10) /  250 / ! 092      11

      DATA IDCRXX(11) / 102     / ! 
      DATA NMODXX(11) /  9      / ! 
      DATA IDMOXX( 1,11) /  260 / ! 102       1
      DATA IDMOXX( 2,11) /  270 / ! 102       2
      DATA IDMOXX( 3,11) /  261 / ! 102       3
      DATA IDMOXX( 4,11) /  271 / ! 102       4
      DATA IDMOXX( 5,11) /  262 / ! 102       5
      DATA IDMOXX( 6,11) /  263 / ! 102       6
      DATA IDMOXX( 7,11) /  272 / ! 102       7
      DATA IDMOXX( 8,11) /  273 / ! 102       8
      DATA IDMOXX( 9,11) /  251 / ! 102       9

      DATA IDCRXX(12) / 112     / ! 
      DATA NMODXX(12) /  8      / ! 
      DATA IDMOXX( 1,12) /  107 / ! 112       1
      DATA IDMOXX( 2,12) /  100 / ! 112       2
      DATA IDMOXX( 3,12) /  101 / ! 112       3
      DATA IDMOXX( 4,12) /  102 / ! 112       4
      DATA IDMOXX( 5,12) /  103 / ! 112       5
      DATA IDMOXX( 6,12) /  104 / ! 112       6
      DATA IDMOXX( 7,12) /  105 / ! 112       7
      DATA IDMOXX( 8,12) /  106 / ! 112       8

      DATA IDCRXX(13) / 122     / ! 
      DATA NMODXX(13) / 10      / ! 
      DATA IDMOXX( 1,13) /  160 / ! 122       1
      DATA IDMOXX( 2,13) /  161 / ! 122       2
      DATA IDMOXX( 3,13) /  162 / ! 122       3
      DATA IDMOXX( 4,13) /  163 / ! 122       4
      DATA IDMOXX( 5,13) /  164 / ! 122       5
      DATA IDMOXX( 6,13) /  165 / ! 122       6
      DATA IDMOXX( 7,13) /  166 / ! 122       7
      DATA IDMOXX( 8,13) /  167 / ! 122       8
      DATA IDMOXX( 9,13) /  150 / ! 122       9
      DATA IDMOXX(10,13) /  153 / ! 122      10

      DATA IDCRXX(14) / 132     / ! 
      DATA NMODXX(14) /  5      / ! 
      DATA IDMOXX( 1,14) /  061 / ! 132       1
      DATA IDMOXX( 2,14) /  062 / ! 132       2
      DATA IDMOXX( 3,14) /  064 / ! 132       3
      DATA IDMOXX( 4,14) /  065 / ! 132       4
      DATA IDMOXX( 5,14) /  067 / ! 132       5

      DATA IDCRXX(15) / 142     / ! 
      DATA NMODXX(15) / 10      / ! 
      DATA IDMOXX( 1,15) /  207 / ! 142       1
      DATA IDMOXX( 2,15) /  200 / ! 142       2
      DATA IDMOXX( 3,15) /  201 / ! 142       3
      DATA IDMOXX( 4,15) /  202 / ! 142       4
      DATA IDMOXX( 5,15) /  203 / ! 142       5
      DATA IDMOXX( 6,15) /  204 / ! 142       6
      DATA IDMOXX( 7,15) /  205 / ! 142       7
      DATA IDMOXX( 8,15) /  215 / ! 142       8
      DATA IDMOXX( 9,15) /  235 / ! 142       9
      DATA IDMOXX(10,15) /  245 / ! 142      10

      DATA IDCRXX(16) / 152     / ! 
      DATA NMODXX(16) /  9      / ! 
      DATA IDMOXX( 1,16) /  212 / ! 152       1
      DATA IDMOXX( 2,16) /  222 / ! 152       2
      DATA IDMOXX( 3,16) /  232 / ! 152       3
      DATA IDMOXX( 4,16) /  213 / ! 152       4
      DATA IDMOXX( 5,16) /  223 / ! 152       5
      DATA IDMOXX( 6,16) /  233 / ! 152       6
      DATA IDMOXX( 7,16) /  214 / ! 152       7
      DATA IDMOXX( 8,16) /  224 / ! 152       8
      DATA IDMOXX( 9,16) /  234 / ! 152       9

      DATA IDCRXX(17) / 162     / ! 
      DATA NMODXX(17) / 11      / ! 
      DATA IDMOXX( 1,17) /  112 / ! 162       1
      DATA IDMOXX( 2,17) /  122 / ! 162       2
      DATA IDMOXX( 3,17) /  132 / ! 162       3
      DATA IDMOXX( 4,17) /  113 / ! 162       4
      DATA IDMOXX( 5,17) /  123 / ! 162       5
      DATA IDMOXX( 6,17) /  133 / ! 162       6
      DATA IDMOXX( 7,17) /  114 / ! 162       7
      DATA IDMOXX( 8,17) /  124 / ! 162       8
      DATA IDMOXX( 9,17) /  134 / ! 162       9
      DATA IDMOXX(10,17) /  115 / ! 162      10
      DATA IDMOXX(11,17) /  135 / ! 162      11

      DATA IDCRXX(18) / 172     / ! 
      DATA NMODXX(18) /  9      / ! 
      DATA IDMOXX( 1,18) /  012 / ! 172       1
      DATA IDMOXX( 2,18) /  022 / ! 172       2
      DATA IDMOXX( 3,18) /  032 / ! 172       3
      DATA IDMOXX( 4,18) /  013 / ! 172       4
      DATA IDMOXX( 5,18) /  023 / ! 172       5
      DATA IDMOXX( 6,18) /  033 / ! 172       6
      DATA IDMOXX( 7,18) /  015 / ! 172       7
      DATA IDMOXX( 8,18) /  025 / ! 172       8
      DATA IDMOXX( 9,18) /  035 / ! 172       9
C----------------------------------------------------------------------
      IF(NCRATE.GT.MXCRAT) THEN
         NMODUS=-1
      ELSE
         IDCRAT=IDCRXX(NCRATE)
         NMODUS=NMODXX(NCRATE)
         DO 100 I=1,12
            IDMODU(I)=IDMOXX(I,NCRATE)
100      CONTINUE
      ENDIF

  999 RETURN
      END
