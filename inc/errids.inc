C---------------------------------------------------
C-   Filename  ERRIDS.INC
C-   Purpose   Storage for logged messages of character
C-             type 
C-             NOTE: MAXSTR is defined in ERRMSG.PARAMS
C-   Created   18-DEC-1988    Jun-jang Jeng (MSU)
C---------------------------------------------------
       CHARACTER*32 ID
       COMMON /ERRIDS/ ID(MAXSTR)

