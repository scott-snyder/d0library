C----------------------------------------------------------------------
      SUBROUTINE D3U_INI (IDIV,PATH,FOPT,IOPT,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To initialise DBL3 parameters. Should only be
C-    called once in a session. Use D3U_SET_*, to change parameters 
C-    e.g to change a top directory/file, D3U_SET_TOPD('topdir',0).
C-    In the case where you only are using one dbl3 file and one subpath,
C-    it is enough just to call D3U_INI.
C-
C-   Inputs  : IDIV (I)     Users Zebra division
C-             PATH (C)     Path name (max 24 characters)
C-             FOPT (C)     Options to fetch elements (argument in DBUSE)
C-             IOPT (C)     Options for inserting elements (argument in 
C-                          DBENTR), plus information if key 3 and 4 is time
C-                          or not time, 'R-NOT', means use option 'R' in
C-                          DBENTR and key 3 and 4 is not time, 'R' means
C-                          that key 3 and 4 is time.
C-
C-   Outputs : IRET         0=OK
C-   Controls: 
C-
C-   Created  26-NOV-1990   Lars O. Rasmussen
C-   Modified 10-OCT-1992   Lor, to be consistent with new d3u_set routines
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      LOGICAL D3U_SET_ZDIV,D3U_SET_PATH,D3U_SET_FOPT,D3U_SET_IOPT
C
      INTEGER          IDIV,IRET
      CHARACTER *(*)   PATH,FOPT,IOPT
C----------------------------------------------------------------------
      IRET = 0
      D3_DIV=IDIV
      IF (.NOT. D3U_SET_ZDIV(' ',IDIV)) THEN
         IRET = -1
         RETURN
      END IF
      IF (.NOT. D3U_SET_PATH(PATH,0)) THEN
         IRET = -1
         RETURN
      END IF
      IF (.NOT. D3U_SET_FOPT(FOPT,0)) THEN
         IRET = -1
         RETURN
      END IF
      IF (.NOT. D3U_SET_IOPT(IOPT,0)) THEN
         IRET = -1
         RETURN
      END IF
C
999   RETURN
      END 
