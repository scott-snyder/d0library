C----------------------------------------------------------------------
      LOGICAL FUNCTION D3U_SET_PATH(CIN,IIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : A bunch of functions for changing dbl3 
C-    parameters. All the functions has two arguments, the first 
C-    argument is a character string, the second an integer. Each
C-    function is using only one of the arguments. 
C-    e.g pass a string   :  D3U_SET_PATH('//BUM/MELUM/OOOH',0)
C-    e.g pass an integer :  D3U_SET_ZDIV(' ',IDVSTP)
C-
C-    D3U_SET_PATH (PATH,*)
C-         input   :  PATH (C)  set/change path. It can have the format
C-                    of a full path (//head/sub) or a subparth (/sub),
C-                    in the last case the current top directory will be used.
C-
C-    D3U_SET_TOPD (TOPD,*)
C-         input   :  TOPD (C) set/change top directory (set/change file).
C-                    The current sub-path will be used.
C-
C-    D3U_SET_FOPT (FOPT,*)
C-         input   :  FOPT (C)  set/change options to fetch an element
C-                              e.g 'S348910'
C-    D3U_SET_IOPT (IOPT,*)
C-         input   :  IOPT (C)  set/change options to insert an element
C-                              e.g. 'R' or 'R-NOT' to specify that that
C-                              key 3,4 is NOT time.
C-    D3U_SET_ZDIV (*,IDIV)
C-         input   :  IDIV (I) Set/change used zebra division
C-
C-   Returned value  :  .true. = ok, .false. = something went wrong
C-
C-   Created  10-OCT-1992   Lars Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INTEGER ICLOC,ICFIND,ICDECI
      INTEGER D3UTOP,D3UNIT
      LOGICAL D3U_SET_ZDIV
      LOGICAL D3U_SET_TOPD
      LOGICAL D3U_SET_FOPT
      LOGICAL D3U_SET_IOPT
C
      CHARACTER *(*) CIN
      INTEGER IIN
C
      INTEGER I,J,K,LC,C1,C2,IRET
      CHARACTER*1  CH
      CHARACTER*48 CHLP
      CHARACTER*10  CURTOP
      CHARACTER*42 CURSUB
      DATA CURTOP /' '/
      DATA CURSUB /' '/
C----------------------------------------------------------------------
      CHLP = CIN
      CALL UPCASE (CHLP,CHLP)
      CALL DBSBLC(CHLP,CHLP,LC)
      IF (LC .LE. 0) THEN
         D3U_SET_PATH = .FALSE.
         RETURN
      END IF
      C1 = ICLOC('//',2,CHLP,1,LC)
      IF (C1 .GE. 1 .AND. C1 .LE. LC) THEN
         C1 = C1 + 2
      ELSE
         C1 = 1
      END IF
      C2 = ICFIND('/',CHLP,C1,LC)
      IF (C2 .GE. 1 .AND. C2 .LE. LC) THEN
         C2 = C2 - 1
      ELSE
         D3U_SET_PATH = .FALSE.   ! we need at least a sub parth
         RETURN
      END IF
      IF (C1 .GT. 1) THEN         ! there was top directory
         D3_PATH = CHLP
         CURTOP = '//'//CHLP(C1:C2)
         CURSUB = CHLP(C2+1:)
      ELSE
         CALL DBSBLC(CURTOP,CURTOP,LC)
         D3_PATH = CURTOP(1:LC)//CHLP(1:)
         CURSUB = CHLP(1:LEN(CHLP))
      END IF
      D3U_SET_PATH = .TRUE.
      RETURN
C
C----------------------------------------------------------------------
      ENTRY D3U_SET_ZDIV(CIN,IIN)
C----------------------------------------------------------------------
      D3_DIV = IIN
      D3U_SET_ZDIV = .TRUE.
      RETURN
C
C----------------------------------------------------------------------
      ENTRY D3U_SET_TOPD(CIN,IIN)
C----------------------------------------------------------------------
      CHLP = CIN
      IRET = D3UTOP(CHLP,CHLP)
      IF (IRET .LE. 0) THEN
         D3U_SET_TOPD = .FALSE.
      ELSE
         CURTOP = '//'//CHLP(1:IRET)
         D3_PATH = '//'//CHLP(1:IRET)//CURSUB(1:)
         D3_UNIT = D3UNIT(' ',D3_PATH)
         D3U_SET_TOPD = .TRUE.
      END IF
      RETURN
C
C----------------------------------------------------------------------
      ENTRY D3U_SET_FOPT(CIN,IIN)
C----------------------------------------------------------------------
      D3U_SET_FOPT = .TRUE.
      CHLP = CIN
      CALL UPCASE (CHLP,CHLP)
      CALL DBSBLC(CHLP,CHLP,LC)
      IF (LC .LE. 0) THEN
         D3_FSTR = ' '
      ELSE
         D3_FSTR = CHLP(1:LC)
      END IF
C
C- Get array of extra keys to search on
C
      D3_NKK = 0
      K = 1
      DO WHILE (K .LE. LC)
         CH = D3_FSTR(K:K)
         IF (CH .EQ. '7' .OR. CH .EQ. '8' .OR. CH .EQ. '9') THEN
            D3_NKK = D3_NKK + 1
            D3_XKI(D3_NKK) = ICDECI(CH,1,1)
         ELSE IF ((CH .EQ. '1' .OR. CH .EQ. '2') .AND. K .LE. LC-1) THEN
            D3_NKK = D3_NKK + 1
            D3_XKI(D3_NKK) = 10*ICDECI(CH,1,1)
            K = K + 1
            CH = D3_FSTR(K:K)
            D3_XKI(D3_NKK) = D3_XKI(D3_NKK)+ ICDECI(CH,1,1)
         END IF
         K = K + 1
      END DO
      RETURN
C
C----------------------------------------------------------------------
      ENTRY D3U_SET_IOPT(CIN,IIN)
C----------------------------------------------------------------------
      CHLP = CIN
      CALL UPCASE (CHLP,CHLP)
      CALL DBSBLC(CHLP,CHLP,LC)
      C1 = ICLOC('-NOT',4,CHLP,1,LC)
      IF (C1 .GT. 0 .AND. C1 .LT. LC) THEN
         D3_NOT = .TRUE.
         DO I = 0,3
            CHLP(C1+I:C1+I) = ' '
         END DO
      ELSE
         D3_NOT = .FALSE.
      END IF
      CALL DBSBLC(CHLP,CHLP,LC)
      IF (LC .LE. 0) THEN
         D3_ISTR = ' '
      ELSE
         D3_ISTR = CHLP(1:LC)
      END IF
      D3U_SET_FOPT = .TRUE.
C
  993 RETURN
C
      END
