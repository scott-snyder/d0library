C----------------------------------------------------------------------
      SUBROUTINE D0DBL3_UNPHED (WHAT,PATH,NKEY,KEY,CCON,HVEC,NHVEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will unpack the header vector, returned from
C-    FZIN
C-
C-   Inputs  : HVEC    (I)   Packed information in DBL3 format
C-             NHVEC   (I)   Length of HVEC
C-
C-   Outputs : WHAT    (I)   What to do  10 : Entering a data object in D0
C-                                            style where KEY(3,4) not times
C-                                       11 : Entering a data object in D0
C-                                            style where KEY(3,4) are times
C-                                       30 : Deleting a data object in D0
C-                                            style where KEY(3,4) not times
C-                                       31 : Deleting a data object in D0
C-                                            style where KEY(3,4) are times
C-                                       50 : Modify keys in D0 style
C-                                            style where KEY(3,4) not times
C-                                       51 : Modify keys in D0 style
C-                                            style where KEY(3,4) are times
C-             PATH    (C)   DBL3 RZ directory
C-             NKEY    (I)   Total number of passed keys (minimum=7)
C-             KEY     (I)   Array of keys. In case WHAT=50, KEY(1:NKEY) 
C-                           is the old keys and KEY(NKEY+1:2*NKEY) is the
C-                           new keys.
C-             CCON    (C)   Character option. 
C-                           WHAT = 10 : Character option in DBENTR and DBUSE,
C-                                       separated with a '-' : 
C-                                       'option(dbentr)'-'option(dbuse)'
C-                           WHAT = 11 : Character option in DBENTR and DBUSE,
C-                                       separated with a '-' : 
C-                                       'option(dbentr)'-'option(dbuse)'
C-                           WHAT = 30 : Character option in DBPURK and DBUSE
C-                                       separated with a '-' : 
C-                                       'option(dbpurk)'-'option(dbuse)'
C-                           WHAT = 31 : Character option in DBPURK and DBUSE
C-                                       separated with a '-' : 
C-                                       'option(dbpurk)'-'option(dbuse)'
C-                           WHAT = 50 : Character option in DBUSE
C-                           WHAT = 51 : Character option in DBUSE
C-   Controls: 
C-
C-   Created   8-JUN-1992   Lars O. Rasmussen
C-   Updated  19-JUN-1992   Stuart Fuess  
C-                          Initialize return strings to ' ' before
C-                          filling in order to blank fill 
C-   Updated  19-JUN-1992   Stuart Fuess  
C-                          Change modify to WHAT=50 to add character option
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER *(*) PATH,CCON
      INTEGER NKEY,KEY(*),NHVEC,HVEC(*),WHAT,NHOL
C
      INTEGER I,J,K,NCOP,NCPA,IDAT,ITIM
      CHARACTER*80 CHLP
C----------------------------------------------------------------------
      WHAT = HVEC(1)
      NKEY = HVEC(2)
      NCOP = HVEC(3)
      NCPA = HVEC(4)
C
C- Initialize return strings to ' ' before filling in order to blank fill
C
      PATH = ' '
      CCON = ' '
C
C- Entering a data object in D0 style 
C
      IF ((WHAT .EQ. 10) .OR. (WHAT .EQ. 11)) THEN
         HVEC(5) = 0
         DO I = 1,NKEY
            KEY (I) = HVEC(5+I)
         END DO
         CALL UHTOC (HVEC(NKEY+6),4,CCON,NCOP*4)
         CALL UHTOC (HVEC(NKEY+HVEC(3)+6),4,PATH,NCPA*4)
C
C- Deleting a data object in D0 style 
C
      ELSE IF ((WHAT .EQ. 30) .OR. (WHAT.EQ.31)) THEN
         DO I = 1,NKEY
            KEY (I) = HVEC(6+I)
         END DO
         CALL UHTOC (HVEC(NKEY+7),4,CCON,NCOP*4)
         CALL UHTOC (HVEC(NKEY+HVEC(3)+7),4,PATH,NCPA*4)
C
C- Modifying keys in D0 style
C
      ELSE IF ((WHAT .EQ. 50) .OR. (WHAT.EQ.51)) THEN
         DO I = 1,2*NKEY
            KEY(I) = HVEC(5+I)
         END DO
         CALL UHTOC (HVEC(2*NKEY+6),4,CCON,NCOP*4)
         CALL UHTOC (HVEC(2*NKEY+HVEC(3)+6),4,PATH,NCPA*4)
C
C- Nothing to do
C
      ELSE 
         CALL ERRMSG ('Invalid option','D0DBL3_UNPHED',' ','E')
         WHAT = -1
C
      END IF
C
  999 RETURN
      END
