C----------------------------------------------------------------------
      SUBROUTINE D0DBL3_READFZ
     &  (FILNAM,ZDIV,WHAT,PATH,NKEY,KEY,CCON,LINK,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will read in a FZ file, unpack the header
C-    vector and pass the link.
C-
C-   Inputs  : FILNAM  (C)   Name of input FZ file
C-             ZDIV    (I)   Zebra division used
C-
C-   Output  : WHAT    (I)   What to do  10 : Entering a data object in D0
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
C-             PATH    (C)   RZ directory
C-             NKEY    (I)   Total number of passed keys (minimum=7)
C-             KEY     (I)   Array of keys. In case WHAT=50, KEY(1:NKEY) 
C-                           is the old keys and KEY(NKEY+1:2*NKEY) is the
C-                           new keys.
C-             CCON    (C)   Character option. 
C-                           WHAT = 10 : Character option in DBENTR and DBUSE,
C-                                       seperated with a '-' : 
C-                                       'option(dbentr)'-'option(dbuse)'
C-                           WHAT = 11 : Character option in DBENTR and DBUSE,
C-                                       seperated with a '-' : 
C-                                       'option(dbentr)'-'option(dbuse)'
C-                           WHAT = 30 : Character option in DBPURK and DBUSE
C-                                       separated with a '-' : 
C-                                       'option(dbpurk)'-'option(dbuse)'
C-                           WHAT = 31 : Character option in DBPURK and DBUSE
C-                                       separated with a '-' : 
C-                                       'option(dbpurk)'-'option(dbuse)'
C-                           WHAT = 50 : Character option in DBUSE
C-                           WHAT = 51 : Character option in DBUSE
C-             LINK    (I)   Link to zebra bank or structure
C-                           only used in case 10
C-
C-             IRET    (I)   <0=ERROR
C-   Controls: 
C-
C-   Created   8-JUN-1992   Lars O. Rasmussen
C-   Updated  19-JUN-1992   Stuart Fuess  
C-                          Change modify to WHAT=50 to add character option
C-   Updated  19-AUG-1992   Stuart Fuess  Remove call to FZLOGL (LUNI,4)
C-   Updated  01-NOV-1992   Lars Rasmussen, change default fz file to 
C-                          exchange format, alpha mode.
C-   Updated  24-NOV-1992   Lars Rasmussen, change default fz file to 
C-                          exchange format, binary mode.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
C
      CHARACTER*(*) FILNAM,PATH,CCON
      INTEGER ZDIV,NKEY,KEY(*),LINK,WHAT,IRET
C
      INTEGER LUNI,IOSTAT,IR,ILEN
      INTEGER NHVEC,HVEC(200),LH,IOWDS(20)
      CHARACTER*12 ZCHP
      LOGICAL IOK
C----------------------------------------------------------------------
      IRET = -1
C
C- Read in FZ file
C
      CALL GTUNIT (984,LUNI,IR)
      CALL D0OPEN (LUNI,FILNAM,'IG',IOK)
      IF (.NOT. IOK) THEN
         IRET = -2
         CALL ERRMSG 
     &     ('Error in opening FZ file','D0DBL3_READFZ',' ','E')
         RETURN
      END IF
      CALL XZRECL(ILEN,ZCHP)
      CALL FZFILE (LUNI,ILEN,'IX')
      LH = 200
      CALL FZIN (LUNI,ZDIV,LINK,2,' ',LH,HVEC)
      IF (IQUEST(1) .NE. 0) THEN
         IRET = -3
         CALL ERRMSG ('ERROR in FZIN','D0DBL3_READFZ',' ','E')
         RETURN
      END IF
      CALL FZENDI (LUNI,'T')
      CLOSE (LUNI)
      CALL RLUNIT (984,LUNI,IR)
C
C- Unpack header vector
C
      CALL D0DBL3_UNPHED (WHAT,PATH,NKEY,KEY,CCON,HVEC,LH)
      IF (LH .LE. 0) THEN
         IRET = -4
         CALL ERRMSG 
     &     ('Error in upacking header','D0DBL3_READFZ',' ','E')
         RETURN
      END IF
      IRET = 0
C
      RETURN
      END
