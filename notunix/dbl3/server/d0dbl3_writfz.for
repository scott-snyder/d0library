C----------------------------------------------------------------------
      SUBROUTINE D0DBL3_WRITFZ
     &  (DIR,ZDIV,WHAT,PATH,NKEY,KEY,CCON,LINK,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will write a FZ file, with dbl3 information
C-    packed into the header vector.
C-
C-   Inputs  : DIR     (C)   Directory for output FZ file
C-                           This is normally the logical DB$TODO
C-             ZDIV    (I)   Zebra division used
C-             WHAT    (I)   What to do  10 : Entering a data object in D0
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
C-             KEY     (I)   Array of keys. In case WHAT=50,51 KEY(1:NKEY2) 
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
C-             LINK    (I)   Link to zebra bank or structure
C-                           only used in case 10
C-
C-   Outputs : IRET    (I)   <0=ERROR
C-   Controls: 
C-
C-   Created   8-JUN-1992   Lars O. Rasmussen
C-   Updated  18-JUN-1992   Stuart Fuess
C-                          Change input argument to specify directory
C-                          instead of file name; add call to D0DBL3_FILNAM;
C-                          add calls to D0DBL3_RESETFZN upon errors.
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
      INTEGER LENOCC,ICFIND
      EXTERNAL LENOCC,ICFIND
C
      CHARACTER*(*) DIR,PATH,CCON
      INTEGER ZDIV,NKEY,KEY(*),LINK,WHAT,IRET
C
      INTEGER LUNI,IOSTAT,IR,ILEN
      INTEGER NHVEC,HVEC(200),LC,NHOL,IOWDS(20)
      CHARACTER CHOP*1,CFRM*80,ZCHP*12
      CHARACTER*255 FILNAM
      LOGICAL IOK
C----------------------------------------------------------------------
      IRET = -1
C
C- Build the appropriate file name for the next file to go into
C- the TODO area
C
      CALL D0DBL3_FILNAM ( DIR, FILNAM, IOK )
      IF (.NOT.IOK) THEN
         IRET = -2
         RETURN
      END IF
C
C- Pack the header vector
C
      CALL D0DBL3_MAKHED (WHAT,PATH,NKEY,KEY,CCON,HVEC,NHVEC)
      IF (NHVEC .LE. 0) RETURN
      IF ((WHAT .NE. 10) .AND. (WHAT .NE. 11)) THEN
         CHOP = 'Z'
      ELSE 
         LC = LENOCC(CCON)
         IR = ICFIND ('R',CCON,1,LC)
         IF (IR .GE. 1 .AND. IR. LE. LC) THEN
            CHOP = 'L' 
         ELSE
            CHOP = ' '
         END IF
      END IF
      NHOL = HVEC(3)+HVEC(4)
      WRITE (CFRM,'(I6,A1,X,I6,A1)') NHVEC-NHOL,'I',NHOL,'H'
      CALL MZIOCH (IOWDS,20,CFRM)
C
C- Open the FZ file
C
      CALL GTUNIT (984,LUNI,IR)
      CALL D0OPEN (LUNI,FILNAM,'OG',IOK)
C
C- If error, must also reset sequential file number
C
      IF (.NOT. IOK) THEN
         CALL ERRMSG ('ERROR in open FZ file','D0DBL3_WRITFZ',' ','E')
         CALL D0DBL3_RESETFZN ( DIR, IOK )
         IRET = -3
         RETURN
      END IF
C
C- If successful, write the file
C
      CALL XZRECL(ILEN,ZCHP)
      CALL FZFILE (LUNI,ILEN,'OX')
      CALL FZOUT (LUNI,ZDIV,LINK,1,CHOP,IOWDS,NHVEC,HVEC)
C
C- Again, if error, must also reset sequential file number
C
      IF (IQUEST(1) .NE. 0) THEN
         CALL ERRMSG ('ERROR in FZOUT','D0DBL3_WRITFZ',' ','E')
         CALL D0DBL3_RESETFZN ( DIR, IOK )
         IRET = -4
         RETURN
      END IF
      CALL FZENDO (LUNI,'T')
      CLOSE (LUNI)
      CALL RLUNIT (984,LUNI,IR)
C
C- Everything worked, so set success code
C
      IRET = 0
C
  999 RETURN
      END
