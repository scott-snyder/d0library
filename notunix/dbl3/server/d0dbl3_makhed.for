C----------------------------------------------------------------------
      SUBROUTINE D0DBL3_MAKHED (WHAT,PATH,NKEY,KEY,CCON,HVEC,NHVEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will pack dbl3 information into a vector, can
C-    be used by FZOUT
C-
C-   Inputs  : WHAT    (I)   What to do  10 : Entering a data object in D0
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
C-                           are the old keys and KEY(NKEY+1:2*NKEY) are the
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
C-
C-   Outputs : HVEC    (I)   Packed information in DBL3 format
C-             NHVEC   (I)   Length of HVEC, <=0=ERROR
C-   Controls: 
C-
C-   Created   8-JUN-1992   Lars O. Rasmussen
C-   Updated  19-JUN-1992   Stuart Fuess  
C-                          Change modify to WHAT=50 to add character option
C-   Updated  14-JAN-1994   S. ABACHI  Provisions for handling empty compact
C-                                     FZ file was made
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LENOCC
      EXTERNAL LENOCC
C
      CHARACTER *(*) PATH,CCON
      INTEGER NKEY,KEY(*),NHVEC,HVEC(*),WHAT,NHOL
C
      INTEGER I,J,K,NCOP,NCPA,IDAT,ITIM
      CHARACTER*80 CHLP
C----------------------------------------------------------------------
      HVEC(1) = WHAT
      HVEC(2) = NKEY
      NCOP = LENOCC(CCON)
      HVEC(3) = (NCOP+3)/4
      NCPA = LENOCC(PATH)
      HVEC(4) = (NCPA+3)/4
C
C- Entering a data object in D0 style 
C
      IF ((WHAT .EQ. 10) .OR. (WHAT .EQ. 11)) THEN
         HVEC(5) = 0
         DO I = 1,IABS(NKEY)
            HVEC(5+I) = KEY(I)
         END DO
         CALL UCTOH (CCON,HVEC(IABS(NKEY)+6),4,NCOP)
         CALL UCTOH (PATH,HVEC(IABS(NKEY)+HVEC(3)+6),4,NCPA)
         NHVEC = 5+IABS(NKEY)+HVEC(3)+HVEC(4)
C
C- Deleting a data object in D0 style 
C
      ELSE IF ((WHAT .EQ. 30) .OR. (WHAT.EQ.31)) THEN
         HVEC(5) = KEY(3)
         HVEC(6) = 0
         DO I = 1,IABS(NKEY)
            HVEC(6+I) = KEY(I)
         END DO
         CALL UCTOH (CCON,HVEC(IABS(NKEY)+7),4,NCOP)
         CALL UCTOH (PATH,HVEC(IABS(NKEY)+HVEC(3)+7),4,NCPA)
         NHVEC = 6+IABS(NKEY)+HVEC(3)+HVEC(4)
C
C- Modifying keys in D0 style
C
      ELSE IF ((WHAT .EQ. 50) .OR. (WHAT.EQ.51)) THEN
         HVEC(5) = 0
         DO I = 1,2*IABS(NKEY)
            HVEC(5+I) = KEY(I)
         END DO
         CALL UCTOH (CCON,HVEC(2*IABS(NKEY)+6),4,NCOP)
         CALL UCTOH (PATH,HVEC(2*IABS(NKEY)+HVEC(3)+6),4,NCPA)
         NHVEC = 5+2*IABS(NKEY)+HVEC(3)+HVEC(4)
C
C- Nothing to do
C
      ELSE 
         CALL ERRMSG ('Invalid option','D0DBL3_MAKHED',' ','E')
         NHVEC = 0
C
      END IF
C
  999 RETURN
      END
