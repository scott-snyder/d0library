C----------------------------------------------------------------------
      LOGICAL FUNCTION D0DBL3_INFZ (
     &  FILID,CHOP,WHAT,PATH,NKEY,KEY,CCON,LINK,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will read the next data structure in an
C-    already open FZ file (it should had been opened by a call to
C-    D0DBL3_OPENFZ)
C-
C-   Return   .true.  it went ok
C-            .false. something went wrong
C-
C-   Inputs  : FILID   (I)   Identifier of the FZ file, output from
C-                           D0DBL3_OPENFZ
C-             CHOP    (C)   currently not used
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
C-                           only used in case 10 or 11
C-
C-   Output    IRET    (I)   = iquest(1)
C-                         if -10 invalid file ID
C-
C-   Updated  24-NOV-1992   Lars Rasmussen, Make it possible to have
C-                          mutiple open FZ files
C-   Updated  15-JAN-1993   Shahriar Abachi  IRET corrected & extended
C-   Updated  18-NOV-1993   Shahriar Abachi  D0DBL3_GTHVEC entry added
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
C
      CHARACTER*(*) PATH,CCON,CHOP
      INTEGER FILID,NKEY,KEY(*),LINK,WHAT,IRET
C
      INTEGER IR,NHVEC,HVEC(200),LH,IOWDS(20)
C
      INTEGER DBFZ_MXFZ
      PARAMETER (DBFZ_MXFZ=8)
      INTEGER DBFZ_NFLG,I,NH,HV(200)
      PARAMETER (DBFZ_NFLG=8)
      INTEGER DBFZ_NFZ, DBFZ_LUN(DBFZ_MXFZ), DBFZ_ZDV(DBFZ_MXFZ)
      INTEGER DBFZ_FLG(DBFZ_NFLG,DBFZ_MXFZ)
      LOGICAL D0DBL3_GTHVEC
      COMMON /D0DBL3FZ/ DBFZ_NFZ, DBFZ_LUN, DBFZ_ZDV, DBFZ_FLG
C----------------------------------------------------------------------
      D0DBL3_INFZ = .FALSE.
      IRET = 0
      IF (FILID .LT. 1 .OR. FILID .GT. DBFZ_MXFZ) THEN
        CALL ERRMSG ('Invalide File ID','D0DBL3_INFZ',' ','E')
        IRET = -10
        RETURN
      END IF
C
C- Read next data structure.
C
      LH = 200
      CALL FZIN (DBFZ_LUN(FILID),DBFZ_ZDV(FILID),LINK,2,' ',LH,HVEC)
      IF (IQUEST(1) .NE. 0) THEN
        IRET = IQUEST(1)
        RETURN
      END IF
      CALL D0DBL3_UNPHED (WHAT,PATH,NKEY,KEY,CCON,HVEC,LH)
      IF (LH .LE. 0) THEN
        IRET = -1
        CALL ERRMSG
     &     ('Error in upacking header','D0DBL3_INFZ',' ','E')
        RETURN
      END IF
C
      D0DBL3_INFZ = .TRUE.
C
  999 RETURN
C
C - ****************************************************************
      ENTRY D0DBL3_GTHVEC(HV,NH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To return the HVEC and NHVEC info (contents
C-                         of FZ header file).
C-
C-   Inputs  :
C-
C-   Output  : HV       Arrary conataning the FZ header info
C-             NH       Dimension of HV
C-
C-   Created  18-NOV-1993   Shahriar Abachi
C-
C----------------------------------------------------------------------
      D0DBL3_GTHVEC = .FALSE.
C
      NH = LH
      DO I=1,NH
        HV(I) = HVEC(I)
      ENDDO
C
      D0DBL3_GTHVEC = .TRUE.
 1999 RETURN
      END
