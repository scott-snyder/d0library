C----------------------------------------------------------------------
      LOGICAL FUNCTION D0DBL3_OUTFZ (
     &  FILID,CHOP,WHAT,PATH,NKEY,KEY,CCON,LINK,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will write a data structure to the currently
C-    open FZ file (should have been opened by D0DBL3_OPENFZ), with dbl3
C-    information packed into the header vector.
C-
C-   Inputs  : FILID   (I)   Identifier of the FZ file, output from
C-                           D0DBL3_OPENFZ
C-             CHOP    (C)   Currently not used
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
C-                          IF -10 invalid file ID
C-                          IF -11 d0dbl3_makehed failure
C-   Controls:
C-
C-   Created   26-OCT-1992   Lars O. Rasmussen
C-   Updated   24-NOV-1992   Lars Rasmussen, Make it possible to have
C-                           mutiple open FZ files
C-   Updated   15-JAN-1992   Shahriar Abachi   IRET corrected & extended
C-   Updated   14-JAN-1994   Shahriar Abachi   Provisons for empty compact
C-                                             FZ file was made
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
C
      CHARACTER*(*) PATH,CCON,CHOP
      INTEGER FILID,NKEY,KEY(*),LINK,WHAT,IRET
C
      INTEGER NHVEC,HVEC(200),LC,NHOL,IOWDS(20)
      CHARACTER CHP*1,CHLP*12,CFRM*80
      LOGICAL IOK
C
      INTEGER DBFZ_MXFZ
      PARAMETER (DBFZ_MXFZ=8)
      INTEGER DBFZ_NFLG
      PARAMETER (DBFZ_NFLG=8)
      INTEGER DBFZ_NFZ, DBFZ_LUN(DBFZ_MXFZ), DBFZ_ZDV(DBFZ_MXFZ)
      INTEGER DBFZ_FLG(DBFZ_NFLG,DBFZ_MXFZ)
      COMMON /D0DBL3FZ/ DBFZ_NFZ, DBFZ_LUN, DBFZ_ZDV, DBFZ_FLG
C----------------------------------------------------------------------
      IRET = 0
      D0DBL3_OUTFZ = .FALSE.
      CHLP = CCON
      CALL UPCASE (CHLP,CHLP)
      IF (FILID .LT. 1 .OR. FILID .GT. DBFZ_MXFZ) THEN
        CALL ERRMSG ('Invalide File ID','D0DBL3_OUTFZ',' ','E')
        IRET = -10
        RETURN
      END IF
C
C- Pack the header vector
C
      CALL D0DBL3_MAKHED (WHAT,PATH,NKEY,KEY,CCON,HVEC,NHVEC)
      IF (NHVEC .LE. 0) THEN
        IRET = -11
        RETURN
      ENDIF
      IF (NKEY .LT. 0 .OR. ((WHAT .NE. 10) .AND. (WHAT .NE. 11))) THEN
        CHP = 'Z'
      ELSE
        IF (INDEX(CHLP,'R') .GT. 0) THEN
          CHP = 'L'
        ELSE
          CHP = ' '
        END IF
      END IF
      NHOL = HVEC(3)+HVEC(4)
      WRITE (CFRM,'(I6,A1,X,I6,A1)') NHVEC-NHOL,'I',NHOL,'H'
      CALL MZIOCH (IOWDS,20,CFRM)
      CALL FZOUT (DBFZ_LUN(FILID),
     &  DBFZ_ZDV(FILID),LINK,1,CHP,IOWDS,NHVEC,HVEC)
      IF (IQUEST(1) .NE. 0) THEN
        IRET = IQUEST(1)
        CALL ERRMSG ('ERROR in FZOUT','D0DBL3_OUTFZ',' ','E')
        RETURN
      END IF
C
      D0DBL3_OUTFZ = .TRUE.
C
      RETURN
      END
