      SUBROUTINE DUKEYD( PATH, KEYS, NKEYS, NWKEYS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Give whole key vectors at given PATH
C-
C-   Inputs  : PATH  : Path name 
C-   Outputs : KEYS  : KEY vectors
C-                     You should put in caller for easy access key vectors
C-                        INTEGER  KEYV( Number_of_word_in_a_key, MAX_NKEYS)
C-                        EUQIVALENCE (KEYV(1,1), KEYS(1,1) )
C-             NKEYS  : number of KEY vector
C-             NWKEYS : number of words in a KEY
C-   Controls: None
C-   Error Condtion   : IQUEST(1) = 0 : No error
C-                                = other : Error
C-
C-   Created   9-JAN-1992   Atsushi Taketani
C-
C----------------------------------------------------------------------
      CHARACTER*(*) PATH
      INTEGER  KEYS(*), NKEYS, NWKEYS
C
      INCLUDE 'D0$INC:QUEST.INC'
      CHARACTER*80 PATHN
      INTEGER  NCHAR, IKEY, KEY(100), KTKEY, KTKEYS 
      COMMON /DBUSER/ IDEBDB, IDISDB, IDIVDB, IHKYDB, IKDRDB, IONODB
     +              , KOFSDB, KOFUDB, LBADDB, LBDADB, LBFXDB, LBFYDB
     +              , LBKYDB, LBNODB, LFIXDB, LREFDB(7),      LSAVDB
     +              , LTOPDB, LPRTDB, NTOPDB
C----------------------------------------------------------------------
*** Suppress blanks from the path name
*
      CALL DBSBLC (PATH, PATHN, NCHAR)
*
* *** Create (or complete) database skeleton in memory
*                       (banks NODB and KYDB)
*
      CALL DBNODE (PATHN, LBNODB)
      IF (IQUEST(1).NE.0) THEN
        NKEY = 0
        NWKEY = 0
        GOTO 999
      END IF
*
      CALL RZCDIR( PATHN, ' ' )       ! KEY BANK INFORMATION
      IF (IQUEST(1).NE.0) THEN
        NKEY = 0
        NWKEY = 0
        GOTO 999
      END IF

      NKEYS  = IQUEST(7)
      NWKEYS = IQUEST(8)
      IF ( (NKEYS.LE.0).OR.(NWKEYS.LE.0) ) GOTO 999
*
      KTKEYS = 0
      DO IKEY = 1,NKEYS
        CALL DBKEYR( IKEY, NWKEYS, KEY )
        DO KTKEY=1,NWKEYS
          KTKEYS = KTKEYS + 1
          KEYS(KTKEYS) = KEY(KTKEY)
        END DO
      END DO
*
  999 RETURN
      END
