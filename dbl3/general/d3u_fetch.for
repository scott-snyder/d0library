C----------------------------------------------------------------------
      SUBROUTINE D3U_FETCH (KEY,NLNK,LNK,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will fetch elements from the data base,
C-   corresponding to KEYS, and insert it into memory
C-   P.S. CURRENT VERSION ONLY PASS ONE LINK.
C-
C-   Inputs  : KEY           Array of keys for the search.
C-             NLNK          Length of passed array LNK
C-   Outputs : LNK           Array of links to data elements
C-             IRET          Number of elements found
C-             
C-   Controls: 
C-
C-   Created  26-JUN-1991   Lars O. Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER D3UTOP
C
      INTEGER          KEY(D3_NK)
      INTEGER          NLNK
      INTEGER          LNK(NLNK)
      INTEGER          IRET
C
      CHARACTER*24     TOPD
      INTEGER          LSH,LSN,LST
      INTEGER          I,J,IR
      INTEGER *2       K
      INTEGER          KEYC(D3_NK)
C----------------------------------------------------------------------
      IRET = 0
      LNK (1) = 0
C
      IR = D3UTOP(D3_PATH,TOPD)
      CALL RZCDIR('//'//TOPD,' ')
      IF (IQUEST(1) .NE. 0) THEN
         CALL MSGO ('e','D3U_FETCH','Path name invalid',0)
         RETURN
      END IF
C
      DO I = 1,D3_NK
         KEYC(I) = KEY(I)
      END DO
      KEYC(4) = KEY(3)
      CALL DBUSE (D3_PATH,D3_LK,D3_LD,KEYC(3),KEYC,D3_FSTR)
      IF (IQUEST(1) .EQ. 24) THEN
         GOTO 999     ! It's no error to find nothing
      ELSE IF (IQUEST(1) .NE. 0 .OR. D3_LD(1) .LE. 0) THEN
         CALL MSGO('en','D3U_FETCH','DBUSE error, quest=',iquest(1))
      END IF
      IRET = 1
      LNK(1) = D3_LD(1)
C      
  999 RETURN
      END
