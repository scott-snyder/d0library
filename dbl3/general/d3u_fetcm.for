C----------------------------------------------------------------------
      SUBROUTINE D3U_FETCM (KYS,LINK,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will first scan memory for valid data (defined 
C-    by KYS). It will use KEY(3) to check if validity range still is ok. 
C-    It will require a match for the extra keys defined in D3U_SET_FOPT 
C-    or D3U_INI. The node or path is define by D3U_SET_PATH or D3U_INI. 
C-    If no valid data in memory, if will fetch new data from current dbl3 
C-    file.
C-
C-   Inputs  : KYS    Array of keys, 
C-   Outputs : LINK   Link to data
C-             IRET   = -2 something went wrong
C-                    = -1 no data anywhere
C-                    =  0 found old data
C-                    =  1 found new data
C-
C-   Created  15-OCT-1992   Lars Rasmussen
C-
C-   (like D3U_FETCH, except that it checks memory before fetching)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER D3UTOP
C
      INTEGER KYS(*),LINK,IRET
      INTEGER I,J,K,LNOD,LKEY
      LOGICAL HERE
      INTEGER KEYC(D3_NK)
      INTEGER *2 K2,IR
      CHARACTER*24 TOPD
C----------------------------------------------------------------------
      LINK = 0
      IRET = -2
C
      IR = D3UTOP(D3_PATH,TOPD)
      CALL RZCDIR('//'//TOPD,' ')
      IF (IQUEST(1) .NE. 0) THEN
         CALL MSGO ('e','D3U_FETCM','Path name invalid',0)
         RETURN
      ELSE IF (IQUEST(7) .LE. 0) THEN
         CALL MSGO ('w','D3U_FETCM','Path is empty',0)
         RETURN
      END IF
C
C- Check if valid data in memory
C
      CALL DBUME (D3_PATH,KYS,D3_NK,LKEY,D3_FSTR)
      IF (LKEY .GT. 0) THEN
         IRET = 0
         LINK = LC(LKEY-1)
         RETURN
      END IF
C
C- We will fetch a new element
C
      DO I = 1,D3_NK
         KEYC(I) = KYS(I)
      END DO
      KEYC(4) = KYS(3)
      CALL DBUSE (D3_PATH,LKEY,LINK,KEYC(3),KEYC,D3_FSTR)
      IF (IQUEST(1) .EQ. 24) THEN
         IRET = -1
         LINK = 0     ! it's no error to find nothing
         RETURN
      ELSE IF (IQUEST(1) .NE. 0) THEN
         IRET = -2
         LINK = 0
         CALL MSGO('wn','D3U_FETCM','DBUSE error, quest=',iquest(1))
         RETURN
      END IF
      CALL DBFREE (D3_PATH,LKEY,KEYC,D3_FSTR)
      IRET = 1
C
      RETURN
      END
