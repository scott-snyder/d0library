      SUBROUTINE STP_GSLINK (USER,NLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-   Reserve a location in the STP_ZLINKA common (general STP link area)
C-   STP_GSLINK will bomb if no more free room
C-   
C-   Outputs :
C-   NLINK = reserved structural link location 
C-
C-   ENTRY STP_RSLINK: release a reserved structural link location
C-                 will not release the link if USER does not match
C-
C-   Input:
C-   USER  = user identifier (character*8)
C-   NLINK = location to release
C-
C-   Created   25-SEP-1990   Chip Stewart (based on GSLINK)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER NSL(STP_NSLINK),I,II,NLINK,K
      CHARACTER*80 MSG
      CHARACTER*(*) USER
      CHARACTER*8 TEMP,USERS(STP_NSLINK)
      DATA NSL/STP_NSLINK*0/
C
      DO 1 I=1,STP_NSLINK
      II=I
      IF (NSL(I) .EQ. 0) GO TO 3
    1 CONTINUE
C
C           no more room      
C
      CALL INTMSG(' No more structural links available, list of users f
     &ollows')
      DO 2 I=1,STP_NSLINK,8
        MSG(1:10)=USERS(I)//'  '
        DO 22 K=1,7
   22   MSG=MSG(1:K*10)//USERS(I+K)//'  '
        CALL INTMSG(MSG)
    2 CONTINUE
      CALL ERRMSG(' No more links in STP_ZLINKA','STP_GSLINK',
     &  ' ','F')
C
C       keep track of reserved link
C
    3 NLINK=II
      CALL STP_INZLNK
      NSL(II)=II
      USERS(II)=USER
      RETURN
C
C----------------------------------------------------------------------
      ENTRY STP_RSLINK(USER,NLINK)
C
C        release link if called by owner
      TEMP = USER
      IF(TEMP.EQ.USERS(NLINK)) NSL(NLINK)=0      
C
  999 RETURN
      END
