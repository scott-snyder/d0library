      SUBROUTINE GSLINK (USER,NLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-   Reserve a location in the ZLINKA common (general link area)
C-   GSLINK will bomb if no more free room
C-   
C-   Outputs :
C-   NLINK = reserved structural link location 
C-
C-   ENTRY RSLINK: release a reserved structural link location
C-                 will not release the link if USER does not match
C-
C-   Input:
C-   USER  = user identifier (character*8)
C-   NLINK = location to release
C-
C-   Created   9-SEP-1987   Serban D. Protopopescu
C-   Updated   9-JUN-1994   Rajendran Raja  added entry GTSLINK_ONE 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER NSL(NSLINK),I,II,NLINK,K,LLINK
      CHARACTER*80 MSG
      CHARACTER*(*) USER,USER_WANTED
      CHARACTER*8 TEMP,USERS(NSLINK)
      INTEGER NLINK_TOTAL,J,LINKS_WANTED(NSLINK),MAXLINK_WANTED
      INTEGER NLINK_WANTED,LINK_WANTED
      DATA NSL/NSLINK*0/
C
      DO 1 I=1,NSLINK
      II=I
      IF (NSL(I) .EQ. 0) GO TO 3
    1 CONTINUE
C
C           no more room      
C
      CALL INTMSG(' No more structural links available, list of users f
     &ollows')
      DO 2 I=1,NSLINK,8
        MSG(1:10)=USERS(I)//'  '
        DO 22 K=1,7
   22   MSG=MSG(1:K*10)//USERS(I+K)//'  '
        CALL INTMSG(MSG)
    2 CONTINUE
      CALL ERRMSG(' No more links in ZLINKA','GSLINK',' ','F')  
C
C       keep track of reserved link
C
    3 NLINK=II
      NSL(II)=II
      USERS(II)=USER
      GOTO 999 
C
C----------------------------------------------------------------------
      ENTRY RSLINK(USER,NLINK)
C
C        release link if called by owner
      TEMP = USER
      IF(TEMP.EQ.USERS(NLINK)) THEN
        NSL(NLINK)=0      
        USERS(NLINK)='        '
      ENDIF
C
      GOTO 999 
C
C----------------------------------------------------------------------
      ENTRY GTSLINK(USER_WANTED,MAXLINK_WANTED,NLINK_TOTAL,LINKS_WANTED)
      NLINK_TOTAL=0
      TEMP = USER_WANTED
      DO J =1, NSLINK
        IF (USERS(J).EQ.TEMP) THEN
          NLINK_TOTAL = NLINK_TOTAL + 1
          LINKS_WANTED(NLINK_TOTAL) = LSLINK(J)
          IF (NLINK_TOTAL.GE.MAXLINK_WANTED) THEN
            GOTO 999
          ENDIF
        ENDIF
      ENDDO
      GOTO 999
      ENTRY GTSLINK_ONE(USER_WANTED,NLINK_WANTED,LINK_WANTED)
C Gets specified link number
      TEMP = USER_WANTED
      LLINK = 0
      LINK_WANTED = 0
      DO J =1, NSLINK
        IF (USERS(J).EQ.TEMP) THEN
          LLINK = LLINK + 1
          IF ( LLINK.EQ.NLINK_WANTED ) THEN
             LINK_WANTED = LSLINK(J)
             RETURN
          ENDIF
        ENDIF
      ENDDO
  999 RETURN
      END
