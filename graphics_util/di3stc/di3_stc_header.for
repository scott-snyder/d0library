      SUBROUTINE STC_HEADER(HEDSTR)
C----------------------------------------------------------------------
C-   Purpose and Methods : Accept string for STC_PLOT header
C-   Created  12-NOV-1990   Michael Shupe
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DI3STC.INC'
      CHARACTER*(*) HEDSTR
C----------------------------------------------------------------------
      HEADR=HEDSTR
      IF(INPROG.EQ.1) THEN
        CALL JVPORT(-1.,1.,-ASPECT,ASPECT)
C        CALL JPURGE(200)
        IF(IDEV.EQ.1) THEN
          CALL JROPEN(200)
        ELSE
          CALL JOPEN
        ENDIF
        CALL JSIZE(.02,.02)
        LENG=MYL(HEADR)
        CALL JJUST(1,3)
        CALL JMOVE(-.28,1.1)
        CALL J3STRG(HEADR(1:LENG))
        IF(IDEV.EQ.1) THEN
          CALL JRCLOS
        ELSE
          CALL JCLOSE
        ENDIF
        CALL STC_NEWFRAME(0)
      ENDIF
  999 RETURN
      END
