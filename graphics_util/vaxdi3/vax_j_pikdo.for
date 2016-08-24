      SUBROUTINE J_PIKDO(X,Y,Z)
C  SET UP PICK LIMITS RECTANGLE AS A GIVEN PICKID IS PUT INTO
C  SEGMENT STORAGE
      INCLUDE 'D0$INC:DI3INC.INC'
C  GET VIRTUAL COORDINATES
      CALL JCONWV(X,Y,Z,VX,VY)
      IF(IPICID.NE.IPNOW) GO TO 100
      IF(ISEGNM.NE.ISNOW) GO TO 100
C  WE ARE PROCESSING THIS SEGMENT AND PICK ID ALREADY
    5 IF(VX.GE.PKXMN(NPICKS)) GO TO 10
      PKXMN(NPICKS)=VX
      GO TO 20
   10 IF(VX.LE.PKXMX(NPICKS)) GO TO 20
      PKXMX(NPICKS)=VX
   20 IF(VY.GE.PKYMN(NPICKS)) GO TO 30
      PKYMN(NPICKS)=VY
      RETURN
   30 IF(VY.LE.PKYMX(NPICKS))RETURN
      PKYMX(NPICKS)=VY
      RETURN
C  THIS IS A NEW PICK OR SEGMENT
  100 IF(NPICKS.EQ.100) GO TO 1000
      NPICKS=NPICKS+1
      IPNOW=IPICID
      ISNOW=ISEGNM
      IPKSEG(NPICKS)=ISEGNM
      IDPICK(NPICKS)=IPICID
      GO TO 5
C  OVERFLOW
 1000 WRITE(JUNIT,*) 'PICK STORAGE OVERFLOW'
      END