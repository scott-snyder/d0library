      SUBROUTINE GEAISA(IPCGEA,IPCISA)
C-----------------------------------------------------------------------
C-    This routine converts from geant particle code to Isajet code.
C- The valid range of the Geant particle code is between 1 and 41.
C-
C-  Input:  IPCGEA =  Geant particle code. 
C-  Output: IPCISA =  Isajet particle code.   0 for invalid Geant code.
C-
C-  S.Kunori,    Mar.,1986
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IPCGEA,IPCISA
      INTEGER TABLE(41)
      LOGICAL FIRST 
      DATA FIRST/.TRUE./
      IF(FIRST) THEN
         FIRST=.FALSE.
         TABLE(1)=10
         TABLE(2)=-12
         TABLE(3)=12
         TABLE(4)=-11
         TABLE(5)=-14
         TABLE(6)=14
         TABLE(7)=110
         TABLE(8)=120
         TABLE(9)=-120
         TABLE(10)=-20
         TABLE(11)=130
         TABLE(12)=-130
         TABLE(13)=1220
         TABLE(14)=1120
         TABLE(15)=-1120
         TABLE(16)=20
         TABLE(17)=220
         TABLE(18)=2130
         TABLE(19)=1130
         TABLE(20)=1230
         TABLE(21)=2230
         TABLE(22)=1130
         TABLE(23)=2330
         TABLE(24)=3331
         TABLE(25)=-1220
         TABLE(26)=-2130
         TABLE(27)=-2230
         TABLE(28)=-1230
         TABLE(29)=-1130
         TABLE(30)=-1330
         TABLE(31)=-2330
         TABLE(32)=-3331
         TABLE(33)=16
         TABLE(34)=-16
         TABLE(35)=-240
         TABLE(36)=240
         TABLE(37)=-140
         TABLE(38)=140
         TABLE(39)=-340
         TABLE(40)=340
         TABLE(41)=2140
      ENDIF
      IF(IPCGEA.GE.1.AND.IPCGEA.LE.41) THEN
          IPCISA=TABLE(IPCGEA)
      ELSE
          IPCISA=0
      ENDIF
      RETURN
      END
