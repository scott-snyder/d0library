      SUBROUTINE JPICK(DSPDEV,PHYDEV,ECHOLV,BUTTON,SEGNAM,PICKID)
C  VIRTUAL PICK FUNCTION
      INTEGER DSPDEV,PHYDEV,ECHOLV,BUTTON,SEGNAM,PICKID
      CALL JLOCAT(DSPDEV,PHYDEV,ECHOLV,BUTTON,VX,VY)
      CALL JSPICK(VX,VY,DSPDEV,SEGNAM,PICKID)
      END