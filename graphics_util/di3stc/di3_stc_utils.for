      INTEGER FUNCTION MYL(STR)
C Returns the length of a string minus any trailing unwanted characters
      CHARACTER STR*(*)
      MYL=LEN(STR)
    1 IT=ICHAR(STR(MYL:MYL))
      IF(IT.NE.32.AND.IT.NE.13.AND.IT.NE.10.AND.IT.NE.0) RETURN
      MYL=MYL-1
      IF(MYL.EQ.0) RETURN
      GO TO 1
      END


