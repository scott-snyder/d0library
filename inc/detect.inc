C-- main detector geometry bank
      PARAMETER (NKW=20000,NDW=60)
      COMMON/DETECT/KW(NKW)
      DIMENSION SW(NKW)
      EQUIVALENCE (KW(1),SW(1))
C
