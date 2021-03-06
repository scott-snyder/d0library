      COMMON/DHZMAP/NZDIM,NZCNT,ZNUM(200),LNKD(200),LXUP(200),
     1 NQZDIM,NQZCNT,QZLST(41),NQZL,QZPNT(2,5),
     2 NPSDIM,NPS,PSPNT(2,50),PSLST(500)
      INTEGER NZDIM,NZCNT,ZNUM,LNKD,LXUP,NQZDIM,NQZCNT,NQZL,
     1 QZLST,QZPNT,NPSDIM,NPS,PSPNT,PSLST
C
      COMMON/DHZMAC/ZBNAM(200),DEFIO(200),PSNAM(50)
      CHARACTER ZBNAM*4,DEFIO*16,PSNAM*60
C
C  ************** BEGIN DESCRIPTION OF /DHZMAP/ **************
C
C  NZDIM      :  MAXIMUM NUMBER OF BANK NAMES.
C  NZCNT      :  NUMBER OF BANK NAMES IN LIST.
C  ZBNAM(K)   :  NAME OF BANK (CHARACTER VARIABLE).
C  DEFIO(K)   :  IO CHARACTERISTIC OF BANK.
C  LNKD(K)    :  NAME ARRAY INDEX OF PARENT BANK.
C  LXUP(K)    :  INDEX OF SUPPORTING STRUCTURAL LINK.
C  NQZDIM     :  MAXIMUM NUMBER OF BANKS IN AMBIGUITY LIST.
C  NQZCNT     :  NUMBER OF BANKS IN AMBIGUITY LIST.
C  NQZL       :  NUMBER OF AMBIGUOUS GROUPS.
C  QZPNT(J,K) :  POINTERS TO AMBIGUOUS BANKS FOR GROUP K.
C  NPSDIM     :  MAXIMUM NUMBER OF LINK LISTS.
C  NPS        :  NUMBER OF LINK LISTS.
C  PSPNT(J,K) :  POINTERS TO ELEMENTS OF LINK LIST K.
C  PSLST(L)   :  LIST OF LINK INDICES.
C  PSNAM(K)   :  PATH NAME FOR LINK LIST K.
C
C  **************  END DESCRIPTION OF /DHZMAP/  **************
