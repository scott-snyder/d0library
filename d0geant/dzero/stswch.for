      LOGICAL FUNCTION STSWCH
C-
C-
C-    Purposes and Methods:
C-    --------------------
C-    This routine allows the Program Builder user to set and reset
C-    (ON/OFF) the run time switches associated with the packages
C-    Created by the PROGRAM BUILDER Release V1.10
C-    14-JUL-1989 11:46
C-
C-
      LOGICAL PRODUC,FLGVAL
      LOGICAL Ld0
      LOGICAL Lcal
      LOGICAL Lcdc
      LOGICAL Lfdc
      LOGICAL Ltrd
      LOGICAL Lvtx
      LOGICAL Lmuo
      LOGICAL Llv0
      INTEGER NUMPAR
      DATA NUMPAR /8/
      CHARACTER*30 LABELS(8)
      CHARACTER TYPARR(8)
      INTEGER LIMITS(2,8)
C-
      DATA LABELS /
     X          'd0 Selected'
     X         ,'cal Selected'
     X         ,'cdc Selected'
     X         ,'fdc Selected'
     X         ,'trd Selected'
     X         ,'vtx Selected'
     X         ,'muo Selected'
     X         ,'lv0 Selected'
     X       /
      DATA TYPARR /
     X           'L'
     X          ,'L'
     X          ,'L'
     X          ,'L'
     X          ,'L'
     X          ,'L'
     X          ,'L'
     X          ,'L'
     X       /
C-
      IF (.NOT. PRODUC()) THEN
         Ld0    = FLGVAL('PBD_d0')
         Lcal   = FLGVAL('PBD_cal')
         Lcdc   = FLGVAL('PBD_cdc')
         Lfdc   = FLGVAL('PBD_fdc')
         Ltrd   = FLGVAL('PBD_trd')
         Lvtx   = FLGVAL('PBD_vtx')
         Lmuo   = FLGVAL('PBD_muo')
         Llv0   = FLGVAL('PBD_lv0')
         CALL GETDIS(NUMPAR,LABELS,TYPARR,LIMITS
     X              ,Ld0
     X              ,Lcal
     X              ,Lcdc
     X              ,Lfdc
     X              ,Ltrd
     X              ,Lvtx
     X              ,Lmuo
     X              ,Llv0
     X                                          )
C-
         CALL FLGSET('PBD_d0',Ld0   )
         CALL FLGSET('PBD_cal',Lcal  )
         CALL FLGSET('PBD_cdc',Lcdc  )
         CALL FLGSET('PBD_fdc',Lfdc  )
         CALL FLGSET('PBD_trd',Ltrd  )
         CALL FLGSET('PBD_vtx',Lvtx  )
         CALL FLGSET('PBD_muo',Lmuo  )
         CALL FLGSET('PBD_lv0',Llv0  )
      END IF
C-
      STSWCH = .TRUE.
      RETURN
      END
