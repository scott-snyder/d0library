      SUBROUTINE UDST_GET_VTXT(LVTXT,XDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Fill VTXT parameters into array XDATA
C-
C-   Inputs  : LVTXT - link to VTXT
C-   Outputs : XDATA 
C-         
C-   Created  15-OCT-1994   Ulrich Heintz
C-
C---------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE   'D0$INC:ZEBCOM.INC'
      INTEGER   I,LVTXT,KVTXT,KVTXT1,ID_VTXT
C
      PARAMETER (KVTXT=12)
      CHARACTER*8 VTXT_TAGS(KVTXT)
      DATA VTXT_TAGS/
     &   'PHIV'  ,'XV'    ,'YV'    ,'THV'   ,'DZTHV' ,'ZV' 
     &  ,'DZDRV' ,'DPHIV' ,'DXYV'  ,'DTHV'  ,'DRZV'  ,'DEDXV' /
      REAL PHIV,XV,YV,THV,DZTHV,ZV,DZDRV,DPHIV,DXYV,DTHV,DRZV,DEDXV  
      COMMON/VTXT_OBJ/
     &     PHIV,XV,YV,THV,DZTHV,ZV,DZDRV,DPHIV,DXYV,DTHV,DRZV,DEDXV  
      REAL XX(KVTXT),XDATA(KVTXT)
      EQUIVALENCE (XX,PHIV)
C----------------------------------------------------------------------
      CALL VZERO(XX,KVTXT)
      IF(LVTXT.GT.0)THEN
        PHIV   = Q(LVTXT+ 6)
        XV     = Q(LVTXT+ 7)
        YV     = Q(LVTXT+ 8)
        THV    = Q(LVTXT+ 9)
        DZTHV  = Q(LVTXT+10)
        ZV     = Q(LVTXT+11)
        DZDRV  = Q(LVTXT+14)
        DPHIV  = Q(LVTXT+16)
        DXYV   = Q(LVTXT+17)
        DTHV   = Q(LVTXT+18)
        DRZV   = Q(LVTXT+19)
        DEDXV  = Q(LVTXT+20) ! dE/dx from vertex chamber
      ENDIF
      DO I=1,KVTXT
        XDATA(I) = XX(I)
      ENDDO
  999 RETURN
C----------------------------------------------------------------------
C
      ENTRY UDST_VTXT_TAGS(KVTXT1,ID_VTXT)
C----------------------------------------------------------------------
      KVTXT1=KVTXT
      ID_VTXT=18
      CALL UDST_BOOK_GROUP(ID_VTXT,'VTXT',VTXT_TAGS,KVTXT)
C----------------------------------------------------------------------
      RETURN
      END
