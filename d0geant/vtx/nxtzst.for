      SUBROUTINE NXTZST(IZFRST,NZ,LAYERZ,ENDLAZ)
C----------------------------------------------------------------------
C-   Purposes and Methods :
C-  Searches through z-strip address array ZDAT_ADDR in /VZDATA/ in the order
C-  given by ordering index array ZDAT_INDX in /VZDATA/ for the next group of
C-  z-strip hits with the same address (i.e. on the same z-strip).
C-   Inputs  : data in common VZDATA
C-   Outputs :
C-     IZFRST is the position in the index array IZINDX of the first hit
C-            on this z-strip.
C-     NZ     is the number of hits on this z-strip.
C-     LAYERZ is the z layer number (0-5).
C-     ENDLAZ is .TRUE. for the last hit in LAYERZ, .FALSE. otherwise.  Needed
C-            because the z strip arraycontains two z layers at the same
C-            time, the inner and outer z layers of one chamber layer.
C-  The z-strip address is IZADR(IZINDX(IZFRST)).
C-  If there are no more hits, NZ=0, IZFRST=0, LAYERZ=-1 and ENDLAZ=.FALSE.
C-
C-  This routine assumes NZHITS.LE.NZHTMX, as has been required in BLVZDA
C-
C-   Controls: called by BLVZDA
C-
C-       T. Trippe, Sep. 22, 1986
C-   Modified 17-NOV-1989 P. Grudberg - new common VZDATA instead of ZHITSV
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VZDATA.INC/LIST'
C
      INTEGER IZFRST, NZ, IZCOMP, IAFRST, IACOMP, LAYERZ, LAZCMP
      LOGICAL INIT, ENDLAZ
      DATA INIT / .TRUE. /
C----------------------------------------------------------------------
C
C ****  Initialize at the beginning of processing the two z layers associated
C ****  with one wire layer
C
      IF (INIT) THEN
        IZFRST = 1
        IAFRST = ZDAT_ADDR(ZDAT_INDX(IZFRST))   ! channel address
        LAYERZ = IBITS(IAFRST,9,3)       ! z layer (0-5)
        INIT = .FALSE.
      ELSE
        IZFRST = IZCOMP
        IAFRST = IACOMP
        LAYERZ = LAZCMP
      ENDIF
C
C ****  Have all the hits been processed?
C
      IF ( IZFRST .GT. NZDATA ) THEN
        IZFRST = 0
        NZ = 0
        LAYERZ = -1
        ENDLAZ = .FALSE.
        INIT = .TRUE.
        GO TO 999
      END IF
C
C **** So far, one hit with the address IAFRST has been found.
C **** Loop to find all other hits with same z-strip address.
C
      NZ = 1
   10 IZCOMP = IZFRST + NZ
      IF ( IZCOMP .LE. NZDATA ) THEN
        IACOMP = ZDAT_ADDR(ZDAT_INDX(IZCOMP))
        IF (IACOMP .EQ. IAFRST) THEN
          NZ = NZ+1
          GO TO 10
        END IF
C
C **** Is this the end of a layer?
C
        LAZCMP = IBITS(IACOMP,9,3)              ! z layer (0-5)
        IF (LAZCMP .EQ. LAYERZ) THEN
          ENDLAZ = .FALSE.
        ELSE
          ENDLAZ = .TRUE.
        ENDIF
      ELSE
        ENDLAZ = .TRUE.
      END IF
C
  999 RETURN
      END
