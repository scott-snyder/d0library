       SUBROUTINE KPLGN(NPOLG,NVERP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To draw polygon on Evans & sutherland.
C-         this module displays a Nvertice polygon defined by VERTIC with the
C-         current attributes.
C-
C-   Inputs  :
C-              NPOLG          :  Number of polygons.
C-              NVERP(n)       :  Number of vertices of n-th polygon.
C-   Outputs :
C-   Controls:
C-
C-   Created   08-MAR-1990   SHAHRIAR ABACHI
C-   UPDATED   10-JUN-1990   SHAHRIAR ABACHI     Rendering nodes added.
C-
C----------------------------------------------------------------------
       IMPLICIT NONE
       EXTERNAL ERRHND
       INCLUDE 'D0$INC:SEGINF.INC/LIST'
       INCLUDE 'D0$INC:PRIMVR.INC/LIST'
       INCLUDE 'D0$INC:LINATT.INC/LIST'
       INCLUDE 'D0$INC:PLGATT.INC/LIST'
       INCLUDE 'D0$INC:GRFPAR.INC/LIST'
       INCLUDE 'D0$INC:NEWDI3.INC/LIST'
       INCLUDE 'D0$INC:PROCNS.INC/LIST'
C
       INCLUDE 'D0$INC:GDINEW.INC/LIST'
C
       INTEGER DIM,ISTRV
       PARAMETER (DIM = 250)
       INTEGER NVERP(1000000)
       INTEGER NPOLG, NRS
       REAL VERTP(4,DIM)
       REAL NORMALS(DIM)
       LOGICAL*1 COPLANAR, NORMALSPEC,VEDGES(DIM)
       REAL OPAQUE, DIFFUS
       INTEGER ISPECU, NVR
       CHARACTER*4 PRIMI
       INTEGER DIMENS, I,J,K,ICOL, EEFCT
       REAL  VINTEN, ZILUM, SEC
       LOGICAL FIRST
       DATA NRS, FIRST, SEC /0, .TRUE., 2.5/
C
       CALL KPRIM(PRIMI)
       IF (THREED) THEN
         DIMENS = 3
       ELSE
         DIMENS = 2
       ENDIF
       NVR = 0
       DO I=1,NPOLG
         NVR = NVR + NVERP(I)
       ENDDO
       IF(.NOT. NUGDI) THEN
         DO 10 I=2,NVR
           POSLIN(I) = .TRUE.
   10    CONTINUE
       ENDIF
       POSLIN(1) = .FALSE.
       VINTEN = 1.0
       EEFCT = MOD(CPEDGE, 2)
C
       COPLANAR = .FALSE.
       NORMALSPEC = .FALSE.
       OPAQUE = 0.6
       DIFFUS = 0.75
       ISPECU = 5
C
       IF(IREND .EQ. 3 .AND. CPINTR .GT. 0) THEN
         NRS = NRS + NVR * 50
         CALL PRSVST(NRS,ERRHND)
         CALL PATTR('ATR"', HUECOL(CPIDCO+1, 1),
     &               SATCOL(CPIDCO+1, 1),
     &               1.0, OPAQUE, DIFFUS, ISPECU, ERRHND)
       ENDIF
C
       ICOL = CPIDCO
       IF(EEFCT .EQ. 0) ICOL = CURCOL
C
       CALL PBEGS(PRIMI, ERRHND)
       IF(IREND .EQ. 3) CALL PSURRE('SURRN"', '"', ERRHND)
       CALL PSECOL('"', HUECOL(ICOL+1, 1), SATCOL(ICOL+1, 1),
     +               '"', ERRHND)
       ZILUM = 1.0
       IF(IREND .EQ. 3) CALL PILLUM('"', 0.,0.,ZILUM,
     &    HUECOL(CPIDCO+1, 1),SATCOL(CPIDCO+1, 1),0.9, 0.7, ERRHND)
       CALL PPLYGB('"', ERRHND)
       ISTRV = 0
       DO I=1,NPOLG
         DO J = 1,NVERP(I)
           VEDGES(J) = .TRUE.
           DO K=1,4
             VERTP(K,J) = VERTIC(K,J+ISTRV)
             IF(K .EQ. 4) VERTP(K,J) = VINTEN
           ENDDO
         ENDDO
CC         IF(IREND .EQ. 3) CALL PPLYGO(HUECOL(ICOL+1, 1),ERRHND)
         CALL PPLYGO(HUECOL(ICOL+1, 1),ERRHND)
         CALL PPLYGL(COPLANAR,NVERP(I),VERTP,VEDGES,NORMALSPEC,NORMALS,
     &             ERRHND)
         IF(IREND .EQ. 3) CALL PPLYGA('ATR"', ERRHND)
         ISTRV = ISTRV + NVERP(I)
       ENDDO
       CALL PPLYGE(ERRHND)
       CALL PENDS(ERRHND)
C
       CALL PINCL(PRIMI, INST, ERRHND)
C----------------------------------------------------------------------
       IF(IREND .EQ. 3 .AND. CPINTR .GT. 0) THEN
         CALL PPURGE(ERRHND)
         IF(FIRST) THEN
           CALL PFN('TURNONACP"', 'CONSTANT', ERRHND)
           FIRST = .FALSE.
         ENDIF
         CALL PCONN(PRIMI//'.SURRN"', 1, 1, 'TURNONACP"', ERRHND)
         CALL PCONN('TURNONACP"', 1, 1, 'TURNONDISPLAY', ERRHND)
         CALL PSNFIX(0, 2, 'TURNONACP"', ERRHND)
         CALL PSNFIX(7, 1, PRIMI//'.SURRN"', ERRHND)
         CALL PPURGE(ERRHND)
         CALL LIB$WAIT(SEC)
         CALL PDI('TURNONACP"', 1, 1, 'TURNONDISPLAY', ERRHND)
         CALL PDI(PRIMI//'.SURRN"', 1, 1, 'TURNONACP"', ERRHND)
CC         CALL PPURGE(ERRHND)
       ENDIF
C----------------------------------------------------------------------
  999  RETURN
       END
