      SUBROUTINE OBJECT_EVENT(IDX,NMAX,ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return quantities for object EVENT.
C-
C-   Inputs  : IDX      [I]   Object Number (Unused)
C-             NMAX     [I]   Maximum number of quantities to return
C-   Outputs : ARRAY(*) [R]   Array of quantities
C-
C
C ****  STORE ALL HEAD BANK INFO EXCEPT:
C ****  RECORD_TYPE
C ****  MICRONAME
C ****  LOCAL RUN#
C ****  GLOBAL RUN#
C
C-
C-            HEAD word
C-
C- ARRAY(1-2) 4-5       I  DATE_TIME (standard VAX 64 bit format)
C- ARRAY(3-4) 7-8       I  INPUT_EVENT # (40 bit Level 1 trigger number)
C-                         word 7 is lower 24 bits;    word 8 is upper 16 bits
C-                         Thus, both words are guaranteed positive
C- ARRAY(5)     9       I  OUTPUT_EVENT # (counts accepted events;
C-                                         assigned by logger)
C- ARRAY(6)    10       I  EVENT_TYPE
C- ARRAY(7)    11       I  TRIGGER BITS
C- ARRAY(8)    13       I  CODE VERSION
C- ARRAY(9)    14       I  HEADER VERSION
C- ARRAY(10)   15       I  FILTER BITS  31-0
C- ARRAY(11)   16       I  FILTER BITS  63-32
C- ARRAY(12)   17       I  FILTER BITS  95-64
C- ARRAY(13)   18       I  FILTER BITS  127-96
C- ARRAY(14-23)19-28    I  RECO BITS
C- ARRAY(24)   29       I  ERROR CODE put on by Level-2 framework
C- ARRAY(25)   30       I  FLAG       Bit 0: Set=Micro-blanking ON
C-
C-   Controls:
C-
C-   Created  21-APR-1993   Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Corrected FLINT complaints
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX, NMAX
      REAL    ARRAY(*)
      INTEGER NOBJS, NSIZE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER MAXBUF
      PARAMETER( MAXBUF = 50 )
      INTEGER IBUFFER(MAXBUF), NN
      REAL    BUFFER(MAXBUF)
      EQUIVALENCE (IBUFFER(1),BUFFER(1))
C----------------------------------------------------------------------
      INTEGER NTOTAL
      PARAMETER( NTOTAL = 25 )
C----------------------------------------------------------------------
C
C ****  OBJECT: EVENT
C
      NN = MIN(NMAX,NTOTAL)
      CALL VZERO(BUFFER,NN)
      CALL UCOPY(Q(LHEAD+4),  BUFFER(1),2)
      CALL UCOPY(Q(LHEAD+7),  BUFFER(3),5)
      CALL UCOPY(Q(LHEAD+13), BUFFER(8),18)
      CALL UCOPY(BUFFER,ARRAY,NN)
      RETURN
C
      ENTRY NOBJ_EVENTS(NOBJS,NSIZE)
C
      NOBJS = 1
      NSIZE = NTOTAL
C
      RETURN
      END
