************************************************************************
*
*       INFOCACHE.INC
*
************************************************************************
*
        INCLUDE '/d0library/params/infocache.params'
*
**         lock status block with value block and name
        STRUCTURE /ICLSTB/
          INTEGER*2     STATUS, NULL
          INTEGER*4     ID, LVB(4)
          CHARACTER*(IC__MAXLEN+7) NAME    
        END STRUCTURE
        STRUCTURE /INFOCACHE/
          CHARACTER*(IC__MAXLEN+4) ID       
          INTEGER*2 MODE        
          INTEGER*2 TYPE        
          INTEGER STAT          
          INTEGER AUTH          
          INTEGER LOCK          
          INTEGER BCID          
          INTEGER AST           
          INTEGER UAST          
          INTEGER BAST          
          RECORD /ICLSTB/ LKST  
        END STRUCTURE
        RECORD /INFOCACHE/ CACHE(IC__MAXCACHE+1)
        COMMON /ICACHSAV/ CACHE,NCACHE

        REAL DEFTIMOUT /20./  
