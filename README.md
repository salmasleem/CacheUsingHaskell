# Cache Project in Haskell
## Description 
Cache is a component that stores data so that future requests for that data can be served faster. Recently or frequently used data are stored temporarily in the cache to speed up the retrieval of the data by reducing the time needed for accessing cache clients data such as the CPU.

In this project, me and my colleagues presented a successful implementation of a simplified CPU cache system that works by retrieving data (from cache if possible, otherwise
from memory) given the memory addresses of the data and successfully updating the cache upon the data retrieval.
The idea of a cache is that it introduces hierarchy into memory systems. Thus, instead of having one level for a large memory which will probably be slow because it needs to be cheap, we can have multiple levels.

In this project we have assumed that we have two level memory Hierarchy.

## The operation
1.  A specific block of data is requested. The block is identified by its location in the main memory
2.  First, the system has to look if the block at the required address was mapped somewhere in the cache.
3.  If found in the cache, there is no need to go to the main memory.
4.  If the data is not available in the cache then:

    a.  The system has to rad the block from the main memory.
    <br/>
    b. The bread block has to be placed in the cache.
     <br/>
    c. The place where the block is placed in the cache depends on the type of the cache as shown in the next section.
     <br/>
    d. If the chosen cache location(line) is not empty, then the block already in the cache will be overwritten by the new block read from the main memory.
    
## Authors 
1. [Salma Sleem](https://github.com/salmasleem)
2. 
3. 
4. 

