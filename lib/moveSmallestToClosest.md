
```r
# pop: Matrix holding population data information. Latest pass is the last
# column.  dm: Distance matrix pass: Pass number
moveSmallestToClosest <- function(pop, dm, verbose = FALSE) {
    # Find the smallest city in the last column
    smallest <- minindex.nonzero(pop[, ncol(pop)])
    
    # Find the closest city that still has population, other than itself
    # Multiply the vactor of distances from the smallest city by the population
    # of all cities so that emptied out cities are ignored.
    closest <- minindex.nonzero(dm[smallest, ] * pop[, ncol(pop)])
    
    if (verbose) {
        printf("PASS %d: Move %s (%d) to %s (%d) a distance of %f miles", ncol(pop), 
            rownames(dm)[smallest], pop[smallest, 1], rownames(dm)[closest], 
            pop[closest, 1], dm[smallest, closest])
    }
    
    # Record the pass in the pop
    pass <- pop[, ncol(pop)]
    pass[closest] <- pass[closest] + pass[smallest]
    pass[smallest] <- 0
    pop <- cbind(pop, pass)
    
    return(pop)
}
```

