# Exercises for Chapter 24

**1. Extend `adapter_db1` so that calling `adapter_db1:new(persistent)` creates a tuple module with a persistent data store.**

**2. Write a key-value store that stores small values in memory and large values on disk. Make an module that makes this have the same interface as the previous adapter in this chapter.**

**3. Make a key value store where some key-value pairs are persistent and others are transient. Calling `put(Key, memory, Val)` will store a key-value pair in memory, `put(Key, disk, Val)` should store the data on disk. Use a pair of processes to do this, one for the persistent store and one for the disk store. Reuse the earlier code in the chapter.**
