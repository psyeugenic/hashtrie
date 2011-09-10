Hashtries
=========

**pht** is a simple hamt with bitcount tricks.
Should be used with [erlang/otp branch](https://github.com/psyeugenic/otp/commits/egil/pht-features)
Smaller memory footprint than **ht** but could be better. (We don't need the extraword in records)
Will probably rework this as an built-in-datastructure.

**ht** is a hashtrie with good insert properties and descent lookup properties.
It has similar properties to dict in lookup, but faster inserts. 

The test data indicates very good properties but a big disclaimer here,
- Update seems very slow.
- The overall structure consumes more memory then dict.
- This is not very well tested so there might be some problems somewhere =)

Also this is just a concept, not intended for production.

![data put](https://github.com/psyeugenic/hashtrie/raw/master/example/data_put.png)
![data get](https://github.com/psyeugenic/hashtrie/raw/master/example/data_get.png)


**tarray** tree array. Work in progress.
