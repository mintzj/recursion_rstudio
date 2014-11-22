library(hash)

cache <- hash()
cache[["bob"]] <- 24
cache[["joe"]] <- 27
print(cache)

print(cache[["bob"]])

print(has.key("bob", cache))
print(has.key("jerry", cache))

print(keys(cache))
cache[["bob"]] <- NULL

print(cache)
