def foo():
    yield 1
    yield 2
    yield 3

a = foo()  
print(a)        # Generator object        
print(next(a))  # 1
print(next(a))  # 2
print(next(a))  # 3
print(next(a))  # StopIteration() exception
