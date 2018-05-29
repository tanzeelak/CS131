# TA Note

# The variable 'i' is a class variable.
# For an instance variable, use with the 'self' object.

# The 'self' object can be renamed.
#     def __iter__(s):
#        return s
class firstn(object):
    i = 123
    def __init__(self, n):
        self.n = n
        self.num, self.nums = 0, []
    def __iter__(self):
        return self
    def __next__(self): 
        return self.next()
    def next(self):
        if self.num < self.n:
            cur, self.num = self.num, self.num + 1
            return cur
        else:
            raise StopIteration()
sum_of_first_n = sum(firstn(10000000))
print (sum_of_first_n)