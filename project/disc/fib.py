import time

def fib():
	a, b = 0, 1
	while True:
		yield a
		a, b = b, a+b

for i in fib():
	print(i)
	time.sleep(0.1)
