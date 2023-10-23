import time

def benchmark2(n):
    before = time.time()
    f = 0
    for i in range(n):
        f = f + 1
    print(f)
    after = time.time()
    print(int((after - before) * 1000), end="")
    print("ms")

benchmark2(1000000)
    