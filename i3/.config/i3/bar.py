import time

def main():
    x = 0
    while True:
        print('Hello, world', x, flush=True)
        x += 1
        time.sleep(1)

if __name__=='__main__':
    main()
