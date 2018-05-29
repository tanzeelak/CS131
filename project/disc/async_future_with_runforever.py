import asyncio

async def slow_operation(future):
    await asyncio.sleep(1)
    future.set_result('Future is done!')

def got_result(future):
    print(future.result())
    loop.stop()

loop = asyncio.get_event_loop()
future = asyncio.Future()
asyncio.ensure_future(slow_operation(future))

# When the loop is supposed to run infinitely.
# Such run_until_complete(...) function can't work with run_forever(). 
future.add_done_callback(got_result)

try:
    loop.run_forever()
finally:
    loop.close()