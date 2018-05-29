import asyncio
import aiohttp

async def fetch_page(session, url):
    with aiohttp.Timeout(10):
		# Now, we are sending our HTTP request (GET method) to the HTTP server.
		# Even it takes too much time, await keyword will successfully "yield" computing resource to other functions.
        async with session.get(url) as response:
            assert response.status == 200
            return await response.read()

loop = asyncio.get_event_loop()
with aiohttp.ClientSession(loop=loop) as session:
    content = loop.run_until_complete(
        fetch_page(session, 'http://python.org'))
    print(content)
loop.close()